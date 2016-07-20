context("reading bgm")


## extract the number in a 
namenumber <- function(x) {
  x <- unlist(lapply(strsplit(x, "\\."), "[", 1))
  as.integer(gsub("[[:alpha:]]|[[:punct:]]", "", x))
}
removetrailing <- function(x) gsub("\\s+$", "", x)
removeleading <- function(x) gsub("^\\s+", "", x)
singlespace <- function(x) gsub("\\s+", " ", x)
addnewline <- function(x) sprintf("%s\n", x)
spaceseparate1 <- function(x) sort(unique(unlist(lapply(strsplit(singlespace(removetrailing(removeleading(x))), " "), function(a) gsub("[[:digit:]]", "", a[1L])))))
dotvertlines <- function(x, type, app) {
  grep(sprintf("^%s[0-9]+.%s\\s", type, app), x, value = TRUE)
}
vertlines <- function(x, type) {
  grep(sprintf("^%s", type), x, value = TRUE)
}
verts <- function(x, type, app = "") {
  x1 <- if (app == "") {vertlines(x, type)}  else {x <- dotvertlines(x, type, app)}
  x2 <- singlespace(x1)
  x3 <- removetrailing(x2)
  x4 <- removeleading(x3)
  x5 <- addnewline(x4)
  
  x6 <- setNames(read.delim(text = x5, sep= " ", header = FALSE, stringsAsFactors = FALSE), c("name", "x", "y"))
  if (!app == "") x6$number <- namenumber(x6$name)
  tibble::as_tibble(x6)
}


library(dplyr)
files <- bgmfiles::bgmfiles()
basetoks <- c("#", "bnd_vert", "box.area", "box.botz", "box.horizmix", "box.ibox",
              "box.iface", "box.inside", "box.label", "box.nconn", "box.vert",
              "box.vertmix", "face.cs", "face.length", "face.lr", "face.p",
              "maxwcbotz", "nbox", "nface", "projection")


for (i in seq_along(files)) {
  f <- files[i]
  d <- tibble::tibble(tx = readLines(f))
  xtoks <- spaceseparate1(d$tx) 
  ## remove any lines that start with "##A"nyletter 
  bad <- grep("^##[[:alpha:]]", xtoks)
  if (length(bad) > 0)  xtoks <- xtoks[-bad]
  #print(all(xtoks == basetoks))
  
  test_that("known tokens are always the same", {
    ## this test
    ## removes leading and trailing whitespace
    ## converts all whitespace to single spaces (including tabs, not include new lines)
    ## removes any lines that begin with two hashes and a letter "^##[[:alpha:]]"
    ## splits every line in the file on " " and lists the unique first tokens and sorts them
    ## compares that list to basetoks above
    ## if this test fails it may mean we have a new kind of file
    ## with a new bit of information
    ## or without something the other files have
    testthat::expect_that(xtoks, testthat::is_identical_to(basetoks))
    
  })
  
  test_that("all vertices are read as data frames", {
    
    testthat::expect_that(bverts <- verts(d$tx, "box", "vert"), is_a("tbl_df"))
    testthat::expect_that(f1verts <- verts(d$tx, "face", "p1"), is_a("tbl_df"))
    testthat::expect_that(f2verts <- verts(d$tx, "face", "p2"), is_a("tbl_df"))
    testthat::expect_that(bndverts <- verts(d$tx, "bnd_vert", ""), is_a("tbl_df"))
    
  })
  
}

