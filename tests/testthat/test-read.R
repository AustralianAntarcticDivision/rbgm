context("reading bgm")

namenumber <- function(x) {
  x <- unlist(lapply(strsplit(x, "\\."), "[", 1))
  as.integer(gsub("[[:alpha:]]|[[:punct:]]", "", x))
}
removetrailing <- function(x) gsub("\\s+$", "", x)
removeleading <- function(x) gsub("^\\s+", "", x)
singlespace <- function(x) gsub("\\s+", " ", x)
addnewline <- function(x) sprintf("%s\n", x)

library(dplyr)
files <- bgmfiles::bgmfiles()
basetoks <- c("#", "bnd_vert", "box.area", "box.botz", "box.horizmix", "box.ibox",
              "box.iface", "box.inside", "box.label", "box.nconn", "box.vert",
              "box.vertmix", "face.cs", "face.length", "face.lr", "face.p",
              "maxwcbotz", "nbox", "nface", "projection")

test_that("known tokens are always the same\n", {
## this test
  ## removes leading and trailing whitespace
  ## converts all whitespace to single spaces (including tabs, not include new lines)
  ## splits every line in the file on " " and lists the unique first tokens and sorts them
  ## compares that list to basetoks above
  ## if this test fails it may mean we have a new kind of file
  ## with a new bit of information
  ## or without something the other files have
for (i in seq_along(files)) {
  f <- files[i]
  d <- data_frame(tx = readLines(f))
  xtoks <- sort(unique(unlist(lapply(strsplit(singlespace(removetrailing(removeleading(d$tx))), " "), function(x) gsub("[[:digit:]]", "", x[1L])))))
  ## remove any lines that start with "##A"nyletter 
  bad <- grep("^##[[:alpha:]]", xtoks)
  if (length(bad) > 0)  xtoks <- xtoks[-bad]
  #print(all(xtoks == basetoks))
  testthat::expect_that(xtoks, testthat::is_identical_to(basetoks))
}
})

# 
# verts <- function(x, type, app) {
#   x1 <- grep(sprintf("^%s[0-9]+.%s\\s", type, app), x, value = TRUE)
#   x2 <- singlespace(x1)
#   x3 <- removetrailing(x2)
#   x4 <- removeleading(x3)
#   x5 <- addnewline(x4)
# 
#   x6 <- setNames(read.delim(text = x5, sep= " ", header = FALSE, stringsAsFactors = FALSE), c("name", "x", "y"))
#   x6$number <- namenumber(x6$name)
#   as_data_frame(x6)
# }
# bverts <- verts(d$tx, "box", "vert")
# f1verts <- verts(d$tx, "face", "p1")
# f2verts <- verts(d$tx, "face", "p2")
# 
# 
# 
