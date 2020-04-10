#' Title
#'
#' @param x bgm file name
#'
#' @return
#' @noRd
#' @examples
#' x <- bgmfiles::bgmfiles("antarctica_28")
#' x <- unproject_bgm(x)
#' writeLines(x, "LLversion.bgm")
unproject_bgm <- function(x) {
  bgm <- bgmfile(x)
  
  txt <- readLines(x)
  patts <- c("inside", "\\.vert ", "_vert", "\\.p1", "\\.p2")

  for (i in seq_along(patts)) {  
  idx <- grep(patts[i], txt, value = FALSE)
  asub <- txt[idx]
  coord <- read_coord(asub)
  coordLL <- reproj::reproj(coord, source = bgm$extra$projection, 
                            target = "+proj=longlat +datum=WGS84")
  print(range(coordLL))
  replace <- write_coord(coordLL, read_lab(asub))
  txt[idx] <- replace
  }
  
  ## update the projection!
  txt[grep("projection", txt)[1]] <- "+proj=longlat +datum=WGS84"
  
  txt  
}

read_lab <- function(x) {
  do.call(rbind, lapply(strsplit(x, "\\s+"), function(x) head(x, 1)))
}
read_coord <- function(x) {
  out <- do.call(rbind, lapply(strsplit(x, "\\s+"), function(x) utils::tail(x, 2)))
  mode(out) <- "double"
  out
}

write_coord <- function(x, lab) {
  sprintf("%s %f %f", lab, x[,1], x[,2])
}

