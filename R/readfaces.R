##' Partial read for .bgm files
##'
##' Read geometry from BGM files
##'
##' @title Read BGM
##' @param x bgm file
##' @param sp return Spatial object or list
##' @param densify factor to increase density of vertices
##' @param ... arguments passed to \code{\link{gcIntermediate}}
##' @return A SpatialLinesDataFrame if \code{sp} is \code{TRUE},
##' otherwise a list of 2x2 matrices containing the end point
##' coordinates, the list is a named vector with the label for each
##' face
##' @importFrom geosphere gcIntermediate
##' @examples
##' ## pull in faces as a SpatialLinesDataFrame, densified to 30km between vertices
##' \dontrun{
##' x <- read.faces("file.bgm", densify = 30, sp = TRUE)
##' }
##' @noRd
read.faces <- function(x, sp = TRUE, densify, ...) {
  xlines <- readLines(x)
  
  ## assume PROJ.4 string occurs at first mention of "projection"
  projind <- grep("^projection", xlines)[1L]
  prjstring <- gsub("^projection ", "", xlines[projind])
  
  facenumind <- grep("# Face", xlines)
  facenum <- sapply(strsplit(xlines[facenumind], " "), function(x) x[length(x)])
  strfacept <- function(x) do.call("rbind", lapply(strsplit(x, " "), function(x) as.numeric(x[-1])))
  
  xf <- lapply(facenumind, function(x) strfacept(xlines[c(x + c(1L, 2L))]))
  
  if (!missing(densify)) {
    if (!densify > 1) stop("densify must be an integer > 1")
    ##xf <- lapply(xf, function(x) gcIntermediate(x[1L,], x[2L,], n = densify, addStartEnd = TRUE, ...))
    xf <- lapply(xf, densifymindist, mindist = densify, longlat = isLonLat(prjstring))
  }
  if (sp) {
    sLines <- vector("list", length(facenum))
    for (i in seq_along(sLines)) {
      ## single pair coordinate line, each a Lines object (so each can have attributes)
      sLines[[i]] <- Lines(list(Line(xf[[i]])), facenum[i])
    }
    spLines <- SpatialLines(sLines, proj4string = CRS(prjstring, doCheckCRSArgs = FALSE))
    spdf <- SpatialLinesDataFrame(spLines, data.frame(id = facenum, row.names = facenum))
    return(spdf)
  }
  xf
}