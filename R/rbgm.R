#' Utilities for BGM files for Atlantis
#'
#' Tools for handling network data for Atlantis from box-geometry
#' model (BGM) files
#'
#' @name rbgm-package
#' @docType package
#' @author Michael D. Sumner
#' @import raster sp
NULL


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
##' @export
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
        spLines <- SpatialLines(sLines, proj4string = CRS(prjstring))
        spdf <- SpatialLinesDataFrame(spLines, data.frame(id = facenum, row.names = facenum))
        return(spdf)
    }
    xf
}

##' Densify vertices so that no span is  is greater than mindist
##'
##' Using great circle distance in km add vertices to a matrix of longitude latitude coordinates
##' @title  Densify lonlat vertices
##' @param x matrix of 2 coordinates
##' @param mindist minimum distance in km if \code{longlat} is \code{TRUE}, otherwise in the units of the projection used (probably metres)
##' @param longlat calculate distance on great circle or Cartesian
##' @return matrix of (possibly) densified coordinates
densifymindist <- function(x, mindist, longlat = longlat) {
    if (missing(mindist)) {
        warning("No minimum distance specified, no densifying done")
        return(x)
    }

    dist <- spDistsN1(x[1L,,drop=FALSE], x[2L,,drop=FALSE], longlat = TRUE)
    if (dist >= mindist) {
        n <- dist %/% mindist
        x <- gcIntermediate(x[1L,], x[2L,], n = n, addStartEnd = TRUE)
    }
    x

}


##' Build dz layer values for Atlantis from a bottom value, up through successive intervals. Each value is the positive offset required to rise
##' to the top of the current interval. 
##'
##' Offset values are returned to move from \code{z} against the intervals in \code{zlayers}. The intervals are assumed
##' to be sorted and increasing in value from \code{-Inf}inity. Once the maximum layer is reached the result is padded
##' by that top value. 
##' 
##' @title Build Atlantis dz Values
##' @param z lowermost value
##' @param zlayers intervals of layer values
##' @return numeric vector of offset values
##' @export
##' @examples 
##' ## sanity tests
##' build_dz(-5000)
##' build_dz(-1500)
##' ##build_dz(300)  ## error
##' build_dz(0)    ## ok


##' ## data
##' dd <- c(-4396.49, -2100.84, -4448.81, -411.96, -2703.56, -5232.96, 
##'        -4176.25, -2862.37, -3795.6, -1024.64, -897.93, -1695.82, -4949.76, 
##'     -5264.24, -2886.81)
##' ## all values in a matrix for checking
##' ## [zlayers, dd]
##'dzvals <- sapply(dd, build_dz)

##' ## process into text
##' f1 <- function(x) sprintf("somelabel,%i,%s", x, paste(build_dz(dd[x]), collapse = ","))
##' tex1 <- sapply(seq(length(dd)),  f1)
##' ## for example
##' f2 <- function(x) {
##' sprintf("morelabel,%i,%s", x, paste(as.integer(build_dz(dd[x])), collapse = ","))
##' }
##' tex2 <- sapply(seq(length(dd)),  f2)


build_dz <- function(z, zlayers = c(-Inf, -2000, -1000, -750, -400, -300, -200, -100, -50, -20, 0)) {
  if (length(z) > 1L) {
    z <- z[1L]
    warning("length of z is longer than 1, only the first element will be used")
  }
  layerindex <- findInterval(z, zlayers, rightmost.closed = TRUE) + 1L
  
  if (layerindex > length(zlayers)) stop("z is not in the intervals defined by zlayers")
  startdz <- zlayers[layerindex] - z
  out <- c(startdz, diff(zlayers[-seq(layerindex - 1)]))
  c(out, rep(max(zlayers), (length(zlayers) - 1) - length(out)))
}



