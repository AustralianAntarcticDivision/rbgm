#' Utilities for BGM files for Atlantis
#'
#' Tools for handling network data for Atlantis from box-geometry
#' model (BGM) files
#'
#' @importFrom dplyr row_number
#' @importFrom raster isLonLat 
#' @importFrom sp spDistsN1 Line Lines SpatialLines CRS SpatialLinesDataFrame
#' @name rbgm-package
#' @docType package
#' @author Michael D. Sumner
NULL



#' Extract 
#'
#' Take the output of \code{\link{read_bgm}} and return a \code{\link[sp]{Spatial}} object. 
#' @param bgm BGM file
#'
#' @return Spatial* object 
#' \itemize{
#' \item boxSpatial \code{\link[sp]{SpatialPolygonsDataFrame}} 
#' \item faceSpatial  \code{\link[sp]{SpatialLinesDataFrame}} 
#' }
#' @export
#' @rdname rbgm-Spatial
#' @importFrom sp point.in.polygon Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame 
#' @importFrom dplyr inner_join select
#' @examples
#' fname <- system.file("extdata", "Antarctica_28.bgm", package = "rbgm")
#' bgm <- read_bgm(fname)
#' spdf <- boxSpatial(bgm)
#' sldf <- faceSpatial(bgm)
#' 
#' plot(boxSpatial(bgm), col = grey(seq(0, 1, length = nrow(b$boxes))))
#' plot(faceSpatial(bgm), col = rainbow(nrow(b$faces)), lwd = 2,  add = TRUE)
boxSpatial <- function(bgm) {
  data <- bgm$boxes
  boxverts <- data %>% select_(".bx0", "label") %>% 
    inner_join(bgm$boxesXverts, ".bx0") %>% 
    inner_join(bgm$vertices, ".vx0") %>% 
    dplyr::select(quote(-.vx0), quote(-.bx0))
  
  data <- as.data.frame(data)
  rownames(data) <- data$label
  ## test for inside boundary
  inside <- point.in.polygon(data$insideX, data$insideY, bgm$boundaryvertices$x, bgm$boundaryvertices$y)
  data$boundary <- inside < 1
  SpatialPolygonsDataFrame(sptableBox(boxverts, object = "label", crs = bgm$extra["projection"]), data)
}

sptableBox <- function(x, object = ".bx0", xy = c("x", "y"), crs = NA_character_) {
  p1 <- lapply(split(x[, xy], x[[object]]), function(x) Polygon(as.matrix(x)))
  IDs <- unique(x[[object]])
  p1 <- p1[IDs]  ## override the lex sort
  p2 <- lapply(seq_along(p1), function(ii) Polygons(p1[ii], IDs[ii]))
  SpatialPolygons(p2, proj4string = CRS(crs))             
} 

#' @export
#' @rdname rbgm-Spatial
boundarySpatial <- function(bgm) {
  SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(bgm$boundaryvertices %>% dplyr::select_("x", "y") %>% as.matrix)), "bdy"))), 
                           data.frame(label = "boundary", row.names = "bdy"))
}

  
#' @export
#' @rdname rbgm-Spatial
faceSpatial <- function(bgm) {
  data <- bgm$faces  %>% dplyr::mutate_("label" = sprintf("face%i", .fx0))
  faceverts <- data %>% dplyr::select_(".fx0", "label") %>% 
    inner_join(bgm$facesXverts, ".fx0") %>% 
    inner_join(bgm$vertices, ".vx0") %>% 
    select_(quote(-.vx0), quote(-.fx0))
  
  data <- as.data.frame(data)
  rownames(data) <- data$label
  SpatialLinesDataFrame(sptableFace(faceverts, object = "label", crs = bgm$extra["projection"]), data)
  
}

sptableFace <- function(x, object = ".fx0", xy = c("x", "y"), crs = NA_character_) {
  l1 <- lapply(split(x[, xy], x[[object]]), function(x) Line(as.matrix(x)))
  IDs <- unique(x[[object]])
  l2 <- lapply(seq_along(l1), function(ii) Lines(l1[ii], IDs[ii]))
  SpatialLines(l2, proj4string = CRS(crs))
}

##' Partial read for .bgm files
##'
##' Read geometry from BGM files
##'
##' @title Read BGM
##' @param x path to a bgm file
##' @export
#' @importFrom dplyr %>% select distinct_ as_data_frame data_frame arrange bind_rows bind_cols distinct mutate inner_join
read_bgm <- function(x) {
  tx <- readLines(x)  
  

  ## all indexes
  facesInd <- grep("^face", tx)
  boxesInd <- grep("^box", tx)
  bnd_vertInd <- grep("^bnd_vert", tx)
  ## all comments
  hashInd <- grep("^#", tx)
  
  ## unique starting tokens
  ust <- sort(unique(sapply(strsplit(tx[-c(facesInd, boxesInd, bnd_vertInd, hashInd)], "\\s+"), "[", 1)))
  extra <- sapply(ust, function(x) gsub("\\s+$", "", gsub("^\\s+", "", gsub(x, "", grep(x, tx, value = TRUE)))))
  ## what's left
  extra["projection"] <- sprintf("+%s", gsub(" ", " +", extra["projection"]))
  
  faceslist <- grepItems(tx[facesInd], "face", as.numeric(extra["nface"]))
  ## remove len, cs, lr from faceparse, all belong on the face not the face verts
  faceverts <-  do.call(dplyr::bind_rows, lapply(seq_along(faceslist), function(xi) {a <- facevertsparse(faceslist[[xi]]); a$.fx0 <- xi - 1; a}))
  faces <-   do.call(dplyr::bind_rows, lapply(seq_along(faceslist), function(xi) {a <- facedataparse(faceslist[[xi]]); a$.fx0 <- xi - 1; a}))
 
  
  boxeslist <- grepItems(tx[boxesInd], "box", as.numeric(extra["nbox"]))
  boxes0 <- lapply(seq_along(boxeslist), function(xi) {a <- boxparse(boxeslist[[xi]]); a$.bx0 <- xi - 1; a})
  ## we only need boxverts for non-face boxes (boundary faces), but use to check data sense
  boxverts <- do.call(dplyr::bind_rows, lapply(seq_along(boxes0), function(xa) {aa <- boxes0[[xa]]$verts; .bx0 = rep(xa - 1, nrow(boxes0[[xa]]$verts)); aa$.bx0 <- .bx0; aa}))
  boxes<- do.call(dplyr::bind_rows, 
                  lapply(boxes0, function(a) dplyr::bind_cols(dplyr::as_data_frame(a[["meta"]]), 
                                                              dplyr::as_data_frame(a[c("insideX", "insideY", ".bx0")]))))
  facesXboxes <- do.call(dplyr::bind_rows, lapply(boxes0, "[[", "faces"))
  
  bnd_verts <- do.call(rbind, lapply(strsplit(tx[bnd_vertInd], "\\s+"), function(x) as.numeric(x[-1])))
  boundaryverts <- data_frame(x = bnd_verts[,1], y = bnd_verts[,2], bndvert = seq(nrow(bnd_verts)))
  
for (i in seq(ncol(boxes))) {
  if (is.character(boxes[[i]])) {
    boxes[[i]] <- type.convert(boxes[[i]], as.is = TRUE)
  }
}
  
  ## OUTPUT
  ## vertices     x,y, .vx0
  ## facesXverts  .vx0, .fx0, .p0 ## .po is p1/p2 ends of face
  ## faces        .fx0, length, cos0, sin0, leftbox, rightbox  ## cos/sin rel. to (0, 0) left/right looking from p2
  ## facesXboxes  .bx0, .fx0
  ## boxesXverts  .bx0, .vx0
  ## boxes        .bx0, label, insideX, insideY, nconn, botz, area, vertmix, horizmix
  
  
    
  ## I think bnd_verts already all included in box_verts
  vertices <- bind_rows(faceverts[, c("x", "y")], boxverts[, c("x", "y")], boundaryverts[, c("x", "y")]) %>% distinct_() %>% dplyr::arrange_("x", "y") %>% mutate(.vx0 = row_number())
  
  facesXverts <- faceverts %>% mutate(.p0 = rep(1:2, length = nrow(faceverts)))  %>% inner_join(vertices, c("x" = "x", "y" = "y")) %>% dplyr::select_(quote(-x), quote(-y))
  
  boxesXverts <- boxverts %>% inner_join(vertices, c("x" = "x", "y" = "y")) %>% dplyr::select_(quote(-x), quote(-y))

 # allverts <- allverts %>% select(x, y)
  list(vertices = vertices, facesXverts = facesXverts, faces = faces, facesXboxes = facesXboxes, boxesXverts = boxesXverts, boxes = boxes, boundaryvertices = boundaryverts, extra = extra)
}



#' @importFrom dplyr select_
box2pslg <- function(x) {
  x <- head(x$verts, -1) %>% dplyr::select_("x", "y") %>% as.matrix
  RTriangle::pslg(x, S = segmaker(x))
}
segmaker <- function(x) {
  on.exit(options(op))
  op <- options(warn = -1)
  matrix(seq(nrow(x)), nrow = nrow(x) + 1, ncol  = 2)[seq(nrow(x)), ]
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



