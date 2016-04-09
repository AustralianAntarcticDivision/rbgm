
#' Extract 
#'
#' Take the output of \code{\link{read_bgm}} and return a \code{\link[sp]{Spatial}} object. 
#' @param bgm BGM file
#'
#' @return Spatial* object 
#' \itemize{
#' \item boxSpatial \code{\link[sp]{SpatialPolygonsDataFrame}} 
#' \item faceSpatial  \code{\link[sp]{SpatialLinesDataFrame}} 
#' \item boundarySpatial \code{\link[sp]{SpatialPolygonsDataFrame}} 
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
    dplyr::select_(quote(-.vx0), quote(-.bx0))
  
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
  data <- bgm$faces 
  data$label <- sprintf("face%i", data$.fx0)
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
