## a bit like spbabel::sp() or sfheaders::sf_pol
sptableBox <- function(x, object = ".bx0", xy = c("x", "y"), crs = NA_character_) {
  p1 <- lapply(split(x[, xy], x[[object]]), function(x) Polygon(as.matrix(x)))
  IDs <- unique(x[[object]])
  p1 <- p1[IDs]  ## override the lex sort
  p1 <- unname(p1)  ## save us, sp can't have named elements
  p2 <- lapply(seq_along(p1), function(ii) Polygons(p1[ii], IDs[ii]))
  SpatialPolygons(p2, proj4string = CRS(crs, doCheckCRSArgs = FALSE))             
} 


#' Convert to spatial format
#'
#' Take the output of \code{\link{bgmfile}} and return a \code{\link[sp]{Spatial}} object 
#' or a sf object. 
#' 
#'  Note that the `_sp` forms are aliased to original functions called `*Spatial`, and 
#'  now have `_sf` counterparts to return that format.  
#' @param bgm output of a BGM file, as returned by \code{\link{bgmfile}} 
#' 
#' @section Warning: 
#' The sf objects created by `box_sf()`, `node_sf()`, `face_sf()`, `boundary_sf()` and
#' `point_sf()` were not created by the sf package. They were created with 
#' reference to the sf format prior to November 2019. If you have problems 
#' it may be necessary to recreate the 'crs' part of the of the object with code like
#' `x <- box_sf(bgm); library(sf); st_crs(x) <- st_crs(attr(x$geometry, "crs")$proj)`. 
#' 
#' Get in touch ([create an issue](https://github.com/AustralianAntarcticDivision/rbgm/issues)) if you have any troubles. 
#' @return Spatial* object or sf object
#' \itemize{
#' \item box_sp \code{\link[sp]{SpatialPolygonsDataFrame}} 
#' \item face_sp  \code{\link[sp]{SpatialLinesDataFrame}} 
#' \item boundary_sp \code{\link[sp]{SpatialPolygonsDataFrame}} 
#' \item node_sp \code{\link[sp]{SpatialPointsDataFrame}} 
#' \item point_sp \code{\link[sp]{SpatialPointsDataFrame}} 
#' }
#' \itemize{
#' \item box_sf \code{sf with sfc_POLYGON column} 
#' \item face_sf  \code{sf with sfc_LINESTRING column} 
#' \item boundary_sf \code{sf with sfc_POLYGON column} 
#' \item node_sf \code{sf with sfc_POINT column} 
#' \item point_sf \code{sf with sfc_POINT column}
#' }
#' @export
#' @rdname rbgm-Spatial
#' @importFrom sp point.in.polygon Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame 
#' @examples
#' fname <- bgmfiles::bgmfiles(pattern = "antarctica_28")
#' bgm <- bgmfile(fname)
#' spdf <- box_sp(bgm)
#' sfdf <- box_sf(bgm)
#' sldf <- face_sp(bgm)
#' 
#' plot(spdf, col = grey(seq(0, 1, length = nrow(bgm$boxes))))
#' plot(sldf, col = rainbow(nrow(bgm$faces)), lwd = 2,  add = TRUE)
boxSpatial <- function(bgm) {
  data <- bgm$boxes
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  rownames(data) <- data$label

    
  boxverts <- data %>% dplyr::select(.data$.bx0, .data$label) %>% 
    dplyr::inner_join(bgm$boxesXverts, ".bx0") %>% 
    dplyr::inner_join(bgm$vertices, ".vx0") %>% 
    dplyr::select(-.data$.vx0, -.data$.bx0)
  
 ## test for inside boundary
  inside <- point.in.polygon(data$insideX, data$insideY, bgm$boundaryvertices$x, bgm$boundaryvertices$y)
  data$boundary <- inside < 1
  out <- SpatialPolygonsDataFrame(sptableBox(boxverts, object = "label", crs = bgm$extra$projection), data)
  ## add on box_id for bgmeriser
  out$box_id <- seq(0, nrow(out) - 1)
  out
}
#' @export
#' @rdname rbgm-Spatial
box_sp <- boxSpatial

#' @rdname rbgm-Spatial
#' @export
box_sf <- function(bgm) {
  data <- bgm$boxes
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  rownames(data) <- data$label
  
  
  boxverts <- data %>% dplyr::select(.data$.bx0, .data$label) %>% 
    dplyr::inner_join(bgm$boxesXverts, ".bx0") %>% 
    dplyr::inner_join(bgm$vertices, ".vx0") %>% 
    dplyr::select(-.data$.vx0, -.data$.bx0)
  
  ## test for inside boundary
  inside <- point.in.polygon(data$insideX, data$insideY, bgm$boundaryvertices$x, bgm$boundaryvertices$y)
  data$boundary <- inside < 1
  sfc <- sfheaders::sfc_polygon(boxverts, x = "x", y = "y", polygon_id = "label", linestring_id = "label")

    ## this is tricky, it seems bad not to use the going-forwards format for crs
  ## but we can't generate a WKT string without manual insertion or invoking GDAL/PROJ
  ## so obviously we stick with the older style and folks will have to apply
  ## workarounds until the tools get better
  attr(sfc, "crs") <- structure(list(epsg = NA_integer_, proj4string = bgm$extra$projection), class = "crs")
  
  data[["geometry"]] <- sfc
  attr(data, "sf_column") <- "geometry"
  class(data) <- c("sf", "tbl_df", "tbl", "data.frame")
  ## add on box_id for bgmeriser
  data[["box_id"]] <- seq(0, nrow(data) - 1)
  data
  
}




#' @export
#' @rdname rbgm-Spatial
boundarySpatial <- function(bgm) {
  SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(bgm$boundaryvertices %>% 
                          dplyr::select(.data$x, .data$y) %>% as.matrix)), "bdy"))), 
                           data.frame(label = "boundary", row.names = "bdy"))
}

#' @export
#' @rdname rbgm-Spatial
boundary_sp <- boundarySpatial

#' @export
#' @rdname rbgm-Spatial
boundary_sf <- function(bgm) {
  x <- bgm$boundaryvertices %>% dplyr::select(.data$x, .data$y)
  x$id <- 1
  out <- sfheaders::sf_polygon(x, x= "x", y = "y", polygon_id = "id", linestring_id = "id")
  out[["label"]] <- "boundary"
  attr(out[["geometry"]], "crs") <- structure(list(epsg = NA_integer_, proj4string = bgm$extra$projection), class = "crs")
  
  out
}

#' Vertices as Spatial points. 
#' 
#' Obtain all vertices as a \code{\link[sp]{SpatialPointsDataFrame}} or
#' a sf dataframe. 
#' 
#' Nodes are the unique coordinates (or vertices), points are the instances of
#' those coordinates that exist in the model. \code{\link{point_sp}} or \code{\link{point_sf}} 
#' return all instances of the vertices with information about which boxes they belong
#' to. \code{\link{node_sp}} and \code{\link{node_sf}} return all vertices.
#' 
#' @param bgm BGM object from \code{\link{bgmfile}}
#'
#' @return  \code{\link[sp]{SpatialPointsDataFrame}} or sf data frame
#' @export
#' @importFrom sp SpatialPointsDataFrame proj4string 
#' @aliases node_sf
#' @section Warning: 
#' The sf objects created by `box_sf()`, `node_sf()`, `face_sf()`, `boundary_sf()` and
#' `point_sf()` were not created by the sf package. They were created with 
#' reference to the sf format prior to November 2019. If you have problems 
#' it may be necessary to recreate the 'crs' part of the of the object with code like
#' `x <- node_sf(bgm); library(sf); st_crs(x) <- st_crs(attr(x$geometry, "crs")$proj)`. 
#' 
#' Get in touch ([create an issue](https://github.com/AustralianAntarcticDivision/rbgm/issues)) if you have any troubles. 
#' @examples
#' fname <- bgmfiles::bgmfiles(pattern = "antarctica_28")
#' bgm <- bgmfile(fname)
#' spnode <- node_sp(bgm)
#' names(spnode)
#' nrow(spnode)  ## only unique vertices
#' nrow(bgm$vertices)
#' 
#' sppoints <- point_sp(bgm)
#' names(sppoints)
#' nrow(sppoints)
#' names(point_sf(bgm))
nodeSpatial <- function(bgm) {
  SpatialPointsDataFrame(as.matrix(bgm$vertices[, c("x", "y")]), data.frame(.vx0 = bgm$vertices$.vx0), 
                         proj4string = CRS(bgm$extra["projection"][[1]], doCheckCRSArgs = FALSE))
}

#' @export
#' @rdname rbgm-Spatial
node_sp <- nodeSpatial

#' @rdname nodeSpatial
#' @export 
node_sf <- function(bgm) {
  out <- sfheaders::sf_point(data.frame(x = bgm$vertices$x, y = bgm$vertices$y))
  attr(out[["geometry"]], "crs") <- structure(list(epsg = NA_integer_, proj4string = bgm$extra$projection), class = "crs")
  
  out
}
#' @rdname nodeSpatial
#' @export 
pointSpatial <- function(bgm) {
  bgmv <- bgm$vertices %>% inner_join(bgm$facesXverts)
  df <- bgmv
  df$x <- NULL
  df$y <- NULL
  SpatialPointsDataFrame(as.matrix(bgmv[, c("x", "y")]), as.data.frame(df), 
                         proj4string = CRS(bgm$extra["projection"][[1]], doCheckCRSArgs = FALSE))
}
#' @export
#' @rdname rbgm-Spatial
point_sp <- pointSpatial

#' @rdname nodeSpatial
#' @export 
point_sf <- function(bgm) {
  bgmv <- bgm$vertices %>% inner_join(bgm$facesXverts)
  out <- sfheaders::sf_point(bgmv, x = "x", y = "y", keep = TRUE) 
  attr(out[["geometry"]], "crs") <- structure(list(epsg = NA_integer_, proj4string = bgm$extra$projection), class = "crs")
  
  out
}
#' @export
#' @rdname rbgm-Spatial
faceSpatial <- function(bgm) {
  data <- bgm$faces 
  #data$label <- sprintf("face%i", data$.fx0)
  faceverts <- data %>% dplyr::select(.data$.fx0, .data$label) %>% 
    inner_join(bgm$facesXverts, ".fx0") %>% 
    inner_join(bgm$vertices, ".vx0") %>% 
    select(-.data$.vx0, -.data$.fx0)
  
  data <- as.data.frame(data)
  rownames(data) <- data$label
  SpatialLinesDataFrame(sptableFace(faceverts, object = "label", crs = bgm$extra$projection), data)
  
}
#' @export
#' @rdname rbgm-Spatial
face_sp <- faceSpatial

#' @export
#' @rdname rbgm-Spatial
face_sf <- function(bgm) {
  data <- bgm$faces 
  #data$label <- sprintf("face%i", data$.fx0)
  faceverts <- data %>% dplyr::select(.data$.fx0, .data$label) %>% 
    inner_join(bgm$facesXverts, ".fx0") %>% 
    inner_join(bgm$vertices, ".vx0") %>% 
    select(-.data$.vx0, -.data$.fx0)
  
  data <- as.data.frame(data)
  rownames(data) <- data$label
  faceverts$label <- as.integer(factor(faceverts$label))
  out <- sfheaders::sf_linestring(faceverts, x = "x", y = "y", linestring_id = "label")
  attr(out[["geometry"]], "crs") <- structure(list(epsg = NA_integer_, proj4string = bgm$extra$projection), class = "crs")
  out
}
sptableFace <- function(x, object = ".fx0", xy = c("x", "y"), crs = NA_character_) {
  IDs <- unique(x[[object]])
  ## ouch, lapply returns sorted on x[[object]] - ouch!  
  l1 <- lapply(split(x[, xy], x[[object]]), function(x) Line(as.matrix(x)))[IDs]
  l1 <- unname(l1)
  l2 <- lapply(seq_along(l1), function(ii) Lines(l1[ii], IDs[ii]))
  l2 <- unname(l2) ## no names or sp fail
  SpatialLines(l2, proj4string = CRS(crs, doCheckCRSArgs = FALSE))
}
