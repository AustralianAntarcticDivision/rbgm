#' Utilities for BGM files for Atlantis
#'
#' Tools for handling network data for Atlantis from box-geometry
#' model (BGM) files
#'
#' @section rbgm features:
#' 
#' \itemize{
#'  \item read .bgm files and faithfully store all information so it can be round-tripped
#'  \item conversion from .bgm forms to Spatial classes (lines and polygons)
#'  \item (not yet implemented: write to .bgm)
#' }
#' 
#' @section I. Import: 
#' \tabular{ll}{
#'  \code{\link{bgmfile}} \tab read directly from a .bgm file  \cr
#' }
#' 
#' @section II. Conversion: 
#' \tabular{ll}{
#'  \code{\link{boxSpatial}} \tab convert boxes to a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}\cr
#'  \code{\link{faceSpatial}} \tab convert faces to a \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}} \cr
#'  \code{\link{boundarySpatial}} \tab convert boundary to a single-row \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}\cr
#'  \code{\link{nodeSpatial}} \tab obtain all vertices as points \cr
#'  \code{\link{pointSpatial}} \tab obtain all instances of vertices as points \cr
#' }
#'
#' @section III. Miscellaneous: 
#' \tabular{ll}{
#'  \code{\link{build_dz}} \tab Build Atlantis dz Values \cr
#'  }
#' 
#' @importFrom dplyr row_number
#' @importFrom raster isLonLat 
#' @importFrom rlang .data
#' @importFrom sp spDistsN1 Line Lines SpatialLines CRS SpatialLinesDataFrame
#' @name rbgm-package
"_PACKAGE"






