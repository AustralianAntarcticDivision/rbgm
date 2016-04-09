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
#'  \code{\link{read_bgm}} \tab read directly from a .bgm file  \cr
#'  \code{\link{read.faces}} \tab read faces in with option to densify vertices \cr
#' }
#' 
#' @section II. Conversion: 
#' \tabular{ll}{
#'  \code{\link{boxSpatial}} \tab convert boxes to a \code{\link{SpatialPolygonsDataFrame}}\cr
#'  \code{\link{faceSpatial}} \tab convert faces to a \code{\link{SpatialLinesDataFrame}} \cr
#'  \code{\link{boundarySpatial}} \tab convert boundary to a single-row \code{\link{SpatialPolygonsDataFrame}}\cr
#' }
#'
#' @section III. Miscellaneous: 
#' \tabular{ll}{
#'  \code{\link{build_dz}} \tab Build Atlantis dz Values \cr
#'  }
#' 
#' @importFrom dplyr row_number
#' @importFrom raster isLonLat 
#' @importFrom sp spDistsN1 Line Lines SpatialLines CRS SpatialLinesDataFrame
#' @name rbgm-package
#' @docType package
#' @author Michael D. Sumner
NULL






