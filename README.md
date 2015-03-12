rbgm - R tools for Box Geometry model files
================================================

rbgm allows reading of geometry from BGM files, either in raw
coordinate form or as Spatial objects

There is support to "densify" (or segmentize) the line faces to a
minimum distance between vertices.

## Clone and build with roxygen documentation

1. library(roxygen2);roxygenize("rbgm")
2. R CMD build rbgm
3. R CMD INSTALL rbgm_[version].tar.gz

library(rbgm)
x <- read.faces("file.bgm")

## More information

The BGM format and usage is described at the (registration-required) Atlantis wiki: http://atlantis.cmar.csiro.au/


