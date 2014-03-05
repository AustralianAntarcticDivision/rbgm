rbgm - R tools for Box Geometry model files
================================================

rbgm allows reading of geometry from BGM files, either in raw
coordinate form or as Spatial objects

There is support to "densify" (or segmentize) the line faces to a
minimum distance between vertices.

## Check out from the Git Stash and build with roxygen documentation

1. git clone http://michae_sum@stash.aad.gov.au/scm/mds/rbgm.git
2. library(roxygen2);roxygenize("rbgm")
3. R CMD build rbgm
4. R CMD INSTALL rbgm_0.0-1.tar.gz

library(rbgm)
x <- read.faces("file.bgm")




