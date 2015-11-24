rbgm - R tools for Box Geometry model files
================================================

rbgm allows reading of geometry from BGM files, either in raw
coordinate form or as Spatial objects. 

rbgm aims to make all of this straightforward: 

* read of the BGM format, maintaining all topology and attributes
* use of the BGM specification for visualization and data extraction
* creation of BGM from from shapefiles, R spatial objects and whatever else

There is some overlapping functionality with [oms](https://github.com/mdsumner/oms), for coupling with ROMS output and with [gris](https://github.com/mdsumner/gris), for building
topological data structures. All of thse packages are in development and are subject to change. 

There is support to "densify" (or segmentize) the line faces to a
minimum distance between vertices based on Earth geometry, but it's not clear if this is necessary now.


## Install dev version

```R
# install.packages("devtools")
devtools::install_github("mdsumner/rbgm")
```

## More information

The BGM format and usage is described at the (registration-required) Atlantis wiki: http://atlantis.cmar.csiro.au/

## Questions

* polygon maps may have lines requiring denser sets of vertices (say for lines of constant latitude), this can be an issue for ROMS where all lookup is done in long-lat (the coupler should sort this out IMO, not BGM)
* 

