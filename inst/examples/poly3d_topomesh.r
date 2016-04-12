## devtools::install_github("mdsumner/rbgm")
library(rbgm)
## example data set in package
fname <- system.file("extdata", "Antarctica_28.bgm", package = "rbgm")
bgm <- read_bgm(fname)
plot(boxSpatial(bgm), col = grey(seq(0, 1, length = nrow(bgm$boxes)), alpha = 0.5))

##  bgm format doesn't know which boxes are boundary
bgm$boxes$boundary <- boxSpatial(bgm)$boundary

library(dplyr)
## table of polygon topology
tab0 <- bgm$boxes  %>% select(.bx0)  %>% inner_join(bgm$boxesXverts)  %>% inner_join(bgm$vertices)   %>% select(x, y, .bx0)

## prepare in polygon format
NAdf <- data_frame(x = NA_real_, y = NA_real_)
tab <- head(do.call(bind_rows, lapply(split(tab0, tab0$.bx0), bind_rows, NAdf)), -1)



## tools to get raster topography
library(raadtools)
topo <- readtopo("etopo2")

topo <- crop(topo, extent(spTransform(boxSpatial(bgm), projection(topo))))

## resize a raster by nn sampling
deci <- function(x, fact = 10) {
  y <- raster(x); res(y) <- res(y) * fact
  setValues(y, extract(x, coordinates(y)))
}

dem <- deci(topo, 4)
## vertical exaggeration
ex <- 40
## raster as a quadmesh
topomesh <- gris::quadmeshFromRaster(dem, dem)
topomesh$vb[1:3, ] <- t(gris::llh2xyz(t(topomesh$vb[1:3, ]), exag = ex))


library(rgl)
for (i in seq(nrow(bgm$boxes))) {
   x <- bgm$boxes[i, ]  %>% select(.bx0)  %>% inner_join(bgm$boxesXverts)  %>% inner_join(bgm$vertices)   %>% select(x, y, .bx0)
  aa <- extrude3d(x$x, x$y, thickness = bgm$boxes$botz[i])
  
  xyz <- t(aa$vb[1:3, ])
    
    xyz[,1:2] <- rgdal::project(xyz[,1:2], proj = bgm$extra["projection"], inv = TRUE)
    aa$vb[1:3,] <- t(gris::llh2xyz(xyz, exag = ex))
 
  if (bgm$boxes$boundary[i]){
    wire3d(aa, col = "black")
  } else {
  shade3d(aa, col = rainbow(nrow(bgm$boxes))[i], alpha =0.5, specular = "black")
  }
}


)
quadlines <- function(x, ...) {
  lines3d(t(x$vb[, as.vector(rbind(x$ib,x$ib[1,], NA_real_))]), ...)
}

quadlines(topomesh,  alpha = 0.6)


