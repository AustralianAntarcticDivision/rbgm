x <- bgm$boxes  %>% select(.bx0)  %>% inner_join(bgm$boxesXverts)  %>% inner_join(bgm$vertices)   %>% select(x, y, .bx0)
NAdf <- data_frame(x = NA_real_, y = NA_real_)
y <- head(do.call(bind_rows, lapply(split(x, x$.bx0), bind_rows, NAdf)), -1)
## grr bgm doesn't know which boxes are boundary
xx <- boxSpatial(bgm)
bgm$boxes$boundary <- xx$boundary


library(raster)
topo <- raster("E:\\DATA\\Etopo\\Etopo1Ice\\Etopo1.tif")
topo <- crop(topo, extent(spTransform(xx, projection(topo))))
deci <- function(x, fact = 10) {
  y <- raster(x); res(y) <- res(y) * fact
  setValues(y, extract(x, coordinates(y)))
}
ex <- 40
topomesh <- gris::quadmeshFromRaster(deci(topo, 5), deci(topo, 5))
topomesh$vb[1:3, ] <- t(gris::llh2xyz(t(topomesh$vb[1:3, ]), exag = ex))

quadlines <- function(x) {
  lines3d(t(x$vb[, as.vector(rbind(x$ib,x$ib[1,], NA_real_))]))
}

quadlines(topomesh)


globe <- TRUE
for (i in seq(nrow(bgm$boxes))) {
   x <- bgm$boxes[i, ]  %>% select(.bx0)  %>% inner_join(bgm$boxesXverts)  %>% inner_join(bgm$vertices)   %>% select(x, y, .bx0)
  aa <- extrude3d(x$x, x$y, thickness = bgm$boxes$botz[i])
  
  if (globe) {
    xyz <- t(aa$vb[1:3, ])
    
    xyz[,1:2] <- rgdal::project(xyz[,1:2], proj = bgm$extra["projection"], inv = TRUE)
    aa$vb[1:3,] <- t(gris::llh2xyz(xyz, exag = ex))
  }
  if (bgm$boxes$boundary[i]){
    wire3d(aa, col = "black")
  } else {
  shade3d(aa, col = rainbow(nrow(bgm$boxes))[i], alpha =0.5, specular = "black")
  }
}

wire3d(topomesh, alpha = 0.5)
#aspect3d(1, 1, .1)

