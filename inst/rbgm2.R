library(dplyr)


# facepslg <- function(x) {
#   RTriangle::pslg(RTriangle::pslg(as.matrix(x[, c("x", "y")])), S = matrix(seq(nrow(x)), byrow = TRUE, ncol = 2))
# }

#facepslg(facepairs[facepairs$face %in% boxes[[9]]$faces$iface, ])

#tx <- readLines("/home/shared/data/Atlantis/SETas/VMPA_setas.bgm")
tx <- readLines("/home/shared/data/Atlantis/BanzareBank/BanzareAtlantis.bgm")
## all face tokens
facesInd <- grep("^face", tx)
boxesInd <- grep("^box", tx)
## parse boxes
bnd_vertInd <- grep("^bnd_vert", tx)
## parse bnd_verts

## all comments
hashInd <- grep("^#", tx)
## unique starting tokens
ust <- sort(unique(sapply(strsplit(tx[-c(facesInd, boxesInd, bnd_vertInd, hashInd)], "\\s+"), "[", 1)))
extra <- sapply(ust, function(x) gsub("\\s+$", "", gsub("^\\s+", "", gsub(x, "", grep(x, tx, value = TRUE)))))

## WTF
extra["projection"] <- sprintf("+%s", gsub(" ", " +", extra["projection"]))
## parse faces
## expect extra["nface"]s

## sanity check
##if (!length(facesInd) == length(unlist(tx[facesInd]))) warning("faces data and count out of synch")
faceslist <- grepItems(tx[facesInd], "face", extra["nface"])
facepairs <- do.call(bind_rows, lapply(seq_along(faceslist), function(xi) {a <- rbgm:::faceparse(faceslist[[xi]]); a$face <- xi - 1; a}))
boxeslist <- grepItems(tx[boxesInd], "box", extra["nbox"])
#boxes <- do.call(bind_rows, 
boxes <-                  lapply(seq_along(boxeslist), function(xi) {a <- rbgm:::boxparse(boxeslist[[xi]]); a$box <- xi - 1; a})
                 #)



## gris format
v <- facepairs  %>% dplyr::mutate(.vx0 = row_number())
bXv <- v%>% mutate(.br0 = face) %>% dplyr::select(.br0, .vx0)
v$face <- NULL
tri <- RTriangle::triangulate(gris::mkpslg(gris:::normalizeVerts2(v, bXv, c("x", "y"))))


## some of this is in oms
roms <- "/rdsi/PRIVATE/dev/ROMS/s_corney/cpolar/ocean_his_3101_uvwtempsalt.nc"
lon <- raster(roms, varname = "lon_u")
lat <- raster(roms, varname = "lat_u")
ll <- cbind(values(lon), values(lat))

## triangle identity
tri_ <- rbgm:::ptTriangle(tri$T, tri$P, ll)

## box identity
box <- ptPolygon(boxes, ll)

## extract surface temperature
temp <- raster(roms, varname = "temp", level =1 )
h <- raster(roms, varname = "h")
Cs_r <- local({
  library(RNetCDF)
  nc <- open.nc(roms)
  var.get.nc(nc, "Cs_r")
})


btemp <- numeric(length(boxes))
for (i in seq_along(btemp)) btemp[i] <- mean(extract(temp, which(box == i & !is.na(box))), na.rm = TRUE)
cols <- palr::sstPal(btemp  * 3)

library(rgl)
cols <- c("", "", "", "", "green", "grey")

for (offs in c(-2000, -1000, -750, -400, -300, -200, -100, -50, -20, 0)) {
  for (i in seq_along(boxes)[5:6]) shade3d(box2oh(boxes[[i]], offs), col = cols[i])
}
aspect3d(1, 1, 0.25)





plot(facepairs$x, facepairs$y)
lapply(seq_along(boxes), function(i) pbox1(boxes[[i]], col = cols[i]))
lapply(seq_along(boxes), function(i) text(mean(boxes[[i]]$verts$x), mean(boxes[[i]]$verts$y), lab = boxes[[i]]$meta$botz))

## convert a face into indexes for a plane
library(nabor)
#install.packages("nabor")

nnfun <- WKNNF(ll)
ex <- 20 + extent(xyFromCell(lon, nnfun$query(as.matrix(facepairs[, c("x", "y")]), k = 1, eps = 0)$nn.idx))

pal <- palr::sstPal(palette = TRUE)
for (j in seq(1, nrow(facepairs) - 1, by = 2)) {
  ln <- facepairs[c(j, j + 1),] %>% dplyr::select(x, y) %>% as.matrix
  ln1 <- cbind(approx(1:2, ln[,1])$y, approx(1:2, ln[,2])$y)
  res <- nnfun$query(ln1, k = 1, eps = 0)
  ## line in ROMS space is 
  #res$nn.idx
  # points(xyFromCell(temp, res$nn.idx), pch = 16, cex = 0.4)
   romline <- xyFromCell(temp, res$nn.idx)
   faceXYindex <- extract(temp, spLines(romline), cellnumbers = TRUE)[[1]][, "cell"]
   romdepth <- outer(extract(h, faceXYindex), Cs_r)
   sslice <- tslice <- wslice <- uslice <- vslice <- matrix(NA_real_, ncol = 31, nrow = length(faceXYindex))
    for (i in 1:31) {
      uslice[, i] <- extract(raster(roms, varname = "u", level = i), faceXYindex)
      vslice[, i] <- extract(raster(roms, varname = "v", level = i), faceXYindex)
      wslice[, i] <- extract(raster(roms, varname = "w", level = i), faceXYindex)
      tslice[, i] <- extract(raster(roms, varname = "salt", level = i), faceXYindex)
      sslice[, i] <- extract(raster(roms, varname = "temp", level = i), faceXYindex)
      
      
    }

  par(mfrow = c(2, 2))
  
  plot(crop(temp, ex))
  for (aa in seq_along(boxes)) {
    xy <- as.matrix(boxes[[aa]]$verts[, c("x", "y")])
    polygon(xyFromCell(lon, nnfun$query(xy, k = 1, eps = 0)$nn.idx))
    lines(xyFromCell(lon, nnfun$query(ln1, k = 1, eps = 0)$nn.idx), col = "red", lwd = 4)
  }
  plot(facepairs$x, facepairs$y)
  lapply(seq_along(boxes), function(i) pbox1(boxes[[i]], col = "transparent"))
  lines(ln1, lwd = 4, col = "red")
   image(sqrt(uslice^2 + vslice^2 + wslice^2), col = pal$cols)
   contour(romdepth, add = T, level = c(seq(0, -500, by = -50), seq(550, -5000, by = -500)))
  
   scan("", 1)
}


pbox1 <- function(x, col = "grey") {
  points(x$verts, pch = 16)
  polygon(x$verts, col = col)
}

pf <- function(x) {
  for (i in unique(x$face)) {
    lines((x %>% subset(face == i))[, c("x", "y")], col = "red")
  }
}
