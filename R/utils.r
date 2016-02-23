
box2oh <- function(x, offset = 0) {
  tri <- RTriangle::triangulate(box2pslg(x))
  obj <- rgl::tetrahedron3d()
  obj$vb <- t(cbind(tri$P, as.numeric(x$meta$botz) + offset, 1))
  obj$it <- t(tri$T)
  obj
}


i_parse <- function(x) as.numeric(unlist(strsplit(x, "\\s+")))

## which triangle is each point in 
ptTriangle <- function(T, P, pt) {
  pt_t <- rep(NA_integer_, nrow(pt))
  
  for (i in seq(nrow(T))) {
    triang <- P[T[i,], ]
    asub <- pt[,1] >= min(triang[,1]) & pt[,1] <= max(triang[,1]) &
      pt[,2] >= min(triang[,2]) & pt[,2] <= max(triang[,2])
    if (any(asub)) {
      uvw <- geometry::cart2bary(triang, pt[asub, ])
      pt_t[asub][uvw[,1] > 0 & uvw[, 2] > 0 & rowSums(uvw[,1:2]) < 1] <- i
    }
  }
  pt_t
}


ptPolygon <- function(boxes, pt) {
  pt_p <- rep(NA_integer_, nrow(pt))
  
  
  for (i in seq(length(boxes))) {
    poly <- boxes[[i]]$vert %>% select(x, y) %>% as.matrix
    asub <- pt[,1] >= min(poly[,1]) & pt[,1] <= max(poly[,1]) &
      pt[,2] >= min(poly[,2]) & pt[,2] <= max(poly[,2])
    if (any(asub)) {
      #  print(i)
      pip <- point.in.polygon(pt[asub,1], pt[asub,2], poly[,1], poly[,2], mode.checked = TRUE) > 0
      #print(sum(pip))
      pt_p[asub][pip] <- i
    }
  }
  pt_p
}
