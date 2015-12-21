g <- list(v = bind_cols(r$verts, data_frame(.vx0 = seq(nrow(r$verts)))), bXv = data_frame(.br0 = rep(seq(length(r$boxind)), sapply(r$boxind, length)), .vx0 = unlist(r$boxind)), 
b = data_frame(.br0 = seq(length(r$boxind)), .ob0 = seq(length(r$boxind))), o = data_frame(.ob0 = seq(length(r$boxind))))


class(g) <- c("gris", "list")


g1 <- RTriangle::triangulate(mkpslg(g))

library(rgl)
r1 <- tetrahedron3d()
r1$vb <- t(cbind(g1$P, 0, 1))
r1$it <- t(g1$T)
wire3d(r1)


for (jj in 1:8) {
for (i in seq(nrow(g$o))) {
 xo <- g[i, ]
 g1 <- RTriangle::triangulate(mkpslg(xo))
 r1$vb <- t(cbind(g1$P, -sum(head(build_dz(r$boxdata$botz[i]), jj)), 1))
 r1$it <- t(g1$T)
 wire3d(r1)


}

}

