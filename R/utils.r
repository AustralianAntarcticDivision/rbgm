# 
# box2oh <- function(x, offset = 0) {
#   tri <- RTriangle::triangulate(box2pslg(x))
#   obj <- rgl::tetrahedron3d()
#   obj$vb <- t(cbind(tri$P, as.numeric(x$meta$botz) + offset, 1))
#   obj$it <- t(tri$T)
#   obj
# }
# 
# 
# 
# ## which triangle is each point in 
# ptTriangle <- function(T, P, pt) {
#   pt_t <- rep(NA_integer_, nrow(pt))
#   
#   for (i in seq(nrow(T))) {
#     triang <- P[T[i,], ]
#     asub <- pt[,1] >= min(triang[,1]) & pt[,1] <= max(triang[,1]) &
#       pt[,2] >= min(triang[,2]) & pt[,2] <= max(triang[,2])
#     if (any(asub)) {
#       uvw <- geometry::cart2bary(triang, pt[asub, ])
#       pt_t[asub][uvw[,1] > 0 & uvw[, 2] > 0 & rowSums(uvw[,1:2]) < 1] <- i
#     }
#   }
#   pt_t
# }
# 
# 
# ptPolygon <- function(boxes, pt) {
#   pt_p <- rep(NA_integer_, nrow(pt))
#   
#   
#   for (i in seq(length(boxes))) {
#     poly <- boxes[[i]]$vert %>% select(x, y) %>% as.matrix
#     asub <- pt[,1] >= min(poly[,1]) & pt[,1] <= max(poly[,1]) &
#       pt[,2] >= min(poly[,2]) & pt[,2] <= max(poly[,2])
#     if (any(asub)) {
#       #  print(i)
#       pip <- point.in.polygon(pt[asub,1], pt[asub,2], poly[,1], poly[,2], mode.checked = TRUE) > 0
#       #print(sum(pip))
#       pt_p[asub][pip] <- i
#     }
#   }
#   pt_p
# }
# 
# gris_bgm <- function(x) {
#   x <- read_bgm(x)
#   
#   data_frame(face = unlist(x$boxfaces) - 1) %>%  inner_join(x$facepairs)
#   data_frame(face = unlist(x$boxind) - 1, box = rep(seq_along(x$boxind), sapply(x$boxind, length))) %>%  inner_join(x$facepairs)
#   ## gris format
#   v <- x$verts %>% mutate(.vx0 = row_number())
#   bXv <- v %>% mutate(.br0 = rep(seq(nrow(x$facepairs)), each = 2)) %>% select(.br0, .vx0)
#   bXv2 <- data_frame(.br0 = unlist(lapply(seq_along(x$boxes), function(xa) rep(xa, length(x$boxes[[xa]])))) + max(bXv$.br0), 
#                      .vx0 = unlist(x$boxes))
#   
#   v2 <- bXv2  %>% select(-.br0)  %>% inner_join(v, c(".vx0" = ".vx0")) 
#   bXv2$.vx0 <- bXv2$.vx0 + nrow(v)
#   # v2 <- bXv2 <- NULL
#   gr <- gris:::normalizeVerts2(bind_rows(v, v2),  bXv %>% bind_rows(bXv2), c("x", "y"))
#   
#   gr$b <- gr$bXv %>% select(.br0) %>% distinct() %>% mutate(.ob0 = .br0)
#   gr$o <- data_frame(.ob0 = seq(nrow(gr$b)), elem = c(rep("face", nrow(x$faces)), rep("box", length(x$boxes))))
#   class(gr) <- c("gris", "list")
#   tri <- RTriangle::triangulate(gris::mkpslg(gris:::normalizeVerts2(v, bXv, c("x", "y"))))
#   
# }

