
# fixproj("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# fixproj("proj=aea lat_1=-18 lat_2=-36 lat_0=0 lon_0=134 x_0=3000000 y_0=6000000 ellps=GRS80 towgs84=0,0,0,0,0,0,0 units=m no_defs")
# fixproj("+proj=longlat +a=6378137.0 +es=0.0066943799901413165 +lon_0=0d00 +lat_0=0d00 +x_0=0.0 +y_0=0.0")
# fixproj("proj=utm + zone=55 + datum = WGS84 +units=m +no_defs")
# fixproj("+proj=laea +lat_0=-63 +lon_0=82 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84")
# fixproj("+proj=longlat +a=6378137.0 +es=0.0066943799901413165 +lon_0=0d00 +lat_0=0d00 +x_0=0.0 +y_0=0.0")
# fixproj("proj=lcc lat_1=17.5 lat_2=29.5 lat_0=0 lon_0=-102.0 x_0=2000000.0 y_0=0 ellps=clrk66 datum=NAD27 units=m no_defs")
fixproj <- function(x) {

  ## remove all "+"
  ss <- gsub("\\+", "", x[1])
  ## collapse any space around "="
  ss <- gsub("\\s+=", "=", ss)
  ss <- gsub("=\\s+", "=", ss)
  ss <- gsub("\\s+", " ", ss)
  ## split on space
  ss <- strsplit(ss, "\\s+")[[1]]
  paste(unlist(lapply(ss, prependplus)), collapse = " ")
}

prependplus <- function(x) {
  if (!grepl("^\\+", x)) {
    x <- sprintf("+%s", x)
  }
  x
}
##' Densify vertices so that no span is  is greater than mindist
##'
##' Using great circle distance in km add vertices to a matrix of longitude latitude coordinates
##' @title  Densify lonlat vertices
##' @param x matrix of 2 coordinates
##' @param mindist minimum distance in km if \code{longlat} is \code{TRUE}, otherwise in the units of the projection used (probably metres)
##' @param longlat calculate distance on great circle or Cartesian
##' @return matrix of (possibly) densified coordinates
##' @noRd
densifymindist <- function(x, mindist, longlat = longlat) {
  if (missing(mindist)) {
    warning("No minimum distance specified, no densifying done")
    return(x)
  }
  
  dist <- spDistsN1(x[1L,,drop=FALSE], x[2L,,drop=FALSE], longlat = TRUE)
  if (dist >= mindist) {
    n <- dist %/% mindist
    x <- gcIntermediate(x[1L,], x[2L,], n = n, addStartEnd = TRUE)
  }
  x
  
}


##' Build dz layer values for Atlantis from a bottom value, up through successive intervals. Each value is the positive offset required to rise
##' to the top of the current interval. 
##'
##' Offset values are returned to move from \code{z} against the intervals in \code{zlayers}. The intervals are assumed
##' to be sorted and increasing in value from \code{-Inf}inity. Once the maximum layer is reached the result is padded
##' by that top value. 
##' 
##' @title Build Atlantis dz Values
##' @param z lowermost value
##' @param zlayers intervals of layer values
##' @return numeric vector of offset values
##' @export
##' @examples 
##' ## sanity tests
##' build_dz(-5000)
##' build_dz(-1500)
##' ##build_dz(300)  ## error
##' build_dz(0)    ## ok


##' ## data
##' dd <- c(-4396.49, -2100.84, -4448.81, -411.96, -2703.56, -5232.96, 
##'        -4176.25, -2862.37, -3795.6, -1024.64, -897.93, -1695.82, -4949.76, 
##'     -5264.24, -2886.81)
##' ## all values in a matrix for checking
##' ## [zlayers, dd]
##'dzvals <- sapply(dd, build_dz)

##' ## process into text
##' f1 <- function(x) sprintf("somelabel,%i,%s", x, paste(build_dz(dd[x]), collapse = ","))
##' tex1 <- sapply(seq(length(dd)),  f1)
##' ## for example
##' f2 <- function(x) {
##' sprintf("morelabel,%i,%s", x, paste(as.integer(build_dz(dd[x])), collapse = ","))
##' }
##' tex2 <- sapply(seq(length(dd)),  f2)


build_dz <- function(z, zlayers = c(-Inf, -2000, -1000, -750, -400, -300, -200, -100, -50, -20, 0)) {
  if (length(z) > 1L) {
    z <- z[1L]
    warning("length of z is longer than 1, only the first element will be used")
  }
  layerindex <- findInterval(z, zlayers, rightmost.closed = TRUE) + 1L
  
  if (layerindex > length(zlayers)) stop("z is not in the intervals defined by zlayers")
  startdz <- zlayers[layerindex] - z
  out <- c(startdz, diff(zlayers[-seq(layerindex - 1)]))
  c(out, rep(max(zlayers), (length(zlayers) - 1) - length(out)))
}







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
#   x <- bgmfile(x)
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

