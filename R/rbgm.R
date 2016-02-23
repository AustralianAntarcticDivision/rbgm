#' Utilities for BGM files for Atlantis
#'
#' Tools for handling network data for Atlantis from box-geometry
#' model (BGM) files
#'
#' @name rbgm-package
#' @docType package
#' @author Michael D. Sumner
#' @import raster sp
NULL

gris_bgm <- function(x) {
  x <- read_bgm(x)
  
  data_frame(face = unlist(x$boxfaces) - 1) %>%  inner_join(x$facepairs)
  data_frame(face = unlist(x$boxind) - 1, box = rep(seq_along(x$boxind), sapply(x$boxind, length))) %>%  inner_join(x$facepairs)
  ## gris format
  v <- x$verts %>% mutate(.vx0 = row_number())
  bXv <- v %>% mutate(.br0 = rep(seq(nrow(x$facepairs)), each = 2)) %>% select(.br0, .vx0)
  bXv2 <- data_frame(.br0 = unlist(lapply(seq_along(x$boxes), function(xa) rep(xa, length(x$boxes[[xa]])))) + max(bXv$.br0), 
                    .vx0 = unlist(x$boxes))

  v2 <- bXv2  %>% select(-.br0)  %>% inner_join(v, c(".vx0" = ".vx0")) 
  bXv2$.vx0 <- bXv2$.vx0 + nrow(v)
 # v2 <- bXv2 <- NULL
  gr <- gris:::normalizeVerts2(bind_rows(v, v2),  bXv %>% bind_rows(bXv2), c("x", "y"))
  
  gr$b <- gr$bXv %>% select(.br0) %>% distinct() %>% mutate(.ob0 = .br0)
  gr$o <- data_frame(.ob0 = seq(nrow(gr$b)), elem = c(rep("face", nrow(x$faces)), rep("box", length(x$boxes))))
  class(gr) <- c("gris", "list")
  tri <- RTriangle::triangulate(gris::mkpslg(gris:::normalizeVerts2(v, bXv, c("x", "y"))))
  
}


##' Partial read for .bgm files
##'
##' Read geometry from BGM files
##'
##' @title Read BGM
##' @param x path to a bgm file
##' @export
#' @importFrom dplyr %>% select as_data_frame data_frame arrange bind_rows distinct mutate inner_join
read_bgm <- function(x, sp = FALSE) {
  tx <- readLines(x)  
  ## all face tokens
  facesInd <- grep("^face", tx)
  boxesInd <- grep("^box", tx)
  ## parse boxes
  bnd_vertInd <- grep("^bnd_vert", tx)
  
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
  faceslist <- grepItems(tx[facesInd], "face", as.numeric(extra["nface"]))
  facepairs <- do.call(bind_rows, lapply(seq_along(faceslist), function(xi) {a <- faceparse(faceslist[[xi]]); a$face <- xi - 1; a}))
  boxeslist <- grepItems(tx[boxesInd], "box", as.numeric(extra["nbox"]))
  boxes <-                  lapply(seq_along(boxeslist), function(xi) {a <- boxparse(boxeslist[[xi]]); a$box <- xi - 1; a})
  
  bnd_verts <- do.call(rbind, lapply(strsplit(tx[bnd_vertInd], "\\s+"), function(x) as.numeric(x[-1])))
  bnd_verts <- data_frame(x = bnd_verts[,1], y = bnd_verts[,2])
  boxverts <- do.call(bind_rows, lapply(boxes, "[[", "verts"))
  boxdata <- do.call(bind_rows, lapply(boxes, function(a) as_data_frame(a[["meta"]])))
  for (i in seq(ncol(boxdata))) boxdata[[i]] <- type.convert(boxdata[[i]], as.is = TRUE)
  verts <- facepairs %>% select(x, y)   
  ## I think bnd_verts already all included in box_verts
  allverts <- bind_rows(verts, boxverts, bnd_verts) %>% distinct() %>% arrange(x, y) %>% mutate(nr = row_number())
  
  boxind <- lapply(boxes, function(x) (allverts %>% inner_join(x$verts %>% mutate(ord = row_number()), c("x" = "x", "y" = "y")) %>% arrange(ord))$nr)
  faceind <- lapply(split(facepairs %>% select(x, y, face), facepairs$face), function(x) (allverts %>% inner_join(x %>% mutate(ord = row_number()), c("x" = "x", "y" = "y")) %>% arrange(ord))$nr)
  bndind <-  ((allverts %>% inner_join(bnd_verts %>% mutate(ord = row_number()), c("x" = "x", "y" = "y"))) %>% arrange(ord))$nr
  
  ##allverts %>% inner_join(boxes[[2]]$verts %>% mutate(ord = row_number())) %>% arrange(ord)
 
  ## maintain the order before the join
 # boxind <- lapply(boxes, function(x) (verts %>% mutate(nr = row_number()) %>% inner_join(x$verts %>% mutate(ord = row_number()) %>% arrange(ord)))$nr)

  allverts <- allverts %>% select(x, y)
  ##(verts %>% mutate(nr = row_number()) %>% inner_join(x$verts %>% mutate(ord = row_number()) ) %>% arrange(ord))$nr
 #faces <- matrix(seq(nrow(verts)), byrow = TRUE, ncol = 2)
  boxfaces <- lapply(boxes, function(x) x$faces$iface + 1)
  list(verts = allverts, facepairs = facepairs, faceind = faceind, 
       bndind = bndind, boxind = boxind, extra = extra, boxfaces = boxfaces, 
       boxdata = boxdata)
}




box2pslg <- function(x) {
  x <- head(x$verts, -1) %>% select(x, y) %>% as.matrix
  RTriangle::pslg(x, S = segmaker(x))
}
segmaker <- function(x) {
  on.exit(options(op))
  op <- options(warn = -1)
  matrix(seq(nrow(x)), nrow = nrow(x) + 1, ncol  = 2)[seq(nrow(x)), ]
}


grepItems <- function(tex, itemname, nitem) {
  alist <- vector("list", nitem)
  for (i in seq_along(alist)) {
    alist[[i]] <- grep(sprintf("%s%i\\.", itemname, i - 1), tex, value = TRUE)
  }
  alist
}

faceparse <- function(x) {
  ind <- grep("length", x)
  x0 <- strsplit(x, "\\s+")
  
  len <- as.numeric(x0[[ind]][2])
  p1 <- as.numeric(x0[[grep("p1", x)]][2:3])
  p2 <- as.numeric(x0[[grep("p2", x)]][2:3])
  cs <- as.numeric(x0[[grep("cs", x)]][2:3])
  lr <- as.integer(x0[[grep("lr", x)]][2:3])
  data_frame(x = c(p1[1], p2[1]), y = c(p1[2], p2[2]), cs = cs, lr = lr, len = c(0, len))
  
}

boxparse <- function(x) {
  vertind <- grep(".vert ", x)
  verts <-  do.call(rbind, lapply(strsplit(x[vertind], "\\s+"), function(x) as.numeric(x[2:3])))
  x <- x[-vertind]
  insideind <- grep("inside", x)
  centroid <- as.numeric(strsplit(x[insideind], "\\s+")[[1]][2:3])
  
  metalabs <- sapply(strsplit(sapply(strsplit(x[-insideind], "\\s+"), function(x) x[1]), "\\."), "[", 2)
  
  metavals <- sapply(strsplit(x[-insideind], "\\s+"), function(x) x[-1])
  names(metavals) <- metalabs
  
  list(verts = data_frame(x = verts[,1], y = verts[,2]), 
       faces =  data_frame(iface = i_parse(metavals[["iface"]]), ibox = i_parse(metavals[["ibox"]])), 
       meta = metavals[!names(metavals) %in% c("iface", "ibox")])
}




##' Densify vertices so that no span is  is greater than mindist
##'
##' Using great circle distance in km add vertices to a matrix of longitude latitude coordinates
##' @title  Densify lonlat vertices
##' @param x matrix of 2 coordinates
##' @param mindist minimum distance in km if \code{longlat} is \code{TRUE}, otherwise in the units of the projection used (probably metres)
##' @param longlat calculate distance on great circle or Cartesian
##' @return matrix of (possibly) densified coordinates
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



