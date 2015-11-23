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
  verts <- facepairs %>% dplyr::select(x, y) 
  faces <- matrix(seq(nrow(verts)), byrow = TRUE, ncol = 2)
  boxes <- lapply(boxes, function(x) x$faces$iface)
  list(verts = verts, faces = faces, boxes = boxes, bnd_verts = bnd_verts, extra = extra)
}



## gris format
v <- facepairs  %>% dplyr::mutate(.vx0 = row_number())
bXv <- v%>% mutate(.br0 = face) %>% dplyr::select(.br0, .vx0)
v$face <- NULL
tri <- RTriangle::triangulate(gris::mkpslg(gris:::normalizeVerts2(v, bXv, c("x", "y"))))



box2pslg <- function(x) {
  x <- head(x$verts, -1) %>% dplyr::select(x, y) %>% as.matrix
  RTriangle::pslg(x, S = segmaker(x))
}
segmaker <- function(x) {
  on.exit(options(op))
  op <- options(warn = -1)
  matrix(seq(nrow(x)), nrow = nrow(x) + 1, ncol  = 2)[seq(nrow(x)), ]
}
box2oh <- function(x) {
  tri <- RTriangle::triangulate(box2pslg(x))
}

box2oh <- function(x, offset = 0) {
  tri <- RTriangle::triangulate(box2pslg(x))
  obj <- rgl::tetrahedron3d()
  obj$vb <- t(cbind(tri$P, as.numeric(x$meta$botz) + offset, 1))
  obj$it <- t(tri$T)
  obj
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
    poly <- boxes[[i]]$vert %>% dplyr::select(x, y) %>% as.matrix
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

##' Partial read for .bgm files
##'
##' Read geometry from BGM files
##'
##' @title Read BGM
##' @param x bgm file
##' @param sp return Spatial object or list
##' @param densify factor to increase density of vertices
##' @param ... arguments passed to \code{\link{gcIntermediate}}
##' @return A SpatialLinesDataFrame if \code{sp} is \code{TRUE},
##' otherwise a list of 2x2 matrices containing the end point
##' coordinates, the list is a named vector with the label for each
##' face
##' @importFrom geosphere gcIntermediate
##' @examples
##' ## pull in faces as a SpatialLinesDataFrame, densified to 30km between vertices
##' \dontrun{
##' x <- read.faces("file.bgm", densify = 30, sp = TRUE)
##' }
##' @export
read.faces <- function(x, sp = TRUE, densify, ...) {
    xlines <- readLines(x)

    ## assume PROJ.4 string occurs at first mention of "projection"
    projind <- grep("^projection", xlines)[1L]
    prjstring <- gsub("^projection ", "", xlines[projind])

    facenumind <- grep("# Face", xlines)
    facenum <- sapply(strsplit(xlines[facenumind], " "), function(x) x[length(x)])
    strfacept <- function(x) do.call("rbind", lapply(strsplit(x, " "), function(x) as.numeric(x[-1])))

    xf <- lapply(facenumind, function(x) strfacept(xlines[c(x + c(1L, 2L))]))

    if (!missing(densify)) {
        if (!densify > 1) stop("densify must be an integer > 1")
        ##xf <- lapply(xf, function(x) gcIntermediate(x[1L,], x[2L,], n = densify, addStartEnd = TRUE, ...))
        xf <- lapply(xf, densifymindist, mindist = densify, longlat = isLonLat(prjstring))
    }
    if (sp) {
        sLines <- vector("list", length(facenum))
        for (i in seq_along(sLines)) {
            ## single pair coordinate line, each a Lines object (so each can have attributes)
            sLines[[i]] <- Lines(list(Line(xf[[i]])), facenum[i])
        }
        spLines <- SpatialLines(sLines, proj4string = CRS(prjstring))
        spdf <- SpatialLinesDataFrame(spLines, data.frame(id = facenum, row.names = facenum))
        return(spdf)
    }
    xf
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



