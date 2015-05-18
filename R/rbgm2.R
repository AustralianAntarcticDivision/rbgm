library(dplyr)

faceparse <- function(x) {
  ind <- grep("length", x)
  x0 <- strsplit(x, "\\s+")
  
  len <- as.numeric(x0[[ind]][2])
  p1 <- as.numeric(x0[[grep("p1", x)]][2:3])
  p2 <- as.numeric(x0[[grep("p2", x)]][2:3])
  cs <- as.numeric(x0[[grep("cs", x)]][2:3])
  lr <- as.integer(x0[[grep("p1", x)]][2:3])
  data_frame(x = c(p1[1], p2[1]), y = c(p1[2], p2[2]), cs = cs, lr = lr, len = c(0, len))
  
}

boxparse <- function(x) {
  vertind <- grep(".vert ", x)
  verts <-  do.call(rbind, lapply(strsplit(x[vertind], "\\s+"), function(x) as.numeric(x[2:3])))
  x <- x[-vertind]
  insideind <- grep("inside", x)
  centroid <- as.numeric(strsplit(x[insideind], "\\s+")[[1]][2:3])
   
  metalabs <- sapply(strsplit(sapply(strsplit(x[-insideind], "\\s+"), function(x) x[1]), "\\."), "[", 2)
  metavals <- sapply(strsplit(x[-insideind], "\\s+"), function(x) x[2])
  data_frame(x = verts[,1], y = verts[,2])
}

tx <- readLines("/home/shared/data/Atlantis/SETas/VMPA_setas.bgm")

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
grepItems <- function(tex, itemname, nitem) {
  alist <- vector("list", nitem)
  for (i in seq_along(alist)) {
    alist[[i]] <- grep(sprintf("%s%i\\.", itemname, i - 1), tex, value = TRUE)
  }
  alist
}
## sanity check
##if (!length(facesInd) == length(unlist(tx[facesInd]))) warning("faces data and count out of synch")
faceslist <- grepItems(tx[facesInd], "face", extra["nface"])
facepairs <- do.call(bind_rows, lapply(seq_along(faceslist), function(xi) {a <- faceparse(faceslist[[xi]]); a$face <- xi - 1; a}))
boxeslist <- grepItems(tx[boxesInd], "box", extra["nbox"])
boxes <- do.call(bind_rows, lapply(seq_along(boxeslist), function(xi) {a <- boxparse(boxeslist[[xi]]); a$box <- xi - 1; a}))

