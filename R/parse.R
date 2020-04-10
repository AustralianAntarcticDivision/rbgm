i_parse <- function(x) as.numeric(unlist(strsplit(x, "\\s+")))


facevertsparse <- 
  function(x) {
    ind <- grep("length", x)
    x0 <- strsplit(x, "\\s+")
    
    # len <- as.numeric(x0[[ind]][2])
    p1 <- as.numeric(x0[[grep("p1", x)]][2:3])
    p2 <- as.numeric(x0[[grep("p2", x)]][2:3])
    #  cs <- as.numeric(x0[[grep("cs", x)]][2:3])
    #  lr <- as.integer(x0[[grep("lr", x)]][2:3])
    tibble::tibble(x = c(p1[1], p2[1]), y = c(p1[2], p2[2]))
    
  }
facedataparse <- function(x) {
  ind <- grep("length", x)
  x0 <- strsplit(x, "\\s+")
  
  len <- as.numeric(x0[[ind]][2])
  cs <- as.numeric(x0[[grep("cs", x)]][2:3])
  lr <- as.integer(x0[[grep("lr", x)]][2:3])
  tibble::tibble(cosine = cs[1], sine = cs[2], left = lr[1], right = lr[2], length = len)
  
}


boxparse <- function(x) {
  ## need variable amounts of white space, some are tabs, some single space
  vertind <- grep(".vert\\s+", x)
  verts <-  do.call(rbind, lapply(strsplit(x[vertind], "\\s+"), function(x) as.numeric(x[2:3])))
  x <- x[-vertind]
  insideind <- grep("inside", x)
  centroid <- strsplit(x[insideind], "\\s+")[[1]][2:3]
  
  metalabs <- sapply(strsplit(sapply(strsplit(x[-insideind], "\\s+"), function(x) x[1]), "\\."), "[", 2)
  
  ## use lapply to avoid type de-stability :)
  metavals <- lapply(strsplit(x[-insideind], "\\s+"), function(x) x[-1])
  names(metavals) <- metalabs
  
  list(verts = tibble::tibble(x = verts[,1], y = verts[,2]), 
       faces =  tibble::tibble(iface = i_parse(metavals[["iface"]]), ibox = i_parse(metavals[["ibox"]])), 
       meta = metavals[!names(metavals) %in% c("iface", "ibox")], insideX = centroid[1], insideY = centroid[2])
}
grepItems <- function(tex, itemname, nitem) {
  alist <- vector("list", nitem)
  for (i in seq_along(alist)) {
    alist[[i]] <- grep(sprintf("%s%i\\.", itemname, i - 1), tex, value = TRUE)
  }
  alist
}


