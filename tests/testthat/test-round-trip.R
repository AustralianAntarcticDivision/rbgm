library(testthat)
context("round-trip of BGM links")

#devtools::install_github("mdsumner/rbgm")
library(rbgm)
library(dplyr)
bfile <- grep("antarctica_99", bgmfiles::bgmfiles(), value = TRUE)
#for (bfile in bgmfiles::bgmfiles()) {
  context(sprintf("reading file %s", basename(bfile)))
  bgm <- bgmfile(bfile)
  polybgm <- rbgm::boxSpatial(bgm)
  linebgm <- rbgm::faceSpatial(bgm)
  
#}
  
## run on some systems
  # library(bgmfiles)
  # library(rbgm)
  # bgm <- bgmfile(grep("NGOM", bgmfiles::bgmfiles(), value = TRUE))
  # 
  # boxes <- boxSpatial(bgm)
  # 
  # boxLL <- spTransform(boxes, "+proj=longlat +ellps=WGS84")
  # 
  # writeOGR(boxLL, ".", "NGOM_LL", "ESRI Shapefile", overwrite_layer = TRUE)
  # 
  # ## re-create our original bgm? 
  # ##system('java -jar bgmeriser-stripped.jar  -from "+proj=longlat +ellps=WGS84" -to  "+proj=utm +a=6378137.0 +es=0.006694380022900787 +lon_0=-81d00 +lat_0=0d00 +x_0=500000.0 +y_0=0.0 +k=0.9996 +zone=17" NGOM_LL.shp')
  # 
  
  