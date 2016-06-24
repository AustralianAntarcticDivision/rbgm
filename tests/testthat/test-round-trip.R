library(testthat)
context("round-trip of BGM links")

#devtools::install_github("mdsumner/rbgm")
library(rbgm)
library(dplyr)
library(spdplyr)
bfile <- grep("antarctica_99", bgmfiles::bgmfiles(), value = TRUE)
#for (bfile in bgmfiles::bgmfiles()) {
  context(sprintf("reading file %s", basename(bfile)))
  bgm <- bgmfile(bfile)
  polybgm <- rbgm::boxSpatial(bgm)
  linebgm <- rbgm::faceSpatial(bgm)
  
  ## note the minus one on seq
  for (abox in (seq(nrow(polybgm)) -1)) {
    aline <- linebgm %>% filter(.fx0 %in% (bgm$facesXboxes %>% dplyr::filter(.bx0 == abox))$iface)
    poly <- polybgm %>% filter(.bx0 == abox) 
#    plot(poly)
#    plot(aline, add = TRUE, col = "red")
    
    #scan("", 1)
    test_that("round tripping works to get the right box and faces", {
      expect_true(rgeos::gIntersects(aline, poly))
      expect_true(rgeos::gCovers(poly, aline))
      expect_true(rgeos::gCoveredBy(aline, poly))
      
    })
  }
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
  
  