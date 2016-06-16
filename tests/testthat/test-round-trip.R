library(testthat)
context("round-trip of BGM links")

#devtools::install_github("mdsumner/rbgm")
library(rbgm)
library(dplyr)
library(spdplyr)
bfile <- grep("antarctica_99", bgmfiles::bgmfiles(), value = TRUE)
bgm <- bgmfile(bfile)
polybgm <- rbgm::boxSpatial(bgm)
linebgm <- rbgm::faceSpatial(bgm)

## note the minus one on seq
for (abox in (seq(nrow(polybgm)) -1)) {
  aline <- linebgm %>% filter(.fx0 %in% (bgm$facesXboxes %>% dplyr::filter(.bx0 == abox))$iface)
  poly <- polybgm %>% filter(.bx0 == abox) 
  plot(poly)
  plot(aline, add = TRUE, col = "red")
  
  #scan("", 1)
  test_that("round tripping works to get the right box and faces", {
    expect_true(rgeos::gIntersects(aline, poly))
    expect_true(rgeos::gCovers(poly, aline))
    expect_true(rgeos::gCoveredBy(aline, poly))
    
  })
}