library(testthat)
context("round-trip of BGM links")

## TODO: Rename context
## TODO: Add more tests

#devtools::install_github("mdsumner/rbgm")
library(rbgm)
library(dplyr)
library(spdplyr)
bfile <- grep("antarctica_99", bgmfiles::bgmfiles(), value = TRUE)
bgm <- bgmfile(bfile)
polybgm <- rbgm::boxSpatial(bgm)
linebgm <- rbgm::faceSpatial(bgm)

abox <- 9
aline <- linebgm %>% filter(.fx0 %in% (bgm$facesXboxes %>% dplyr::filter(.bx0 == abox))$iface)

plot(polybgm %>% filter(.bx0 == abox))
plot(aline, add = TRUE, col = "red")
aline <- linebgm %>% inner_join(
  bgm$facesXboxes %>% dplyr::filter(.bx0 == abox) %>% dplyr::inner_join(bgm$faces, c("iface" = ".fx0")) %>% transmute(.fx0 = iface))


test_that("round tripping", {
 
})
