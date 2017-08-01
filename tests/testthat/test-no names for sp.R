context("no names for sp")

bgm <- bgmfile(bgmfiles::bgmfiles()[3])
bgmbox <- boxSpatial(bgm)

bgmface <- faceSpatial(bgm)

test_that("sp gets no names", {
  expect_null(names(bgmbox@polygons[[1]]@Polygons))
  expect_null(unlist(lapply(bgmbox@polygons, function(x) names(x@Polygons))))
  expect_null(names(bgmface@lines[[1]]@Lines))
  expect_null(unlist(lapply(bgmface@lines, function(x) names(x@Lines))))
  
})
