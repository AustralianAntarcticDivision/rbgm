context("basic-bgm")

files <- bgmfiles::bgmfiles()
test_that("bgmfile works", {
  expect_that(
  bgmfile(files[1])
  , is_a("list"))
  ## this might change if the numfaces/actual_numfaces thing is fixed
  expect_output(lapply(files, bgmfile))
})

for (i in seq_along(files)) {
b <- bgmfile(files[i])
test_that("conversion to Spatial works", {
          expect_that(boxSpatial(b), is_a("SpatialPolygonsDataFrame"))
          expect_that(faceSpatial(b), is_a("SpatialLinesDataFrame"))
          expect_that(boundarySpatial(b), is_a("SpatialPolygonsDataFrame"))
          expect_that(pointSpatial(b), is_a("SpatialPointsDataFrame"))
          expect_that(nodeSpatial(b), is_a("SpatialPointsDataFrame"))
         # testthat::expect_lt(nrow(pointSpatial(b)), nrow(nodeSpatial(b)))
}
          
          )
}

