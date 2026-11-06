
skip_if_not_installed("bgmfiles")

files <- bgmfiles::bgmfiles()
test_that("bgmfile works", {
  expect_type(bgmfile(files[1]), "list")
  ## this might change if the numfaces/actual_numfaces thing is fixed
  expect_output(lapply(files, bgmfile))
})

for (i in seq_along(files)) {
b <- bgmfile(files[i])
test_that("conversion to Spatial works", {
          expect_s4_class(boxSpatial(b), "SpatialPolygonsDataFrame")
          expect_s4_class(faceSpatial(b), "SpatialLinesDataFrame")
          expect_s4_class(boundarySpatial(b), "SpatialPolygonsDataFrame")
          expect_s4_class(pointSpatial(b), "SpatialPointsDataFrame")
          expect_s4_class(nodeSpatial(b), "SpatialPointsDataFrame")
         # testthat::expect_lt(nrow(pointSpatial(b)), nrow(nodeSpatial(b)))
}
          
          )
}

