context("basic-bgm")

files <- list.files(system.file("extdata/bgm", package = "bgmfiles"), pattern = "bgm$", 
                    full.names = TRUE)

test_that("bgmfile works", {
  expect_that(
  bgmfile(files[1])
  , is_a("list"))
  expect_silent(lapply(files, bgmfile))
})

for (i in seq_along(files)) {
b <- bgmfile(files[i])
test_that("conversion to Spatial works", {
          expect_that(boxSpatial(b), is_a("SpatialPolygonsDataFrame"))
          expect_that(faceSpatial(b), is_a("SpatialLinesDataFrame"))
          expect_that(boundarySpatial(b), is_a("SpatialPolygonsDataFrame"))
}
          
          )
}

