context("basic-bgm")

files <- list.files(system.file("extdata/bgm", package = "bgmfiles"), pattern = "bgm$", 
                    full.names = TRUE)

test_that("read_bgm works", {
  expect_that(
  read_bgm(files[1])
  , is_a("list"))
  expect_silent(lapply(files, read_bgm))
})

b <- read_bgm(sample(files, 1))
test_that("conversion to Spatial works", {
          expect_that(boxSpatial(b), is_a("SpatialPolygonsDataFrame"))
          expect_that(faceSpatial(b), is_a("SpatialLinesDataFrame"))
          expect_that(boundarySpatial(b), is_a("SpatialPolygonsDataFrame"))
}
          
          )
