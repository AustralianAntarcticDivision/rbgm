context("basic-bgm")

files <- list.files(system.file("extdata/bgm", package = "bgmfiles"), pattern = "bgm$", 
                    full.names = TRUE)

test_that("read_bgm", {
  expect_that(
  read_bgm(files[1])
  , is_a("list"))
  expect_silent(lapply(files, read_bgm))
})
