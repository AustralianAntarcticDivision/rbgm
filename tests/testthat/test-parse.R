context("parse")
library(testthat)
## TODO: Rename context
## TODO: Add more tests

test_that("bodgy files fail gracefully", {
          expect_that(rbgm::bgmfile(""), throws_error("empty string"))
          expect_that(rbgm::bgmfile("sooner_or_later"), throws_error("no file found"))
          afile <- tempfile()
          file.create(afile)
          on.exit(unlink(afile))
          expect_that(rbgm::bgmfile(afile), throws_error("no lines found in file"))
          
}
          )
boxtext_tab <- c("box0.label\tBoundaryBox0 ", "box0.inside\t1560270.052 2063299.641", 
                 "box0.nconn\t1 ", "box0.iface\t0 ", "box0.ibox\t1 ", "box0.botz\t-200 ", 
                 "box0.area\t2279336755 ", "box0.vertmix\t0.000001 ", "box0.horizmix\t1", 
                 "box0.vert\t1529167.119 2084607.892", "box0.vert\t1564897.829 2089063.371", 
                 "box0.vert\t1603981.132 2072002.25", "box0.vert\t1535623.553 2033062.466", 
                 "box0.vert\t1529167.119 2084607.892")
boxtext_space <- gsub("\\t", " ", boxtext_tab)
test_that("box parse handles different whitespace", {
  expect_that(rbgm:::boxparse(boxtext_tab), is_a("list"))
  expect_that(rbgm:::boxparse(boxtext_space), is_a("list"))
})

test_that("fixproj can handle many weird proj.4 strings", {
  expect_silent(sp::CRS(rbgm:::fixproj("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
  expect_silent(sp::CRS(rbgm:::fixproj("proj=aea lat_1=-18 lat_2=-36 lat_0=0 lon_0=134 x_0=3000000 y_0=6000000 ellps=GRS80 towgs84=0,0,0,0,0,0,0 units=m no_defs")))
  expect_silent(sp::CRS(rbgm:::fixproj("+proj=longlat +a=6378137.0 +es=0.0066943799901413165 +lon_0=0d00 +lat_0=0d00 +x_0=0.0 +y_0=0.0")))
  expect_silent(sp::CRS(rbgm:::fixproj("proj=utm + zone=55 + datum = WGS84 +units=m +no_defs")))
  expect_silent(sp::CRS(rbgm:::fixproj("+proj=laea +lat_0=-63 +lon_0=82 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84")))
  expect_silent(sp::CRS(rbgm:::fixproj("+proj=longlat +a=6378137.0 +es=0.0066943799901413165 +lon_0=0d00 +lat_0=0d00 +x_0=0.0 +y_0=0.0")))
  expect_silent(sp::CRS(rbgm:::fixproj("proj=lcc lat_1=17.5 lat_2=29.5 lat_0=0 lon_0=-102.0 x_0=2000000.0 y_0=0 ellps=clrk66 datum=NAD27 units=m no_defs")))
})