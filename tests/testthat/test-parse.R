context("parse")

## TODO: Rename context
## TODO: Add more tests

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
