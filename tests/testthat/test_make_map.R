context("make_map")

test_that("leaflet map is made", {
  expect_equal_to_reference(makeLeafletmap(), "data/leaflet.rds")
})

test_that("ggplot2 map is made", {
  expect_equal_to_reference(plotggmap(mapfile = ), "data/ggmap.rds")
})
