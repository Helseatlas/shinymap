context("make_map")

test_that("leaflet map is made", {
  expect_equal_to_reference(makeLeafletmap(), "data/leaflet.rds")
})
