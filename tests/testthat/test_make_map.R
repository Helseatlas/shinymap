context("make_map")

test_that("make_map returns error if map is not given", {
  expect_error(suppressWarnings(make_map(type = "simple")))
})

test_that("make_map returns error if type is unknown", {
  expect_error(make_map(type = "qwerty"))
})

test_that("make_map returns error if type is unknown and map is given", {
  testmap <- geojsonio::geojson_read("data/test.geojson", what = "sp")
  expect_error(make_map(type = "siple", map = testmap))
})

test_that("make_map returns error if no input is given", {
  expect_error(make_map())
})
