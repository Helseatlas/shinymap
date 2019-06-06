context("make_map")

test_that("make leaflet-map", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(make_map(data = testdata,
                                    map = testmap,
                                    type = "leaflet",
                                    utm33 = TRUE),
                            "data/make_map1.rds",
                            tolerance = 5e-5)
})

test_that("make leaflet-map", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(make_map(data = testdata,
                                    map = testmap,
                                    type = "simple",
                                    utm33 = TRUE),
                            "data/make_map2.rds")
})

test_that("make simple map", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(make_map(data = testdata,
                                     map = testmap,
                                     type = "simple",
                                     utm33 = FALSE),
                            "data/make_map3.rds")
})

test_that("default values", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(make_map(data = testdata,
                                     map = testmap),
                            "data/make_map4.rds")
})

test_that("make_map returns error if map is not given", {
  expect_error(suppressWarnings(make_map(type = "simple")))
})

test_that("make_map returns NULL if type is unknown", {
  expect_null(make_map(type = "qwerty"))
})

test_that("make_map returns NULL if type is unknown and map is given", {
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_null(make_map(type = "siple", map = testmap))
})

test_that("make_map returns error if no input is given", {
  expect_error(make_map())
})
