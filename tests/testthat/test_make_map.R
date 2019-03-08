context("make_map")

test_that("makeMap is OK", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(makeMap(data = testdata, map = testmap, type = "leaflet"), "data/makeMap1.rds", tolerance=5e-5)
  expect_equal_to_reference(makeMap(data = testdata, map = testmap, type = "simple"), "data/makeMap2.rds")
  expect_null(makeMap(type = "siple"))
  expect_error(makeMap())
})


test_that("plotLeafletmap is OK", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(plotLeafletmap(data = testdata, map = testmap, utm33 = TRUE), "data/leaflet1.rds", tolerance=5e-5)
  expect_equal_to_reference(plotLeafletmap(data = testdata, map = testmap, utm33 = FALSE), "data/leaflet2.rds")
  expect_equal_to_reference(plotLeafletmap(data = NULL, map = testmap, utm33 = FALSE), "data/leaflet2.rds")
  expect_error(plotLeafletmap())
  expect_error(plotLeafletmap(utm33 = FALSE))
})

test_that("plotSimpleMap is OK", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(plotSimpleMap(data = testdata, map = testmap), "data/simpleMap1.rds")
  expect_equal_to_reference(plotSimpleMap(data = NULL, map = testmap), "data/simpleMap1.rds")
  expect_error(suppressWarnings(plotSimpleMap()))
  expect_error(suppressWarnings(plotSimpleMap(utm33 = FALSE)))
})
