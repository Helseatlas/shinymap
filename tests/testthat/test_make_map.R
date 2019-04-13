context("make_map")

test_that("make_map is OK", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(make_map(data = testdata,
                                    map = testmap,
                                    type = "leaflet"),
                            "data/make_map1.rds",
                            tolerance = 5e-5)
  expect_equal_to_reference(make_map(data = testdata,
                                    map = testmap,
                                    type = "simple"),
                            "data/make_map2.rds")
  expect_null(make_map(type = "siple"))
  expect_error(make_map())
})


test_that("plotLeafletmap is OK", {
  testdata <- readRDS("data/eldre.rds")
  testmap <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(plotLeafletmap(data = testdata, map = testmap, utm33 = TRUE),
                            "data/leaflet1.rds", tolerance = 5e-5)
  expect_equal_to_reference(plotLeafletmap(data = testdata, map = testmap, utm33 = FALSE),
                            "data/leaflet2.rds")
  expect_equal_to_reference(plotLeafletmap(data = NULL,
                                           map = testmap,
                                           utm33 = FALSE),
                            "data/leaflet2.rds")
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
