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
  expect_equal_to_reference(make_map(data = testdata,
                                     map = testmap,
                                     type = "simple",
                                     utm33 = FALSE),
                            "data/make_map3.rds")
  expect_error(make_map(type = "simple"))
  expect_null(make_map(type = "siple", map = testmap))
  expect_error(make_map())
})