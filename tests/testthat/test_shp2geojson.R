context("shp2geojson")

test_that("shp2geojson is OK", {
  # Default
  expect_equal_to_reference(shp2geojson(folder = "data/maps",
                                        shapefile = "shapefile1",
                                        geojson = NULL),
                            "data/shp2geojson1.json")

  # Do not reduce size
  expect_equal_to_reference(shp2geojson(folder = "data/maps",
                                        shapefile = "shapefile1",
                                        geojson = NULL,
                                        reduce_size = FALSE),
                            "data/shp2geojson2.json")

  # Do not reduce size, but change amount parametre (should be equal test above)
  expect_equal_to_reference(shp2geojson(folder = "data/maps",
                                        shapefile = "shapefile1",
                                        geojson = NULL,
                                        reduce_size = FALSE,
                                        amount = 0.01),
                            "data/shp2geojson2.json")

  # Reduce size with a factor 0.01 (default 0.1)
  expect_equal_to_reference(shp2geojson(folder = "data/maps",
                                        shapefile = "shapefile2",
                                        geojson = NULL,
                                        amount = 0.01),
                            "data/shp2geojson3.json")
})

test_that("utm33toLeaflet is OK", {
    # Unit test that map can be converted from utm33 to leaflet projection
  map <- geojsonio::geojson_read("data/maps/test.geojson", what = "sp")
  expect_equal_to_reference(utm33toLeaflet(map), "data/utm33toLeaflet.rds", tolerance=1e-4)
})

test_that("reduce_map_size is OK", {
    # Unit test that map can be reduced in size
  map <- geojsonio::geojson_read("data/maps/kommuner.geojson", what = "sp")
  expect_equal_to_reference(reduce_map_size(map, 0.1), "data/reduce_map_size.rds", tolerance=1e-8)
})
