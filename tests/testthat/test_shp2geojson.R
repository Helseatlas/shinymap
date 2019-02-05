context("reduce_map_size")

test_that("map is reduced in size", {
  expect_equal_to_reference(shp2geojson(folder = "data/maps",
                                        shapefile = "eldre",
                                        geojson = "test"),
                            "data/shp2geojson.json")
})
