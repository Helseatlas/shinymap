context("shp2geojson")

test_that("shp2geojson is OK", {
  # Default
  expect_equal_to_reference(
    shp2geojson(
      folder = "data/maps",
      shapefile = "shapefile1",
      geojson = NULL
    ),
    "data/shp2geojson1.json"
  )

  # Do not reduce size
  expect_equal_to_reference(
    shp2geojson(
      folder = "data/maps",
      shapefile = "shapefile1",
      geojson = NULL,
      reduce_size = FALSE
    ),
    "data/shp2geojson2.json"
  )

  # Do not reduce size, but change amount parametre (should be equal test above)
  expect_equal_to_reference(
    shp2geojson(
      folder = "data/maps",
      shapefile = "shapefile1",
      geojson = NULL,
      reduce_size = FALSE,
      amount = 0.01
    ),
    "data/shp2geojson2.json"
  )

  # Reduce size with a factor 0.01 (default 0.1), and save file
  expect_equal_to_reference(
    shp2geojson(
      folder = "data/maps",
      shapefile = "shapefile2",
      geojson = "tmp",
      amount = 0.01
    ),
    "data/shp2geojson3.json"
  )
  file.remove("tmp.geojson")
})
