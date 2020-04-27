context("make_map")

test_that("make_map returns error if map is not given", {
  expect_error(suppressWarnings(make_map(type = "simple")))
})

test_that("make_map returns error if type is unknown", {
  expect_error(make_map(type = "qwerty"))
})

test_that("make_map returns error if type is unknown and map is given", {
  expect_error(make_map(type = "siple", map = testmap))
})

test_that("make_map returns error if no input is given", {
  expect_error(make_map())
})

test_that("make_map can plot testmap/testdata", {
  test_data <- testdata %>%
    dplyr::filter(level1 == 1) %>%
    dplyr::filter(level2 == 1) %>%
    dplyr::filter(level3 == 1)
  test_map <- make_map(data = test_data, map = testmap)
  expect_true("leaflet" %in% class(test_map))
  expect_true("htmlwidget" %in% class(test_map))

  expect_true(grepl("EPSG3857", test_map$x$options$crs$crsClass))
  expect_equal(test_map$x$options$minZoom, 5)
  expect_equal(test_map$x$options$maxZoom, 5)

  expect_equal(round(test_map$x$limits$lat, 5), c(57.96052, 71.18488))
  expect_equal(round(test_map$x$limits$lng, 5), c(4.50865, 31.16453))

  expect_equal(test_map$sizingPolicy$defaultHeight, 400)
  expect_equal(test_map$sizingPolicy$defaultWidth, "100%")

  expect_equal(test_map[["x"]][["calls"]][[1]][["method"]], "addPolygons")

  # This should not be hard coded!
  expect_equal(test_map[["x"]][["calls"]][[2]][["args"]][[1]][["title"]], "Antall per 100 000 innbygger")

  expect_equal_to_reference(test_map[["x"]][["calls"]][[1]][["args"]][[7]], "data/make_map_1.rds")

  # Test with another data point
  test_data <- testdata %>%
    dplyr::filter(level1 == 1) %>%
    dplyr::filter(level2 == 2) %>%
    dplyr::filter(level3 == 3)
  test_map <- make_map(data = test_data, map = testmap)
  expect_equal_to_reference(test_map[["x"]][["calls"]][[1]][["args"]][[7]], "data/make_map_2.rds")
})
