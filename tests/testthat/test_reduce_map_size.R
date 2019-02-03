context("reduce_map_size")

test_that("map is reduced in size", {
  
  expect_equal_to_reference(reduce_map_size(), "data/reduced_map.rds")
})
