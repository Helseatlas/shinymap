context("plot_variation")

test_that("plot_variation is OK", {
  expect_null(plot_variation())

  data <- readRDS("data/start_plot.rds")
  expect_equal_to_reference(plot_variation(input_data = data),
                            "data/plot_variation1.rds")

  expect_equal_to_reference(plot_variation(input_data = data, num_groups = 4),
                            "data/plot_variation1.rds")

  expect_equal_to_reference(plot_variation(input_data = data, num_groups = 11),
                            "data/plot_variation2.rds")

  expect_null(plot_variation(input_data = data, type = "tmp"))
})
