context("plot_variation")

test_that("plot_variation is OK", {
  expect_null(plot_variation())

  data <- readRDS("data/start_plot.rds")
  expect_null(plot_variation(input_data = data, type = "tmp"))
})
