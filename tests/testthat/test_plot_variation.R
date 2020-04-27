test_that("plot_variation is OK", {
  expect_null(plot_variation())

  data <- readRDS("data/start_plot.rds")
  expect_null(plot_variation(input_data = data, type = "tmp"))

  test_plot <- plot_variation(input_data = data)
  expect_equal_to_reference(test_plot$data, "data/plot_variation_data.rds")
  expect_equal_to_reference(test_plot$labels, "data/plot_variation_labels.rds")
  expect_equal_to_reference(test_plot$mapping, "data/plot_variation_mapping.rds")
})
