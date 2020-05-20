test_that("plot_variation is OK", {
  expect_null(plot_variation())

  data <- "TEST"
  data$value <- "1"
  expect_null(plot_variation(input_data = data, type = "tmp"))
})
