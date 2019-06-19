context("plot_variation")

test_that("plot_variation is OK", {
  expect_null(plot_variation())

  expect_null(plot_variation(input_data = data, type = "tmp"))
})
