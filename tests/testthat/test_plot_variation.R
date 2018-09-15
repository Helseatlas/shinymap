context("plot_variation")

test_that("plot_variation is working", {
  tmp <- readRDS("data/filter_1.rds")
  myplot <- plotVariation(inputData = tmp, xlab = "Opptaksomr", ylab = "Rate", type = "histogram")
  expect_equal_to_reference(myplot, "data/plot1.rds")
})
