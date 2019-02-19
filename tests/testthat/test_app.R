context("Test shiny app")

library(shinytest)

test_that("app gets expected output", {
  testthat::skip_on_cran()
  appdir <- system.file(package = "shinymap", "app")
  expect_pass(testApp(appdir, compareImages = FALSE))
})
