context("Test shiny app")

test_that("app gets expected output", {
  testthat::skip_on_cran()
  appdir <- system.file(package = "shinymap", "app")
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
})
