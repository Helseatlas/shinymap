library(shinytest)
library(testthat)

context("Test shiny app")

#open shiny app
app <- ShinyDriver$new('../../inst/app')

test_that("app gets expected output", {
  #set numeric input
  app$setInputs(level1 = "20")
  #get output
  output <- app$getValue(name = "pickLevel1")
  #test
  expect_equal(output, "20")  
})

#stop shiny app
app$stop()