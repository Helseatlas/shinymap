test_that("app_server", {
  expect_error(app_server())
  expect_equal(typeof(app_server(output = NULL)), "environment")
})
