test_that("run_app", {
  expect_equal(typeof(run_app()), "list")

  test_chr <- as.character(run_app())
  expect_true(grepl("function", test_chr[1]))
  expect_true(grepl("server", test_chr[2]))
  expect_equal(test_chr[3], "NULL")
  expect_equal(test_chr[4], "list()")
  expect_true(grepl("appDir", test_chr[5]))
})
