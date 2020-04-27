test_that("version_info is returning something meaningful", {
  test_string <- version_info()
  expect_true(grepl("helseatlas", test_string))
  expect_true(grepl("data", test_string))
  expect_true(grepl("kart", test_string))
  expect_true(grepl("SKDEr", test_string))
})
