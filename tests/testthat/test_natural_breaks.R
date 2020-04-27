test_that("natural_breaks is OK", {
  test_data <- c(158098, 160652, 154949, 172279, 160625,
                 164622, 165661, 153231, 162108, 166592,
                 173040, 181220, 164244, 165394, 178204,
                 181195, 154442, 174781, 167731)
  expect_error(natural_breaks(data = test_data, num = 1))
  expect_equal_to_reference(natural_breaks(data = test_data, num = 2), "data/natural_breaks_2.rds")
  expect_equal_to_reference(natural_breaks(data = test_data, num = 3), "data/natural_breaks_3.rds")
  expect_equal_to_reference(natural_breaks(data = test_data, num = 4), "data/natural_breaks_4.rds")
  expect_equal_to_reference(natural_breaks(data = test_data, num = 8), "data/natural_breaks_8.rds")
  expect_warning(natural_breaks(data = test_data, num = 20))
  expect_error(suppressWarnings(natural_breaks(num = 2)))
  expect_error(suppressWarnings(natural_breaks(data = "qwerty")))
  expect_error(suppressWarnings(natural_breaks(data = c("qwerty", "fsdf", "fsdfd", "fsdfsd", "fsdf"))))
})
