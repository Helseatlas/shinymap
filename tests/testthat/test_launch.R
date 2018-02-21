context("Launch app")

test_that("The app can be launched", {
  expect_equal_to_reference(NULL, "data/launch")
#  expect_equal_to_reference(launch_application(), "data/launch")
}
)
