context("Create tmpdir")

test_that("The temporary directory can be created", {
  expect_is(create_appDir(healthatlas_data = NULL), "character")
}
)
