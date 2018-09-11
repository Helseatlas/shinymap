context("Create tmpdir")

test_that("The temporary directory can be created", {
  expect_is(create_appDir(data = NULL), "character")
}
)
