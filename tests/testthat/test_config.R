test_that("create_config", {
  expect_equal(create_config(dir = "data"), paste0("Cannot create data/_helseatlas.yml config file: ",
                                                   "already exists. ",
                                                   "(run with force = TRUE if you want to overwrite file)"))

  expect_false(file.exists("_helseatlas.yml"))
  expect_equal(create_config(), "./_helseatlas.yml file created: fill it in")
  expect_true(file.exists("_helseatlas.yml"))
  file.remove("_helseatlas.yml")
  expect_false(file.exists("_helseatlas.yml"))
})

test_that("check_config", {
  test_config <- readRDS("data/get_config.rds")
  expect_null(check_config(test_config))

  test_config <- NULL
  expect_error(check_config(test_config))

  test_config <- list()
  expect_error(check_config(test_config))

  test_config <- list("a" = 2.5, "b" = TRUE)
  expect_error(check_config(test_config))

  expect_error(check_config("qwerty"))
})

test_that("get_config", {
  expect_equal_to_reference(get_config(dir = "data"), "data/get_config.rds")

  # This will fail if default version of helseatlas.yml has been changed.
  expect_equal_to_reference(get_config(), "data/get_config_default.rds")
})
