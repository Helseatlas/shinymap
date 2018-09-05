context("Read IAjson")

test_that("JSON files from IA can be read", {
  file = "data/eldre.js"
  tmp <- readIAjson(json_file = file)
  expect_equal_to_reference(tmp, "data/json1")
  file = "data/eldre_eng.js"
  tmp <- readIAjson(json_file = file)
  expect_equal_to_reference(tmp, "data/json2")
#  file = "data/barn.js"
#  tmp <- readIAjson(json_file = file)
#  expect_equal_to_reference(NULL, "data/json3")
}
)
