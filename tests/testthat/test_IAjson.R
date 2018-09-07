context("Read IAjson")

test_that("JSON files from IA can be read", {
  file = "data/eldre.json"
  tmp <- readIAjson(json_file = file)
  expect_equal_to_reference(tmp, "data/json1.rds")
  file = "data/eldre_eng.json"
  tmp <- readIAjson(json_file = file)
  expect_equal_to_reference(tmp, "data/json2.rds")
  file = "data/barn.json"
  tmp <- readIAjson(json_file = file)
  expect_equal_to_reference(tmp, "data/json3.rds")
}
)
