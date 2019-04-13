context("Read IAjson")

test_that("JSON files from IA can be read", {
  file <- "data/eldre.json"
  tmp <- read_iajson(json_file = file)
  expect_equal_to_reference(tmp, "data/eldre.rds")
  file <- "data/eldre_eng.json"
  tmp <- read_iajson(json_file = file)
  expect_equal_to_reference(tmp, "data/eldre_eng.rds")
  file <- "data/barn.json"
  tmp <- read_iajson(json_file = file)
  expect_equal_to_reference(tmp, "data/barn.rds")
  file <- "data/kols.json"
  tmp <- read_iajson(json_file = file)
  expect_equal_to_reference(tmp, "data/kols.rds")
  file <- "data/dagkir.json"
  tmp <- read_iajson(json_file = file)
  expect_equal_to_reference(tmp, "data/dagkir.rds")
  file <- "data/nyfodt.json"
  tmp <- read_iajson(json_file = file)
  expect_equal_to_reference(tmp, "data/nyfodt.rds")
})
