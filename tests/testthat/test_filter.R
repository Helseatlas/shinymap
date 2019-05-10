context("filter_out")

test_that("eldre can be filtered correctly", {
  file <- readRDS("data/eldre.rds")
  tmp <- filter_out(file,
                   filter1 = "Allmennlegetjenesten",
                   filter2 = "Alle konsultasjoner")
  expect_equal_to_reference(tmp, "data/filter_1.rds")
})

test_that("dagkir can be filtered correctly", {
  file <- readRDS("data/dagkir.rds")
  tmp <- filter_out(file,
                   filter1 = "Allmennlegetjenesten",
                   filter2 = "Alle konsultasjoner")
  expect_equal(length(tmp$area), 0)

  tmp <- filter_out(file,
                   filter2 = "Menisk")
  expect_equal_to_reference(tmp, "data/filter_2.rds")
})

test_that("kols can be filtered correctly", {
  file <- readRDS("data/kols.rds")
  tmp <- filter_out(file,
                   filter1 = "Akuttinnleggelser",
                   filter2 = "Akuttinnlagte personer")
  expect_equal_to_reference(tmp, "data/filter_3.rds")
})

test_that("barn can be filtered correctly", {
  file <- readRDS("data/barn.rds")
  tmp <- filter_out(file,
                   filter1 = "Medisinske tilstander, spesialisthelsetjenesten",
                   filter2 = "Kontakttype",
                   filter3 = "Utvalgte akutte diagnoser")
  expect_equal_to_reference(tmp, "data/filter_4.rds")
})

test_that("barn can be filtered correctly 2", {
  file <- readRDS("data/barn.rds")
  tmp <- filter_out(file,
                   filter1 = "Medisinske tilstander, spesialisthelsetjenesten",
                   filter2 = "Kontakttype")
  expect_equal_to_reference(tmp, "data/filter_5.rds")
})

test_that("barn can be filtered correctly 3", {
  file <- readRDS("data/barn.rds")
  tmp <- filter_out(file)
  expect_equal(tmp, file)
})

test_that("filter_out will return NULL", {
  expect_null(filter_out(NULL))
})

test_that("filter_out will return ERROR", {
  expect_error(filter_out())
})
