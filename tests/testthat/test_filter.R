context("filter_out")

test_that("eldre can be filtered correctly", {
  file = readRDS("data/eldre.rds")
  tmp <- filterOut(file, filter1 = "Allmennlegetjenesten", filter2 = "Alle konsultasjoner")
  expect_equal_to_reference(tmp, "data/filter_1.rds")
})

test_that("dagkir can be filtered correctly", {
  file = readRDS("data/dagkir.rds")
  tmp <- filterOut(file, filter1 = "Allmennlegetjenesten", filter2 = "Alle konsultasjoner")
  expect_equal(length(tmp$area), 0)
  
  tmp <- filterOut(file, filter2 = "Menisk")
  expect_equal_to_reference(tmp, "data/filter_2.rds")
  
})

test_that("kols can be filtered correctly", {
  file = readRDS("data/kols.rds")
  tmp <- filterOut(file, filter1 = "Akuttinnleggelser", filter2 = "Akuttinnlagte personer")
  expect_equal_to_reference(tmp, "data/filter_3.rds")
})

test_that("barn can be filtered correctly", {
  file = readRDS("data/barn.rds")
  tmp <- filterOut(file, filter1 = "Medisinske tilstander, spesialisthelsetjenesten", filter2 = "Kontakttype", filter3 = "Utvalgte akutte diagnoser")
  expect_equal_to_reference(tmp, "data/filter_4.rds")
})

