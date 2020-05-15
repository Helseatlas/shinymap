# Scripts to create data used by the tests

fix_norw_chr <- function(testdata) {
  testdata$area_name <- gsub("å", "aa", testdata$area_name)
  testdata$area_name <- gsub("ø", "o", testdata$area_name)
  testdata$area_name <- gsub("æ", "ae", testdata$area_name)
  testdata$area_name <- gsub("Å", "AA", testdata$area_name)
  testdata$area_name <- gsub("Ø", "O", testdata$area_name)
  testdata$area_name <- gsub("Æ", "AE", testdata$area_name)

  testdata$level1_name <- gsub("å", "aa", testdata$level1_name)
  testdata$level1_name <- gsub("ø", "o", testdata$level1_name)
  testdata$level1_name <- gsub("æ", "ae", testdata$level1_name)
  testdata$level1_name <- gsub("Å", "AA", testdata$level1_name)
  testdata$level1_name <- gsub("Ø", "O", testdata$level1_name)
  testdata$level1_name <- gsub("Æ", "AE", testdata$level1_name)

  if ("level2_name" %in% names(testdata)) {
    testdata$level2_name <- gsub("å", "aa", testdata$level2_name)
    testdata$level2_name <- gsub("ø", "o", testdata$level2_name)
    testdata$level2_name <- gsub("æ", "ae", testdata$level2_name)
    testdata$level2_name <- gsub("Å", "AA", testdata$level2_name)
    testdata$level2_name <- gsub("Ø", "O", testdata$level2_name)
    testdata$level2_name <- gsub("Æ", "AE", testdata$level2_name)
  }
  if ("level3_name" %in% names(testdata)) {
    testdata$level3_name <- gsub("å", "aa", testdata$level3_name)
    testdata$level3_name <- gsub("ø", "o", testdata$level3_name)
    testdata$level3_name <- gsub("æ", "ae", testdata$level3_name)
    testdata$level3_name <- gsub("Å", "AA", testdata$level3_name)
    testdata$level3_name <- gsub("Ø", "O", testdata$level3_name)
    testdata$level3_name <- gsub("Æ", "AE", testdata$level3_name)
  }

  return(testdata)
}

kols <- fix_norw_chr(data::kols)
saveRDS(kols, "tests/testthat/data/kols.rds")

barn <- fix_norw_chr(data::barn)
saveRDS(barn, "tests/testthat/data/barn.rds")

eldre <- fix_norw_chr(data::eldre)
saveRDS(eldre, "tests/testthat/data/eldre.rds")

dagkir <- fix_norw_chr(data::dagkir)
saveRDS(dagkir, "tests/testthat/data/dagkir.rds")

dagkir2_en <- fix_norw_chr(data::dagkir2_en)
saveRDS(dagkir2_en, "tests/testthat/data/dagkir2_en.rds")
