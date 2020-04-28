test_that("app_ui", {
  ref_chr <- as.character(purrr::flatten(app_ui()))[4]
  grep_elements <- c("shiny-html-output",
                     "hn.png",
                     "Helseatlas",
                     "app_info",
                     "plot_histogram"
                     )
  for (i in grep_elements) {
    expect_true(grepl(i, ref_chr, fixed = TRUE))
  }
})
