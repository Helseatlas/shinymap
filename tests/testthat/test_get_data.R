test_that("get_data", {
  test_data <- get_data()
  current_atlas <- c("fodsel", "gyn", "ortopedi", "dagkir2", "kols", "eldre", "nyfodt", "barn", "dagir")
  for (i in current_atlas) {
    # check if atlas is in data
    expect_true(i %in% names(test_data))

    # check if area numbers in map correspond to numbers in Norwegian data
    expect_equal(sort(test_data[[i]]$map$area_num),
                 sort(unique(dplyr::filter(test_data[[i]]$data_no, area < 100)$area))
    )
    # check if area numbers in map correspond to numbers in English data
    expect_equal(sort(test_data[[i]]$map$area_num),
                 sort(unique(dplyr::filter(test_data[[i]]$data_en, area < 100)$area))
    )
  }
})
