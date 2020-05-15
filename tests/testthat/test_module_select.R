test_that("select_server", {
  kols <- readRDS("data/kols.rds")
  eldre <- readRDS("data/eldre.rds")
  barn <- readRDS("data/barn.rds")
  dagkir <- readRDS("data/dagkir.rds")
  dagkir2 <- readRDS("data/dagkir2_en.rds")

  atlas_data <- list(
    "kols"   = list("data_no" = kols,
                    "data_en" = kols,
                    "year" = "2013–2015",
                    "title_no" = "Kols",
                    "title_en" = "COPD"
                    ),
    "eldre"  = list("data_no" = eldre,
                    "data_en" = eldre,
                    "year" = "2013–2015",
                    "title_no" = "Eldre",
                    "title_en" = "Elderly"
                    ),
    "barn"   = list("data_no" = barn,
                    "data_en" = barn,
                    "year" = "2011–2014",
                    "title_no" = "Barn",
                    "title_en" = "Children"
                    ),
    "dagir"  = list("data_no" = dagkir,
                    "data_en" = dagkir,
                    "year" = "2011–2013",
                    "title_no" = "Dagkirurgi",
                    "title_en" = "Day surgey"
                    ),
    "dagir2" = list("data_no" = dagkir2,
                    "data_en" = dagkir2,
                    "year" = "qwerty",
                    "title_no" = "Dagkirurgi v2",
                    "title_en" = "Day surgey v2"
                    )
  )

  shiny::testServer(select_server, {

    session$setInputs(atlas = "kols")
    # Same selection as in test_filter_out
    session$setInputs(menu_level1 = "Akuttinnleggelser")
    session$setInputs(menu_level2 = "Akuttinnlagte personer")

    expect_equal_to_reference(output$pick_atlas, "data/pick_atlas_1.rds")
    expect_equal_to_reference(output$pick_level1, "data/pick_level1_1.rds")
    expect_equal_to_reference(output$pick_level2, "data/pick_level2_1.rds")
    expect_null(output$pick_level3)
    expect_equal_to_reference(filtered_data(), "data/filter_3.rds")

  }, args = list(language = shiny::reactive("nb"),
                 data = atlas_data,
                 config = readRDS("data/get_config.rds")
                 )
  )

  # Test three level atlas, english language
  shiny::testServer(select_server, {

    session$setInputs(menu_level1 = "Medisinske tilstander, spesialisthelsetjenesten")
    session$setInputs(menu_level2 = "Kontakttype")

    expect_equal_to_reference(output$pick_atlas, "data/pick_atlas_2.rds")
    session$setInputs(atlas = "barn")
    expect_equal_to_reference(output$pick_level1, "data/pick_level1_2.rds")
    expect_equal_to_reference(output$pick_level2, "data/pick_level2_2.rds")
    expect_equal_to_reference(output$pick_level3, "data/pick_level3_2.rds")
    expect_equal_to_reference(filtered_data(), "data/filtered_data_2.rds")
    session$setInputs(menu_level3 = "Utvalgte akutte diagnoser")
    expect_equal_to_reference(filtered_data(), "data/filter_4.rds")

  }, args = list(language = shiny::reactive("en"),
                 data = atlas_data,
                 config = readRDS("data/get_config.rds")
  )
  )

  # Data with years, to test year slider in pick_level2
  shiny::testServer(select_server, {

    session$setInputs(atlas = "dagir2")
    session$setInputs(menu_level1 = "Menisci")
    session$setInputs(menu_level2 = "2017")

    expect_equal_to_reference(output$pick_atlas, "data/pick_atlas_1.rds")
    expect_equal_to_reference(output$pick_level1, "data/pick_level1_3.rds")
    expect_equal_to_reference(output$pick_level2, "data/pick_level2_3.rds")
    expect_null(output$pick_level3)
    expect_equal_to_reference(filtered_data(), "data/filtered_data_4.rds")

    session$setInputs(menu_level2 = "qwerty")
    expect_equal(nrow(filtered_data()), 0)

  }, args = list(language = shiny::reactive("nb"),
                 data = atlas_data,
                 config = readRDS("data/get_config.rds")
  )
  )

})
