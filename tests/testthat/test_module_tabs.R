test_that("tab_server", {

  # filtrert data
  filtered_data <- readRDS("data/filter_2.rds")
  kart <- kart::dagkir

  shiny::testServer(tab_server, {

    expect_equal(class(output$plot_map), "json")
    map <- as.character(output$plot_map)
    # Check that it is some human made values in map
    expect_true(grepl("<strong>Forde", map))
    # Check that map is a large json...
    expect_true(nchar(map) > 1000000)

    expect_equal(class(output$make_table), "json")
    table <- as.character(output$make_table)
    # Check that it is some human made values in table
    expect_true(grepl("<th>Innbyggere", table))

    histogram <- output$plot_histogram
    # Check y values (name of health trusts)
    expect_equal_to_reference(histogram[["coordmap"]][["panels"]][[1]][["domain"]][["discrete_limits"]][["y"]], "data/plot_histogram_y.rds")
    expect_equal(histogram[["coordmap"]][["panels"]][[1]][["mapping"]][["x"]], "get(\"value\")")
    expect_equal(histogram[["coordmap"]][["panels"]][[1]][["mapping"]][["y"]], "get(\"area_name\")")
    expect_equal(histogram[["coordmap"]][["panels"]][[1]][["mapping"]][["fill"]], "get(\"brks\")")

  }, args = list(data = shiny::reactive(filtered_data),
                  map = shiny::reactive(kart),
                  config = readRDS("data/get_config.rds"),
                  language = shiny::reactive("nb")
                  )
  )
})
