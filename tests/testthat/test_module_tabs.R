test_that("tab_server", { 
  
  # filtrert data
  filtered_data <- readRDS("data/filtered_data_3.rds")
  kart <- sf::st_read("data/kart_dagkir.shp")
  
  #class(map_to_plot)
  #sf::st_crs(kart)
  #sf::st_crs(map_to_plot)
  
  shiny::testServer(tab_server, {
    
    expect_equal_to_reference(output$plot_map, "data/plot_map.rds")
    expect_equal_to_reference(output$make_table, "data/make_table.rds")
  
    expect_equal_to_reference(output$plot_histogram, "data/plot_histogram.rds")
    
    #lar seg ikke bruke..src er character/URL..ingen stedsnavn i den. 
      #plot_units <- output$plot_histogram[["src"]]
      #expect_true(grepl(" ", plot_units))
    
  } , args = list(data = filtered_data,
                  map = shiny::reactive(kart),
                  config = readRDS("data/get_config.rds"),
                  language = shiny::reactive("nb")
  )
                
  )
  
}
)
