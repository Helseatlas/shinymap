# UI module
tab_ui <- function(id){
  ns <- shiny::NS(id)
  
  tagList(
      shiny::tabPanel("Map"),
      shiny::tabPanel("Barchart"),
      shiny::tabPanel("Table")
    )
}


# Server module
tab_server <- function(id, output, session) {
    shiny::moduleServer(id, function(input, output, session) {
  
  #data: filtered_data
  
  
  #tab: map
  
  
  output$plot_map <- leaflet::renderLeaflet({
    if (is.null(input$menu_level1) | isTRUE(getOption("shiny.testmode"))) {
      return(NULL)
    }
    filtered_data <- helseatlas::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)
    
    if (is.null(nrow(filtered_data)) || nrow(filtered_data) == 0) {
      # Return null if data in invalid
      return(NULL)
    }
    
    map <- helseatlas::make_map(map = atlas_map(), data = filtered_data)
    return(map)
    
  })
    })
}

  





