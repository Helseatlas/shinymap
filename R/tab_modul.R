# tab modul

tab_ui1 <-  function(id){
          ns <- shiny::NS(id)
            tagList(leaflet::leafletOutput(ns("plot_map")))}

tab_ui2 <-  function(id){
          ns <- shiny::NS(id)
            tagList(shiny::plotOutput(ns("plot_histogram")))}


tab_ui3 <-  function(id){
          ns <- shiny::NS(id)
            tagList(shiny::tableOutput(ns("make_table")))}



tab_server <- function(id) {
                  shiny::moduleServer(id, 
                              function(input, output, session) {
                                
                  # MAP
                   
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
                                
                  # BARCHART
                       
                                
                                output$plot_histogram <- shiny::renderPlot({
                                  filtered_data <- helseatlas::filter_out(atlas_data(),
                                                                          filter1 = input$menu_level1,
                                                                          filter2 = input$menu_level2,
                                                                          filter3 = input$menu_level3)
                                  
                                  plot <- helseatlas::plot_variation(
                                    input_data = filtered_data,
                                    xlab = config$plot$xlab[[input$language]],
                                    ylab = input$menu_level1
                                  )
                                  return(plot)
                                }
                                , height = 800, width = 600)
                                
                                       
                                
                  # TABLE
                                
                                output$make_table <- shiny::renderTable({
                                  if (is.null(input$menu_level1)) {
                                  return(NULL)
                                 }
                                  filtered_data <- helseatlas::filter_out(atlas_data(),
                                                            filter1 = input$menu_level1,
                                                            filter2 = input$menu_level2,
                                                            filter3 = input$menu_level3)
                    
                                # Return null if data in invalid
                                if (is.null(nrow(filtered_data)) || nrow(filtered_data) == 0) {
                                return(NULL)
                                }
                    
                                tabular_data <- data.frame(filtered_data$area_name)
                                colnames(tabular_data) <- c(config$plot$xlab[[input$language]])
                                value_name <- as.character(unique(filtered_data$type))
                                tabular_data[value_name] <- filtered_data$value
                                numerator_name <- as.character(unique(filtered_data$numerator_name))
                                tabular_data[numerator_name] <- filtered_data$numerator
                                denominator_name <- as.character(unique(filtered_data$denominator_name))
                                tabular_data[denominator_name] <- filtered_data$denominator
                                # Sort data
                                tabular_data <- tabular_data[order(tabular_data[, 2], na.last = TRUE, decreasing = TRUE), ]
                                # Format numbers
                                tabular_data[, -1] <- sapply(tabular_data[, -1],
                                                 FUN = function(x) format(x,
                                                                          digits = 2,
                                                                          decimal.mark = config$num$decimal[[input$language]],
                                                                          big.mark = config$num$big[[input$language]]
                                                 )
                                )
                                return(tabular_data)
                                 }
                               , align = "lrrr")
})}
                  
                                
                            