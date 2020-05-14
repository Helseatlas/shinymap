# tab modul

tab_ui1 <-  function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(leaflet::leafletOutput(ns("plot_map"), height = 800)
   )
  }

tab_ui2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::plotOutput(ns("plot_histogram"))
   )
  }

tab_ui3 <-  function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(DT::DTOutput(ns("make_table"))
   )
  }

tab_server <- function(id, data, map, config, language) {
  shiny::moduleServer(id, function(input, output, session) {
    # MAP
    output$plot_map <- leaflet::renderLeaflet({
      #map_to_plot <- sf::st_transform(map(), 32633) #unødvendig kode
      map <- helseatlas::make_map(map = map(), data = data())
      return(map)
     }
    )
    # BARCHART
    output$plot_histogram <- shiny::renderPlot({
      plot <- helseatlas::plot_variation(
        input_data = data(),
        xlab = config$plot$xlab[[language()]],
        ylab = "input$menu_level1"
        )
      return(plot)
      }
      , height = 800, width = 600)
    # TABLE
    output$make_table <- DT::renderDT({
      tabular_data <- data.frame(data()$area_name)
      colnames(tabular_data) <- c(config$plot$xlab[[language()]])
      value_name <- as.character(unique(data()$type))
      tabular_data[value_name] <- data()$value
      numerator_name <- as.character(unique(data()$numerator_name))
      tabular_data[numerator_name] <- data()$numerator
      denominator_name <- as.character(unique(data()$denominator_name))
      tabular_data[denominator_name] <- data()$denominator
      # Sort data
      tabular_data <- tabular_data[order(tabular_data[, 2], na.last = TRUE, decreasing = TRUE), ]
      # Format numbers
      tabular_data[, -1] <- sapply(tabular_data[, -1],
                              FUN = function(x) format(x,
                                                       digits = 2,
                                                       decimal.mark = config$num$decimal[[language()]],
                                                       big.mark = config$num$big[[language()]]
                                                       )
                              )
      return(tabular_data)
      }
      , rownames = FALSE
      , options = list(columnDefs = list(list(class = "dt-right", targets = 1:3)),
                                  info = FALSE, lengthMenu = list(c(-1, 15), c("All", "15"))))
   })
}
