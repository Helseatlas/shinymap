select_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pick_atlas")),
    shiny::uiOutput(ns("pick_level1")),
    shiny::uiOutput(ns("pick_level2")),
    shiny::uiOutput(ns("pick_level3"))
  )
}

select_server <- function(id, language, healthatlas_data, config) {
  stopifnot(is.reactive(language))
  shiny::moduleServer(id, function(input, output, session) {

    output$pick_atlas <- shiny::renderUI({
      shiny::req(language())
      if (!is.data.frame(healthatlas_data)) {
        mytitle <- c()
        atlasnames <- names(healthatlas_data)
        for (i in atlasnames) {
          if (language() == "nb") {
            mytitle <- c(mytitle, paste(healthatlas_data[[i]]$title_no, healthatlas_data[[i]]$year, sep = " "))
          } else if (language() == "en") {
            mytitle <- c(mytitle, paste(healthatlas_data[[i]]$title_en, healthatlas_data[[i]]$year, sep = " "))
          }
        }
        names(atlasnames) <- mytitle
        
        shiny::selectInput(
          selectize = FALSE,
          inputId = session$ns("atlas"),
          label = config$menus$atlas[[language()]],
          choices = atlasnames,
          selected = atlasnames[1]
        )
      }
    })

    atlas_data <- reactive({
      shiny::req(language())
      shiny::req(input$atlas)
      if (language() == "nb") {
        healthatlas_data[[input$atlas]][[1]]
      } else if (language() == "en") {
        healthatlas_data[[input$atlas]][[2]]
      }
    })

    output$pick_level1 <- shiny::renderUI({
      shiny::req(language())
      shiny::req(input$atlas)
      shiny::req(atlas_data())

      # Possible values for level 1
      pickable_level1 <- unique(factor(atlas_data()$level1_name))
      # The selector
      shiny::selectInput(
        selectize = FALSE,
        inputId = session$ns("menu_level1"),
        label = config$menus$level[[language()]],
        choices = pickable_level1,
        selected = pickable_level1[1]
      )
    })
    
    output$pick_level2 <- shiny::renderUI({
      shiny::req(input$menu_level1)
      
      if ("level2_name" %in% colnames(atlas_data())) {
        # Filter out data according to what is choosen for level 1
        tmpdata <- dplyr::filter(atlas_data(), atlas_data()$level1_name == input$menu_level1)
        # Possible values for level 2
        pickable_level2 <- unique(factor(tmpdata$level2_name))
        if (suppressWarnings(all(!is.na(as.numeric(as.character(pickable_level2)))))) {
          # If all the pickable values are integers then it is probably year
          years <- as.numeric(as.character(pickable_level2))
          if (is.null(years) | length(years) == 0) {
            return(NULL)
          }
          shiny::tags$div(class = "year-slider", shiny::sliderInput(
            inputId = session$ns("menu_level2"),
            label = config$menus$year[[language()]],
            min = min(years),
            max = max(years),
            step = 1,
            sep = "",
            ticks = TRUE,
            animate = TRUE,
            value = max(years)
          ))
        } else {
          # The selector
          shiny::selectInput(
            selectize = FALSE,
            inputId = session$ns("menu_level2"),
            label = config$menus$level[[language()]],
            choices = pickable_level2,
            selected = pickable_level2[1]
          )
        }
      }
    })
    
    output$pick_level3 <- shiny::renderUI({
      shiny::req(input$menu_level2)
      if ("level3_name" %in% colnames(atlas_data())) {
        # Filter out data according to what is choosen for level 1
        tmpdata1 <- dplyr::filter(atlas_data(), atlas_data()$level1_name == input$menu_level1)
        # Filter out data according to what is choosen for level 2
        tmpdata2 <- dplyr::filter(tmpdata1, tmpdata1$level2_name == input$menu_level2)
        # Possible values for level 3
        pickable_level3 <- unique(factor(tmpdata2$level3_name))
        # The selector
        shiny::selectInput(
          selectize = FALSE,
          inputId = session$ns("menu_level3"),
          label = config$menus$level[[language()]],
          choices = pickable_level3,
          selected = pickable_level3[1]
        )
      }
    })
    
    filtered_data <- shiny::reactive({
      helseatlas::filter_out(atlas_data(),
                             filter1 = input$menu_level1,
                             filter2 = input$menu_level2,
                             filter3 = input$menu_level3)
      
    })
    return(list(atlas = shiny::reactive(input$atlas), data = filtered_data))
#    return(filtered_data)
#    return(shiny::reactive(input$atlas))
    #    return(list(input$atlas, filtered_data))
  })
}