# Settings before server session starts

if (file.exists("data/data.RData")) {
  # load information sent through "launch_application"
  load("data/data.RData")
}

shiny::shinyServer(
  function(input, output) {

    if (!exists("healthatlas_data")) {
      healthatlas_data <- NULL
    }

    if (!exists("healthatlas_map")) {
      healthatlas_map <- NULL
    }

    if (isTRUE(getOption("shiny.testmode"))) {
      # Load static/dummy data if this is a test run
      healthatlas_data <- shinymap::testdata
      healthatlas_map <- shinymap::testmap
    }

    output$pick_language <- shiny::renderUI({
        shinyWidgets::radioGroupButtons(
          size = "sm",
          direction = "vertical",
          status = "",
          inputId = "language",
          label = "",
          choices = c("NO" = 1,
                      "EN" = 2
          ),
          selected = 1
        )
    })

    output$pick_atlas <- shiny::renderUI({
      if (!is.data.frame(healthatlas_data)) {
        if (is.null(input$language)) {
          return(NULL)
        }
        mytitle <- c()
        atlasnames <- names(healthatlas_data)
        for (i in atlasnames) {
          if (input$language == "1") {
            mytitle <- c(mytitle, paste(healthatlas_data[[i]]$title_no, healthatlas_data[[i]]$year, sep = " "))
          } else if (input$language == "2") {
            mytitle <- c(mytitle, paste(healthatlas_data[[i]]$title_en, healthatlas_data[[i]]$year, sep = " "))
          }
        }
        names(atlasnames) <- mytitle

        shiny::selectInput(
          selectize = FALSE,
          inputId = "atlas",
          label = c("Velg atlas:", "Pick an atlas")[as.numeric(input$language)],
          choices = atlasnames,
          selected = atlasnames[1]
        )
      }
    })

    atlas_data <- shiny::reactive({
      # Return the data for a given atlas.
      if (!is.data.frame(healthatlas_data)) {
        if (is.null(input$atlas)) {
          return(NULL)
        } else {
          return(healthatlas_data[[input$atlas]][[as.numeric(input$language)]])
        }
      } else {
        return(healthatlas_data)
      }
    })

    atlas_map <- shiny::reactive({
      if (!is.data.frame(healthatlas_map)) {
        if (is.null(input$atlas)) {
          return(NULL)
        } else {
          return(kart::utm33_to_leaflet(healthatlas_data[[input$atlas]][["map"]]))
        }
      } else {
        return(kart::utm33_to_leaflet(healthatlas_map))
      }
    })

    output$pick_level1 <- shiny::renderUI({
      # Possible values for level 1
      pickable_level1 <- unique(factor(atlas_data()$level1_name))
      # The selector
      shiny::selectInput(
        selectize = FALSE,
        inputId = "menu_level1",
        label = c("Velg et tema:", "Pick a subject")[as.numeric(input$language)],
        choices = pickable_level1,
        selected = pickable_level1[1]
      )
    })

    output$pick_level2 <- shiny::renderUI({
      if ("level2_name" %in% colnames(atlas_data())) {
        if (is.null(input$menu_level1)) {
          return(NULL)
        }
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
          tags$div(class = "year-slider", shiny::sliderInput(
            inputId = "menu_level2",
            label = c("År:", "Year")[as.numeric(input$language)],
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
            inputId = "menu_level2",
            label = c("Velg et tema:", "Pick a subject")[as.numeric(input$language)],
            choices = pickable_level2,
            selected = pickable_level2[1]
          )
        }
      }
    })

    output$pick_level3 <- shiny::renderUI({
      if ("level3_name" %in% colnames(atlas_data())) {
        if (is.null(input$menu_level2)) {
          return(NULL)
        }
        # Filter out data according to what is choosen for level 1
        tmpdata1 <- dplyr::filter(atlas_data(), atlas_data()$level1_name == input$menu_level1)
        # Filter out data according to what is choosen for level 2
        tmpdata2 <- dplyr::filter(tmpdata1, tmpdata1$level2_name == input$menu_level2)
        # Possible values for level 3
        pickable_level3 <- unique(factor(tmpdata2$level3_name))
        # The selector
        shiny::selectInput(
          selectize = FALSE,
          inputId = "menu_level3",
          label = c("Velg et tema:", "Pick a subject")[as.numeric(input$language)],
          choices = pickable_level3,
          selected = pickable_level3[1]
        )
      }
    })

    output$title <- shiny::renderUI({
      if (!exists("webpage_title") || is.null(webpage_title)) {
        # Define the atlas title, if not defined
        webpage_title <- c("Helseatlas", "The Norwegian healthcare atlas")[as.numeric(input$language)]
      }

      return(shiny::HTML(paste0("<h1>", webpage_title, "</h1>")))
    })

    output$title_table <- shiny::renderUI({
      return(icon("table"))
    })

    output$title_map <- shiny::renderUI({
      return(icon("globe"))
    })

    output$title_hist <- shiny::renderUI({
      return(icon("chart-bar"))
    })

    output$plot_map <- shiny::renderPlot({
      if (is.null(input$menu_level1) | isTRUE(getOption("shiny.testmode"))) {
        return(NULL)
      }
      filtered_data <- shinymap::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)

      if (is.null(nrow(filtered_data)) || nrow(filtered_data) == 0) {
        # Return null if data in invalid
        return(NULL)
      }

      map <- shinymap::make_map(map = atlas_map(), data = filtered_data)
      return(map)
    }
    , height = 800, width = 600)

    output$plot_histogram <- shiny::renderPlot({
      filtered_data <- shinymap::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)

      plot <- shinymap::plot_variation(
        input_data = filtered_data,
        xlab = c("Opptaksomr\u00E5de", "Area")[as.numeric(input$language)],
        ylab = input$menu_level1
      )
      return(plot)
    }
    , height = 800, width = 600)

    output$make_table <- shiny::renderTable({
      if (is.null(input$menu_level1)) {
        return(NULL)
      }
      filtered_data <- shinymap::filter_out(atlas_data(),
                                            filter1 = input$menu_level1,
                                            filter2 = input$menu_level2,
                                            filter3 = input$menu_level3)

      # Return null if data in invalid
      if (is.null(nrow(filtered_data)) || nrow(filtered_data) == 0) {
        return(NULL)
      }

      tabular_data <- data.frame(filtered_data$area_name)
      colnames(tabular_data) <- c(c("Opptaksomr", "Area")[as.numeric(input$language)])
      value_name <- as.character(unique(filtered_data$type))
      tabular_data[value_name] <- filtered_data$value
      numerator_name <- as.character(unique(filtered_data$numerator_name))
      tabular_data[numerator_name] <- filtered_data$numerator
      denominator_name <- as.character(unique(filtered_data$denominator_name))
      tabular_data[denominator_name] <- filtered_data$denominator
      # Sort data
      tabular_data <- tabular_data[order(tabular_data[,2], na.last = TRUE, decreasing = TRUE),]
      # Format numbers
      tabular_data[, -1] <- sapply(tabular_data[, -1],
                                   FUN = function(x) format(x,
                                                            digits = 2,
                                                            decimal.mark = c(",", ".")[as.numeric(input$language)],
                                                            big.mark = c(" ", ",")[as.numeric(input$language)]
                                                            )
                                   )
      return(tabular_data)
    }
    , align = "lrrr")

  }
)
