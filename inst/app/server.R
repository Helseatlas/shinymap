shiny::shinyServer(
  function(input, output) {
    if (file.exists("data/data.RData")) {
      # load information sent through "launch_application"
      load("data/data.RData")
    }

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

    if (!exists("language") || is.null(language)) {
      # Define language to Norwegian, if not defined
      language <- "no"
    }

    if (language == "no") {
      lang <- 1
    } else if (language == "en") {
      lang <- 2
    } else {
      lang <- 1 # default language value
    }

    output$pick_language <- shiny::renderUI({
        shiny::selectInput(
          inputId = "language",
          label = "",
          choices = c("Norsk" = 1,
                      "English" = 2
          ),
          selected = 1
        )
    })
    
    output$pick_atlas <- shiny::renderUI({
      if (!is.data.frame(healthatlas_data)) {
        shiny::selectInput(
          inputId = "atlas",
          label = c("Velg atlas:", "Pick an atlas")[as.numeric(input$language)],
          choices = c("Dagkirurgi 2011-2013" = "dagkir",
                      "Barn" = "barn",
                      "Nyfødt" = "nyfodt",
                      "Eldre" = "eldre",
                      "Kols" = "kols",
                      "Dagkirurgi 2013-2017" = "dagkir2",
                      "Ortopedi" = "ortopedi",
                      "Gynekologi" = "gyn",
                      "Fødselshjelp" = "fodsel"
          ),
          selected = "dagkir"
        )
      }
    })

    atlas_data <- shiny::reactive({
      # Return the data for a given atlas.
      if (!is.data.frame(healthatlas_data)) {
        if (is.null(input$atlas)){
          return(NULL)
        } else if (input$atlas == "dagkir") {
          return(data::dagkir)
        } else if (input$atlas == "barn") {
          return(data::barn)
        } else if (input$atlas == "nyfodt") {
          return(data::nyfodt)
        } else if (input$atlas == "eldre") {
          return(data::eldre)
        } else if (input$atlas == "kols") {
          return(data::kols)
        } else if (input$atlas == "dagkir2") {
          return(data::dagkir2)
        } else if (input$atlas == "ortopedi") {
          return(data::ortopedi)
        } else if (input$atlas == "gyn") {
          return(data::gyn)
        } else if (input$atlas == "fodsel") {
          return(data::fodsel)
        } else {
          return(NULL)
        }
      } else {
        return(healthatlas_data)
      }
    })

    atlas_map <- shiny::reactive({
      if (!is.data.frame(healthatlas_map)) {
        if (is.null(input$atlas)){
          return(NULL)
        } else if (input$atlas == "dagkir") {
          return(kart::utm33_to_leaflet(kart::dagkir))
        } else if (input$atlas == "barn") {
          return(kart::utm33_to_leaflet(kart::barn))
        } else if (input$atlas == "nyfodt") {
          return(kart::utm33_to_leaflet(kart::nyfodt))
        } else if (input$atlas == "eldre") {
          return(kart::utm33_to_leaflet(kart::eldre))
        } else if (input$atlas == "kols") {
          return(kart::utm33_to_leaflet(kart::kols))
        } else if (input$atlas == "dagkir2") {
          return(kart::utm33_to_leaflet(kart::dagkir2))
        } else if (input$atlas == "gyn") {
          return(kart::utm33_to_leaflet(kart::gyn))
        } else if (input$atlas == "fodsel") {
          return(kart::utm33_to_leaflet(kart::fodsel))
        } else {
          return(NULL)
        }
      } else {
        return(healthatlas_map)
      }
    })

    output$pick_level1 <- shiny::renderUI({
      # Possible values for level 1
      pickable_level1 <- c(levels(factor(atlas_data()$level1_name)))
      # The selector
      shiny::selectInput(
        inputId = "menu_level1",
        label = c("Velg et tema:", "Pick a subject")[as.numeric(input$language)],
        choices = pickable_level1,
        selected = pickable_level1[1]
      )
    })

    output$pick_level2 <- shiny::renderUI({
      if ("level2_name" %in% colnames(atlas_data())) {
        # Filter out data according to what is choosen for level 1
        tmpdata <- dplyr::filter(atlas_data(), atlas_data()$level1_name == input$menu_level1)
        # Possible values for level 2
        pickable_level2 <- c(levels(factor(tmpdata$level2_name)))
        # The selector
        shiny::selectInput(
          inputId = "menu_level2",
          label = c("Velg et tema:", "Pick a subject")[as.numeric(input$language)],
          choices = pickable_level2,
          selected = pickable_level2[1]
        )
      }
    })

    output$pick_level3 <- shiny::renderUI({
      if ("level3_name" %in% colnames(atlas_data())) {
        # Filter out data according to what is choosen for level 1
        tmpdata1 <- dplyr::filter(atlas_data(), atlas_data()$level1_name == input$menu_level1)
        # Filter out data according to what is choosen for level 2
        tmpdata2 <- dplyr::filter(tmpdata1, tmpdata1$level2_name == input$menu_level2)
        # Possible values for level 3
        pickable_level3 <- c(levels(factor(tmpdata2$level3_name)))
        # The selector
        shiny::selectInput(
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
        webpage_title <- c("Helseatlas", "The Norwegian healthcare atlas")[lang]
      }

      return(shiny::HTML(paste0("<h1>", webpage_title, "</h1>")))
    })

    output$title_table <- shiny::renderUI({
      return(c("Tabell", "Table")[as.numeric(input$language)])
    })

    output$title_map <- shiny::renderUI({
      return(c("Kart", "Map")[as.numeric(input$language)])
    })

    output$title_hist <- shiny::renderUI({
      return(c("Histogram", "Histogram")[as.numeric(input$language)])
    })

    output$plot_map <- shiny::renderPlot({
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
      tabular_data[c("Rate", "Rate")[as.numeric(input$language)]] <- filtered_data$value
      tabular_data[c("Antall", "Num")[as.numeric(input$language)]] <- filtered_data$numerator
      tabular_data[c("Innbyggere", "Inhab")[as.numeric(input$language)]] <- filtered_data$denominator
      return(tabular_data)
    })

  }
)
