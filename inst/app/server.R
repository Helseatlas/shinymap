shiny::shinyServer(
  function(input, output) {
    if (file.exists("data/data.RData")) {
      # load information sent through "launch_application"
      load("data/data.RData")
    }

    if (!exists("healthatlas_data")) {
      healthatlas_data <- data::barn
    }

    if (!exists("healthatlas_map")) {
      healthatlas_map <- shinymap::testmap
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

    if (!exists("webpage_title") || is.null(webpage_title)) {
      # Define the atlas title, if not defined
      webpage_title <- c("Helseatlas", "The Norwegian healthcare atlas")[lang]
    }

    pickable_level1 <- c(levels(factor(healthatlas_data$level1_name)))

    pickable_level2 <- shiny::eventReactive(input$menu_level1, {
      tmpdata <- dplyr::filter(healthatlas_data, healthatlas_data$level1_name == input$menu_level1)
      return(c(levels(factor(tmpdata$level2_name))))
    })

    pickable_level3 <- shiny::eventReactive(c(input$menu_level1, input$menu_level2), {
      if (is.null(input$menu_level2)) {
        return()
      }
      tmpdata1 <- dplyr::filter(healthatlas_data, healthatlas_data$level1_name == input$menu_level1)
      tmpdata2 <- dplyr::filter(tmpdata1, tmpdata1$level2_name == input$menu_level2)
      return(c(levels(factor(tmpdata2$level3_name))))
    })

    output$pick_level1 <- shiny::renderUI({
      shiny::selectInput(
        inputId = "menu_level1",
        label = c("Velg et tema:", "Pick a subject")[lang],
        choices = pickable_level1,
        selected = pickable_level1[1]
      )
    })

    output$pick_level2 <- shiny::renderUI({
      if ("level2" %in% colnames(healthatlas_data)) {
        shiny::selectInput(
          inputId = "menu_level2",
          label = c("Velg et tema:", "Pick a subject")[lang],
          choices = pickable_level2(),
          selected = pickable_level2()[1]
        )
      }
    })

    output$pick_level3 <- shiny::renderUI({
      if ("level3" %in% colnames(healthatlas_data)) {
        shiny::selectInput(
          inputId = "menu_level3",
          label = c("Velg et tema:", "Pick a subject")[lang],
          choices = pickable_level3(),
          selected = pickable_level3()[1]
        )
      }
    })

    output$make_overview <- shiny::renderUI({
      if (isTRUE(getOption("shiny.testmode"))) {
        # Show a regular table when testing
        shiny::tableOutput("tabell")
      } else {
        shiny::verticalLayout(
          shiny::splitLayout(
            cellWidths = c("25%", "75%"),
            cellArgs = list(style = "padding: 6px"),
            shiny::renderTable({
              picked_data()
            }),
            leaflet::renderLeaflet({
              shinymap::make_map(type = "leaflet", map = healthatlas_map)
            })
          ),
          shiny::splitLayout(
            shiny::renderPlot({
              shinymap::plot_variation(
                input_data = kartlag_input(),
                xlab = c("Opptaksomr\u00E5de", "Area")[lang],
                ylab = input$menu_level1
              )
            })
          )
        )
      }
    })

    output$make_table <- shiny::renderUI({
      shiny::tableOutput("tabell")
    })

    kartlag_input <- reactive({
      datasett <- shinymap::filter_out(healthatlas_data,
        filter1 = input$menu_level1,
        filter2 = input$menu_level2,
        filter3 = input$menu_level3
      )
      return(datasett)
    })

    picked_data <- shiny::reactive({
      new_tab <- data.frame(kartlag_input()$area_name)
      colnames(new_tab) <- c(c("Opptaksomr", "Area")[lang])
      new_tab[c("Rate", "Rate")[lang]] <- kartlag_input()$value
      new_tab[c("Antall", "Num")[lang]] <- kartlag_input()$numerator
      new_tab[c("Innbyggere", "Inhab")[lang]] <- kartlag_input()$denominator
      return(new_tab)
    })

    output$tabell <- shiny::renderTable({
      picked_data()
    })

    output$title <- shiny::renderUI({
      return(shiny::HTML(paste0("<h1>", webpage_title, "</h1>")))
    })

    output$title_overview <- shiny::renderUI({
      return(c("Oversikt", "Overview")[lang])
    })

    output$title_table <- shiny::renderUI({
      return(c("Tabell", "Table")[lang])
    })

    output$title_map <- shiny::renderUI({
      return(c("Kart", "Map")[lang])
    })

    output$title_hist <- shiny::renderUI({
      return(c("Histogram", "Histogram")[lang])
    })

    output$pick_map <- shiny::renderUI({
      shiny::selectInput(
        inputId = "maptype",
        label = c("Velg karttype", "Choose map type")[lang],
        choices = c("leaflet", "simple"),
        selected = "leaflet"
      )
    })
    output$plot_map <- shiny::renderUI({
      # Make a leaflet map
      type <- input$maptype
      if (is.null(type)) {
        return(NULL)
      }
      switch(type,
        leaflet = {
          leaflet::leafletOutput("leafletmap")
        },
        simple = {
          shiny::plotOutput(outputId = "simplemap")
        }
      )
    })

    output$plot_histogram <- shiny::renderUI({
      # Make a histogram plot
      shiny::plotOutput(outputId = "histogram")
    })

    output$simplemap <- shiny::renderPlot({
      shinymap::make_map(type = "simple", map = healthatlas_map)
    })

    output$histogram <- shiny::renderPlot({
      shinymap::plot_variation(
        input_data = kartlag_input(),
        xlab = c("Opptaksomr\u00E5de", "Area")[lang],
        ylab = input$menu_level1
      )
    })

    output$leafletmap <- leaflet::renderLeaflet({
      shinymap::make_map(type = "leaflet", map = healthatlas_map)
    })
  }
)
