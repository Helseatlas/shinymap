shinyServer(
  function(input, output) {
    if (file.exists("data/data.RData")) {
      # load information sent through "launch_application"
      load("data/data.RData")
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

    pickable_level1 <- c(levels(factor(healthatlas_data$level1)))

    pickable_level2 <- eventReactive(input$menu_level1, {
      tmpdata <- dplyr::filter(healthatlas_data, level1 == input$menu_level1)
      level2 <- c(levels(factor(tmpdata$level2)))
      return(level2)
    })

    pickable_level3 <- eventReactive(c(input$menu_level1, input$menu_level2), {
      if (is.null(input$menu_level2)) {
        return()
      }
      tmpdata1 <- dplyr::filter(healthatlas_data, level1 == input$menu_level1)
      tmpdata2 <- dplyr::filter(tmpdata1, level2 == input$menu_level2)
      level3 <- c(levels(factor(tmpdata2$level3)))
      return(level3)
    })

    output$pick_level1 <- renderUI({
      selectInput(
        inputId = "menu_level1",
        label = c("Velg et tema:", "Pick a subject")[lang],
        choices = pickable_level1,
        selected = pickable_level1[1]
      )
    })

    output$pick_level2 <- renderUI({
      if ("level2" %in% colnames(healthatlas_data)) {
        selectInput(
          inputId = "menu_level2",
          label = c("Velg et tema:", "Pick a subject")[lang],
          choices = pickable_level2(),
          selected = pickable_level2()[1]
        )
      }
    })

    output$pick_level3 <- renderUI({
      if ("level3" %in% colnames(healthatlas_data)) {
        selectInput(
          inputId = "menu_level3",
          label = c("Velg et tema:", "Pick a subject")[lang],
          choices = pickable_level3(),
          selected = pickable_level3()[1]
        )
      }
    })

    output$make_overview <- renderUI({
      # will not use the following when running tests due to some random numbers generated
      if (isFALSE(getOption("shiny.testmode"))) {
        verticalLayout(
          splitLayout(
            cellWidths = c("25%", "75%"),
            cellArgs = list(style = "padding: 6px"),
            renderTable({
              picked_data()
            }),
            leaflet::renderLeaflet({
              shinymap::make_map(type = "leaflet", map = healthatlas_map)
            })
          ),
          splitLayout(
            renderPlot({
              shinymap::plot_variation(inputData = kartlag_input(),
                                      xlab = c("Opptaksomr\u00E5de", "Area")[lang],
                                      ylab = input$menu_level1)
            })
          )
        )
      } else {
        # Show a regular table when testing
        tableOutput("tabell")
      }
    })

    output$make_table <- renderUI({
      tableOutput("tabell")
    })

    kartlag_input <- reactive({
      datasett <- shinymap::filterOut(healthatlas_data,
        filter1 = input$menu_level1,
        filter2 = input$menu_level2,
        filter3 = input$menu_level3
      )
      return(datasett)
    })

    picked_data <- reactive({
      new_tab <- data.frame(kartlag_input()$area)
      colnames(new_tab) <- c(c("Opptaksomr", "Area")[lang])
      new_tab[c("Rate", "Rate")[lang]] <- kartlag_input()$rate
      new_tab[c("Antall", "Num")[lang]] <- kartlag_input()$numerater
      new_tab[c("Innbyggere", "Inhab")[lang]] <- kartlag_input()$denominator
      return(new_tab)
    })

    output$tabell <- renderTable({
      picked_data()
    })

    output$title <- renderUI({
      return(HTML(paste0("<h1>", webpage_title, "</h1>")))
    })

    output$title_overview <- renderUI({
      return(c("Oversikt", "Overview")[lang])
    })

    output$title_table <- renderUI({
      return(c("Tabell", "Table")[lang])
    })

    output$title_map <- renderUI({
      return(c("Kart", "Map")[lang])
    })

    output$title_hist <- renderUI({
      return(c("Histogram", "Histogram")[lang])
    })

    output$pick_map <- renderUI({
      selectInput(
        inputId = "maptype",
        label = c("Velg karttype", "Choose map type")[lang],
        choices = c("leaflet", "simple"),
        selected = "leaflet"
      )
    })
    output$plot_map <- renderUI({
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

    output$plot_histogram <- renderUI({
      # Make a histogram plot
      shiny::plotOutput(outputId = "histogram")
    })

    output$simplemap <- renderPlot({
      shinymap::make_map(type = "simple", map = healthatlas_map)
    })

    output$histogram <- renderPlot({
      shinymap::plot_variation(
        inputData = kartlag_input(),
        xlab = c("Opptaksomr\u00E5de", "Area")[lang],
        ylab = input$menu_level1
      )
    })

    output$leafletmap <- leaflet::renderLeaflet({
      shinymap::make_map(type = "leaflet", map = healthatlas_map)
    })
  }
)
