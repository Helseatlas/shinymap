shinyServer(

  function(input, output) {

    if (file.exists("data/data.RData")){
      # load information sent through "launch_application" or "submit_application"
      load("data/data.RData")
    }

    if (!exists("healthatlas_data")){
      healthatlas_data <- NULL
    }

    if (is.null(language) || !exists("language")){
      # Define language to Norwegian, if not defined
      language <- "no"
    }

    if (is.null(title) || !exists("title")){
      # Define the atlas title, if not defined
      title <- "Helseatlas"
    }

    if (language == "no"){
      lang = 1
    } else if (language == "en"){
      lang = 2
    } else { # default language value
      lang = 1
    }

    level1 <- c(levels(factor(healthatlas_data$level1)))

    level2 <- eventReactive(input$level1,{
      tmpdata <- dplyr::filter(healthatlas_data, level1 == input$level1)
      level2 <- c(levels(factor(tmpdata$level2)))
      return(level2)
    })

    level3 <- eventReactive(c(input$level1, input$level2), {
      if(is.null(input$level2)){return()}
      tmpdata1 <- dplyr::filter(healthatlas_data, level1 == input$level1)
      tmpdata2 <- dplyr::filter(tmpdata1, level2 == input$level2)
      level3 <- c(levels(factor(tmpdata2$level3)))
      return(level3)
    })

    output$pickLevel1 <- renderUI({
      selectInput(inputId = "level1",
                  label = c("Velg et tema:", "Pick a subject")[lang],
                  choices = level1,
                  selected = level1[1])
    })

    output$pickLevel2 <- renderUI({
      if ("level2" %in% colnames(healthatlas_data)){
        selectInput(inputId = "level2",
                    label = c("Velg et tema:", "Pick a subject")[lang],
                    choices = level2(),
                    selected = level2()[1])

      }
    })

    output$pickLevel3 <- renderUI({
      if ("level3" %in% colnames(healthatlas_data)){
        selectInput(inputId = "level3",
                    label = c("Velg et tema:", "Pick a subject")[lang],
                    choices = level3(),
                    selected = level3()[1])
      }
    })


    output$makeTable <- renderUI({
      tableOutput("tabell")
    })

    kartlagInput <- reactive({

      datasett <- shinymap::filterOut(healthatlas_data, input$level1, input$level2, input$level3)
      return(datasett)
    })

    pickedData <- reactive({
      new_tab <- data.frame(kartlagInput()$area)
      colnames(new_tab) <- c(c("Opptaksomr", "Area")[lang])
      new_tab[c("Rate", "Rate")[lang]] <- kartlagInput()$rate
      new_tab[c("Antall", "Num")[lang]] <- kartlagInput()$numerater
      new_tab[c("Innbyggere", "Inhab")[lang]] <- kartlagInput()$denominator
      return(new_tab)
    })

    output$tabell<-renderTable({
      pickedData()
    })

    output$title <- renderUI({
      return(HTML(paste0("<h1>", title, "</h1>")))
    })

    output$titleTable <- renderUI({
      return(c("Tabell","Table")[lang])
    })

    output$titleMap <- renderUI({
      return(c("Kart", "Map")[lang])
    })

    output$titleHist <- renderUI({
      return(c("Histogram", "Histogram")[lang])
    })

    output$makeMap <- renderUI({
      # Make a leaflet map
      leaflet::leafletOutput("leafletmap")
    })

    output$plotHistogram <- renderUI({
      # Make a histogram plot
      shiny::plotOutput(outputId = "histogram")
    })

    output$histogram <- renderPlot({

      shinymap::plotVariation(inputData = kartlagInput(), xlab = c("Opptaksomr\u00E5de", "Area")[lang], ylab = input$level1)

    })

    output$leafletmap <- leaflet::renderLeaflet({

      shinymap::makeLeafletmap(inputData = kartlagInput())

    })

  }

)
