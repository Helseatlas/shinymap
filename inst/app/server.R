shinyServer(
  
  function(input, output) {
    
    
    if (!exists("minedata")){
      if (file.exists("data/data.RData")){
        minedata <- get(load("data/data.RData"))
      } else {
        minedata <- NULL
      }
    }
    
    level1 <- c(levels(factor(minedata$level1)))
    
    level2 <- eventReactive(input$level1,{
      tmpdata <- dplyr::filter(minedata, level1 == input$level1)
      level2 <- c(levels(factor(tmpdata$level2)))
      return(level2)
    })
    
    level3 <- eventReactive(c(input$level1, input$level2), {
      if(is.null(input$level2)){return()}
      tmpdata1 <- dplyr::filter(minedata, level1 == input$level1)
      tmpdata2 <- dplyr::filter(tmpdata1, level2 == input$level2)
      level3 <- c(levels(factor(tmpdata2$level3)))
      return(level3)
    })
    
    output$pickLevel1 <- renderUI({
      selectInput(inputId = "level1",
                  label = "Velg et tema:",
                  choices = level1,
                  selected = level1[1])
    })
    
    output$pickLevel2 <- renderUI({
      if ("level2" %in% colnames(minedata)){
        selectInput(inputId = "level2",
                    label = "Velg et tema:",
                    choices = level2(),
                    selected = level2()[1])
        
      }
    })
    
    output$pickLevel3 <- renderUI({
      if ("level3" %in% colnames(minedata)){
        selectInput(inputId = "level3",
                    label = "Velg et tema:",
                    choices = level3(),
                    selected = level3()[1])
      }
    })
    
    
    output$makeTable <- renderUI({
      tableOutput("tabell")
    })
    
    kartlagInput <- reactive({
      
      datasett <- shinymap::filterOut(minedata, input$level1, input$level2, input$level3)
      return(datasett)
    })
    
    pickedData <- reactive({
      new_tab <- data.frame(kartlagInput()$area)
      colnames(new_tab) <- c("Opptaksomr")
      new_tab["Rate"] <- kartlagInput()$rate
      new_tab["Antall"] <- kartlagInput()$numerater
      new_tab["Innbyggere"] <- kartlagInput()$denominator
      return(new_tab)
    })
    
    output$tabell<-renderTable({
      pickedData()
    })
    
    output$title <- renderUI({
      return("Helseatlas kols")
    })
    
    output$titleTable <- renderUI({
      return("Tabell")
    })
    
    output$titleMap <- renderUI({
      return("Kart")
    })
    
    output$titleHist <- renderUI({
      return("Histogram")
    })
    
    output$makeMap <- renderUI({
      leaflet::leafletOutput("mymap")
    })
    
    output$plotHistogram <- renderUI({
      plotOutput(outputId = "histogram")
    })
    
    output$histogram <- renderPlot({
      
      shinymap::plotVariation(inputData = kartlagInput(), xlab = "Opptaksområde", ylab = input$level1)
      
    })
    
    output$mymap <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
        leaflet::addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
      
    })
    
  }
  
)
