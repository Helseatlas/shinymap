shinyServer(
  
  function(input, output) {
    
    output$pickTheme <- renderUI({
      selectInput(inputId = "kartlag",
                  label = "Velg et tema:",
                  choices = c("Personer til fastlege/legevakt", 
                              "Personer poliklinikk", 
                              "Akuttinnlagte personer"))
    })
    
    output$makeTable <- renderUI({
      tableOutput("kolstabell")
    })
    
    kartlagInput <- reactive({
      switch(input$kartlag,
             "Personer til fastlege/legevakt" = dplyr::filter(kols, id == "i0"),
             "Personer poliklinikk" = dplyr::filter(kols, id == "i2"),
             "Akuttinnlagte personer" = dplyr::filter(kols, id == "i4"))
    })
    
    pickedData <- reactive({
      new_tab <- data.frame(kartlagInput()$area)
      colnames(new_tab) <- c("Opptaksomr")
      new_tab["Rate"] <- kartlagInput()$rate
      new_tab["Antall"] <- kartlagInput()$numerater
      new_tab["Innbyggere"] <- kartlagInput()$denominator
      return(new_tab)
    })
    
    output$kolstabell<-renderTable({
      pickedData()
    })
    
    output$title <- renderUI({
      return("Helseatlas kols")
    })
    
    output$subtitle1 <- renderUI({
      return("Velg tema")
    })
    
    output$subtitle2 <- renderUI({
      return("Plott")
    })
    
    output$titletab1 <- renderUI({
      return("Kart")
    })
    
    output$titletab2 <- renderUI({
      return("Histogram")
    })
    
    output$makeMap <- renderUI({
      leafletOutput("mymap")
    })
    
    output$plotHistogram <- renderUI({
      plotOutput(outputId = "kolshisto")
    })
    
    output$kolshisto <- renderPlot({
      
      kolsdata <- data.frame(bohf=kartlagInput()$area, rate=kartlagInput()$rate)
      
      # barplot
      ggplot(data=kartlagInput(), aes(x=reorder(area, rate), y=rate)) +
        geom_bar(stat="identity", fill="#95BDE6") + 
        labs(x = "Opptaksområde", y = input$kartlag) + 
#        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggplot2::coord_flip() +
        ggthemes::theme_tufte()
#        theme(panel.background = element_blank())
      
    })
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
      
    })
    
  }
  
)
