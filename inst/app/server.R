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
             "Personer til fastlege/legevakt" = kols$FLLV_pers_Rate,
             "Personer poliklinikk" = kols$Poli_Pers_Rate,
             "Akuttinnlagte personer" = kols$Akutt_pers_Rate)
    })
    
    output$kolstabell<-renderTable(
      kartlagInput()
      )
    
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
      
      kolsdata <- data.frame(bohf=kols$Opptaksomr, kartlag=kartlagInput())
      
      # barplot
        ggplot(data=kolsdata, aes(x=reorder(bohf, kartlag), y=kartlag)) +
        geom_bar(stat="identity", fill="#95BDE6") + 
        labs(x = "Opptaksområde", y = input$kartlag) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        theme(panel.background = element_blank())
      
    })
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
      
    })
    
  }
  
)
