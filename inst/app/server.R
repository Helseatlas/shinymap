shinyServer(
  
  function(input, output) {
    
    if (file.exists("data/data.RData")){
      minedata <- get(load("data/data.RData"))
    }
    
    if (!exists("minedata")){
      minedata <- NULL
    }

    head(minedata$level1)

    level1 <- c(levels(factor(minedata$level1)))
    
    level2 <- reactive({
      tmpdata <- dplyr::filter(minedata, level1 == input$level1)
      level2 <- c(levels(factor(tmpdata$level2)))
      return(level2)
    })
    
    # level3 <- reactive({
    #   tmpdata1 <- dplyr::filter(minedata, level1 == input$level1)
    #   tmpdata2 <- dplyr::filter(tmpdata1, level2 == input$level2)
    #   level3 <- c(levels(factor(tmpdata2$level3)))
    #   return(level3)
    # })
    
    output$pickTheme <- renderUI({
      selectInput(inputId = "level1",
                  label = "Velg et tema:",
                  choices = level1,
                  selected = level1[1])
    })
    
    output$pickTheme2 <- renderUI({
#      if (exist("level2")){
        selectInput(inputId = "level2",
                    label = "Velg et tema:",
                    choices = level2(),
                    selected = level2()[1])
        
#      }
    })

#     output$pickTheme3 <- renderUI({
# #      if (exist("level3")){
#         selectInput(inputId = "level3",
#                   label = "Velg et tema:",
#                   choices = level3(),
#                   selected = level3()[1])
# #      }
#     })
    
    
    output$makeTable <- renderUI({
      tableOutput("kolstabell")
    })
    
    
    
    uniqueID <- reactive({
      # Extract the id from the choices of levels made by the user
      FALSE
    })
    
    kartlagInput <- reactive({
      # Filter out data according to choices made by user
      datasett <- dplyr::filter(minedata, level1 == input$level1) %>% dplyr::filter(level2 == input$level2) #%>% dplyr::filter(level3 == "Alle kontakter")
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
        labs(x = "Opptaksområde", y = input$level1) + 
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
