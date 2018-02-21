library(shiny)
library(ggplot2)
library(leaflet)

shinyServer(

  function(input, output) {
    
    
    kartlagInput <- reactive({
      switch(input$kartlag,
             "Personer til fastlege/legevakt" = kols$FLLV_pers_Rate,
             "Personer poliklinikk" = kols$Poli_Pers_Rate,
             "Akuttinnlagte personer" = kols$Akutt_pers_Rate)
    })
    
    output$kolstabell<-renderTable(
      kartlagInput()
      )

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
