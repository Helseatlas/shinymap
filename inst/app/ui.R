library(shiny)
library(leaflet)

shinyUI(function(request){
  navbarPage(title = "",
             tabPanel("Velg tema", 
                      sidebarPanel(selectInput(inputId = "kartlag",
                                          label = "Velg et tema:",
                                          choices = c("Personer til fastlege/legevakt", 
                                                      "Personer poliklinikk", 
                                                      "Akuttinnlagte personer"))
                      ),
                      mainPanel(tableOutput("kolstabell")
                        
                      )
             ),
             
             
             
             navbarMenu(title="Plots",
                         tabPanel("kart", leafletOutput("mymap")),
                         tabPanel("Kols histogram", plotOutput(outputId = "kolshisto"))
             )

            
  )
})
