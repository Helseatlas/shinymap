library(shiny)
library(leaflet)

shinyUI(function(request){
  navbarPage(title = "",
#             tabPanel("input", sliderInput(inputId = "bins",
#                                           label = "Number of bins:",
#                                           min = 1,
#                                           max = 50,
#                                           value = 30)
#                      ),
             tabPanel("Velg tema", 
                      sidebarPanel(selectInput(inputId = "kartlag",
                                          label = "Velg et tema:",
                                          choices = c("Personer til fastlege/legevakt", 
                                                      "Personer på poliklinikk", 
                                                      "Akuttinnlagte personer"))
                      ),
                      mainPanel(tableOutput("kolstabell")
                        
                      )
             ),
             
             
             
             navbarMenu(title="Plots",
#                        tabPanel("Waiting", plotOutput(outputId = "waiting")),
#                        tabPanel("Eruptions", plotOutput(outputId = "eruptions")),
                         tabPanel("kart", leafletOutput("mymap")),
                         tabPanel("Kols histogram", plotOutput(outputId = "kolshisto"))
             )

            
  )
})
