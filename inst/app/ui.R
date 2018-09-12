
shinyUI(function(request){
  navbarPage(title = uiOutput("title"),
             tabPanel(uiOutput("subtitle1"), 
                      sidebarPanel(
                        uiOutput("pickLevel1"),
                        uiOutput("pickLevel2"),
                        uiOutput("pickLevel3")
                      ),
                      mainPanel(
                        uiOutput("makeTable")
                        
                      )
             ),
             navbarMenu(title=uiOutput("subtitle2"),
                        tabPanel(uiOutput("titletab1"), uiOutput("makeMap")),
                        tabPanel(uiOutput("titletab2"), uiOutput("plotHistogram"))
             )
  )
})
