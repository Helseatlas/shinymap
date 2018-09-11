
shinyUI(function(request){
  navbarPage(title = uiOutput("title"),
             tabPanel(uiOutput("subtitle1"), 
                      sidebarPanel(
                        uiOutput("pickTheme")
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
