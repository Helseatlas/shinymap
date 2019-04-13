
shinyUI(function(request) {
  fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    titlePanel(tags$head(
      tags$link(rel = "icon", type = "image/png", href = "hn.png"),
      tags$title("Helseatlas"),
      tags$style(type = "text/css", "a{color: #808080;}"), # Link and inactive tab color
      tags$style(type = "text/css", "h1{color: #003A8C;}") # , # Title color
    )), uiOutput("title"),

    sidebarPanel(
      uiOutput("pickLevel1"),
      uiOutput("pickLevel2"),
      uiOutput("pickLevel3")
    ),
    mainPanel(
        tabsetPanel(#type = "tabs", id="tab",
                 tabPanel(uiOutput("titleOverview"), uiOutput("makeOverview")),
                 tabPanel(uiOutput("titleTable"), uiOutput("makeTable")),
                 tabPanel(uiOutput("titleMap"), uiOutput("pickMap"), uiOutput("plotMap")),
                 tabPanel(uiOutput("titleHist"), uiOutput("plotHistogram"))
    ))
  )
})
