library(shiny)

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
      uiOutput("pick_level1"),
      uiOutput("pick_level2"),
      uiOutput("pick_level3")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(uiOutput("title_overview"), uiOutput("make_overview")),
        tabPanel(uiOutput("title_table"), uiOutput("make_table")),
        tabPanel(uiOutput("title_map"), uiOutput("pick_map"), uiOutput("plot_map")),
        tabPanel(uiOutput("title_hist"), uiOutput("plot_histogram"))
      )
    )
  )
})
