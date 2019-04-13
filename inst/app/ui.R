
shiny::shinyUI(function(request) {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    shiny::titlePanel(tags$head(
      tags$link(rel = "icon", type = "image/png", href = "hn.png"),
      tags$title("Helseatlas"),
      tags$style(type = "text/css", "a{color: #808080;}"), # Link and inactive tab color
      tags$style(type = "text/css", "h1{color: #003A8C;}") # , # Title color
    )), shiny::uiOutput("title"),

    shiny::sidebarPanel(
      shiny::uiOutput("pick_level1"),
      shiny::uiOutput("pick_level2"),
      shiny::uiOutput("pick_level3")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(shiny::uiOutput("title_overview"), shiny::uiOutput("make_overview")),
        shiny::tabPanel(shiny::uiOutput("title_table"), shiny::uiOutput("make_table")),
        shiny::tabPanel(shiny::uiOutput("title_map"), shiny::uiOutput("pick_map"), shiny::uiOutput("plot_map")),
        shiny::tabPanel(shiny::uiOutput("title_hist"), shiny::uiOutput("plot_histogram"))
      )
    )
  )
})
