library(shiny)

shinyUI(function(request) {
  ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                  titlePanel(tags$head(
                    tags$link(rel = "icon", type = "image/png", href = "hn.png"),
                    tags$title("Helseatlas"),
                    tags$style(type = "text/css", "a{color: #808080;}"), # Link and inactive tab color
                    tags$style(type = "text/css", "h1{color: #003A8C;}") # , # Title color
                  )),
                  fluidRow(
                    column(8,
                           uiOutput("title")
                    ),
                    column(2, offset = 2,
                           uiOutput("pick_language")
                    )
                  ),
                  fluidRow(
                    column(4,
                           uiOutput("pick_atlas"),
                           uiOutput("pick_level1"),
                           uiOutput("pick_level2"),
                           uiOutput("pick_level3"),
                           tableOutput("make_table")
                    ),
                    column(8,
                           tabsetPanel(
                             tabPanel(uiOutput("title_map"), plotOutput("plot_map")),
                             tabPanel(uiOutput("title_hist"), plotOutput("plot_histogram"))
                           ))
                    ))
})
