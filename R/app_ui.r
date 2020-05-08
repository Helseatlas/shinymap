#' Shiny UI
#'
#' @return user interface
#' @export
app_ui <- function() {
  shiny::fluidPage(theme = shinythemes::shinytheme("cerulean"),
                   shinyalert::useShinyalert(),
                   shiny::titlePanel(shiny::tags$head(
                     shiny::tags$link(rel = "icon", type = "image/png", href = "www/hn.png"),
                     shiny::tags$title("Helseatlas"),
                     shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
                  )),
                  shiny::fluidRow(
                    shiny::column(8,
                                  shiny::uiOutput("title")
                    ),
                    shiny::column(1, offset = 3,
                                  shiny::uiOutput("pick_language")
                    )
                  ),
                  shiny::fluidRow(
                    shiny::column(3,
                                  select_ui("atlas"),
                                  shiny::uiOutput("app_info"),
                                  shiny::uiOutput("git_version")
                    ),
                    shiny::column(9,
                                  shiny::tabsetPanel(
                                    shiny::tabPanel(shiny::icon("globe"), leaflet::leafletOutput("plot_map",
                                                                                                         height = 800
                                                                                                         )
                                                    ),
                                    shiny::tabPanel(shiny::icon("chart-bar"), shiny::plotOutput("plot_histogram")),
                                    shiny::tabPanel(shiny::icon("table"), shiny::tableOutput("make_table"))
                           ))
                    ))
}
