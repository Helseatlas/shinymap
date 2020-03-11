#' Shiny UI
#'
#' @return user interface
#' @export
app_ui <- function() {
    fluidPage(theme = shinythemes::shinytheme("cerulean"),
                  titlePanel(tags$head(
                    tags$link(rel = "icon", type = "image/png", href = "hn.png"),
                    tags$title("Helseatlas"),
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                  )),
                  fluidRow(
                    column(8,
                           uiOutput("title")
                    ),
                    column(1, offset = 3,
                           uiOutput("pick_language")
                    )
                  ),
                  fluidRow(
                    column(3,
                           uiOutput("pick_atlas"),
                           uiOutput("pick_level1"),
                           uiOutput("pick_level2"),
                           uiOutput("pick_level3"),
                           uiOutput("git_version")
                    ),
                    column(9,
                           tabsetPanel(
                             tabPanel(uiOutput("title_map"), plotOutput("plot_map")),
                             tabPanel(uiOutput("title_hist"), plotOutput("plot_histogram")),
                             tabPanel(uiOutput("title_table"), tableOutput("make_table"))
                           ))
                    ))
}
