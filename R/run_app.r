#' Run the Shiny Application
#'
#' @export
#'
run_app <- function() {
  shiny::addResourcePath(
    "www", system.file("app/www", package = "shinymap")
  )

  shiny::shinyApp(
    ui = app_ui, server = app_server
  )
}
