#' Run the Shiny Application
#'
#' @export
#'
run_app <- function() {
  shiny::shinyApp(
    ui = app_ui, server = app_server
  )
}
