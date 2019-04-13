#' Launch the application, either locally or to shinyapps.io
#'
#' @param dataset The data set to be loaded into the application
#' @param map The map to be used in the application
#' @param language The language of the atlas ("en" or "no")
#' @param title The title of the atlas
#' @param publish_app If TRUE: deploy app to shinyapps.io (default = FALSE)
#' @param name The appName of the deployed shiny application (default = "experimental")
#' @param shiny_account Which shiny account on shinyapps.io (default = "skde")
#' @param HNproxy If TRUE: deploy app through Helse Nord proxy (default = FALSE)
#'
#' @export
launch_app <- function(dataset = NULL,
                       map = NULL,
                       language = NULL,
                       title = NULL,
                       publish_app = FALSE,
                       name = "experimental",
                       shiny_account = "skde",
                       HNproxy = FALSE) {
  if (is.null(dataset)) {
    # If no dataset is defined, use the dataset shipped with the shinymap package
    # (for testing only)
    dataset <- shinymap::kols
  }

  # Create a directory with all necessary data.
  shinydir <- create_appdir(healthatlas_data = dataset,
                            healthatlas_map = map,
                            language = language,
                            webpage_title = title)

  # Run the app
  if (publish_app) {
    if (HNproxy) {
      options(RCurlOptions = list(proxy = "http://www-proxy.helsenord.no:8080"))
      options(shinyapps.http = "rcurl")
    }
    rsconnect::deployApp(appDir = shinydir, appName = name, account = shiny_account)
  } else {
    shiny::runApp(appDir = shinydir)
  }
}

#' Create an appDir for shiny::runApp and rsconnect::deployApp
#'
#' Create a directory in tempdir() where the installed version of
#' shinymap package is copied and the data is saved.
#' This directory, with its content, will be deployd to or ran by shiny.
#'
#' @param healthatlas_data The data to be saved in the directory, to be used by the app
#' @param healthatlas_map The map
#' @param language The language of the atlas ("en" or "no")
#' @param webpage_title The title of the atlas
#'
#' @return The created directory
#'
create_appdir <- function(healthatlas_data = NULL, healthatlas_map = NULL, language = NULL, webpage_title = NULL) {
  # Name the directory
  tmpshinydir <- paste0(tempdir(), "/", "shiny")
  # Delete old content in directory
  unlink(tmpshinydir, recursive = TRUE, force = TRUE)
  # Create main directory
  dir.create(tmpshinydir)
  # Copy the installed version of the shinymap package to the directory
  file.copy(system.file("app", package = "shinymap"), tmpshinydir, recursive = TRUE)
  # Create data folder
  dir.create(paste0(tmpshinydir, "/", "app/data"))
  # Save the data to a .RData file
  save(healthatlas_data,
       healthatlas_map,
       language,
       webpage_title,
       file = paste0(tmpshinydir, "/", "app/data/data.RData"))
  # Return the name of the main directory
  return(paste0(tmpshinydir, "/", "app"))
}
