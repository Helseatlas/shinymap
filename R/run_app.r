#' Launch the application, either locally or to shinyapps.io
#'
#' @param dataset The data set to be loaded into the application
#' @param map The map to be used in the application
#' @param title The title of the atlas
#' @param publish_app If TRUE: deploy app to shinyapps.io (default = FALSE)
#' @param name The appName of the deployed shiny application (default = "experimental")
#' @param shiny_account Which shiny account on shinyapps.io (default = "skde")
#' @param git_hash Current git sha1 hash
#' @param github_repo Current github repository
#'
#' @export
launch_app <- function(dataset = NULL,
                       map = NULL,
                       title = NULL,
                       publish_app = FALSE,
                       name = "experimental",
                       shiny_account = "skde",
                       git_hash = NULL,
                       github_repo = NULL) {

  # Create a directory with all necessary data.
  shinydir <- create_appdir(healthatlas_data = dataset,
                            healthatlas_map = map,
                            webpage_title = title,
                            git_hash = git_hash,
                            github_repo = github_repo)

  # Run the app
  if (publish_app) {
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
#' @param webpage_title The title of the atlas
#' @param git_hash Current git sha1 hash
#' @param github_repo Current github repository
#'
#' @return The created directory
#'
create_appdir <- function(healthatlas_data = NULL,
                          healthatlas_map = NULL,
                          webpage_title = NULL,
                          git_hash = NULL,
                          github_repo = NULL) {
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
       webpage_title,
       git_hash,
       github_repo,
       file = paste0(tmpshinydir, "/", "app/data/data.RData"))
  # Return the name of the main directory
  return(paste0(tmpshinydir, "/", "app"))
}
