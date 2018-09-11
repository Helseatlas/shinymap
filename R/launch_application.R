#' Launch the application locally
#' @param datasett The data set to be loaded into the application
#'
#' @export
launch_application <- function(datasett = NULL){
  if (is.null(datasett)){
    datasett <- kols
  }
  shinydir <- create_appDir(data = datasett)
  shiny::runApp(appDir = shinydir)
}


#' Submit the application to shinyapp.io
#' @param datasett The data set file (.RData) to be loaded into the application.
#' The absolute path has to be given
#' @param name The appName of the deployed shiny application (default = "experimental")
#' @param HNproxy If TRUE: deploy app through proxy
#' @param shiny_account Which shiny account on shinyapps.io (default = "skde")
#'
#' @export
submit_application <- function(datasett = NULL, name = "experimental", HNproxy = FALSE, shiny_account = "skde"){
  if (HNproxy){
    options(RCurlOptions = list(proxy = "http://www-proxy.helsenord.no:8080"))
    options(shinyapps.http = "rcurl")
  }
  shinydir <- create_appDir(data = datasett)
  rsconnect::deployApp(appDir = shinydir, appName = name, account = shiny_account)
}

#' Create an appDir for shiny::runApp and rsconnect::deployApp
#'
#' Create a directory in tempdir() where the installed version of 
#' shinymap package is copied and the data is saved.
#' This directory, with its content, will be deployd to or ran by shiny.
#' 
#' @param data The data to be saved in the directory, to be used by the app
#'
#' @return The created directory
#'
create_appDir <- function(data = NULL){
  # Name the directory
  tmpshinydir <- paste0(tempdir(),"/shiny")
  # Delete old content in directory
  unlink(tmpshinydir, recursive = TRUE, force = TRUE)
  # Create main directory
  dir.create(tmpshinydir)
  # Copy the installed version of the shinymap package to the directory
  file.copy(system.file("app", package = "shinymap"), tmpshinydir, recursive = TRUE)
  # Create data folder
  dir.create(paste0(tmpshinydir, "/app/data"))
  # Save the data to a .RData file
  save(data, file = paste0(tmpshinydir,"/app/data/data.RData"))
  # Return the name of the main directory
  return(paste0(tmpshinydir, "/app"))
}

