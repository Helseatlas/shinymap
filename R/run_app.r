#' Launch the application, either locally or to shinyapps.io
#'
#' @param dataset The data set to be loaded into the application
#' @param title The title of the atlas
#' @param publish_app If TRUE: deploy app to shinyapps.io (default = FALSE)
#' @param name The appName of the deployed shiny application (default = "experimental")
#' @param shiny_account Which shiny account on shinyapps.io (default = "skde")
#' @param git_hash Current git sha1 hash
#' @param github_repo Current github repository
#'
#' @export
launch_app <- function(dataset = NULL,
                       title = NULL,
                       publish_app = FALSE,
                       name = "experimental",
                       shiny_account = "skde",
                       git_hash = NULL,
                       github_repo = NULL) {

  # Create a directory with all necessary data.
  shinydir <- SKDEr::create_appdir(app_data = dataset,
                                   webpage_title = title,
                                   package = "shinymap",
                                   git_hash = git_hash,
                                   github_repo = github_repo)

  # Run the app
  if (publish_app) {
    rsconnect::deployApp(appDir = shinydir, appName = name, account = shiny_account)
  } else {
    shiny::runApp(appDir = shinydir)
  }
}
