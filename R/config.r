#' Configuration
#'
#' Creates a configuration file based on the default
#' shipped with helseatlas package.
#'
#' @param dir Folder to put config file
#' @param force Overwrite current config file
#'
#' @export
create_config <- function(dir = ".", force = FALSE) {
  ref_file <- system.file("helseatlas.yml", package = "helseatlas")
  new_file <- paste(dir, "_helseatlas.yml", sep = "/")
  if (!file.exists(new_file) | force) {
    file.copy(ref_file, to = new_file)
    return(paste0(new_file, " file created: fill it in"))
  } else {
    return(paste0("Cannot create ", new_file, " config file: already exists.",
    "(run with force = TRUE if you want to overwrite file)"))
  }
}

#' Retrieve Config
#'
#' Retrieves config file.
#'
#' @param dir Folder location of _helseatlas.yml file
#'
#' @export
get_config <- function(dir = ".") {
  config_file <- paste(dir, "_helseatlas.yml", sep = "/")
  if (!file.exists(config_file)) {
    # Use the default if _helseatlas.yml does not exist
    config_file <- system.file("helseatlas.yml", package = "helseatlas")
  }
  config <- yaml::read_yaml(config_file)
  check_config(config)
  return(config)
}

#' Check config file
#'
#' @param config Config file to check
#'
check_config <- function(config) {
  invisible()
}
