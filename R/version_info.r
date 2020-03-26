#' Functions providing informational text about the app(s)
#'
#' @param newline String element defining line break for formatting. Default is
#' \code{<br>}
#'
#' @return String formatted in some sensible manner
#'
#' @export
#' @examples
#' version_info()

version_info <- function(newline = "<br>") {

  conf <- get_config() # nolint
  pkg <- conf$info$version$app
  m <- utils::installed.packages()[pkg, c(1, 3)]
  paste0(pkg, " v", m[pkg, "Version"], newline, collapse = "")
}
