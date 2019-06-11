#' Function that returns a list of SKDE colors.
#'
#' @param num Number of SKDE colors to be returned
#'
#' @return List of colors in HEX format
#' @export
#'
skde_colors <- function(num = 4) {
  # These colors have been taken from https://github.com/Rapporteket/rapbase/blob/22c3bfdf7f338fa939b59fb1ba7f6015dee125e8/R/LibFigFilType.R
  all_colors <- c('#c6dbef', '#6baed6', '#4292c6', '#2171b5', '#084594', '#000059', '#FF7260', '#4D4D4D', '#737373', '#A6A6A6', '#DADADA')
  if (num == 1) {
    return(all_colors[c(4)])
  } else if (num == 2) {
    return(all_colors[c(3, 5)])
  } else if (num == 3) {
    return(all_colors[c(2, 4, 5)])
  } else if (num == 4) {
    return(all_colors[c(1, 2, 4, 5)])
  } else if (num == 5) {
    return(all_colors[c(1, 2, 4, 5, 6)])
  } else if (0 < num && num <= length(all_colors)) {
    return(utils::head(all_colors, num))
  } else {
      warning(paste0("skde_colors should be run with num between 1 and ", length(all_colors)))
      return(all_colors)
  } 
}
