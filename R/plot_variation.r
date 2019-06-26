#' Make a plot based on a data frame
#'
#' @param input_data Data to be plotted
#' @param type What kind of plot. Default and only choice, at the moment:
#'   "histogram"
#' @param xlab Label on x-axis
#' @param ylab Label on y-axis
#' @param num_groups Number of natural break groups
#'
#' @export
plot_variation <- function(input_data = NULL, xlab = "Opptaksomr", ylab = "Rate", type = "histogram", num_groups = 4) {
  
  # Avoid errors if not everything is updated on the server side
  if (is.null(input_data) || length(input_data$value) == 0) {
    return(NULL)
  } 
  
  the_plot <- NULL
  if (type == "histogram") {
    # barplot
    input_data$area_name <- factor(input_data$area_name, levels = input_data$area_name[order(input_data$value)])

    # extract the natural breaks
    input_data$brks <- shinymap::natural_breaks(data = input_data$value, num = num_groups)

    the_plot <- ggplot2::ggplot(data = input_data,
      ggplot2::aes(x = get("area_name"), y = get("value"), fill = brks)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = shinymap::skde_colors(num = num_groups)) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::coord_flip() +
      ggthemes::theme_tufte()
  }
  return(the_plot)
}
