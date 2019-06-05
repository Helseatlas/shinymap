#' Make a plot based on a data frame
#'
#' @param input_data Data to be plotted
#' @param type What kind of plot. Default and only choice, at the moment:
#'   "histogram"
#' @param xlab Label on x-axis
#' @param ylab Label on y-axis
#'
#' @export
plot_variation <- function(input_data = NULL, xlab = "Opptaksomr", ylab = "Rate", type = "histogram") {

  if (is.null(input_data)) {return(NULL)}

  # barplot
  if (type == "histogram") {
    input_data$area_name <- factor(input_data$area_name, levels = input_data$area_name[order(input_data$value)])
    ggplot2::ggplot(data = input_data,
                    ggplot2::aes(x = get("area_name"), y = get("value"))) +
      ggplot2::geom_bar(stat = "identity", fill = "#95BDE6") +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::coord_flip() +
      ggthemes::theme_tufte()
  } else {
    return(NULL)
  }
}
