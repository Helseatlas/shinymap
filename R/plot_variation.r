#' Make a plot based on a data frame
#'
#' @param input_data Data to be plotted
#' @param type What kind of plot. Default and only choice, at the moment:
#'   "histogram"
#' @param xlab Label on x-axis
#' @param ylab Label on y-axis
#' @param num_groups Number of natural break groups
#' @param decimal_mark sign to use for decimal 
#' @param big_mark sign to use for numbers over a thousand 
#'
#' @export
plot_variation <- function(input_data = NULL, xlab = "Area", ylab = "Rate", type = "histogram", num_groups = 5,
                           decimal_mark = ",", big_mark = " ") {

  options(encoding = "UTF-8")

  # Avoid errors if not everything is updated on the server side
  if (is.null(input_data) || length(input_data$value) == 0) {
    return(NULL)
  }

  the_plot <- NULL
  if (type == "histogram") {
    # barplot
    input_data$area_name <- factor(input_data$area_name, levels = input_data$area_name[order(input_data$value)])

    # extract the natural breaks
    input_data$brks <- helseatlas::natural_breaks(data = input_data$value, num = num_groups)

    # norwegian average
    norway_avg <- dplyr::filter(input_data, input_data[["area"]] == 8888)

    the_plot <-
      ggplot2::ggplot(data = input_data,
      ggplot2::aes(x = get("area_name"), y = get("value"), fill = get("brks"),
                   text = paste("<b>", get("area_name"), "</b><br>",
                                get("type"), ":", format(get("value"), decimal.mark = decimal_mark, digits = 2), "<br>",
                                get("numerator_name"), ":", format(get("numerator"), big.mark = big_mark)))) +
   
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_bar(data = norway_avg, fill = "grey", stat = "identity") +

      ggplot2::scale_fill_manual("",
                                 values = SKDEr::skde_colors(num = num_groups),
                                 guide = ggplot2::guide_colorsteps(ticks = FALSE,
                                                                  show.limits = TRUE)) +
      ggplot2::labs(x = NULL, y = ylab) +
      ggplot2::coord_flip() +

      ggthemes::theme_tufte() +
      ggplot2::theme(text = ggplot2::element_text(size = 14),
                     axis.line = ggplot2::element_line(color = "black"),
                     axis.ticks.y = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 1),
                     legend.position = "none") +
      ggplot2::scale_y_continuous(expand = c(0, 0))

      the_plotly <- plotly::ggplotly(the_plot, tooltip = "text")
      return(the_plotly)
  }
}
