#' Make a plot based on a data frame
#'
#' @param inputData Data to be plotted
#' @param type What kind of plot. Default and only choice, at the moment:
#'   "histogram"
#' @param xlab Label on x-axis
#' @param ylab Label on y-axis
#'
#' @export
plotVariation <- function(inputData = NULL, xlab = "Opptaksomr", ylab = "Rate", type = "histogram") {


  # barplot
  if (type == "histogram") {
    inputData$area <- factor(inputData$area, levels = inputData$area[order(inputData$rate)])

    ggplot2::ggplot(data = inputData,
                    ggplot2::aes(x = get("area"), y = get("rate"))) +
      ggplot2::geom_bar(stat = "identity", fill = "#95BDE6") +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::coord_flip() +
      ggthemes::theme_tufte()
  } else {
    return(NULL)
  }
}
