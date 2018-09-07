#' Make a plot based on a data frame
#'
#' @param fullDataFrame Data to be plotted
#' @param which_id Which data to plot (with id == which_id)
#'
#' @export
plotVariation <- function(fullDataFrame = NULL, which_id = "i0"){
  
  # Filter out data by id
  tmp <- dplyr::filter(fullDataFrame, id == which_id)
  
  # Reorder from highest to lowest  
  tmp$bo <- factor(tmp$bo, levels = tmp$bo[order(tmp$rate)])
  
  # Plotting 
  ggplot2::ggplot(data = tmp, ggplot2::aes(y=rate, x=bo)) + ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip() +
    ggthemes::theme_tufte()
}
