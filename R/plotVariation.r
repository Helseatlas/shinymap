#' Make a plot based on a data frame
#'
#' @param inputData Data to be plotted
#' @param type What kind of plot. Default and only choice, at the moment: "histogram"
#'
#' @export
plotVariation <- function(inputData = NULL, xlab = "Opptaksområde", ylab = "Rate", type = "histogram"){
  
  # barplot
  if (type == "histogram"){
    ggplot(data=inputData, aes(x=reorder(area, rate), y=rate)) +
      geom_bar(stat="identity", fill="#95BDE6") + 
      labs(x = xlab, y = ylab) + 
      #        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggplot2::coord_flip() +
      ggthemes::theme_tufte()
    #        theme(panel.background = element_blank())
  } else {
    return(NULL)
  }
}
