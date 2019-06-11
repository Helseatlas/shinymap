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
    input_data$area_name <- factor(input_data$area_name, levels = input_data$area_name[order(input_data$rate)])
    
    library(classInt)
    rate_num<-as.numeric(input_data$rate)
    #print(rate_num)
    rate_num[is.na(rate_num)]<-0
    #print(rate_num)
    interv<-c(classIntervals(rate_num , style="jenks"))
    brks<-unique(interv$brks)
    print(brks)
    input_data$brks<-cut(rate_num, breaks=brks)
    
    ggplot2::ggplot(data = input_data,
      ggplot2::aes(x = get("area_name"), y = get("value"), fill = brks)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values=c("#0099CC", "#33CCFF","#0033CC", "#66CCFF","#6699FF","#3366FF")) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::coord_flip() +
      ggthemes::theme_tufte()
  } else {
    return(NULL)
  }
}
