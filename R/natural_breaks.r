#' Get the jenks natural breaks from a data set 
#'
#' @param data Input data
#' @param num Number of groups
#'
#' @return Natural breaks
#' @export
#'
natural_breaks <- function(data = NULL, num = 4) {
    rate_num <- as.numeric(data)
    rate_num[is.na(rate_num)] <- 0
    interv <- c(classInt::classIntervals(var = rate_num, n = num, style = "jenks"))
    brks <- unique(interv$brks)
    return(cut(rate_num, breaks = brks, include.lowest = TRUE))
}
