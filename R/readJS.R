#' Read json-data from IA
#'
#' @param json_file The json file used by IA
#'
#' @return A data frame
#' @export
#' @importFrom magrittr "%>%"
#'
readIAjson <- function(json_file = NULL){
  
  # Read the json file
  # NOTE: The js-file HAS to be converted from UTF-8 BOM to UTF-8 (in notepad++) before this will work!
  json_data <- jsonlite::fromJSON(json_file)
  
  # make it a tibble data fram
  tbl <- tibble::as_data_frame(json_data$geographies)
  
  # Names of areas are located in json_data$geographies$features
  bo <- data.frame(tbl$features)
  
  # Name of reference is located in json_data$geographies$comparisonFeatures
  ref <- data.frame(tbl$comparisonFeatures)
  
  # All data
  themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()
  
  # Test that number of highest level is equal the length of themes$indicators
  if (length(themes$name) != length(themes$indicators)){
    stop("Something fishy in your json file. ")
  }
  
  all_data <- data.frame()
  
  for (i in 1:length(themes$indicators)){
    # Name, highest level
    level1 <- themes$name[i]
    next_level <- data.frame(themes$indicators[i])
    rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
    for (j in 1:length(next_level)){
      if (!is.na(next_level$id[j])){
        level2 <- next_level$name[j]
        level3 <- NULL
        level3 <- try(next_level$date[j])
        selection_id <- next_level$id[j]
        if (is.null(level3)){
          combined <- data.frame(bo$name, level1, level2, selection_id, rates[j]) 
          colnames(combined) <- c("bo", "level1", "level2", "id", "rate")
        } else {
          combined <- data.frame(bo$name, level1, level2, level3, selection_id, rates[j]) 
          colnames(combined) <- c("bo", "level1", "level2", "level3", "id", "rate")
        }
        all_data <- rbind(all_data, combined)
      }
    } 
  }
  
  return(all_data)
  
}


#' Make a plot based on a data frame
#'
#' @param fullDataFrame Data to be plotted
#' @param which_id Which data to plot (with id == which_id)
#'
#' @export
plotVariasjon <- function(fullDataFrame = NULL, which_id = "i0"){
  
  # Filter out data by id
  tmp <- dplyr::filter(fullDataFrame, id == which_id)

  # Reorder from highest to lowest  
  tmp$bo <- factor(tmp$bo, levels = tmp$bo[order(tmp$rate)])

  # Plotting 
  ggplot2::ggplot(data = tmp, ggplot2::aes(y=rate, x=bo)) + ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip() +
    ggthemes::theme_tufte()
}
