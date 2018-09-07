#' Read json-data from IA
#'
#' @param json_file The json file used by IA
#' @param testing Will convert special characters to non-special characters if set to TRUE
#'
#' @return A data frame
#' @export
#' @importFrom magrittr "%>%"
#'
readIAjson <- function(json_file = NULL, testing = FALSE){
  
  # Read the json file
  # NOTE: The js-file HAS to be converted from UTF-8 BOM to UTF-8 (in notepad++) before this will work!
  json_data <- jsonlite::fromJSON(json_file)
  
  # make it a tibble data fram
  tbl <- tibble::as_data_frame(json_data$geographies)
  
  # Names of areas are located in json_data$geographies$features
  bo <- data.frame(tbl$features)$name
  
  if (testing){
  # Convert all special characters to "normal" characters if running tests,
  # because the sorting with special characters is system dependent.
  
  conv_list1 <- list("æ", "ø", "å", "Æ",  "Ø", "Å", '-', "St. ")
  conv_list2 <- list("ae","o", "a", "AE", "O", "å", "_", "St ")

  for (i in 1:length(conv_list1)){

    bo <- gsub(conv_list1[i], conv_list2[i], bo)
    }
  }
  
  
  # Name of reference is located in json_data$geographies$comparisonFeatures
  # Not yet used!
  ref <- data.frame(tbl$comparisonFeatures)
  
  # All data
  themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()
  
  # Test that number of highest level is equal the length of themes$indicators
  if (length(themes$name) != length(themes$indicators)){
    stop("Something fishy in your json file. ")
  }

  # Define an empty data frame
  all_data <- data.frame()
  
  for (i in 1:length(themes$indicators)){
    # Names for first level
    level1 <- themes$name[i]
    next_level <- data.frame(themes$indicators[i])
    rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
    for (j in 1:length(next_level)){
      if (!is.na(next_level$id[j])){
        # Names for the second level
        level2 <- next_level$name[j]
        level3 <- NULL
        # Names for the third level, if it exists
        level3 <- try(next_level$date[j])
        # ID for level 2 (not unique with three levels)
        selection_id <- next_level$id[j]
        if (is.null(level3)){
          # Only for two-level atlases
          combined <- data.frame(bo, level1, level2, selection_id, rates[j]) 
          colnames(combined) <- c("bo", "level1", "level2", "id", "rate")
        } else {
          # Only for three level atlases
          combined <- data.frame(bo, level1, level2, level3, selection_id, rates[j]) 
          colnames(combined) <- c("bo", "level1", "level2", "level3", "id", "rate")
        }
        all_data <- rbind(all_data, combined)
      }
    } 
  }
  
  return(all_data)
  
}
