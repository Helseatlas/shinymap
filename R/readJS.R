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
  area <- data.frame(tbl$features)$name
  
  # Name of reference is located in json_data$geographies$comparisonFeatures
  ref_area <- data.frame(tbl$comparisonFeatures)$name
  
  if (testing){
    # Convert all special characters to "normal" characters if running tests,
    # because the sorting with special characters is system dependent.
    
    conv_list1 <- list("æ", "ø", "å", "Æ",  "Ø", "Å", '-', "St. ")
    conv_list2 <- list("ae","o", "a", "AE", "O", "å", "_", "St ")
    
    for (i in 1:length(conv_list1)){
      area <- gsub(conv_list1[i], conv_list2[i], area)
      ref_area <- gsub(conv_list1[i], conv_list2[i], ref_area)
    }
  }
  
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
    prev_level3 <- "qwerty" # To check if level3 is equal to previous level3
    k = 0
    next_level <- data.frame(themes$indicators[i])
    # Rates to be plotted
    rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
    # Rates for Norway etc
    ref_rates <- data.frame(next_level$comparisonValues)  %>% tibble::as_data_frame()
    
    # Extract the numeraters and denominators  
    extra <- data.frame(next_level$associates)
    
    for (j in 1:length(next_level)){
      if (!is.na(next_level$id[j])){
        
        k <- j - 1
        if (k == 0){
          post <- ""
        } else {
          post <- paste0(".", k)
        }
        numerater <- data.frame(extra[, paste0("values",post)][2])
        denominator <- data.frame(extra[, paste0("values",post)][1])
        name_numerater <- extra[, paste0("name",post)][2]
        name_denominator <- extra[, paste0("name",post)][1]
        ref_numerater <-  data.frame(extra[, paste0("comparisonValues",post)][2])
        ref_denominator <- data.frame(extra[, paste0("comparisonValues",post)][1])
        
        # Names for the second level
        level2 <- next_level$name[j]
        level3 <- NULL
        # Names for the third level, if it exists
        level3 <- try(next_level$date[j])
        # ID for level 2 (not unique with three levels)
        selection_id <- next_level$id[j]
        if (is.null(level3)){
          
          # Only for two-level atlases
          combined <- data.frame(area, level1, level2, selection_id, rates[j], name_numerater, numerater, name_denominator, denominator, 0) 
          colnames(combined) <- c("area", "level1", "level2", "id", "rate", "name_numerater", "numerater", "name_denominator", "denominator", "ref")
          ref_combined <- data.frame(ref_area, level1, level2, selection_id, ref_rates[j], name_numerater, ref_numerater, name_denominator, ref_denominator, 1) 
          colnames(ref_combined) <- c("area", "level1", "level2", "id", "rate", "name_numerater", "numerater", "name_denominator", "denominator", "ref")
        } else { # Only for three level atlases
          if (level3 != prev_level3){ # If level3 is not equal to previous level3
            k = k + 1
            id2 <- paste0(selection_id, "j", k)
          }
          combined <- data.frame(area, level1, level2, level3, id2, rates[j], name_numerater, numerater, name_denominator, denominator, 0) 
          colnames(combined) <- c("area", "level1", "level2", "level3", "id", "rate", "name_numerater", "numerater", "name_denominator", "denominator", "ref")
          ref_combined <- data.frame(ref_area, level1, level2, level3, id2, ref_rates[j], name_numerater, ref_numerater, name_denominator, ref_denominator, 1) 
          colnames(ref_combined) <- c("area", "level1", "level2", "level3", "id", "rate", "name_numerater", "numerater", "name_denominator", "denominator", "ref")
          prev_level3 <- level3
        }
        all_data <- rbind(all_data, combined, ref_combined)
      }
    } 
  }
  
  return(all_data)
  
}
