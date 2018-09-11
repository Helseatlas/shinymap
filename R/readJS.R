#' Read json-data from IA
#' 
#' All the data used to produce an Instant Atlas is stored in
#' a json file. This function will read such a file and put the
#' data into a data frame.  
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
  
  # Names of areas are located in json_data$geographies$features$name
  area <- data.frame(tbl$features)$name
  
  # Name of reference areas are located in json_data$geographies$comparisonFeatures$name
  ref_area <- data.frame(tbl$comparisonFeatures)$name
  
  if (testing){
    # Convert all special characters to "normal" characters if running tests,
    # because the sorting with special characters is system dependent.
    
    conv_list1 <- list("\u00E5", "\u00F8", "\u00E5", "\u00C6",  "\u00D8", "\u00C5", '-', "St. ")
    conv_list2 <- list("ae","o", "aa", "AE", "O", "AA", "", "St")
    
    for (i in 1:length(conv_list1)){
      area <- gsub(conv_list1[i], conv_list2[i], area)
      ref_area <- gsub(conv_list1[i], conv_list2[i], ref_area)
    }
  }
  
  # The rest of the data is located in json_data$geographies$themes
  themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()
  
  # Test that number of highest level names (json_data$geographies$themes$name) 
  # is equal the length of the data (json_data$geographies$themes$indicators)
  if (length(themes$name) != length(themes$indicators)){
    stop("Something fishy in your json file. ")
  }
  
  # Define an empty data frame, so we can add data to this frame in the loop.
  all_data <- data.frame()
  
  # Loop over level one
  for (i in 1:length(themes$name)){

    # Names for first level
    level1 <- themes$name[i]
    
    # Evereything else is stored in next_level
    next_level <- data.frame(themes$indicators[i])
    
    # Rates to be plotted
    rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
    # Rates for Norway etc
    ref_rates <- data.frame(next_level$comparisonValues)  %>% tibble::as_data_frame()
    # Link to fact sheets
    href <- next_level$href
    
    # Extract the numeraters and denominators
    extra <- data.frame(next_level$associates)
    
    # Dummy definition, to later check if level3 is equal to previous level3
    prev_level3 <- "qwerty"
    
    k = 0
    
    for (j in 1:length(next_level)){
      # Level 2
      
      if (!is.na(next_level$id[j])){
        
        # Names for the second level
        level2 <- next_level$name[j]
        level3 <- NULL
        # Names for the third level, if it exists
        level3 <- try(next_level$date[j])
        # ID for level 2 (not unique with three levels)
        selection_id <- next_level$id[j]
        
        # numeraters and denominaters stored in extra$values.1 etc.
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
        
        combined <- data.frame(area, level1, level2)
        colnames(combined) <- c("area", "level1", "level2")
        ref_combined <- data.frame(ref_area, level1, level2)
        colnames(ref_combined) <- c("area", "level1", "level2")
        
        # Extract metatext, if present
        properties <- NULL
        properties <- try(next_level$properties)
        metatext <- NULL
        if (!is.null(properties)){
          # Metatext can either be stored in next_level$properties[l]$value[n] or in
          # next_level$properties$value[n], next_level$properties$value.1[n] etc.
          if (length(properties) > 1){
            # If metatext is stored in next_level$properties[l]$value[n]
            for (l in 1:length(properties)){
              df_properties <- data.frame(properties[l])
              for (n in 1:length(df_properties$value)){
                if (df_properties$name[n] == "metatext"){
                  metatext[l] <- try(df_properties[, "value"][n])
                }
              } 
            }
          } else {
            # If metatext is stored in next_level$properties$value[n], next_level$properties$value.1[n] etc.
            df_properties <- data.frame(properties)
            for (l in 1:(length(df_properties)/2)){
              m = l - 1
              if (m == 0){
                for (n in 1:length(df_properties$value)){
                  if (df_properties$name[n] == "metatext"){
                    metatext[l] <- try(df_properties[, "value"][n])
                  }
                } 
              } else {
                for (n in 1:length(df_properties$value)){
                  if (df_properties[,paste0("name.", m)][n] == "metatext"){
                    metatext[l] <- try(df_properties[,paste0("value.", m)][n])
                  }
                } 
              }
              
            }
          }
        }
        if (is.null(level3)){
          
          # Only for two-level atlases
          combined["id"] <- selection_id
          ref_combined["id"] <- selection_id
          
        } else {
          # A new unique identifier has to be defined if it is a three-level atlas
          if (level3 != prev_level3){ # If level3 is not equal to previous level3
            k = k + 1
            id2 <- paste0(selection_id, "j", k)
          }
          combined["level3"] <- level3 
          ref_combined["level3"] <- level3 
          combined["id"] <- id2
          ref_combined["id"] <- id2
          prev_level3 <- level3
          
        }
        combined["rate"] <- rates[j]
        ref_combined["rate"] <- ref_rates[j]
        
        combined["name_numerater"] <- name_numerater
        ref_combined["name_numerater"] <- name_numerater
        
        combined["numerater"] <- numerater
        ref_combined["numerater"] <- ref_numerater
        
        combined["name_denominator"] <- name_denominator
        ref_combined["name_denominator"] <- name_denominator
        
        combined["denominator"] <- denominator
        ref_combined["denominator"] <- ref_denominator
        
        combined["ref"] <- 0 
        ref_combined["ref"] <- 1
        
        combined["href"] <- href[j]
        ref_combined["href"] <- href[j]
        
        if (!is.null(metatext)){
          combined["metatext"] <- metatext[j]
          ref_combined["metatext"] <- metatext[j]
        }
        
        
        all_data <- rbind(all_data, combined, ref_combined)
      }
    }
  }
  
  return(all_data)
  
}
