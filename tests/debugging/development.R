# Just testing

rm(list=ls())

myfile <- "tests/testthat/data/eldre.json"

myfile <- "tests/testthat/data/barn.json"

myfile <- "tests/testthat/data/kols.json"

myfile <- "tests/testthat/data/dagkir.json"


json_data <- jsonlite::fromJSON(myfile)

# make it a tibble data fram
tbl <- tibble::as_data_frame(json_data$geographies)

# Names of areas are located in json_data$geographies$features
bo <- data.frame(tbl$features)$name


is.data.frame(bo$name)

#if (testing){
  # Convert all special characters to "normal" characters if running tests,
  # because the sorting with special characters is system dependent.
  
  conv_list1 <- list("æ", "ø", "å", "Æ",  "Ø", "Å", '-', "St. ")
  conv_list2 <- list("ae","o", "a", "AE", "O", "å", "_", "St ")
  
  test <- bo
  for (i in 1:length(conv_list1)){
#    test <- data.frame(lapply(bo, function(x) {
#      gsub(conv_list1[i], conv_list2[i], x)
#    }))
    test <- gsub(conv_list1[i], conv_list2[i], test)
  }
#}
data.frame(test)
data.frame(bo)

data.frame(test,bo)

all.equal(test, bo)

tmp <- readIAjson(json_file = myfile)
tmp2 <- readIAjson(json_file = myfile, testing = TRUE)

all.equal(tmp, tmp2)


ref <- readRDS("tests/testthat/data/dagkir.rds")



all.equal(tmp, ref)


str(tmp$bo)

str(tmp2$bo)

data.frame(tmp2 = levels(tmp2$bo))
data.frame(tmp = levels(tmp$bo))



test <- merge(tmp, ref, by = c("bo", "level1", "level2", "id", "rate"))

length(test$bo)
length(tmp$bo)

df.changes(tmp, ref, KEYS = c("bo"))

akershus <- dplyr::filter(tmp, bo == "Førde")
akershus2 <- dplyr::filter(tmp2, bo == "Førde")

test3 <- df.changes(akershus2, akershus, KEYS = c("bo"))


plotVariasjon(fullDataFrame = tmp, which_id = "i3")

test <- compare::compare(tmp, tmp2)

test2 <- dplyr::all_equal(tmp, tmp2)


test3 <- df.changes(tmp2, tmp, KEYS = c("level1"))

test$tMpartial

httr::set_config(httr::use_proxy(url="http://www-proxy.helsenord.no", port=8080))
devtools::install_github("helseatlas/shinymap", ref = "readIAjson")


json_file <- "tests/testthat/data/barn.json"


# COPY OF readJS.R


# Read the json file
# NOTE: The js-file HAS to be converted from UTF-8 BOM to UTF-8 (in notepad++) before this will work!
json_data <- jsonlite::fromJSON(json_file)

# make it a tibble data fram
tbl <- tibble::as_data_frame(json_data$geographies)

# Names of areas are located in json_data$geographies$features
area <- data.frame(tbl$features)$name

# Name of reference is located in json_data$geographies$comparisonFeatures
ref_area <- data.frame(tbl$comparisonFeatures)$name

testing <- TRUE

if (testing){
  # Convert all special characters to "normal" characters if running tests,
  # because the sorting with special characters is system dependent.
  
  conv_list1 <- list("æ", "ø", "å", "Æ",  "Ø", "Å", '-', "St. ")
  conv_list2 <- list("ae","o", "a", "AE", "O", "å", "_", "St ")
  
  for (i in 1:length(conv_list1)){
    
    area <- gsub(conv_list1[i], conv_list2[i], area)
  }
}

# All data
themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()

# Test that number of highest level is equal the length of themes$indicators
if (length(themes$name) != length(themes$indicators)){
  stop("Something fishy in your json file. ")
}


next_level <- data.frame(themes$indicators[1])
rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
ref_rates <- data.frame(next_level$comparisonValues)  %>% tibble::as_data_frame()



# Define an empty data frame
all_data <- data.frame()
for (i in 1:length(themes$indicators)){
  # Names for first level
  level1 <- themes$name[i]
  prev_level3 <- "qwerty" # To check if level3 is equal to previous level3
  k = 0
  next_level <- data.frame(themes$indicators[i])
  rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
  ref_rates <- data.frame(next_level$comparisonValues)  %>% tibble::as_data_frame()
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
        combined <- data.frame(area, level1, level2, selection_id, rates[j]) 
        colnames(combined) <- c("area", "level1", "level2", "id", "rate")
        ref_combined <- data.frame(ref_area, level1, level2, selection_id, ref_rates[j]) 
        colnames(ref_combined) <- c("area", "level1", "level2", "id", "rate")
        
      } else { # Only for three level atlases
        if (level3 != prev_level3){ # If level3 is not equal to previous level3
          k = k + 1
          id2 <- paste0(selection_id, "j", k)
        }
        combined <- data.frame(area, level1, level2, level3, id2, rates[j]) 
        colnames(combined) <- c("area", "level1", "level2", "level3", "id", "rate")
        ref_combined <- data.frame(ref_area, level1, level2, level3, id2, ref_rates[j]) 
        colnames(ref_combined) <- c("area", "level1", "level2", "level3", "id", "rate")
        prev_level3 <- level3
      }
      all_data <- rbind(all_data, combined, ref_combined)
    }
  } 
}
print(all_data)

