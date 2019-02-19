# Example on how to update package data

testdata <- readRDS("~/repo/shinymap/tests/testthat/data/kols.rds")
save(testdata, file = "data/testdata.RData")




# Testing reduce_map_size.R

#library(spdplyr)
map <- shp2geojson(folder = "tests/testthat/data/maps")

#############################
# From old reduce_map_size: #
#############################

reduced_map <- rgeos::gSimplify(original_map, tol = amount)
reduced_map <- as(reduced_map, "SpatialPolygonsDataFrame")

# Read shapefile attributes
df = data.frame(original_map)

# Keep shp attributes
reduced_map <- sp::SpatialPolygonsDataFrame(reduced_map, df)
rgdal::writeOGR(reduced_map,dsn = folder, layer = new_shapefile, driver="ESRI Shapefile", overwrite_layer=overwrite)
return(reduced_map)

###########################

# Just testing

devtools::install_github("Helseatlas/shinymap", ref = "data_independence")
shinymap::launch_app(datasett = kols, publish_app = TRUE, HNproxy = TRUE, title = "Helseatlas kols", language = "no")

print(ls())

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

data.frame(tmp2 = levels(barn2$area),tmp = levels(barn$area))
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


rm(list=ls())
json_file <- "tests/testthat/data/kols.json"
testing <- FALSE

# COPY OF readJS.R


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
print(all_data)

