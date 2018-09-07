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



# Read the json file
json_data <- jsonlite::fromJSON(json_file)

# make it a tibble data fram
tbl <- tibble::as_data_frame(json_data$geographies)

# Names of areas are located in json_data$geographies$features
bo <- data.frame(tbl$features)

# Name of reference is located in json_data$geographies$comparisonFeatures
ref <- data.frame(tbl$comparisonFeatures)

# All data
themes <- data.frame(tbl$themes) %>% tibble::as_data_frame()

# Names, highest level
level1 <- themes$name

# Test that number of highest level is equal the length of themes$indicators
if (length(themes$name) != length(themes$indicators)){
  stop("Something fishy in your json file. ")
}

all_data <- data.frame()

test <- themes$indicators[2]

test2 <- data.frame(test)

test2$date

print()

next_level <- data.frame(themes$indicators[1])

next_level$id
next_level$values

themes$name
next_level$values[1]

next_level$associates


all_data <- data.frame()
for (i in 1:length(themes$indicators)){
  level1 <- themes$name[i]
  next_level <- data.frame(themes$indicators[i])
  rates <- data.frame(next_level$values)  %>% tibble::as_data_frame()
  for (j in 1:length(next_level)){
    if (!is.na(next_level$id[j])){
      level2 <- next_level$name[j]
      level3 <- next_level$date[j]
      selection_id <- next_level$id[j]
      combined <- data.frame(bo$name, level1, level2, level3, selection_id, rates[j])

      colnames(combined)[1] <- "bo"
      colnames(combined)[2] <- "level1"
      colnames(combined)[3] <- "level2"
      colnames(combined)[4] <- "level3"
      colnames(combined)[5] <- "id"
      colnames(combined)[6] <- "rate"

      all_data <- rbind(all_data, combined)
    }
  }
}

print(all_data)

