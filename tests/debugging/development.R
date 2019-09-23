library(magrittr)



rm(list=ls())

getwd()


all_data = list()


all_names = c("Dagkirurgi 2011-2013",
              "Barn",
              "Nyfødt",
              "Eldre",
              "Kols",
              "Dagkirurgi 2013-2017",
              "Ortopedi",
              "Gynekologi",
              "Fødselshjelp"
          )

all_data <- list(
              data::dagkir_en,
              data::barn,
              data::nyfodt,
              data::kols,
              data::eldre,
              data::dagkir2,
              data::ortopedi,
              data::gyn,
              data::fodsel)

names(all_data) <- all_names



# How to run the app through launch_app
devtools::install_github("Helseatlas/kart")
devtools::install_github("Helseatlas/data")
devtools::install_github("Helseatlas/shinymap")
shinymap::launch_app(dataset = all_data,
  publish_app = FALSE,
  title = "Helseatlas",
  language = "no"
)

# How to submit to shinyapps.io through launch_app
devtools::install_github("Helseatlas/kart")
devtools::install_github("Helseatlas/data")
devtools::install_github("Helseatlas/shinymap")
shinymap::launch_app(
  publish_app = TRUE,
  name = "Helseatlas",
  language = "no"
)

# How to run the app locally

healthatlas_data <- shinymap::kols # or another dataset to use
healthatlas_map <- geojsonio::geojson_read("tests/testthat/data/maps/test.geojson", what = "sp")

language <- "no"
webpage_title <- "Helseatlas kols 2013-2015"
runApp("inst/app")

rm(list = ls())

getwd()
setwd("/Users/arnfinn/repo/shinymap")

# Example on how to update package data, which will be loadable by shinymaps::testdata

testdata <- readRDS("~/repo/shinymap/tests/testthat/data/kols.rds")
save(testdata, file = "data/testdata.RData")

testdata <- get(load("data/kols.RData"))

testdata <- readRDS("~/repo/shinymap/tests/testthat/data/start_plot.rds")

testdata$area_name <- testdata$area
testdata$level1_name <- testdata$level1
testdata$level2_name <- testdata$level2
testdata$level3_name <- testdata$level3
testdata$value <- testdata$rate
testdata <- within(testdata, rm(rate))
testdata$denominator_name <- testdata$name_denominator
testdata <- within(testdata, rm(name_denominator))
testdata$numerator_name <- testdata$name_numerater
testdata <- within(testdata, rm(name_numerater))
testdata$numerator <- testdata$numerater
testdata <- within(testdata, rm(numerater))

start_plot <- testdata
saveRDS(start_plot, file = "~/repo/shinymap/tests/testthat/data/start_plot.rds")

save(kols, file = "data/kols.RData")
save(testdata, file = "data/testdata.RData")

# Make the testmap data
testmap <- kart::barn
usethis::use_data(testmap, overwrite = TRUE)
plot(shinymap::testmap)

# Make the testdata

testdata <- data::barn

testdata$area_name <- as.factor(gsub("å", "aa", testdata$area_name))
testdata$area_name <- as.factor(gsub("ø", "o", testdata$area_name))
testdata$area_name <- as.factor(gsub("æ", "ae", testdata$area_name))
testdata$area_name <- as.factor(gsub("Å", "AA", testdata$area_name))
testdata$area_name <- as.factor(gsub("Ø", "O", testdata$area_name))
testdata$area_name <- as.factor(gsub("Æ", "AE", testdata$area_name))

testdata$level1_name <- as.factor(gsub("å", "aa", testdata$level1_name))
testdata$level1_name <- as.factor(gsub("ø", "o", testdata$level1_name))
testdata$level1_name <- as.factor(gsub("æ", "ae", testdata$level1_name))
testdata$level1_name <- as.factor(gsub("Å", "AA", testdata$level1_name))
testdata$level1_name <- as.factor(gsub("Ø", "O", testdata$level1_name))
testdata$level1_name <- as.factor(gsub("Æ", "AE", testdata$level1_name))

testdata$level2_name <- as.factor(gsub("å", "aa", testdata$level2_name))
testdata$level2_name <- as.factor(gsub("ø", "o", testdata$level2_name))
testdata$level2_name <- as.factor(gsub("æ", "ae", testdata$level2_name))
testdata$level2_name <- as.factor(gsub("Å", "AA", testdata$level2_name))
testdata$level2_name <- as.factor(gsub("Ø", "O", testdata$level2_name))
testdata$level2_name <- as.factor(gsub("Æ", "AE", testdata$level2_name))

testdata$level3_name <- as.factor(gsub("å", "aa", testdata$level3_name))
testdata$level3_name <- as.factor(gsub("ø", "o", testdata$level3_name))
testdata$level3_name <- as.factor(gsub("æ", "ae", testdata$level3_name))
testdata$level3_name <- as.factor(gsub("Å", "AA", testdata$level3_name))
testdata$level3_name <- as.factor(gsub("Ø", "O", testdata$level3_name))
testdata$level3_name <- as.factor(gsub("Æ", "AE", testdata$level3_name))

usethis::use_data(testdata, overwrite = TRUE)

shinymap::testdata


# How two extra test maps were made
testmap <- geojsonio::geojson_read("tests/testthat/data/maps/test.geojson", what = "sp")
geojsonio::geojson_write(testmap, file = "tests/testthat/data/maps/utm33.geojson")
new <- utm33_to_leaflet(testmap)
geojsonio::geojson_write(new, file = "tests/testthat/data/maps/epsg4326.geojson")


##############
# Plot a map #
##############

# Eldre
map <- geojsonio::geojson_read("tests/testthat/data/maps/eldre.geojson", what = "sp")

eldre <- utm33_to_leaflet(map)

leaflet::leaflet(eldre) %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(stroke = FALSE,
                       smoothFactor = 0.3,
                       fillOpacity = 0.2,
                       fillColor = c("green", "blue", "red",
                                     "yellow", "orange", "purple"))


# Map from https://kartkatalog.geonorge.no/metadata/geonorge/norske-fylker-og-kommuner-illustrasjonsdata-2018-klippet-etter-kyst/cbbdf78c-fa3a-48bf-8f3f-9eec74e428fd
# Reduced in size with reduce_map_size function

map <- geojsonio::geojson_read("tests/testthat/data/maps/kommuner.geojson", what = "sp")

new <- utm33_to_leaflet(map)

leaflet::leaflet(new) %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.2, fillColor = c("green", "blue", "red", "yellow", "orange", "purple"))


# Save map to disk
map$Kommunenav <- NULL # rm kommunenavn (trouble with norwegian letters)
geojsonio::geojson_write(map, file = "tests/testthat/data/maps/kommuner.geojson")
