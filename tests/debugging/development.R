library(magrittr)

rm(list=ls())

# All atlas data and maps
all_data <- list(
  "fodsel"   = list("data_no" = data::fodsel,   "data_en" = data::fodsel_en,  "year" = "2015–2017", "title_no" = "Fødselshjelp", "title_en" = "Obstetrics",  "map" = kart::fodsel),
  "gyn"      = list("data_no" = data::gyn,      "data_en" = data::gyn_en,     "year" = "2015–2017", "title_no" = "Gynekologi",   "title_en" = "Gynaecology", "map" = kart::gyn),
  "ortopedi" = list("data_no" = data::ortopedi, "data_en" = data::ortopedi,   "year" = "2012–2016", "title_no" = "Ortopedi",     "title_en" = "Orthopaedic", "map" = kart::gyn),
  "dagkir2"  = list("data_no" = data::dagkir2,  "data_en" = data::dagkir2_en, "year" = "2013–2017", "title_no" = "Dagkirurgi",   "title_en" = "Day surgey",  "map" = kart::dagkir2),
  "kols"     = list("data_no" = data::kols,     "data_en" = data::kols_en,    "year" = "2013–2015", "title_no" = "Kols",         "title_en" = "COPD",        "map" = kart::eldre),
  "eldre"    = list("data_no" = data::eldre,    "data_en" = data::eldre_en,   "year" = "2013–2015", "title_no" = "Eldre",        "title_en" = "Elderly",     "map" = kart::kols),
  "nyfodt"   = list("data_no" = data::nyfodt,   "data_en" = data::nyfodt_en,  "year" = "2009–2014", "title_no" = "Nyfødt",       "title_en" = "Neonatal",    "map" = kart::nyfodt),
  "barn"     = list("data_no" = data::barn,     "data_en" = data::barn_en,    "year" = "2011–2014", "title_no" = "Barn",         "title_en" = "Children",    "map" = kart::barn),
  "dagir"    = list("data_no" = data::dagkir,   "data_en" = data::dagkir_en,  "year" = "2011–2013", "title_no" = "Dagkirurgi",   "title_en" = "Day surgey",  "map" = kart::dagkir)
)

# Install packages from scratch
devtools::install_github("Helseatlas/kart")
devtools::install_github("Helseatlas/data")
devtools::install_github("Helseatlas/shinymap")

# How to run the app through launch_app
# (remember to restart R session first)
.rs.restartR()
shinymap::launch_app(
  dataset = all_data,
  publish_app = FALSE
)

# How to submit to shinyapps.io through launch_app
# (remember to restart R session first)
.rs.restartR()
shinymap::launch_app(
  dataset = all_data,
  publish_app = TRUE
)

# How to run the app locally

healthatlas_data <- all_data

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
