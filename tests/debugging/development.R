
library(magrittr)

print(ls())

rm(list = ls())

getwd()
setwd("/Users/arnfinn/repo/shinymap")

# Example on how to update package data, which will be loadable by shinymaps::testdata

testdata <- readRDS("~/repo/shinymap/tests/testthat/data/kols.rds")
save(testdata, file = "data/testdata.RData")

testdata <- readRDS("~/repo/shinymap/tests/testthat/data/barn.rds")
save(testdata, file = "data/testdata.RData")

testmap <- geojsonio::geojson_read("tests/testthat/data/maps/test.geojson", what = "sp")
save(testmap, file = "data/testmap.RData")



# How to run the app through launch_app

devtools::install_github("Helseatlas/shinymap")
shinymap::launch_app(
  dataset = shinymap::kols,
  map = geojsonio::geojson_read("tests/testthat/data/maps/test.geojson", what = "sp"),
  publish_app = FALSE,
  title = "Helseatlas kols",
  language = "no"
)


# How to submit to shinyapps.io through launch_app
devtools::install_github("Helseatlas/shinymap")
shinymap::launch_app(
  dataset = shinymap::kols,
  map = geojsonio::geojson_read("tests/testthat/data/maps/test.geojson", what = "sp"),
  publish_app = TRUE,
  #                     HNproxy = TRUE,
  title = "Helseatlas kols",
  language = "no"
)


# How to run the app locally

healthatlas_data <- shinymap::kols # or another dataset to use
healthatlas_map <- geojsonio::geojson_read("tests/testthat/data/maps/test.geojson", what = "sp")

language <- "en"
language <- "no"
webpage_title <- "Helseatlas kols 2013-2015"
runApp("inst/app")


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
