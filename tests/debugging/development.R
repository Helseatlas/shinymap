# Example on how to update package data, which will be loadable by shinymaps::testdata

testdata <- readRDS("~/repo/shinymap/tests/testthat/data/kols.rds")
save(testdata, file = "data/testdata.RData")


# How to run the app

devtools::install_github("Helseatlas/shinymap")
shinymap::launch_app(dataset = shinymap::kols, publish_app = FALSE, title = "Helseatlas kols", language = "no")

# How to submit to shinyapps.io

devtools::install_github("Helseatlas/shinymap")
shinymap::launch_app(datasett = shinymap::kols, publish_app = TRUE, HNproxy = TRUE, title = "Helseatlas kols", language = "no")

print(ls())

rm(list=ls())


