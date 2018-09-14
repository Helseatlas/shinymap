#' Make a Leaflet map
#'
#' @param inputData The input data set to be plotted
#'
#' @return a map
#' @export
#'
makeLeafletmap <- function(inputData = NULL){
  
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
    leaflet::addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  
}