#' Make a Leaflet map
#'
#' @param inputData The input data set to be plotted
#'
#' @return a map
#' @export
#'
makeLeafletmap <- function(inputData = NULL){
  norway <- maps::map(database = "world", regions = "Norway", fill = TRUE, col = 1, plot = F)
  
  leaflet::leaflet(norway) %>% leaflet::setView(lng = 10.0, lat = 65.0, zoom = 4) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
}