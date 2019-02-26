#' Make a Leaflet map
#'
#' @return a map
#'
makeLeafletmap <- function(){
  norway <- maps::map(database = "world", regions = "Norway", fill = TRUE, col = 1, plot = F)
  
  leaflet::leaflet(norway) %>% leaflet::setView(lng = 10.0, lat = 65.0, zoom = 4) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(fillColor = grDevices::topo.colors(10, alpha = NULL), stroke = FALSE)
}

#' Title
#'
#' @param data 
#' @param map 
#'
#' @return a map made from a geojson file
makeGeojsonMap <- function(data = NULL, map = NULL){
  return(NULL)
  
}

#' Common function to make a map
#'
#' @param data Data to be plotted in the map
#' @param map The map itself
#' @param type Type of map (default = "leaflet")
#'
#' @return a map
#' @export
#'
makeMap <- function(data = NULL, map = NULL, type = "leaflet"){
  
  if (type == "leaflet"){
    map <- makeLeafletmap()
    return(map)
  }
  if (type == "geojson"){
    return(NULL)
  }
  
  
  
}
