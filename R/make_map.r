#' Make a Leaflet map
#'
#' @return a map
#'
makeLeafletmap <- function(){
  norway <- maps::map(database = "world",
                      regions = "Norway",
                      fill = TRUE,
                      col = 1,
                      plot = F)

  leaflet::leaflet(norway) %>%
    leaflet::setView(lng = 10.0, lat = 65.0, zoom = 4) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(fillColor = grDevices::topo.colors(10, alpha = NULL),
                         stroke = FALSE)
}

#' Plot a map with data
#'
#' @param data Data to be plotted
#' @param map Map to plot
#'
#' @return a map made from a geojson file
makeGeojsonMap <- function(data = NULL, map = NULL){
  geojsonmap <- geojsonio::geojson_read(map, what = "sp")
  graphics::plot(geojsonmap)
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
    output <- makeLeafletmap()
  } else if (type == "geojson"){
    output <- makeGeojsonMap(data = data, map = map)
  } else {
    output <- NULL
  }

  return(output)
}
