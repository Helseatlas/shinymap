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
#' @return a plot made from a geojson map
makeGeojsonMap <- function(data = NULL, map = NULL){
  graphics::plot(map)
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
  
  output <- NULL
  switch(type,
         leaflet = {output <- makeLeafletmap()},
         geojson = {output <- makeGeojsonMap(data = data, map = map)}
         )
  return(output)
}
