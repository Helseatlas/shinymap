#' Make a Leaflet map
#'
#' @param data Data to be presented in map
#' @param map Map to be plotted
#' @param utm33 Convert from UTM 33 to Leaflet projection
#'
#' @return a map
#'
plotLeafletmap <- function(data = NULL, map = NULL, utm33 = TRUE) {
  if (utm33) {
    # convert from utm33 to leaflet
    map <- shinymap::utm33toLeaflet(map = map)
  }

  leaflet::leaflet(map) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(stroke = FALSE,
                         smoothFactor = 0.3,
                         fillOpacity = 0.2,
                         fillColor = c("green", "blue", "red",
                                       "yellow", "orange", "purple"))
}

#' Plot a map with data
#'
#' @param data Data to be plotted
#' @param map Map to plot
#' @param utm33 Convert from UTM 33 to Leaflet projection
#'
#' @return a plot made from a geojson map
plotSimpleMap <- function(data = NULL, map = NULL, utm33 = TRUE) {
  if (utm33) {
    # convert from utm33 to leaflet
    map <- shinymap::utm33toLeaflet(map = map)
  }
  sp::plot(map)
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
makeMap <- function(data = NULL, map = NULL, type = "leaflet") {
  output <- NULL
  switch(type,
    leaflet = {
      output <- plotLeafletmap(data = data, map = map)
    },
    simple = {
      output <- plotSimpleMap(data = data, map = map)
    }
  )
  return(output)
}
