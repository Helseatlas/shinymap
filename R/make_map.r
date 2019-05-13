#' Function to make a map
#'
#' @param data Data to be plotted in the map
#' @param map The map itself
#' @param type Type of map (default = "leaflet")
#' @param utm33 Convert from UTM 33 to Leaflet projection (default = TRUE)
#'
#' @return a map
#' @export
#' @importFrom magrittr "%>%"
#'
make_map <- function(data = NULL, map = NULL, type = "leaflet", utm33 = TRUE) {
  output <- NULL

  if (utm33) {
    # convert from utm33 to leaflet
    map <- shinymap::utm33_to_leaflet(map = map)
  }

  switch(type,
    leaflet = {
      output <- leaflet::leaflet(map) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          stroke = FALSE,
          smoothFactor = 0.3,
          fillOpacity = 0.2,
          fillColor = c(
            "green", "blue", "red",
            "yellow", "orange", "purple"
          )
        )
    },
    simple = {
      output <- sp::plot(map)
    }
  )
  return(output)
}
