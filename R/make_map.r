#' Function to make a map
#'
#' @param data Data to be plotted in the map
#' @param map The map itself
#' @param utm33 Convert from UTM 33 to Leaflet projection (default = TRUE)
#'
#' @return a map
#' @export
#' @importFrom magrittr "%>%"
#'
make_map <- function(data = NULL, map = NULL, utm33 = FALSE) {

  output <- NULL

  simple_data <- data[c("area", "value")]
  map_data <- merge(x = map, y = simple_data, by.x = "area_num", by.y = "area")
  map_data$area_num <- NULL
  map_data$area_name <- NULL

  if (utm33) {
    # convert from utm33 to leaflet
    map_data <- kart::utm33_to_leaflet(map = map_data, sf = TRUE)
  }

  output <- graphics::plot(map_data,
                           main = "",
                           breaks = "quantile", nbreaks = 4,
                           pal = shinymap::skde_colors(num = 4)
                 )

  return(output)
}
