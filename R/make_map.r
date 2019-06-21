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
make_map <- function(data = NULL, map = NULL, type = "leaflet", utm33 = FALSE) {
  output <- NULL

  simple <- data[c("area", "value")]
  map_data <- merge(x = map, y = simple, by.x = "bohf_num", by.y = "area")
  map_data$bohf_num <- NULL

  switch(type,
    leaflet = {
      if (utm33) {
        # convert from utm33 to leaflet
        map_data <- kart::utm33_to_leaflet(map = map_data, sf = TRUE)
      }

      pal_fun <- leaflet::colorQuantile(shinymap::skde_colors(num = 4), NULL, n = 4)
      output <- leaflet::leaflet(map_data) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          stroke = FALSE,
          smoothFactor = 0.3,
          fillOpacity = 0.2,
          fillColor = ~pal_fun(value)
        )
    },
    simple = {
      output <- plot(map_data,
                     main = "",
                     breaks = "quantile", nbreaks = 4,
                     pal = shinymap::skde_colors(num = 4)
                     )
    }
  )
  return(output)
}
