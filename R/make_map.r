#' Function to make a map
#'
#' @param data Data to be plotted in the map
#' @param map The map itself
#' @param decimal_mark sign to use for decimal
#' @param big_mark sign to use for numbers over a thousand
#'
#' @return a map
#' @export
#' @importFrom magrittr "%>%"
#'
make_map <- function(data = NULL, map = NULL, decimal_mark = ",", big_mark = " ") {

  # convert from utm33 to leaflet
  map_data <- kart::utm33_to_leaflet(map = map, sf = TRUE)
  map_data <- methods::as(map_data, Class = "Spatial")
   # remove area_name from map, since we will get it from `data`
  map_data@data$area_name <- NULL
  # Only keep columns that we want to use
  simple_data <- data[c("area", "value", "area_name", "type", "numerator_name", "numerator")]
  # Join data with map
  map_data@data <- dplyr::left_join(map_data@data, simple_data, by = c("area_num" = "area"))
  pal <- leaflet::colorBin(palette = SKDEr::skde_colors(num = 5),
                           domain = map_data@data$value,
                           bins = 5,
                           pretty = FALSE)
  output <- map_data %>%
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 5, maxZoom = 5)) %>%
    leaflet::addPolygons(weight = 1,
                         color  = "black",
                         fillColor = ~pal(value),
                         fillOpacity = 0.8,
                         label = paste0("<strong>",
                                        map_data@data$area_name,
                                        "</strong><br>",
                                        map_data@data$type,
                                        ": ",
                                        format(map_data@data$value,
                                               big.mark = big_mark, decimal.mark = decimal_mark, digits = 2),
                                        "<br>",
                                        map_data@data$numerator_name,
                                        ": ",
                                        format(map_data@data$numerator,
                                               big.mark = big_mark, decimal.mark = decimal_mark, digits = 2)
                                        ) %>%
                                 lapply(htmltools::HTML),
                         popupOptions = leaflet::popupOptions(closeButton = TRUE),
                         stroke = TRUE,
                         smoothFactor = 1,
                         highlight = leaflet::highlightOptions(weight = 2,
                                                               color = "white",
                                                               bringToFront = TRUE
                                                               )
                         ) %>%
    leaflet::addLegend(position = "bottomright",
                       pal = pal,
                       values = ~value,
                       title = "Antall per 100 000 innbygger",
                       labFormat = leaflet::labelFormat(digits = 0)
                       ) %>%
    leaflet.extras::setMapWidgetStyle(list(background = "white"))
  return(output)
}
