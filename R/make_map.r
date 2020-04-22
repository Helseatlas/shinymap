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
make_map <- function(data = NULL, map = NULL, utm33 = TRUE) {

  output <- NULL
  
  if (utm33) {
    # convert from utm33 to leaflet
    map_data <- kart::utm33_to_leaflet(map = map, sf = TRUE)
  }
  
  map_data <- methods::as(map_data, Class = "Spatial")
  simple_data <- data[c("area", "value")]
  map_data@data <- map_data@data %>% dplyr::left_join(simple_data, by = c("area_num" = "area"))
  pal <- leaflet::colorBin(palette = SKDEr::skde_colors(num=5), domain = map_data@data$value, bins = 5, pretty = FALSE)
  
  output <- 
    
    map_data %>% 
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
    leaflet::addPolygons(weight = 1, 
                color  = "black",
                fillColor = ~pal(value),
                fillOpacity = 0.8,
                label = base::sprintf("<strong>%s</strong></br> %g", map_data@data$area_name, round(map_data@data$value,0)) %>% lapply(htmltools::HTML) ,
                popupOptions = leaflet::popupOptions(closeButton = TRUE), stroke = TRUE, smoothFactor = 1,
                highlight = leaflet::highlightOptions(weight = 2, color = "white", bringToFront = TRUE)) %>% 
    leaflet::addLegend(position =  "bottomright", pal = pal, values = ~value, title = "Antall per 100 000 innbygger", 
              labFormat = leaflet::labelFormat(digits = 0)) %>% 
    leaflet.extras::setMapWidgetStyle(list(background= "white")
    )

  return(output)
}
