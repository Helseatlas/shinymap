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
  
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
  
  simple_data <- data[c("area", "value")]
  map_data <- dplyr::inner_join(map, simple_data, by = c("area_num" = "area"))
  
  #map_data$area_num <- NULL
  #map_data$area_name <- NULL

  if (utm33) {
    # convert from utm33 to leaflet
    map_data <- kart::utm33_to_leaflet(map = map_data, sf = TRUE)
  }

  map_data <- as(map_data, Class = "Spatial")
  pal <- leaflet::colorBin(palette = SKDEr::skde_colors(num=5), domain = map_data@data$value, bins = 5, pretty = FALSE)
  
  output <- 
    
    map_data %>% 
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 5)) %>% 
    addPolygons(weight = 1, 
                color  = "black",
                fillColor = ~pal(value),
                fillOpacity = 0.8,
                label = sprintf("<strong>%s</strong></br> %g", map_data@data$area_name, round(map_data@data$value,0)) %>% lapply(htmltools::HTML) ,
                popupOptions = popupOptions(closeButton = TRUE), stroke = TRUE, smoothFactor = 1,
                highlight = highlightOptions(weight = 2, color = "white", bringToFront = TRUE)) %>% 
    addLegend(position =  "bottomright", pal = pal, values = ~value, title = "Antall per 100 000 innbygger", 
              labFormat = labelFormat(digits = 0)) %>% 
    setMapWidgetStyle(list(background= "white")
    )

  return(output)
}
