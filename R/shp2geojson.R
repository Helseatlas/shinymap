#' Function to convert a shapefile to geojson
#'
#' @param shapefile The basename of shapefile to be converted to geojson
#' @param folder The folder where the shapefile is
#' @param geojson The name of the geojson file to be created
#' @param reduce_size Should the file size be reduced?
#' @param amount How much to reduce the file size
#'
#' @export
#'
shp2geojson <- function(shapefile = "eldre",
                        folder = ".",
                        geojson = "eldre_reduced",
                        reduce_size = TRUE,
                        amount = 0.1) {


  # Read shapefile
  original_map <- rgdal::readOGR(dsn = folder, layer = shapefile, verbose = FALSE)

  # Convert to geojson
  geojson_map <- geojsonio::geojson_json(original_map)

  # Reduce file size
  if (reduce_size) {
    geojson_map <- rmapshaper::ms_simplify(geojson_map, keep = amount)
  }

  # Save geojson file to disk
  if (!is.null(geojson)) {
    geojsonio::geojson_write(geojson_map, file = paste0(geojson, ".geojson"))
  }

  return(geojson_map)
}

#' Convert map from UTM 33 projection to epsg:4326 projection
#'
#' @param map map to be converted
#'
#' @return Converted map
#'
#' @export
#'
utm33_to_leaflet <- function(map) {
  # utm33 is "epsg:32633"
  # leaflet is "epsg:4326"

  suppressWarnings(sp::proj4string(map) <- sp::CRS("+init=epsg:32633"))
  new <- sp::CRS("+init=epsg:4326") # WGS 84
  map_transformed <- sp::spTransform(map, new)
  return(map_transformed)
}
