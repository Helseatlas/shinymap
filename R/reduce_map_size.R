#' Function to reduce size of 
#'
#' @param shapefile 
#' @param folder 
#' @param new_shapefile 
#'
#' @return
#' @export
#'
#' @examples
reduce_map_size <- function(shapefile = "eldre", 
                            folder = "tests/testthat/data/maps", 
                            new_shapefile = "eldre2",
                            amount = 1000,
                            overwrite = FALSE){

  # Reduser kart-størrelse
  original_map <- rgdal::readOGR(dsn = folder, layer = shapefile)
  reduced_map <- rgeos::gSimplify(original_map, tol = amount)
  reduced_map <- as(reduced_map, "SpatialPolygonsDataFrame")

  # Read shapefile attributes
  df = data.frame(original_map)
  
  # Keep shp attributes
  reduced_map <- sp::SpatialPolygonsDataFrame(reduced_map, df)
  rgdal::writeOGR(reduced_map,dsn = folder, layer = new_shapefile, driver="ESRI Shapefile", overwrite_layer=overwrite)
  return(reduced_map)
}
