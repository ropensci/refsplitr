########################################
########################################
##	BEGIN: plot_addresses_points():

#' Plot address point locations on world map
#' 
#' \code{plot_addresses_points} This function plots an addresses data.frame object by point overlaid on the countries of the world.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.

plot_addresses_points <- function(addresses) {
  ##	Remove any addresses that have NA values:
  addresses <- addresses[!is.na(addresses$longitude) | !is.na(addresses$latitude),]
  
  ##	Convert our lat/long coordinates to a SpatialPointsDataFrame:
  spatial_points <- SpatialPoints(cbind(as.numeric(addresses$longitude), as.numeric(addresses$latitude)))
  spatial_authors <- SpatialPointsDataFrame(spatial_points, addresses)
  
  
  ##	Get the world map from rworldmap package:
  world_map <- getMap()
  
  plot(world_map)
  plot(spatial_points, add=T, col="red")
}

##	END: plot_addresses_points():
########################################
########################################
