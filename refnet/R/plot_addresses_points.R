########################################
########################################
##	BEGIN: plot_addresses_points():

#' Plot address point locations on world map
#' 
#' \code{plot_addresses_points} This function plots an addresses data.frame object by point overlaid on the countries of the world.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.

plot_addresses_points <- function(data,
                                  mapRegion="world") {
  ##	Remove any addresses that have NA values:
  data <- data[!is.na(data$lon) | !is.na(data$lat),]
  
  ##	Convert our lat/long coordinates to a SpatialPointsDataFrame:
  spatial_points <- SpatialPoints(cbind(as.numeric(data$lon), as.numeric(data$lat)))
  spatial_authors <- SpatialPointsDataFrame(spatial_points, data)
  
  
  ##	Get the world map from rworldmap package:
  world_map <- getMap()
  
  
  if(mapRegion !="world"){
    world_map <- world_map[which(world_map$continent==mapRegion),]
  }
  
  plot(world_map)
  plot(spatial_points, add=T, col="red")
}

##	END: plot_addresses_points():
########################################
########################################
