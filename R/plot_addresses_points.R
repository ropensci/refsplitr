########################################
########################################
## 	BEGIN: plot_addresses_points():

#' Plot address point locations on world map
#'
#' \code{plot_addresses_points} This function plots an addresses data.frame 
#' object by point overlaid on the countries of the world.
#'
#' @param data the `address` element from the list outputted from the `authors_georef()`` function, containing geocoded address latitude and longitude locations.
#' @param mapRegion what portion of the world map to show. possible values include ["world","North America","South America","Australia","Africa","Antarctica","Eurasia"]
#' @export plot_addresses_points 
#' 
plot_addresses_points <- function(data,
                                  mapRegion = "world") {


  
  ## 	Remove any addresses that have NA values:
  points <- data.frame(lat=as.numeric(as.character(data$lat)),
                       lon=as.numeric(as.character(data$lon)))
  
  
  points <- points[!is.na(points$lat), ]

  points <- points[!is.na(points$lon), ]



  ## 	Get the world map from rworldmap package:
  world <- ggplot2::map_data("world")
  world <- world[world$region != "Antarctica", ]


  if (mapRegion != "world") {
    world <- world[which(world$continent == mapRegion), ]
  }


  ggplot2::ggplot() +
    ggplot2::geom_map(
      data = world, map = world,
      ggplot2::aes_string(map_id = "region"),
      color = "gray", fill = "#7f7f7f", size = 0.05, alpha = 1 / 4
    ) +
    ggplot2::geom_point(data = points, ggplot2::aes_string(x = "lon",
                                                           y = "lat")) +
    ggplot2::coord_map(ylim=c(-60,80), xlim=c(-185, 185))+
    ggplot2::ylab("longitude") +
    ggplot2::xlab("latitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text=ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
}

## 	END: plot_addresses_points():
########################################
########################################
