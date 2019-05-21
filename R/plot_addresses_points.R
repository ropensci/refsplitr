#' Plot address point locations on world map
#'
#' This function plots an addresses data.frame 
#' object by point overlaid on the countries of the world.
#'
#' @param data the `address` element from the list outputted from the `authors_georef()`` function, containing geocoded address latitude and longitude locations.
#' @param mapCountry What country to map. Possible values include ["USA", "Brazil", "Australia", "UK"]
#' use \code{data(countries) to see possible names}. No value defaults to the world map.
#' 
#' @examples
#' data(BITR_geocode)
#' 
#' ## Plots the whole world
#' plot_addresses_points(BITR_geocode)
#' 
#' ## mapCountry names can be querried using:
#' data(countries)
#' 
#' ## Just select the United States
#' plot_addresses_points(BITR_geocode, mapCountry = 'USA')
#' 
#' @export plot_addresses_points 
#' 
plot_addresses_points <- function(data,
                                  mapCountry = NULL) {

  ## 	Get the world map from rworldmap package:
  world <- ggplot2::map_data("world")
  world <- world[world$region != "Antarctica", ]

  ## Filter by country
  if (!is.null(mapCountry)){
  data <- data[data$country == tolower(mapCountry), ]
  world <- world[which(world$region == mapCountry), ]
  }
  ## Remove any addresses that have NA values:
  points <- data.frame(lat=as.numeric(as.character(data$lat)),
                       lon=as.numeric(as.character(data$lon)))
  points <- points[!is.na(points$lat), ]
  points <- points[!is.na(points$lon), ]

  ## calculate min and max for plot
  latmin <- min(world$lat) - 2
  latmax <- max(world$lat) + 2
  longmin <- min(world$long) - 2
  longmax <- max(world$long) + 2
  
  if( !is.null(mapCountry) && mapCountry == 'USA') {
    longmax <- max(world$long[world$long < 0]) + 2
  }
  ## Plot
  ggplot2::ggplot() +
    ggplot2::geom_map(
      data = world, map = world,
      ggplot2::aes_string(map_id = "region"),
      color = "gray", fill = "#7f7f7f", size = 0.05, alpha = 1 / 4
    ) +
    ggplot2::geom_point(data = points, ggplot2::aes_string(x = "lon",
                                                           y = "lat")) +
    ggplot2::coord_map(ylim=c(latmin, latmax),
      xlim=c(longmin, longmax))+
    ggplot2::ylab("latitude") +
    ggplot2::xlab("longitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      #axis.text=ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

}