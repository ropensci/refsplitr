########################################
########################################
## 	BEGIN: plot_addresses_country():
##

#' Plot addresses, the number of which are summed by country_name
#'
#' \code{plot_addresses_country} This function plots an addresses data.frame object by country name.
#'
#' @param data address element from the output from the `authors_georef()`` function, containing geocoded address latitude and longitude locations.
#' @param mapRegion what portion of the world map to show. possible values include ["world","North America","South America","Australia","Africa","Antarctica","Eurasia"]
#' @export plot_addresses_country 
#' @importFrom rworldmap addMapLegend
#' 
plot_addresses_country <- function(data,
                                   mapRegion = "world") {
  country_name <- data$country 
  
  country_name <- ifelse(country_name=="England","United Kingdom",country_name)
  
  country_name <- ifelse(country_name=="Scotland","United Kingdom",country_name)

  country_name_table <- as.data.frame(table(country_name))

  mapdata <- rworldmap::joinCountryData2Map(country_name_table, joinCode = "NAME", nameJoinColumn = "country_name", verbose = TRUE)

  graphics::par(mai = c(0, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- rworldmap::mapCountryData(mapdata, nameColumnToPlot = "Freq", addLegend = FALSE, mapTitle = "Authors Records by Country", catMethod = "pretty", mapRegion = mapRegion)
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
}

## 	END: plot_addresses_country():
########################################
########################################
