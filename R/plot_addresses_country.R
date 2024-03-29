#' Plot addresses, the number of which are summed by country_name
#'
#' This function plots an addresses data.frame 
#' object by country name.
#'
#' @param data address element from the output from the `authors_georef()` 
#' function, containing geocoded address latitude and longitude locations.
#' @param mapRegion what portion of the world map to show. possible values 
#' include `"world"`, `"North America"`, `"South America"`, `"Australia"`,
#' `"Africa"`, `"Antarctica"`, and `"Eurasia"`
#' 
#' @examples 
#' 
#' ## Using the output of authors_georef (e.g., BITR_geocode)
#' data(BITR_geocode)
#' ## Plots the whole world
#' plot_addresses_country(BITR_geocode)
#' 
#' ## Just select North America
#' plot_addresses_country(BITR_geocode, mapRegion = 'North America')
#' 
#' @export plot_addresses_country 
#' @importFrom rworldmap addMapLegend
#' 
plot_addresses_country <- function(data,
  mapRegion = "world") {
  country_name <- data$country

  country_name <- ifelse(
    country_name == "england",
    "united kingdom",
    country_name
  )

  country_name <- ifelse(
    country_name == "scotland",
    "united kingdom",
    country_name
  )

  country_name <- ifelse(
    country_name == "wales",
    "united kingdom",
    country_name
  )
  
  country_name <- ifelse(
    country_name == "north ireland",
    "united kingdom",
    country_name
  )
  
  
  country_name_table <- as.data.frame(table(country_name))

  mapdata <- rworldmap::joinCountryData2Map(country_name_table,
    joinCode = "NAME", nameJoinColumn = "country_name", verbose = TRUE)

  graphics::par(mai = c(0, 0, 0.2, 0), xaxs = "i", yaxs = "i")
  mapParams <- rworldmap::mapCountryData(mapdata, nameColumnToPlot = "Freq",
    addLegend = FALSE, mapTitle = "Authors Records by Country",
    catMethod = "pretty", mapRegion = mapRegion)
  do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
}
