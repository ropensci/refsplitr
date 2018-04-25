########################################
########################################
##	BEGIN: plot_addresses_country():
##		

#' Plot addresses, the number of which are summed by country_name
#' 
#' \code{plot_addresses_country} This function plots an addresses data.frame object by country name.
#' 
#' @param addresses output from the extract_country_name() function, containing geocoded address latitude and longitude locations.

plot_addresses_country <- function(data) {
  country_name <- data$country
  
  country_name_table <- as.data.frame(table(country_name))
  
  require(rworldmap)
  
  mapdata <- joinCountryData2Map(country_name_table, joinCode="NAME", nameJoinColumn="country_name", verbose=TRUE)
  
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  mapParams <- mapCountryData( mapdata, nameColumnToPlot="Freq", addLegend=FALSE, mapTitle="Authors Records by Country", catMethod="pretty")
  do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2))
}

##	END: plot_addresses_country():
########################################
########################################
