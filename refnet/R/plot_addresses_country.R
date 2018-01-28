########################################
########################################
##	BEGIN: plot_addresses_country():
##		

#' Plot addresses, the number of which are summed by country_name
#' 
#' \code{plot_addresses_country} This function plots an addresses data.frame object by country name.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.

plot_addresses_country <- function(addresses) {
  ###	Exapmle plot by country (using just reprint author):
  #country_name <- gsub("^.* ([A-z]*)\n$", "\\1", addresses$RP, perl=TRUE)
  country_name <- addresses$country_name
  
  country_name[country_name == "USA"] <- "United States"
  country_name[country_name == "England"] <- "United Kingdom"
  country_name[country_name == "Scotland"] <- "United Kingdom"
  country_name[country_name == "Wales"] <- "United Kingdom"
  country_name[country_name == "The Netherlands"] <- "Netherlands"
  #country_name[country_name == "Zealand"] <- "New Zealand"
  
  country_name_table <- as.data.frame(table(country_name))
  
  #install.packages("rworldmap")
  require(rworldmap)
  
  mapdata <- joinCountryData2Map(country_name_table, joinCode="NAME", nameJoinColumn="country_name", verbose=TRUE)
  
  ##	Don't know why, but in the latest version of mapCountryData() they changed "title" to "mapTitle":
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
  mapParams <- mapCountryData( mapdata, nameColumnToPlot="Freq", addLegend=FALSE, mapTitle="Authors Records by Country", catMethod="pretty")
  do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2))
}

##	END: plot_addresses_country():
########################################
########################################
