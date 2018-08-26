########################################
########################################
##	BEGIN: refnet_geocode():

#' Uses the Google Maps API to process a data.frame of addresses and attempts to geocode them
#' 
#' \code{refnet_geocode} This function is based on the geoPlot geoAddress() function, and takes an address string and uses the Google Maps API to geocode the address if possible.  Unlike the original function this one saves all of the first Placemark data returned for further use.
#' 
#' @param x references data.frame that should contain three columns, the first of which is assumed to be the AU_ID, the second of which can be a code or description of the type of addresses being processed (e.g. "C1") and the third being the address itself, stripped to something Google Maps can accurately locate.
#' @param verbose=FALSE argument that when set to TRUE will output search strings and output from the Google Maps API call as it proceeds, useful for troubleshooting long-running calls to the function

refnet_geocode <- function (x, key="", verbose=FALSE) {
  ##	x in this case is assumed to be a data.frame object with two 
  ##		columns, the first being the ID to tag the record with, the second
  ##		being a descriptive code (e.g. C1, RP, etc.) and the third being the
  ##		search_string:
  
  columns <- c("AU_ID", "type", "search_string", "status_code",
               "accuracy", "address", "country_name_code", 
               "country_name", "administrative_area", "locality",
               "postal_code", "latitude", "longitude", 
               "box_north", "box_south", "box_east", "box_west")
  
  y <- data.frame(t(rep("", length(columns))), 
                  stringsAsFactors=FALSE)
  y <- y[-1, ]
  colnames(y) <- columns
  
  ##	Pull address info from the x data.frame argument:
  y[1, "AU_ID"] <- x[1]
  y[1, "type"] <- x[2]
  temp00 <- data.frame(lapply(x[3], as.character), 
                       stringsAsFactors = FALSE)
  
  ##	We're replacing it with the new Geocoding API format:
  if (key == "") {
    temp01 <- paste("https://maps.googleapis.com/maps/api/geocode/xml?address=", 
                    gsub(" ", "+", temp00), 
                    "&sensor=false", sep = "", 
                    collapse = NULL)
  } else {
    temp01 <- paste("https://maps.googleapis.com/maps/api/geocode/xml?address=", 
                    gsub(" ", "+", temp00), 
                    "&sensor=false&key=", key, 
                    sep = "", 
                    collapse = NULL)
  }
  
  if (verbose) {
    print(temp01)
    
    flush.console()
  }
  
  y[1, "search_string"] <- paste(temp00, 
                                 collapse = " ")
  
  ##	Check to see if the search_string is all spaces, and if not then 
  ##		do the search:
  if (length(grep("^[ \\n]*$", y[1, "search_string"])) == 0) {
    
    temp02 <- readLines(temp01)
    
    temp03 <- grep("<location>", temp02)[1]
    
    temp04 <- gsub("^.*<lat>(.*)</lat>.*$", "\\1", temp02[temp03+1])
    
    if (!is.na(temp04[1])) {
      
      temp05 <- gsub("^.*<lng>(.*)</lng>.*$", "\\1", temp02[temp03+2])
      
    }	else {
      
      temp05 <- temp04
      
    }
    
    y[1, "latitude"] <- temp04
    
    y[1, "longitude"] <- temp05
    
    search_string <- grep("<status>.*</status>", temp02)
    
    if (length(search_string) > 1) search_string <- min(search_string)
    
    if (length(search_string) == 1) y[1, "status_code"] <- gsub("^.*<status>(.*)</status>.*$", "\\1", temp02[search_string])
    
    search_string <- grep("<partial_match>.*</partial_match>", temp02)
    
    if (length(search_string) > 1) search_string <- max(search_string)
    
    if (length(search_string) == 1) y[1, "accuracy"] <- gsub("^.*<partial_match>(.*)</partial_match>.*$", "\\1", temp02[search_string])
    
    search_string <- grep("<formatted_address>.*</formatted_address>",
                          temp02)
    
    if (length(search_string) > 1) search_string <- max(search_string)
    
    if (length(search_string) == 1) y[1, "address"] <- gsub("^.*<formatted_address>(.*)</formatted_address>.*$", "\\1", temp02[search_string])
    
    search_string <- grep("<type>country</type>", temp02)
    
    if (length(search_string) > 1) search_string <- max(search_string)
    
    if (length(search_string) == 1) y[1, "country_name_code"] <- gsub("^.*<short_name>(.*)</short_name>.*$", "\\1", temp02[search_string-1])
    
    if (length(search_string) == 1) y[1, "country_name"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])
    
    search_string <- grep("<type>administrative_area_level_1</type>", temp02)
    if (length(search_string) > 1) search_string <- max(search_string)
    
    if (length(search_string) == 1) y[1, "administrative_area"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])
    
    search_string <- grep("<type>locality</type>", temp02)
    
    if (length(search_string) > 1) search_string <- max(search_string)
    
    if (length(search_string) == 1) y[1, "locality"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])
    
    search_string <- grep("<type>postal_code</type>", temp02)
    
    if (length(search_string) > 1) search_string <- max(search_string)
    
    if (length(search_string) == 1) y[1, "postal_code"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])
    
    ##	NOTE: I stripped out the code that saves off the viewport and/or 
    ##		bounding box for approximate matches...
  }
  
  return(y)
}

##	END: refnet_geocode():
########################################
########################################
