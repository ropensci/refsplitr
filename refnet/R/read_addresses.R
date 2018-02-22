########################################
########################################
##	BEGIN: read_addresses():

#' Processes multiple records of a data.frame of addresses and attempts to geocode them using refnet_geocode()
#' 
#' \code{read_addresses} This function is based on the geoPlot addrListLookup() function, and takes an address string and uses the Google Maps API to geocode the address if possible.  Though the geoPlot package addrListLookup() function works, it seems inconsistent in the way it handles returns, especially in bulk, so I'm going to redefine it adding in a wait period, re-test, and an	ability to simplify the address down to city/state/country.
#' 
#' @param x references data.frame that should contain three columns, the first of which is assumed to be the AU_ID, the second of which can be a code or description of the type of addresses being processed (e.g. "C1") and the third being the address itself, stripped to something Google Maps can accurately locate.
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_addresses.csv" will be appended and the output from the
#'   function will be saved
#' @param verbose=FALSE argument that when set to TRUE will output search strings and output from the Google Maps API call as it proceeds, useful for troubleshooting long-running calls to the function

read_addresses <- function(x, 
                           filename_root="", 
                           key="", 
                           verbose=FALSE) {
  columns <- c("AU_ID", "type", "search_string", "status_code", 
               "accuracy", "address", "country_name_code", 
               "country_name", "administrative_area", "locality", 
               "postal_code", "latitude", "longitude", "box_north", 
               "box_south", "box_east", "box_west")
  
  y <- data.frame(t(rep("", length(columns))), stringsAsFactors=FALSE)
  
  y <- y[-1, ]
  
  colnames(y) <- columns
  
  for (i in 1:nrow(x)) {
    input <- x[i, ]
    
    output <- refnet_geocode(input, 
                             key=key, 
                             verbose=verbose)
    
    counter <- 0
    
    while ((length(grep("^[ \\n]*$", 
                        output$search_string)) == 0) & 
              is.na(output["latitude"]) & counter < 5) {
      ##	Something potentially went wrong so we'll try 5 more times
      ##		before giving up:
      if (verbose) {
        print(output)
        
        flush.console()
      }
      Sys.sleep(0.5)
      
      counter <- counter + 1
      
      output <- refnet_geocode(input, key=key, verbose=verbose)
    }
    
    ##	If it's still a fail, let's try stripping the city, or the first
    ##		item before a comma from the address:
    counter <- 2
    while ((length(grep("^[ \\n]*$", 
                        output$search_string)) == 0) & 
              is.na(output["latitude"]) & counter >= 0) {
      ##	Something potentially went wrong so we'll try 3 more times
      ##		stripping an item each time:
      input$address <- gsub("^[^,]*, (.*)$", "\\1", input$address)
      
      if (verbose) {
        print(input)
        flush.console()
        Sys.sleep(0.5)
      }
      
      counter <- counter - 1
      output <- refnet_geocode(input, key=key, verbose=verbose)
    }
    
    y <- rbind(y, output)
  }
  
  if(filename_root != "") {
    write.csv(y, 
              file=paste(filename_root, "_addresses.csv", sep=""), 
              row.names=FALSE)
  }
  
  return(y)
}

##	END: read_addresses():
########################################
########################################