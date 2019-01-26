#' Extracts the lat and long for each address
#'
#' \code{authors_georef} This function takes the final author list from 
#' refine_authors, and calculates the lat long of the addresses. 
#' It does this by feeding the addresses into data science toolkit and 
#' then googles api. The output is a data.frame of all information from 
#' refine_authors plus new location columns and calculated lat longs.
#'
#' @param data dataframe from `authors_refine()`
#' @param address_column name of column in quotes where the addresses are
#' @param filename_root the filename root, can include relative or absolute
#' @param write_out_missing TRUE or FALSE, do you want a .csv file written 
#' out that has all the entries where we could not determine the lat/long?
#'   path, to which "_addresses.csv" will be appended and the output from the
#'   function will be saved
#'
#' @param retry_limit the amount of times you want to retry querying the 
#' remaining addresses through the google api. We recommend doing it 
#' multiple times to bypass common errors querying the google api when its busy. 
#' @importFrom dplyr full_join
#' @export authors_georef 
#' 
authors_georef <- function(data,
                           address_column = "address",
                           filename_root = "",
                           write_out_missing = TRUE,
                           retry_limit=10) {
  # Read in the CSV data and store it in a variable
  addresses <- data[, c("university",'city','state', "country", 
                  "postal_code", "authorID", "address")]
  
  addresses$university[is.na(addresses$university)] <- ""
  addresses$country[is.na(addresses$country)] <- ""
  addresses$postal_code[is.na(addresses$postal_code)] <- ""
  addresses$city[is.na(addresses$city)] <- ""
  addresses$state[is.na(addresses$state)] <- ""
  addresses$country<-trimws(addresses$country,which='both')
  addresses$city<-trimws(addresses$city,which='both')
  addresses$state<-trimws(addresses$state,which='both')
  addresses$university<-trimws(addresses$university,which='both')
  # without university
  # create these piece meal
  addresses$base<-addresses$country
  addresses$base[addresses$postal_code!='']<-paste0(addresses$base[addresses$postal_code!=''],', ',
                                        addresses$postal_code[addresses$postal_code!=''])
  
  addresses$base[addresses$state!='']<-paste0(addresses$state[addresses$state!=''],', ',
                                  addresses$country[addresses$state!=''])
  # second tier, city > zip > university
  addresses$second<-NA
  addresses$second[addresses$city!='']<-addresses$city[addresses$city!='']
  addresses$second[is.na(addresses$second) & 
               addresses$university!='']<-addresses$university[is.na(addresses$second) & 
                                                     addresses$university!='']
  
  addresses$short_address<-addresses$base
  addresses$short_address[!is.na(addresses$second)]<-paste0(addresses$second[!is.na(addresses$second)],
                                                ', ',addresses$short_address[!is.na(addresses$second)])
  addresses$lat<-NA
  addresses$lon<-NA
  #addresses <- data.frame(short_address = unique(addresses$short_address), lat = NA, 
  #                   lon = NA, stringsAsFactors = FALSE)
  #addresses <- addresses[!is.na(addresses$short_address) & addresses$short_address!='', ]
  addresses$adID <- seq_len(nrow(addresses))
  
  # add lat long for later calculation
  # Loop through the addresses to get the latitude and longitude of each 
  # address and add it to the
  # We're using the data science toolkit first because it has no maximum 
  # queery limits.
  # The remainder of addresses will be thrown into googles api
  check.open <- NA
  
  
  message("Trying data science toolkit first...")
  # we'll check if data science toolkit is working, by pinging a known address 
  check.open<-sum(is.na(ggmap::geocode("1600 Pennsylvania Ave NW, Washington, 
                                       DC 20500", source='dsk')))==0
  
  if (!check.open) {
    warning("data science toolkit is down right now, please try again later")
    break
  }
  
  #Lets try broad strokes first.
  for (i in addresses$adID) {
    address <- as.character(addresses$short_address[i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
                                              output = "latlona",
                                              source = "dsk",
                                              messaging = TRUE
    ))
    addresses$lat[addresses$adID==i] <- result[[2]]
    addresses$lon[addresses$adID==i] <- result[[1]]
  }
  
  
  # see if you can find lat lons using a shorter code (city, state, country)
  remain<-addresses[is.na(addresses$lat),]
  
  remain$short_address<-ifelse(!(is.na(remain$state)|is.na(remain$country)),paste0(remain$city,', ',remain$state,', ',remain$country),NA)
  remain<-remain[!is.na(remain$short_address),]
  
  for (i in remain$adID) {
    address <- as.character(remain$short_address[remain$adID==i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
                                              output = "latlona",
                                              source = "dsk",
                                              messaging = TRUE
    ))
    addresses$lat[addresses$adID==i] <- result[[2]]
    addresses$lon[addresses$adID==i] <- result[[1]]
  }
  
  
  #######################3
  
  ####################
  # try city, country
  remain<-addresses[is.na(addresses$lat),]
  
  remain$short_address<-ifelse(!(is.na(remain$city)|is.na(remain$country)),paste0(remain$city,', ',remain$country),NA)
  remain<-remain[!is.na(remain$short_address),]
  for (i in remain$adID) {
    address <- as.character(remain$short_address[remain$adID==i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
                                              output = "latlona",
                                              source = "dsk",
                                              messaging = TRUE
    ))
    addresses$lat[addresses$adID==i] <- result[[2]]
    addresses$lon[addresses$adID==i] <- result[[1]]
  }
  
  # try using university, country
  remain<-addresses[is.na(addresses$lat),]
  remain$short_address<-ifelse(!(is.na(remain$university)|is.na(remain$country)),paste0(remain$university,', ',remain$country),NA)
  remain<-remain[!is.na(remain$short_address),]
  for (i in remain$adID) {
    address <- as.character(remain$short_address[remain$adID==i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
                                              output = "latlona",
                                              source = "dsk",
                                              messaging = TRUE
    ))
    addresses$lat[addresses$adID==i] <- result[[2]]
    addresses$lon[addresses$adID==i] <- result[[1]]
  }

  addresses <- merge(addresses[,c('authorID','university','postal_code','country','lat','lon')], 
                     data[,c("authorID","groupID", "author_order", "address", "department", "RP_address",
                    "RI", "OI", "UT", "refID")], by = "authorID", all.y = TRUE)
  
  missingaddresses <- addresses[is.na(addresses$lat), ]
  
  if (write_out_missing) {
    utils::write.csv(missingaddresses, file = "missing_addresses.csv", 
                     row.names = FALSE)
  }
  # write out if necessary
  if (filename_root != "") {
    utils::write.csv(addresses,
                     file = paste(filename_root,
                                  "_addresses.csv",
                                  sep = ""
                     ),
                     row.names = FALSE
    )
  }
  
  #
  addresses$lat<-unlist(addresses$lat)
  addresses$lon<-unlist(addresses$lon)
  outputlist <- list()
  
  outputlist$addresses <- addresses
  outputlist$missing_addresses <- missingaddresses
  outputlist$not_missing_addresses <- addresses[!is.na(addresses$lat), ]
  
  return(outputlist)
}
