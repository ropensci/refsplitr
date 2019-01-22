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
  uadd <- data[, c("university",'city','state', "country", 
                  "postal_code", "authorID", "address")]
  
  uadd$university[is.na(uadd$university)] <- ""
  uadd$country[is.na(uadd$country)] <- ""
  uadd$postal_code[is.na(uadd$postal_code)] <- ""
  uadd$city[is.na(uadd$city)] <- ""
  uadd$state[is.na(uadd$state)] <- ""
  uadd$country<-trimws(uadd$country,which='both')
  uadd$city<-trimws(uadd$city,which='both')
  uadd$state<-trimws(uadd$state,which='both')
  uadd$university<-trimws(uadd$university,which='both')
  # without university
  # create these piece meal
  uadd$base<-uadd$country
  uadd$base[uadd$postal_code!='']<-paste0(uadd$base[uadd$postal_code!=''],', ',
                                        uadd$postal_code[uadd$postal_code!=''])
  
  uadd$base[uadd$state!='']<-paste0(uadd$state[uadd$state!=''],', ',
                                  uadd$country[uadd$state!=''])
  # second tier, city > zip > university
  uadd$second<-NA
  uadd$second[uadd$city!='']<-uadd$city[uadd$city!='']
  uadd$second[is.na(uadd$second) & 
               uadd$university!='']<-uadd$university[is.na(uadd$second) & 
                                                     uadd$university!='']
  
  uadd$short_address<-uadd$base
  uadd$short_address[!is.na(uadd$second)]<-paste0(uadd$second[!is.na(uadd$second)],
                                                ', ',uadd$short_address[!is.na(uadd$second)])
  uadd$lat<-NA
  uadd$lon<-NA
  #uadd <- data.frame(short_address = unique(uadd$short_address), lat = NA, 
  #                   lon = NA, stringsAsFactors = FALSE)
  #uadd <- uadd[!is.na(uadd$short_address) & uadd$short_address!='', ]
  uadd$adID <- seq_len(nrow(uadd))
  
  # add lat long for later calculation
  # Loop through the addresses to get the latitude and longitude of each 
  # address and add it to the
  # We're using the data science toolkit first because it has no maximum 
  # queery limits.
  # The remainder of addresses will be thrown into googles api
  check.open <- NA
  
  
  print("Trying data science toolkit first...")
  # we'll check if data science toolkit is working, by pinging a known address 
  check.open<-sum(is.na(ggmap::geocode("1600 Pennsylvania Ave NW, Washington, 
                                       DC 20500", source='dsk')))==0
  
  if (!check.open) {
    warning("data science toolkit is down right now, please try again later")
    break
  }
  
  #Lets try broad strokes first.
  for (i in uadd$adID) {
    address <- as.character(uadd$short_address[i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
                                              output = "latlona",
                                              source = "dsk",
                                              messaging = TRUE
    ))
    uadd$lat[uadd$adID==i] <- result[[2]]
    uadd$lon[uadd$adID==i] <- result[[1]]
  }
  
  
  # see if you can find lat lons using a shorter code (city, state, country)
  remain<-uadd[is.na(uadd$lat),]
  
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
    uadd$lat[uadd$adID==i] <- result[[2]]
    uadd$lon[uadd$adID==i] <- result[[1]]
  }
  
  
  #######################3
  
  ####################
  # try city, country
  remain<-uadd[is.na(uadd$lat),]
  
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
    uadd$lat[uadd$adID==i] <- result[[2]]
    uadd$lon[uadd$adID==i] <- result[[1]]
  }
  
  # try using university, country
  remain<-uadd[is.na(uadd$lat),]
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
    uadd$lat[uadd$adID==i] <- result[[2]]
    uadd$lon[uadd$adID==i] <- result[[1]]
  }
  
  addresses <- merge(uadd, subset(uadd, select = -address), 
                     by = "short_address", all.x = TRUE)
  
  
  addresses <- merge(addresses, subset(data, select = c("authorID",  
                                                        "groupID", "author_order", "address", "department", "RP_address",
                                                        "RI", "OI", "UT", "refID")), by = "authorID", all.x = TRUE)
  
  
  addresses <- subset(addresses, select = c("authorID",  "groupID", 
                                            "author_order", "address", 
                                            "university", "department", "RP_address", "RI", "OI", "UT", "refID",
                                            "postal_code", "country", "lat", "lon"))
  
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
