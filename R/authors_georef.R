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
  p.f <- data[, c("university",'city','state', "country", 
                          "postal_code", "authorID", "address")]
  
  p.f$university[is.na(p.f$university)] <- ""
  p.f$country[is.na(p.f$country)] <- ""
  p.f$postal_code[is.na(p.f$postal_code)] <- ""
  p.f$city[is.na(p.f$city)] <- ""
  p.f$state[is.na(p.f$state)] <- ""
  p.f$country<-trimws(p.f$country,which='both')
  p.f$city<-trimws(p.f$city,which='both')
  p.f$state<-trimws(p.f$state,which='both')
  p.f$university<-trimws(p.f$university,which='both')
  # without university
  # create these piece meal
  p.f$base<-p.f$country
  p.f$base[p.f$postal_code!='']<-paste0(p.f$base[p.f$postal_code!=''],', ',
                                        p.f$postal_code[p.f$postal_code!=''])
  
  p.f$base[p.f$state!='']<-paste0(p.f$state[p.f$state!=''],', ',
                                  p.f$country[p.f$state!=''])
  # second tier, city > zip > university
  p.f$second<-NA
  p.f$second[p.f$city!='']<-p.f$city[p.f$city!='']
  p.f$second[is.na(p.f$second) & 
        p.f$university!='']<-p.f$university[is.na(p.f$second) & 
                                              p.f$university!='']
  
  p.f$short_address<-p.f$base
  p.f$short_address[!is.na(p.f$second)]<-paste0(p.f$second[!is.na(p.f$second)],
                                    ', ',p.f$short_address[!is.na(p.f$second)])
  
  uadd <- data.frame(short_address = unique(p.f$short_address), lat = NA, 
                     lon = NA, stringsAsFactors = FALSE)
  uadd <- uadd[!is.na(uadd$short_address) & uadd$short_address!='', ]
  uadd$adID <- nrow(uadd)
  
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
                                DC 20500", source='dsk')), messaging=FALSE)==0
  
  if (!check.open) {
    print("data science toolkit is down right now, moving onto google API")
  }
  if (check.open) {
    for (i in seq_len(nrow(uadd))) {
      address <- as.character(uadd$short_address[i])
      print(paste("Working... ", address))
      
      if (!is.na(address)) {
        suppressWarnings(result <- ggmap::geocode(address,
                                                  output = "latlona",
                                                  source = "dsk",
                                                  messaging = TRUE
        ))
        uadd$lat[i] <- result[[2]]
        uadd$lon[i] <- result[[1]]
      }
      # We can delete the clock if it's just not working visually
      total <- nrow(uadd)
      pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
      utils::setTxtProgressBar(pb, i)
      utils::flush.console()
    }
  }
  
  # Now we'll run the failed addresses into googles api.
  # 1. The keys here are theres a maximum of 2500 querries per day
  # 2. The server often gets overloaded which results in addresses not being run
  # To solve thise problem we will run all addresses, check if there were 
  # some that did not
  # run because the server was busy, and then run those remainders again, 
  # and again, until
  # all addresses we could get locations from we got. We do this with a 
  # while loop to check
  # if the 'OVER_QUERY_LIMIT' warning was retrieved.
  # We have to use a tryCatch to check for warnings because of 
  # scoping/environment rules
  # that store warnings in strange ways I havent completely grasped yet.
  
  retry <- TRUE
  faileddsk <- uadd[is.na(uadd$lat), c("short_address", "adID")]
  counter<-1
  if(nrow(faileddsk)>0){
    uadd$noresult<-FALSE
    while (retry == TRUE) {
      
      warn.list <- list()
      faileddsk <- uadd[is.na(uadd$lat) & uadd$noresult==FALSE, 
                        c("short_address", "adID")]
      for (p in seq_len(nrow(faileddsk))) {
        #p<-1
        paste_address <- uadd$short_address[faileddsk$adID[p] == uadd$adID][1]
        result <- tryCatch(ggmap::geocode(paste_address,
                                          output = "latlona",
                                          source = "google"
        ), warning = function(w) {
          w
        })
        result
        if (!is.data.frame(result)) {
          warn.list[[paste0(p)]] <- result
          if(grepl('ZERO_RESULTS',result)){uadd$noresult[uadd$adID == 
                                                    faileddsk$adID[p]]<-TRUE}
          next
        }
        result$adID <- faileddsk$adID[p]
        uadd$lat[uadd$adID == result$adID] <- result$lat
        uadd$lon[uadd$adID == result$adID] <- result$lon
        
      }
      # check if all addresses ran or were stopped by a sever limit
      retry <- sum(grepl("OVER_QUERY_LIMIT", warn.list)) > 0
      # if (nrow(faileddsk) == 1 & paste_address == " ") {
      #   retry <- FALSE
      # }
      # we need to cehck if the 2500 limit is being reached. 
      # Hopefully this never happens.
      
      if (ggmap::geocodeQueryCheck(userType = "free") == 0) {
        retry <- FALSE
        print("You've run out of server queries today. Max is 2500. 
          Try again tomorrow with a subsetted data set to finish addresses.")
      }
      #if(counter==retry_limit & nrow(faileddsk)>20){break}
      if(counter==retry_limit){break}
      # put system to sleep for 5 seconds to allow googles query limits to reset
      if (retry == TRUE) {
        print("server busy, trying again in 5 seconds")
        Sys.sleep(5)
      }
      
      counter<-counter+1
    }
  }
  # merge results together
  addresses <- merge(uadd, subset(p.f, select = -address), 
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