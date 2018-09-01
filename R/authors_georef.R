#' Extracts the lat and long for each address
#'
#' \code{authors_georef} This function takes the final author list from refine_authors, and calculates the lat long of the addresses. It does this by feeding the addresses into data science toolkit and then googles api. The output is a data.frame of all information from refine_authors plus new location columns and calculated lat longs.
#'
#' @param data dataframe from `authors_refine()`
#' @param address_column name of column in quotes where the addresses are
#' @param filename_root the filename root, can include relative or absolute
#' @param write_out_missing TRUE or FALSE, do you want a .csv file written out that has all the entries where we could not determine the lat/long?
#'   path, to which "_addresses.csv" will be appended and the output from the
#'   function will be saved
#'
authors_georef <- function(data,
                           address_column = "address",
                           filename_root = "",
                           write_out_missing = FALSE) {
  # Read in the CSV data and store it in a variable
  paste.frame <- data[, c("university", "country", "postal_code", "authorID", "address")]
  paste.frame$university[is.na(paste.frame$university)] <- ""
  paste.frame$country[is.na(paste.frame$country)] <- ""
  paste.frame$postal_code[is.na(paste.frame$postal_code)] <- ""
  # without university
  paste.frame$short_address[paste.frame$address != "Could not be extracted"] <- paste0(paste.frame$city[paste.frame$address != "Could not be extracted"], ", ", paste.frame$state[paste.frame$address != "Could not be extracted"], ", ", paste.frame$postal_code[paste.frame$address != "Could not be extracted"], " ", paste.frame$country[paste.frame$address != "Could not be extracted"])

  paste.frame$short_address <- gsub(", ,|,,|,  ,", ", ", paste.frame$short_address)
  paste.frame$short_address <- trimws(paste.frame$short_address, which = "both")
  paste.frame$short_address <- as.character(paste.frame$short_address)
  uniqueAddress <- data.frame(short_address = unique(paste.frame$short_address), lat = NA, lon = NA, stringsAsFactors = F)
  uniqueAddress <- uniqueAddress[!is.na(uniqueAddress), ]
  uniqueAddress$adID <- 1:nrow(uniqueAddress)

  # add lat long for later calculation
  # Loop through the addresses to get the latitude and longitude of each address and add it to the
  # We're using the data science toolkit first because it has no maximum queery limits.
  # The remainder of addresses will be thrown into googles api
  check.open <- NA
  print("Trying data science toolkit first...")
  ping <- function(x, stderr = FALSE, stdout = FALSE, ...){
  pingvec <- system2("ping", x, stderr = FALSE,stdout = FALSE,...)
  if (pingvec == 0) TRUE else FALSE}
    
check.open<-suppressWarnings(ping('www.datasciencetoolkit.org'))

  if (!check.open) {
    print("data science toolkit is down right now, moving onto google API")
  }
  if (check.open) {
    for (i in 1:nrow(uniqueAddress)) {
      address <- as.character(uniqueAddress$short_address[i])
      print(paste("Working... ", address))

      if (!is.na(address)) {
        suppressWarnings(result <- ggmap::geocode(address,
          output = "latlona",
          source = "dsk",
          messaging = TRUE
        ))
        uniqueAddress$lat[i] <- result[1]
        uniqueAddress$lon[i] <- result[2]
        # if(is.na(result[1])){error.counter<-error.counter+1}else{error.counter<-1}
        # if(error.counter==10){break;print('Data science tooklit seems to be down or your internet is out, trying google api')}
      }
      # We can delete the clock if it's just not working visually
      total <- nrow(uniqueAddress)
      pb <- txtProgressBar(min = 0, max = total, style = 3)
      setTxtProgressBar(pb, i)
      flush.console()
    }
  }

  # Now we'll run the failed addresses into googles api.
  # 1. The keys here are theres a maximum of 2500 querries per day
  # 2. The server often gets overloaded which results in addresses not being run
  # To solve thise problem we will run all addresses, check if there were some that did not
  # run because the server was busy, and then run those remainders again, and again, until
  # all addresses we could get locations from we got. We do this with a while loop to check
  # if the 'OVER_QUERY_LIMIT' warning was retrieved.
  # We have to use a tryCatch to check for warnings because of scoping/environment rules
  # that store warnings in strange ways I havent completely grasped yet.

  retry <- T
  while (retry == T) {
    faileddsk <- uniqueAddress[is.na(uniqueAddress$lat), c("short_address", "adID")]
    warn.list <- list()
    for (p in 1:nrow(faileddsk)) {
      # p<-1
      paste_address <- uniqueAddress$short_address[faileddsk$adID[p] == uniqueAddress$adID][1]
      result <- tryCatch(ggmap::geocode(paste_address,
        output = "latlona",
        source = "google"
      ), warning = function(w) {
        w
      })
      if (!is.data.frame(result)) {
        warn.list[[paste0(p)]] <- result
        next
      }
      result$adID <- faileddsk$adID[p]
      uniqueAddress$lat[uniqueAddress$adID == result$adID] <- result$lat
      uniqueAddress$lon[uniqueAddress$adID == result$adID] <- result$lon
    }
    # check if all addresses ran or were stopped by a sever limit
    retry <- sum(grepl("OVER_QUERY_LIMIT", warn.list)) > 0
    if (nrow(faileddsk) == 1 & paste_address == " ") {
      retry <- F
    }
    # we need to cehck if the 2500 limit is being reached. Hopefully this never happens.

    if (ggmap::geocodeQueryCheck(userType = "free") == 0) {
      retry <- F
      print("You've run out of server queries today. Max is 2500. Try again tomorrow with a subsetted data set to finish addresses.")
    }

    # put system to sleep for 5 seconds to allow googles query limits to reset
    if (retry == T) {
      print("server busy, trying again in 5 seconds")
      Sys.sleep(5)
    }
  }

  # merge results together
  addresses <- merge(uniqueAddress, subset(paste.frame, select = -address), by = "short_address", all.x = T)
  
  
  addresses <- merge(addresses, subset(data, select = c("authorID",  "groupID", "author_order", "address", "department", "RP_address", "RI", "OI", "UT", "refID")), by = "authorID", all.x = T)
  
  
  addresses <- subset(addresses, select = c("authorID",  "groupID", "author_order", "address", "university", "department", "RP_address", "RI", "OI", "UT", "refID", "postal_code", "country", "lat", "lon"))

  missingaddresses <- addresses[is.na(addresses$lat), ]

  if (write_out_missing) {
    write.csv(missingaddresses, file = "missing_addresses.csv", row.names = FALSE)
  }
  # write out if necessary
  if (filename_root != "") {
    write.csv(addresses,
      file = paste(filename_root,
        "_addresses.csv",
        sep = ""
      ),
      row.names = FALSE
    )
  }

  outputlist <- list()

  outputlist$addresses <- addresses
  outputlist$missing_addresses <- missingaddresses
  outputlist$not_missing_addresses <- addresses[!is.na(addresses$lat), ]

  return(outputlist)
}
