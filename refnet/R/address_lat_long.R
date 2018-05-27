#' Extracts the lat and long for each address
#' 
#' \code{address_lat_long} This function takes the final author list from refine_authors, and calculates the lat long of the addresses. It does this by feeding the addresses into data science toolkit and then googles api. The output is a data.frame of all information from refine_authors plus new location columns and calculated lat longs. 
#' 
#' @param data master dataframe from refine_authors
#' @param address_column name of column in quotes where the addresses are
#' @param filename_root the filename root, can include relative or absolute
#' @param write_out_missing TRUE or FALSE, do you want a .csv file written out that has all the entries where we could not determine the lat/long? 
#'   path, to which "_addresses.csv" will be appended and the output from the
#'   function will be saved
#'   
address_lat_long <- function(data,  
                             address_column="address", 
                             filename_root="",
                             write_out_missing=FALSE){
  # Read in the CSV data and store it in a variable 
  # Parse out address information into its respective parts
  origAddress <- separate(data=data, col = address_column,
                          into=c("university","department","short_address"),
                          sep=",",extra = "merge", remove=FALSE) %>%
    mutate(postal_code = str_extract(string=short_address, 
                                     pattern="[:alpha:]{2}[:punct:]{1}[:digit:]{1,8}|[:space:][:upper:][:digit:][:upper:][:space:][:digit:][:upper:][:digit:]|[:alpha:][:punct:][:digit:]{4}")) %>%
    mutate(postal_code = ifelse(is.na(postal_code), 
                                str_extract(string=short_address,
                                            pattern="[:space:][:digit:]{5}"), postal_code)) %>%
    mutate(postal_code = ifelse(is.na(postal_code), 
                                str_extract(string=short_address,
                                            pattern="[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"),
                                postal_code))
  
  
  origAddress <- extract_country_name(origAddress)
  
  #pastes a short address together and replaces NAs with '' for the api search, which doesnt enjoy NAs
  
  origAddress$paste_address <- paste(ifelse(is.na(origAddress$country),'',origAddress$country), ifelse(is.na(origAddress$postal_code),'',origAddress$postal_code))
  
  #create a dataframe of addresses and link them using the 'adID' key.
  uniquead<-data.frame(short_address=unique(origAddress$short_address))
  uniquead$adID<-1:nrow(uniquead)
  uniquead$short_address<-as.character(uniquead$short_address)
  origAddress<-merge(origAddress,uniquead, by='short_address',all.x=T)
  
  #add lat long for later calculation
  uniquead$lat<-NA
  uniquead$lon<-NA
  # Loop through the addresses to get the latitude and longitude of each address and add it to the
  # We're using the data science toolkit first because it has no maximum queery limits. 
  # The remainder of addresses will be thrown into googles api
  
  for(i in uniquead$adID){
    #i<-uniquead$adID[1]
    address<-uniquead$short_address[uniquead$adID==i]
    print(paste("Working... ",address))
    
    if(!is.na(address)){
      suppressWarnings(result <- geocode(address, output = "latlona", 
                                         source = "dsk",
                                         messaging = TRUE))
      uniquead$lat[i]<-result[1]
      uniquead$lon[i]<-result[2]
    
    }
    # We can delete the clock if it's just not working visually
    total <- nrow(uniquead)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
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

  retry<-T
  while(retry==T){
    faileddsk <- uniquead[is.na(uniquead$lat),c('short_address','adID')]
    warn.list<-list()
  for(p in 1:nrow(faileddsk)){
    #p<-1
    paste_address<-origAddress$paste_address[faileddsk$adID[p]==origAddress$adID][1]
    result<-tryCatch(geocode(paste_address, 
            output = "latlona", 
            source="google"),warning=function(w){w})
    if(!is.data.frame(result)){warn.list[[paste0(p)]]<-result;next}
    result$adID<-faileddsk$adID[p]
    uniquead$lat[uniquead$adID==result$adID]<-result$lat
    uniquead$lon[uniquead$adID==result$adID]<-result$lon
    
  }
  # check if all addresses ran or were stopped by a sever limit
  retry<-sum(grepl('OVER_QUERY_LIMIT',warn.list))>0
  if(nrow(faileddsk)==1 & paste_address==' '){retry<-F}
  # we need to cehck if the 2500 limit is being reached. Hopefully this never happens.
  
  if(geocodeQueryCheck(userType = "free")==0){retry<-F;print("You've run out of server queries today. Max is 2500. Try again tomorrow with a subsetted data set to finish addresses.")}
  
  #put system to sleep for 5 seconds to allow googles query limits to reset
  if(retry==T){print('server busy, trying again in 5 seconds');Sys.sleep(5)}
  }
 
  # merge results together
  addresses<-merge(origAddress,uniquead[,c('adID','lat','lon')],by='adID',all.x=T)
  addresses<-subset(addresses,select=c(authorID,author_name,groupID,author_order,address,university,department,RP_address,RI,OI,UT,refID,postal_code,country,lat,lon,adID))

  missingaddresses <- addresses[is.na(addresses$lat),]
  
  if(write_out_missing){

    write.csv(missingaddresses, file="missing_addresses.csv", row.names = FALSE)
  }
  #write out if necessary 
  if(filename_root != "") {
    write.csv(addresses, file=paste(filename_root, 
                                 "_addresses.csv", sep=""), 
              row.names=FALSE)
  }
  
  outputlist <- list()
  
  outputlist$addresses <- addresses
  outputlist$missing_addresses <- missingaddresses
  outputlist$not_missing_addresses <-  addresses[!is.na(addresses$lat),]
  
  return(outputlist)
}
