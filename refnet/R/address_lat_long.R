#' Extracts the lat and long for each address
#' 
#' \code{address_lat_long} This function takes the author list output after the output has been synthesized for incorrect author matches. It contains a similarity score cutoff like read_authors. This however is to further constrain the list. New values ARE NOT created, instead it filters by the sim_score column in the output file. An output file is created using the 'root' argument that specifies the folder/file prefix for the output. The final file will be appended with '_final.csv'. 
#' 
#' @param data dataframe from refine_authors
#' @param address_column name of column in quotes where the addresses are
address_lat_long <- function(data=df,  
                             address_column="address", filename_root=""){
  # Read in the CSV data and store it in a variable 
  
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
  
  #origAddress$postal_code[is.na(origAddress$postal_code)]<-''
  
  origAddress$paste_address <- paste(ifelse(is.na(origAddress$country),'',origAddress$country), ifelse(is.na(origAddress$postal_code),'',origAddress$postal_code))
  
  # Initialize the data frame
  #geocoded <- data.frame(stringsAsFactors = FALSE)
  
  uniquead<-data.frame(short_address=unique(origAddress$short_address))
  uniquead$adID<-1:nrow(uniquead)
  uniquead$short_address<-as.character(uniquead$short_address)
  origAddress<-merge(origAddress,uniquead, by='short_address',all.x=T)
  
  uniquead$lat<-NA
  uniquead$lon<-NA
  # Loop through the addresses to get the latitude and longitude of each address and add it to the
  # origAddress data frame in new columns lat and lon
  
  for(i in uniquead$adID){
    #i<-uniquead$adID[1]
    address<-uniquead$short_address[uniquead$adID==i]
    print(paste("Working... ",address))
    
    if(!is.na(address)){
      suppressWarnings(result <- geocode(address, output = "latlona", 
                                         source = "dsk",
                                         messaging = TRUE))
      uniquead$lat[i]<-result$lat
      uniquead$lon[i]<-result$lon
    
    }
    total <- nrow(uniquead)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
  }
  
  faileddsk <- uniquead[is.na(uniquead$lat),c('short_address','adID')]

  for(p in 1:nrow(faileddsk)){
    paste_address<-origAddress$paste_address[faileddsk$adID[p]==origAddress$adID][1]
    result<-geocode(paste_address, 
            output = "latlona", 
            source="google")
    result$adID<-faileddsk$adID[p]
    uniquead$lat[uniquead$adID==result$adID]<-result$lat
    uniquead$lon[uniquead$adID==result$adID]<-result$lon
    }
  
  addresses<-merge(origAddress,uniquead[,c('adID','lat','lon')],by='adID',all.x=T)
  colnames(addresses)
  addresses<-subset(addresses,select=c(authorID,author_name,groupID,author_order,address,university,department,RP_address,RI,OI,UT,refID,postal_code,country,lat,lon,adID))
  
  if(filename_root != "") {
    write.csv(addresses, file=paste(filename_root, 
                                 "_addresses.csv", sep=""), 
              row.names=FALSE)
  }
  
  return(addresses)
}
