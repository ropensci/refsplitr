#' Extracts the lat and long for each address
#' 
#' \code{address_lat_long} This function takes the author list output after the output has been synthesized for incorrect author matches. It contains a similarity score cutoff like read_authors. This however is to further constrain the list. New values ARE NOT created, instead it filters by the sim_score column in the output file. An output file is created using the 'root' argument that specifies the folder/file prefix for the output. The final file will be appended with '_final.csv'. 
#' 
#' @param data dataframe from refine_authors
#' @param address_column name of column in quotes where the addresses are
address_lat_long <- function(data=df,  
                             address_column="address"){
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

origAddress$paste_address <- paste(origAddress$country, origAddress$postal_code)
  
  # Initialize the data frame
  geocoded <- data.frame(stringsAsFactors = FALSE)
  
  # Loop through the addresses to get the latitude and longitude of each address and add it to the
  # origAddress data frame in new columns lat and lon
  for(i in 1:nrow(origAddress)){
    print(paste("Working... ",i))
    address <- origAddress[i,"short_address"]
    
    if(!is.na(address)){
      suppressWarnings(result <- geocode(address, output = "latlona", 
                      source = "dsk",
                      messaging = TRUE))
    
    origAddress$lon[i] <- as.numeric(result[1])
    origAddress$lat[i] <- as.numeric(result[2])
    }
    total <- nrow(origAddress)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
  }
  
  faileddsk <- origAddress[is.na(origAddress$lat),]
  
  filled_address <- geocode(faileddsk$paste_address, 
                            output = "latlona", 
                            source="google")
  
  faileddsk$lon <- filled_address$lon
  faileddsk$lat <- filled_address$lat
  
  addresses <- rbind(origAddress, faileddsk)
  
  return(addresses)
}
