library(ggmap)
library(tidyverse)
library(stringr)
library(stringi)

load("./output/eb_refined.Rdata")
world <- map_data("world")

# Read in the CSV data and store it in a variable 
origAddress <- separate(data=eb_refined, col = address,
                        into=c("university","department","short_address"),
                        sep=",",extra = "merge", remove=FALSE) %>%
  mutate(country=stri_extract_last_words(short_address),
         postal_code = str_extract(string=short_address, 
                                   pattern="[:alpha:]{2}[:punct:]{1}[:digit:]{1,8}|[:space:][:upper:][:digit:][:upper:][:space:][:digit:][:upper:][:digit:]|[:alpha:][:punct:][:digit:]{4}")) %>%
  mutate(postal_code = ifelse(is.na(postal_code), str_extract(string=short_address,
                                                              pattern="[:space:][:digit:]{5}"), postal_code)) %>%
  mutate(postal_code = ifelse(is.na(postal_code), str_extract(string=short_address,
                                                              pattern="[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"), postal_code),
         paste_address = paste(country, postal_code))

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress)){
  # Print("Working...")
  address <- origAddress[i,"short_address"]
  result <- geocode(address, output = "latlona", source = "dsk")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  
}

faileddsk <- origAddress[is.na(origAddress$lat),]

filled_address <- geocode(faileddsk$paste_address, output = "latlona", source="google")

faileddsk$lon <- filled_address$lon
faileddsk$lat <- filled_address$lat

addresses <- rbind(origAddress, faileddsk)

ggplot()+
  geom_polygon(data=world, aes(y=lat, x=long, group=group), fill=NA, color="black")+
  geom_point(data=addresses, aes(x=lon,y=lat), color="purple")
