#' Extracts the country out of a refined author information
#' 
#' \code{extract_country_name} This function takes the author list output after the output has been synthesized for incorrect author matches. It contains a similarity score cutoff like read_authors. This however is to further constrain the list. New values ARE NOT created, instead it filters by the sim_score column in the output file. An output file is created using the 'root' argument that specifies the folder/file prefix for the output. The final file will be appended with '_final.csv'. 
#' 
#' @param data dataframe from refine_authors
#' @param address_column name of column in quotes where the addresses are
extract_country_name <- function(data,
                                 address_column="address"){
  # loads in country names
  data(countries)
  
  # creates an empty column for country names
  data$country <- NA
  
  # pulls out the address column
  ad <- data[,address_column]

  for(i in 1:length(countries)){
    
    # does the country exist in the character string?
    dd = str_detect(ad, countries[i])
    # make it TRUE and FALSE 
    ee = ifelse(is.na(dd),FALSE, dd)
    # if TRUE, put in the country name
    data[ee,"country"] <- countries[i]
    # repeat!! 
  }
  # spit back out the data frame
  return(data)
}
