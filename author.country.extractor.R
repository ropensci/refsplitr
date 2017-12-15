#this is to help pull out the country from author addresses and convert it to the ISO3 standard code using library(countrycode)
str(ecuador2_authors__references)
# eb_authors__references[eb_authors__references$AU_ID=="Finegan, B_1",]
library(stringr)
library(countrycode)
ecuador2_authors__references$C1
# This removes all the periods in the address
ecuador2_authors__references$C1<-gsub("\\.(?=\\.*$)", "", ecuador2_authors__references$C1, perl=TRUE)
# Here is how that works
# http://stackoverflow.com/questions/12222689/replace-trailing-periods-with-spaces
# \.   # Match a dot
# (?=  # only if followed by
#     \.* # zero or more dots
#     $   # until the end of the string
# )    # End of lookahead assertion.

# Now trying to extract the country name from the address
# Country is usually at the very end of the address, and often sep by a comma, so if we can split the address
# by commas and then select the last word most of the time that could be the country
author.country<-word(ecuador2_authors__references$C1, start = -1, sep=", ") # split by commas, save last word ("start - 1")
author.country<-as.data.frame(author.country) # convert to df
author.country$author.country <- as.character(author.country$author.country) #convert to character
# The USA is often included as "USA", so not registered as a word. In addition it is mixed with state and zip, because
# these are often not separated by comma.  The following finds any string
# that has "USA" in it (eg "FL 32653 USA") and converts it to "USA"
author.country$author.country[grepl('USA', author.country$author.country)] <- 'USA'
#ID those missing country. Note that is the dataset PRIOR to disambiguation, so there are some blanks 
# that are because some authors (e.g. EB_1 and EB_7) haven't been merged
MISSING=subset(author.country, author.country=="") 
MISSING


# Chnage countries as needed, either because country names have changed or to reflect political organization
# this is for library(countrycode). For instance, it doesn't recognize Scotland or Wales and assign UK, so need to do manually 
# also note that Northern Ireland gets converted to  IRL (Ireland)! No bueno.



author.country[author.country == "Scotland"]  <- "UK" #With apologies to Scots everywhere
# author.country[author.country == "SCOTLAND"]  <- "UK" #With apologies to Scots everywhere
author.country[author.country == "Wales"]  <- "UK"
author.country[author.country == "England"]  <- "UK"
author.country[author.country == "German Democratic Republic"]  <- "Germany" #removing old names
author.country[author.country == "US"]  <- "USA" #in case any snuck in
author.country[author.country == "Yugoslavia"]  <- "Croatia" #Authors from Pretinac
author.country[author.country == "French Guiana"]  <- "France" 



# WOS DATA COME CAPITALIZED, THIS CORRECTS SOME OF THE ONES THAT WOS RETURNS WHEN SEARCHING
# Would be worth converting all countries to lower case to ensure no problems due to capitalization

author.country[author.country == "BOPHUTHATSWANA"]  <- "South Africa"
author.country[author.country == "BYELARUS"]  <- "Belarus"
author.country[author.country == "CISKEI"]  <- "South Africa"
author.country[author.country == "ENGLAND"]  <- "UK"
author.country[author.country == "YEMEN ARAB REP"]  <- "Yemen"
author.country[author.country == "WALES"]  <- "UK"

author.country[author.country == "TRANSKEI"]  <- "South Africa"
author.country[author.country == "NETH ANTILLES"]  <- "Netherland Antilles"
author.country[author.country == "MONGOL PEO REP"]  <- "Mongolia"
author.country[author.country == "FR POLYNESIA"]  <- "French Polynesia"
author.country[author.country == "FED REP GER"]  <- "Germany"
author.country[author.country == "GER DEM REP"]  <- "Germany"  
author.country[author.country == "GUINEA BISSAU"]  <- "GUINEA-BISSAU"  
author.country[author.country == "PAPUA N GUINEA"]  <- "PNG"  
author.country[author.country == "W IND ASSOC ST"]  <- NA

# take each country name, and using countrycode will give you the iso3 code for it
# By setting "warn=TRUE" it will tell you which ones it couldn't convert because of spelling mistakes, changes in country name, etc.
author.country$geo.code<-countrycode(author.country$author.country, "country.name", "iso3c", warn = TRUE)

