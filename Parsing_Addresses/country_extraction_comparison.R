WOS2018_master<-WOS2018_stitched_authors$master
refnet_address_parsing<-WOS2018_master %>% select(AF,address, university, department, short_address,postal_code, country)
write.csv(refnet_address_parsing, file = "refnet_address_parsing.csv")



# eb_authors__references[eb_authors__references$AU_ID=="Finegan, B_1",]
library(stringr)
library(countrycode)
EB_country_extractor<-WOS2018_master$address

# This removes all the periods in the address
EB_country_extractor<-gsub("\\.(?=\\.*$)", "", EB_country_extractor, perl=TRUE)
# http://stackoverflow.com/questions/12222689/replace-trailing-periods-with-spaces
# \.   # Match a dot
# (?=  # only if followed by
#     \.* # zero or more dots
#     $   # until the end of the string
# )    # End of lookahead assertion.
EB_author.country<-word(EB_country_extractor, start = -1, sep=", ")
EB_author.country<-as.data.frame(EB_author.country)
EB_author.country$EB_author.country <- as.character(EB_author.country$EB_author.country)
EB_author.country$EB_author.country[grepl('USA', EB_author.country$EB_uthor.country)] <- 'USA'
MISSING=subset(EB_author.country, EB_author.country=="")
MISSING


country_comparison<-cbind(refnet_address_parsing,EB_author.country)
names(country_comparison)
country_comparison$match<-country_comparison$country==country_comparison$EB_author.country
summary(country_comparison$match)
write.csv(country_comparison, file = "EB_refnet_country_comparison.csv")