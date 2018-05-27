# SOME IDEAS ON HOW TO PROCESS ADDRESSES TO EXTRACT CITIES, ZIP CODES, and COUNTRIES
# 
# things to think about 
# 1) convert all names to lower case
# 2) remove accents, umlauts, apostrophes, hyphens, etc
# 3) not all zip codes are continuous seq of numbers - some have spaces
# 4) deleting spaces may make it easier to string match because it avoides issues with both postal codes and cities with multiple words in name 
# 5) watch out for abbreviations of things like St. (Saint), Mt. (Mount), Cd. (Ciudad)
# 6) Here is a wikipedia page of global zip code formats: perhaps use this to search for strings or standardize once you know the author's country: https://en.wikipedia.org/wiki/List_of_postal_codes
# 7) There is a package of US zip codes. Anything similar for outher countries? https://cran.r-project.org/web/packages/zipcode/zipcode.pdf
# 8) states / provicnes someimes as abbreviations (AM), sometimes spelled out (Amazonas). Is there a databsase of subnational units?     
# 9) some places don't have a state Washington DC, Mexico DF
# 10) some countries aren't the ISO standard used in mapping so need to convert with custome library for package "country codes" (i.e., Scotland needs to be converted to GBR) 


# POTENTIAL STEPS FOR PROCESSING
# 1) Convert to lower case
# 2) remove accents, hyphens, etc.
# 3) standardize abbreviations
# 4) remove spaces 
# 5) string match countries
# 6) stirng match states
# 6) string match cities 
# 7) find string that matches structure of zip code (i.e., seq of numbers and letters)

#https://stackoverflow.com/questions/5318076/extracting-country-name-from-author-affiliations
library(maps)
library(tidyverse)
library(zipcode)

# the R package "maps" includes a database (primarily) of world cities of population greater than ~40,000.  
#Also included are capital cities of any population size, and many smaller towns
data(world.cities)  # Load data from package maps
cities<-world.cities$name #vector of the cities in the database
cities<-as.character(tolower(cities)) #convert all to lower case
countries<-world.cities$country.etc  #vector of the countries in which these cities are found 
countries<-as.character(tolower(countries)) #make all letters lower case

# Create dataset on which to practice. It's not very efficient, though, and I quit working on it because I didn't really need it.  
AU_address <- c(
  "Mechanical and Production Engineering Department, National University of SINGAPORE",
  "Cancer Research Campaign Mammalian Cell DNA Repair Group, Department of Zoology, Cambridge, U.K.",
  "Cancer Research Campaign Mammalian Cell DNA Repair Group, Department of Zoology, Cambridge, UK.",
  "Lilly Research Laboratories, Eli Lilly and Company, Indianapolis, IN 46285.")


# Remove punctuation from data
cAU_address<-gsub(pattern="[[:punct:]]", AU_address, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
sAU_address <- strsplit(cAU_address, " ")

# Match on cities in world.cities
# Assumes that if multiple matches, the last takes precedence, i.e. max()
lapply(sAU_address, function(x)x[max(which(x %in% world.cities$name))])

# Match on country in world.countries
lapply(sAU_address, function(x)x[which(x %in% world.cities$country.etc)])

# Assumes that if multiple matches, the last takes precedence, i.e. max()
author_city<-lapply(res, function(x)x[max(which(x %in% cities))])
author_city1<-lapply(author_city, `[`, 1)
unlist(author_city1)
# Match on country in world.countries
author_country<-lapply(sAU_address, function(x)x[which(x %in% countries)])
# select first from list
author_country1<-lapply(author_country, `[`, 1)
unlist(author_country1)

# Match on zipcode in package zipcodes
data(zipcode)
# zipcode$zip
author_zip<-lapply(sAU_address, function(x)x[which(x %in% zipcode$zip)])
# select first from list
author_zip1<-lapply(author_zip, `[`, 1)
unlist(author_zip1)

foo<-cbind(author_city1,author_zip1,author_country1)
foo<-as.data.frame(foo)
foo$clean_zip<-clean.zipcodes(foo$author_zip1)
zipcode$zip<-as.numeric(zipcode$zip)
semi_join(foo,zipcode,by="clean_zip")














### AND SOME OTHER CODE


# Remove "." at end of each 
AU_address<-gsub("[.]", AU_address, replacement="") #might need to leave a space as replacenetb
#convert to lower case
AU_address<-as.character(tolower(AU_address))
# replace all of washington,dc with washington DC
AU_address<-gsub("washington,dc", AU_address, replacement="washington dc,dc") #might need to leave a space as replacenetb
AU_address<-gsub("england", AU_address, replacement="uk") #might need to leave a space as replacenetb


# Split data at commas
sAU_address <- strsplit(AU_address, ",")
library(stringi)
res <- as.data.frame(t(stri_list2matrix(sAU_address)))
str(res)
colnames(res) <- c("inst", "dept", "mix1","mix2")  #,"mix3"),"mix4","mix5","mix6","mix7","mix8")
# str(res)
# str(cities)
# str(countries)
# which(countries=="brazil")
countries<-unique(countries)
# Match on cities in world.cities
# foo<-res$mix1 %in% cities #tells whihc have match in cities and which don't'
# foo2<-which(res$mix2 %in% cities) # tells number of vector in res that has a real city
# foo3<-res$mix2[which(res$mix2 %in% cities)] # gives the name of the city
cities.df<-as.data.frame(cities)

res<-slice(res,1:100)

city_mixl<-lapply(res$mix1, function(x)x[max(which(x %in% cities))])
city_mixl<-unlist(city_mixl)
res$citymix1<-city_mixl

city_mix2<-lapply(res$mix2, function(x)x[max(which(x %in% cities))])
city_mix2<-unlist(city_mix2)
res$citymix2<-city_mix2

city_mix3<-lapply(res$mix3, function(x)x[max(which(x %in% cities))])
city_mix3<-unlist(city_mix3)
res$citymix3<-city_mix3

city_mix4<-lapply(res$mix4, function(x)x[max(which(x %in% cities))])
city_mix4<-unlist(city_mix4)
res$citymix4<-city_mix4

city_mix5<-lapply(res$mix5, function(x)x[max(which(x %in% cities))])
city_mix5<-unlist(city_mix5)
res$citymix5<-city_mix5

city_mix6<-lapply(res$mix6, function(x)x[max(which(x %in% cities))])
city_mix6<-unlist(city_mix6)
res$citymix6<-city_mix6

city_mix7<-lapply(res$mix7, function(x)x[max(which(x %in% cities))])
city_mix7<-unlist(city_mix7)
res$citymix7<-city_mix7

city_mix8<-lapply(res$mix8, function(x)x[max(which(x %in% cities))])
city_mix8<-unlist(city_mix8)
res$citymix8<-city_mix8





# MERGE UP THESE THREE COLUMNS

res = mutate(res, CITY = paste(as.character(citymix1), as.character(city_mix2),as.character(city_mix3),as.character(city_mix4),as.character(city_mix5),as.character(city_mix6),as.character(city_mix7),as.character(city_mix8), sep = ' '))
res$CITY<-gsub("NA", res$CITY, replacement="") #might need to leave a space as replacenetb
trim.trailing <- function (x) sub("\\s+$", "", x)
res$CITY<-trim.trailing(res$CITY)
trim.leading <- function (x)  sub("^\\s+", "", x)
res$CITY<-trim.leading(res$CITY)
res$CITY<-as.factor(res$CITY)



country_mixl<-lappply(res$mix1, function(x)x[max(which(x %in% countries))])
country_mixl<-unlist(country_mixl)
res$countrymix1<-country_mixl

country_mix2<-lappply(res$mix2, function(x)x[max(which(x %in% countries))])
country_mix2<-unlist(country_mix2)
res$countrymix2<-country_mix2

country_mix3<-lappply(res$mix3, function(x)x[max(which(x %in% countries))])
country_mix3<-unlist(country_mix3)
res$countrymix3<-country_mix3



country_mix4<-lappply(res$mix4, function(x)x[max(which(x %in% cities))])
country_mix4<-unlist(country_mix4)
res$countrymix4<-country_mix4

country_mix5<-lappply(res$mix5, function(x)x[max(which(x %in% cities))])
country_mix5<-unlist(country_mix5)
res$countrymix5<-country_mix5

country_mix6<-lappply(res$mix6, function(x)x[max(which(x %in% cities))])
country_mix6<-unlist(country_mix6)
res$countrymix6<-country_mix6

country_mix7<-lappply(res$mix7, function(x)x[max(which(x %in% cities))])
country_mix7<-unlist(country_mix7)
res$countrymix7<-country_mix7

country_mix8<-lappply(res$mix8, function(x)x[max(which(x %in% cities))])
country_mix8<-unlist(country_mix8)
res$countrymix8<-country_mix8



# MERGE UP THESE THREE COLUMNS
res = mutate(res, COUNTRY = paste(as.character(countrymix1), as.character(countrymix2),as.character(countrymix3),as.character(countrymix4),as.character(countrymix5),as.character(countrymix6),as.character(countrymix7),as.character(countrymix8), sep = ' '))
res$COUNTRY<-gsub("NA", res$COUNTRY, replacement="") #might need to leave a space as replacenetb
trim.trailing <- function (x) sub("\\s+$", "", x)
res$COUNTRY<-trim.trailing(res$COUNTRY)
trim.leading <- function (x)  sub("^\\s+", "", x)
res$COUNTRY<-trim.leading(res$COUNTRY)
res$COUNTRY<-as.factor(res$COUNTRY)

#ZIP IS A LITTLE MORE COMPLICATED> NEED TO HAVE ZIP ALL BY ITSELF
# column 1
zip_mix1<-res$mix1
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix1, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")

data(zipcode)
# zipcode$zip
author_zip<-lapply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip1<-lapply(author_zip, `[`, 1)
res$zip1<-as.character(author_zip1)

# column 2
zip_mix2<-res$mix2
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix2, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip2<-lapply(author_zip, `[`, 1)
res$zip2<-as.character(author_zip2)


# column 3
zip_mix3<-res$mix3
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix3, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip3<-lapply(author_zip, `[`, 1)
res$zip3<-as.character(author_zip3)





#ZIP IS A LITTLE MORE COMPLICATED> NEED TO HAVE ZIP ALL BY ITSELF
# column 4
zip_mix4<-res$mix4
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix4, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")

data(zipcode)
# zipcode$zip
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip4<-lapply(author_zip, `[`, 1)
res$zip4<-as.character(author_zip4)



#ZIP IS A LITTLE MORE COMPLICATED> NEED TO HAVE ZIP ALL BY ITSELF
# column 5
zip_mix5<-res$mix5
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix5, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")

data(zipcode)
# zipcode$zip
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip5<-lapply(author_zip, `[`, 1)
res$zip5<-as.character(author_zip5)




#ZIP IS A LITTLE MORE COMPLICATED> NEED TO HAVE ZIP ALL BY ITSELF
# column 6
zip_mix6<-res$mix6
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix6, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")

data(zipcode)
# zipcode$zip
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip6<-lapply(author_zip, `[`, 1)
res$zip6<-as.character(author_zip6)


#ZIP IS A LITTLE MORE COMPLICATED> NEED TO HAVE ZIP ALL BY ITSELF
# column 6
zip_mix7<-res$mix7
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix7, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")

data(zipcode)
# zipcode$zip
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip7<-lapply(author_zip, `[`, 1)
res$zip7<-as.character(author_zip7)


#ZIP IS A LITTLE MORE COMPLICATED> NEED TO HAVE ZIP ALL BY ITSELF
# column8
zip_mix8<-res$mix8
# Remove punctuation from data
foo<-gsub(pattern="[[:punct:]]", zip_mix8, replacement=" ") #might need to leave a space as replacenetb

# Split data at word boundaries
foo <- strsplit(foo, " ")

data(zipcode)
# zipcode$zip
author_zip<-lappply(foo, function(x)x[which(x %in% zipcode$zip)])
# select first from list

author_zip8<-lapply(author_zip, `[`, 1)
res$zip8<-as.character(author_zip8)





# MERGE UP THESE THREE COLUMNS
res = mutate(res, ZIP = paste(as.character(zip1), as.character(zip2),as.character(zip3),as.character(zip4),as.character(zip5),as.character(zip6),as.character(zip7),as.character(zip8), sep = ' '))
res$ZIP<-gsub("NA", res$ZIP, replacement="") #might need to leave a space as replacenetb
trim.trailing <- function (x) sub("\\s+$", "", x)
res$ZIP<-trim.trailing(res$ZIP)
trim.leading <- function (x)  sub("^\\s+", "", x)
res$ZIP<-trim.leading(res$ZIP)
res$ZIP<-as.factor(res$ZIP)
res<-select(res,-citymix1,-citymix2,-citymix3,-countrymix1,-countrymix2,-countrymix3,-zip1,-zip2,-zip3)
res<-select(res,-citymix4,-citymix5,-citymix6,-countrymix4,-countrymix5,-countrymix6,-zip4,-zip5,-zip6)
res<-select(res,-citymix7,-citymix8,-countrymix7,-countrymix8,-zip7,-zip8)

#inferring city from zip code
zipcode<-dplyr::rename(zipcode, ZIP=zip)
res<-left_join(res, zipcode, by = "ZIP")
res$city<-tolower(res$city)
res<-dplyr::rename(res, city_from_zip=city, state_from_zip=state, lat_from_zip=latitude, long_from_zip=longitude)




world.citiesEB<-world.cities
world.citiesEB$name<-tolower(world.citiesEB$name)
world.citiesEB$name<-gsub("'", world.citiesEB$name, replacement="") #might need to leave a space as replacenetb
# world.citiesEB$name<-as.factor(world.citiesEB$name)

res<-dplyr::rename(res, name=CITY) #temporary name change
res2<-left_join(res, world.citiesEB, by = "name")
res2<-dplyr::rename(res2, CITY=name) #change back the temporary name change
res2<-dplyr::rename(res2, country_from_CITY=country.etc) #note that this includes multiple cambridge, they seem to be going in in alpha order.
res2<-dplyr::select(res2,-pop,-lat,-long,-capital)

str(res2)
levels(res2$COUNTRY)

BDFFP.geo<-res2
BDFFP.geo


# END END END END





############0


str(res)
str(world.citiesEB)

res<-dplyr::rename(res, city=CITY)
world.citiesEB<-dplyr::rename(world.citiesEB, city=name)
res2<-left_join(res, world.citiesEB, by = "city")







#note that this includes multiple cambridge, they seem to be going in in alpha order.
str(res2)
res2$country.etc<-tolower(res2$country.etc)

res2<-dplyr::rename(res2,country_from_EBcity=country.etc)

world.citiesEB<-dplyr::rename(world.citiesEB, COUNTRY=country.etc)
world.citiesEB$COUNTRY<-tolower(world.citiesEB$COUNTRY)
res3<-left_join(res2, world.citiesEB, by = "COUNTRY")  #note that this includes multiple cambridge, they seem to be going in in alpha order.
str(res3)
BDFFP.geo<-res3
BDFFP.geo


world.citiesEB$COUNTRY


res$new_val <- c("FALSE", "TRUE")[(res$mix1 %in% cities)+1]

res$new_val <- (res$mix1 %in% cities)
apply(res$mix1, function(x)(res$mix1 %in% cities))











