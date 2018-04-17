library(tidyverse)
library(refnet)
library(stringr)
library(stringi)


load("./output/eb_refined.Rdata")

dat <- separate(data=eb_refined, col = address,
                into=c("university","department","short_address"),
                sep=",",extra = "merge", remove=FALSE) %>%
  mutate(country=stri_extract_last_words(short_address),
         postal_code = str_extract(string=short_address, 
                      pattern="[:alpha:]{2}[:punct:]{1}[:digit:]{1,8}|[:space:][:upper:][:digit:][:upper:][:space:][:digit:][:upper:][:digit:]|[:alpha:][:punct:][:digit:]{4}")) %>%
  mutate(postal_code = ifelse(is.na(postal_code), str_extract(string=short_address,
                      pattern="[:space:][:digit:]{5}"), postal_code)) %>%
  mutate(postal_code = ifelse(is.na(postal_code), str_extract(string=short_address,
                      pattern="[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"), postal_code)) %>%
  select(short_address, postal_code, country)
           

library(tidytext)

tidydat <- separate(data=eb_refined, col = address,
                  into=c("university","department","short_address"),
                  sep=",",extra = "merge", remove=FALSE) %>%
          select(short_address) %>%
          mutate(line = 1:nrow(eb_refined)) %>%
          unnest_tokens(word, short_address)
          










ifelse(is.na(dat$postal_code), str_extract(string=dat$short_address,
                                       pattern="[:space:][:digit:]{5}"), dat$postal_code)

 
         city_state = str_extract(string=short_address,
                          pattern="[[[:alpha:]:space:]:alpha:]{1,30}[,][ ][A-Z][A-Z]")) %>%
  select(short_address, city_state, postal_code, country)


library(rworldmap)
data(countryExData)
countries <- countryExData[ , 2] %>%
        str_trim(side = c("both"))

ss <- str_split(dat$short_address, pattern="[:space:]")

dat <- separate(data=eb_refined, col = address, 
                into=c("university","department","short_address"),
                sep=",",extra = "merge", remove=FALSE) %>%
  mutate(country=str_extract(string=short_address,
                             pattern=countries),
         zip = str_extract(string=short_address, 
                pattern="[:digit:][:digit:][:digit:][:digit:][:digit:]"),
         city = str_extract(string=short_address,
                pattern="^.* ")) %>%
  select(short_address, city, country)


ll <- str_split(dat$short_address, pattern=",")

  
