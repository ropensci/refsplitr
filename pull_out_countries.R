library(ggplot2)
load("./output/eb_refined.Rdata")
load("./data/countries.Rdata")

eb_refined$country <- NA

for(i in 1:length(countries)){
  
eb_refined <- eb_refined %>%
    mutate(dd = str_detect(eb_refined$address, countries[i]),
           dd = ifelse(is.na(dd),FALSE, dd))

eb_refined[eb_refined$dd,"country"] <- countries[i]

}
