
library(stringr)
library(stringi)

dat <- separate(data=final,
         col = address, 
         into=c("university","department","address"),
         sep=",",extra = "merge") %>%
       mutate(country=stri_extract_last_words(address),
        zip = str_extract(string=address, 
          pattern="[:digit:][:digit:][:digit:][:digit:][:digit:]"),
        city_state = str_extract(string=address,
                pattern="[:alnum:]{1,20}[,][ ][A-Z][A-Z]") ) %>%
      select(address, city_state, zip, country)


blahblah$city_state <- str_extract(string=blahblah$address,
            pattern="[:alnum:]{1,20}[,][ ][A-Z][A-Z]")

state_province =  

  
blahblah <- data.frame(address=dat$address)  



blahblah$

str_remove(string=dat$address,pattern = "[:upper:][:upper:],")
 
  
blahblah$state <- 
