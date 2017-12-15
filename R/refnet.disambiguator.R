library(stringr)
AU_ID<-eb_authors__references$AU_ID
AU_ID<-as.data.frame(AU_ID)
name<-select(AU_ID, AU_ID)
name.file<-cbind(AU_ID,name)
names(name.file)[1] <- "ID"
names(name.file)[2] <- "au.name"
summary(name.file)
str(name.file)
name.file$au.name<-gsub("\\_.*", "", name.file$au.name, perl=TRUE)
