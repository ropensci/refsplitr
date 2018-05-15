# Binds the files in a folder together and adds the year represented by each file 
# as a column by estracting it from the file name (assuming you have that)  


FL2017 <- list.files("./data/Florida2017", full.names = T)

FL2017_stitched = lapply(FL2017, function(x) {
  
  y=gsub("./data/Florida2017/","",x)
  y=gsub(".txt","",y)
  
  dat = read_references(x, dir=FALSE,filename_root=paste("./data/Florida2017/",y,sep=""))
  return(dat)
})


#This is returned as a list, when binding below converts to dataframe with country as chr
FL2017_stitched<-bind_rows(FL2017_stitched)
FL2017_stitched<- FL2017_stitched %>% select(-filename) #remove the name of the sub-file the record came from 
str(FL2017_stitched)
names(FL2017_stitched)
FL2017_stitched
write.csv(FL2017_stitched,file="./data/FL2017_stitched.csv",row.names=FALSE)

