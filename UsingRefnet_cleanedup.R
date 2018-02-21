library(refnet)
library(devtools)
library(tidyverse)
library(stringr)

# devtools::install_github("embruna/refnet2", subdir = "refnet")  

file1 <- "Ecuador.txt"
file2 <- "Ecuador_0114.txt"
name <- "ecuador"

step1 <- function(file1, file2, name="files"){
references1 <- read_references(paste0("./data/",file1), 
                                      dir=FALSE, 
                                      filename_root=paste0("./output/",name))

authors1 <- read_authors(references1, 
                          filename_root=paste0("./output/",name))

references2 <- read_references(paste0("./data/",file2), 
                               dir=FALSE, 
                               filename_root=paste0("./output/",name,"2"))

authors2 <- read_authors(references2, 
                         filename_root=paste0("./output/",name,"2"))

no_dupes1 <- remove_duplicates(authors=authors1$authors,
                    authors_references=authors1$authors_references,
                    filename_root=paste0("output/",name,"_nodupe"))

no_dupes2 <- remove_duplicates(authors=authors2$authors,
                         authors_references=authors2$authors_references,
                         filename_root=paste0("output/",name,"2_nodupe"))

output <- merge_records(
      references=references1,
      authors=no_dupes1$authors,
      authors_references=no_dupes1$authors_references,
      references_merge=references2,
      authors_merge=no_dupes2$authors,
      authors_references_merge=no_dupes2$authors_references,
      filename_root = paste0("output/merged",name))

return(output)
}

tt <- Sys.time()
out <- step1(file1=file1, file2=file2, name=name)
tt - Sys.time()

# so 'out' does not have an actual out$authors_references
# it is returning a blank data frame
# which is USELESS TO THE NEXT STEP

save(out, file= paste0(Sys.Date(),"_step1_out.Rdata"))

step1out <- out

step2 <- remove_duplicates(authors=step1out$authors, 
                     authors_references=step1out$authors_references,
                     filename_root=paste0("output/merged_",name,"nodupe"))

save(step2, file=paste0(Sys.Date(),'_step2.Rdata'))
######
##	Sample geographic plotting of author locations based on RP or C1:

authors_working <- step2$authors

##	Process a single address at a time:
refnet_geocode(data.frame("AU_ID"=authors_working$AU_ID[1], 
                          "type"="RP", 
                          "address"=authors_working$RP[1], 
                          stringsAsFactors=FALSE))

##	Process a group of addresses:
read_addresses(data.frame("AU_ID"=authors_working$AU_ID[1:10], 
                          "type"="RP", 
                          "address"=authors_working$RP[1:10], 
                          stringsAsFactors=FALSE), verbose=TRUE)

##	Sample using the first C1 address listed for the first 1000 authors, 
##	keyed by author so we can join it back:

sample_first_CI <- function(authors_working, maxrows=1000){

address_list <- sapply(strsplit(authors_working$C1[1:maxrows],
                                "\n"), 
                               FUN=function(x) { return(x[1]) })

address_list_au_id <- authors_working$AU_ID[1:maxrows][!is.na(address_list)]

address_list2 <- address_list[!is.na(address_list)]

address_list3 <- gsub("^.* (.*,.*,.*)$", "\\1", address_list2)

address_list4 <- gsub("[. ]*$", "", address_list3)

addresses_working <- read_addresses(data.frame("id"=address_list_au_id, 
                                               "type"="C1", 
                                               "address"=address_list4, 
                                               stringsAsFactors=FALSE), 
                    filename_root="output/merged_nodupe_addresses_C1_first1000")

return(addresses_working)
}

step3 <- sample_first_CI(authors_working=step2$authors, max=1000)

save(step3, file=paste0(Sys.Date(),"_step3.Rdata"))

##	Now we can use those addresses to plot things out:
plot_addresses_country(step3)
plot_addresses_points(step3)

net_plot_coauthor(step3, step2$authors_references)
net_plot_coauthor_country(step3, step2$authors_references)
