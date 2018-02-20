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
      authors_references=no_dupes1$author_references,
      references_merge=references2,
      authors_merge=no_dupes2$authors,
      authors_references_merge=no_dupes2$authors_references,
      filename_root = paste0("output/merged",name))

return(output)
}

tt <- Sys.time()
out <- step1(file1=file1, file2=file2, name=name)
tt - Sys.time()


step1out <- out

step2 <- remove_duplicates(authors=step1out$authors, 
                     authors_references=stepout1$merged_authors_references,
                     filename_root=paste0("output/merged_",name,"nodupe"))


