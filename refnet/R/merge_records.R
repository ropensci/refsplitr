########################################
########################################
##	BEGIN: merge_records():

#' Merges two sets of references/authors/authors__references/addresses objects
#' 
#' \code{merge_records} This function will merge two sets of references/authors/authors__references/addresses, making sure that there are no duplicates in references, no duplicates AU_IDs in the authors table, renaming the AU_IDs in the linking table appropriately, and then	doing an automated potential match of authors just like when processing references for the first time.
#' 
#' @param references references data.frame to merge to
#' @param authors authors data.frame to merge to
#' @param authors_references authors_references data.frame to merge to
#' @param addresses optional addresses data.frame to merge to, if you only have addresses from the second set then they will be retained
#' @param references_merge references data.frame to merge with references argument
#' @param authors_merge authors data.frame to merge with authors argument
#' @param authors_references_merge authors_references data.frame to merge with 
#' @param addresses_merge optional addresses data.frame to merge with
#' @param filename_root the filename root, can include relative or absolute
#'   path and will be appended to output file names function will be saved

merge_records <- function(references, 
                          authors, 
                          authors_references, 
                          addresses="", 
                          references_merge, 
                          authors_merge, 
                          authors_references_merge, 
                          addresses_merge="", 
                          filename_root = "") {
#   ##	For testing:
#  ecuador_references <- read.csv("~/refnet2/output/ecuador_references.csv")
#  ecuador_authors <- read.csv("~/refnet2/output/ecuador_authors.csv")
#  ecuador_author_references <- read.csv("~/refnet2/output/ecuador_authors__references.csv")
#  ecuador2_references <- read.csv("~/refnet2/output/ecuador2_references.csv")
#  ecuador2_authors <- read.csv("~/refnet2/output/ecuador2_authors.csv")
#  ecuador2_authors_references <- read.csv("~/refnet2/output/ecuador2_authors__references.csv")
# 
# # output <- merge_records(
#    references=ecuador_references#,
#    authors=ecuador_authors#,
#    authors_references=ecuador_author_references#,
#    references_merge=ecuador2_references#,
#    authors_merge=ecuador2_authors#,
#    authors_references_merge=ecuador2_authors_references#,
#    filename_root = "output/merged"
# # )
 
 
  
  # output <- merge_records(
  #   references=ecuador_references,
  #   authors=ecuador_authors,
  #   authors_references=ecuador_author_references,
  #   references_merge=ecuador2_references,
  #   authors_merge=ecuador2_authors,
  #   authors_references_merge=ecuador2_authors_references,
  #   filename_root = "output/merged"
  # )
  
  ##	First we merge references lists, making sure that we have no duplicates
  ##		to mess with:
  remove_indices <- (references_merge$UT %in% references$UT)
  
  ##	If we have duplicate references then we need to remove them from the 
  ##		references table and the linking table:
  if (sum(remove_indices) > 0) {
    ##	Pull IDs for references to be removed:
    UT <- references_merge$UT[remove_indices]
    
    ##	Remove the references that will be duplicates:
    references_merge <- references_merge[!remove_indices,]
    
    ##	Remove the links matching those references that were removed:
#    authors_references_merge <- authors_references_merge[!(authors_references_merge$UT %in% UT),]
    
    authors_references_merge <- authors_references_merge %>% 
                                    filter(!(UT %in% UT))
    
    ##	Remove any authors from this list that no longer have records in
    ##		the link table:
    linked <- full_join(authors_merge, 
                        authors_references_merge, 
                        by="AU_ID")
    
#    remove_ids <- linked$AU_ID[ is.na(linked$UT) ]
    remove_ids <- linked %>%
                      filter(is.na(UT)) %>%
                      select(AU_ID)
    
#    authors_merge <- authors_merge[ authors_merge$AU_ID %in% 
#                                      remove_ids, ]

      authors_merge <- authors_merge %>%
                          filter(AU_ID %in% remove_ids)
      
    if (addresses_merge != "") {
#      addresses_merge <- addresses_merge[ addresses_merge$AU_ID %in% 
#                                            remove_ids, ]
      addresses_merge <- addresses_merge %>%
                            filter(AU_ID %in% remove_ids)
    } # if (sum(remove_indices) > 0) {
  } # if (sum(remove_indices) > 0) {
  
  
  ##	Now we check each author in the author list to be merged to see if it's AU_ID is already in the list to be merged and increment it:
  for (aut in 1:length(authors_merge$AU_ID)) {
    while (sum(authors_merge[aut, "AU_ID"] %in% authors$AU_ID) > 0) {
      AU_ID_old <- authors_merge[aut, "AU_ID"]
      
      ID_num <- gsub(".*_([0-9]+)$", 
                     "\\1", AU_ID_old)
      
      ID_num <- as.numeric(ID_num) + 1
      
      AU_ID_new <- gsub("(.*_)[0-9]+$", 
                        paste("\\1", ID_num, sep=""),
                        AU_ID_old)
      ##	Now check to make sure we aren't overwriting an existing AU_ID
      ##		in the authors_merge table:
      while (sum(authors_merge$AU_ID == AU_ID_new) > 0) {
        ID_num <- gsub(".*_([0-9]+)$", 
                       "\\1", AU_ID_new)
        
        ID_num <- as.numeric(ID_num) + 1
        
        AU_ID_new <- gsub("(.*_)[0-9]+$", 
                          paste("\\1", ID_num, sep=""),
                          AU_ID_new)
      }
      
      authors_merge[aut, "AU_ID"] <- AU_ID_new
#      authors_references_merge[ authors_references_merge$AU_ID==AU_ID_old,
#                                "AU_ID"] <- AU_ID_new
      authors_references_merge <- authors_references_merge %>%
                                  mutate(AU_ID = ifelse(AU_ID==AU_ID_old),
                                        AU_ID_new, AU_ID)
      
      ##	And if we have addresses replaced old AU_ID values with the new:
      if (addresses_merge != "") {
#        addresses_merge[ addresses_merge$AU_ID == AU_ID_old, "AU_ID"] <- AU_ID_new
        addresses_merge <- addresses_merge %>%
                            mutate(AU_ID = ifelse(AU_ID==AU_ID_old,
                                                  AU_ID_new, AU_ID))
              }
      
      ##	Return to the top to check to see if we've found a unique AU_ID
      ##		with the authors table...
    }
  }
  
  ##	Now we can merge all of the tables:
  suppressWarnings(  references <- bind_rows(references, references_merge))
  
suppressWarnings(authors <- bind_rows(authors, authors_merge))
  
suppressWarnings(authors_references <- bind_rows(
                  authors_references, 
                  authors_references_merge))

  if (addresses_merge != "") {
    if (addresses == "") {
      addresses <- addresses_merge
    } else {
      addresses <- bind_rows(addresses, addresses_merge)
    }
    
  }
  
  ##	Now we check all authors in the merged dataset to see whether there are
  ##		possible duplicates:
  for (aut in 1:length(authors[,"AU"])) {
    indices <- grepl(authors[aut,"AU"], authors[,"AU"])
    if (sum(indices) > 1) {
      similar <- authors[indices,]
      AU_ID_Dupe <- NA 
      Similarity <- 0
      for (i in length(similar$AF)) {
        
        ##	More sophisticated similarity measures could be devised here
        ##		but we'll use a canned distance composite from the 
        ##		RecordLinkage package:
        newSimilarity <- jarowinkler(authors[aut,"AF"], similar[i,"AF"])
        
        ##	See note above about not including country directly in the
        ##		author record:
        #if (!is.na(similar[i,"Country"])) {
        #	newSimilarity <- newSimilarity + 1.0
        #}
        
        if (newSimilarity > Similarity) {
          Similarity <- newSimilarity
          AU_ID_Dupe <- similar[i,"AU_ID"]
        }
      }
      authors[aut,"AU_ID_Dupe"] <- AU_ID_Dupe
      authors[aut,"Similarity"] <- Similarity
    }
  }
  
  
  if(filename_root != "") {
    write.csv(references, 
              file=paste(filename_root, "_references.csv", sep=""), 
              row.names=FALSE)
    
    write.csv(authors, 
              file=paste(filename_root, "_authors.csv", sep=""), 
              row.names=FALSE)
    
    write.csv(authors_references, 
              file=paste(filename_root, "_authors_references.csv", sep=""),
              row.names=FALSE)
  }            
  
  return(list("references"=references, 
              "authors"=authors, 
              "authors_references"=authors_references))
}

##	END: merge_records():
########################################
########################################
