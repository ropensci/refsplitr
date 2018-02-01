#############################################
#############################################
##	BEGIN: remove_duplicates():

#' Processes the output of read_authors() to remove any flagged duplicate authors
#' 
#' \code{remove_duplicates} This function remove duplicates from the combined author and authors__references data.frames that you give it by merging the information from all records with the same AU_ID_Dupe value and replacing any of the merged records AU_ID values in the authors__references list with the AU_ID_Dupe value.
#' 
#' @param authors the first list item from the output from the read_authors() function
#' @param authors__references the second list item from the output from the read_authors() function
#' @param addresses an optional argument, that will allow you to recode the AU_ID of corresponding, removed duplicates, should be a data.frame object output from the read_addresses() function
#' @param filename_root the filename root, can include relative or absolute
#'   path and will be appended to output file names function will be saved

remove_duplicates <- function(authors, authors_references, 
                              addresses="", 
                              filename_root = "") {
  ##	First we will create a new author list with all of those authors removed that have not been tagged as a duplicate:
  authors_removed <- authors[ (is.na(authors$AU_ID_Dupe) & 
                                 !(authors$AU_ID %in% authors$AU_ID_Dupe)), ]
  
  ##	Next we will iterate over the unique values of the AU_ID_Dupe field and create a single record for each set of duplicates:
  
  uni_AU_ID_Dupe <- na.omit(unique(authors[,"AU_ID_Dupe"]))
  
  for (i in 1:length(uni_AU_ID_Dupe)) {
    
    AU_ID_Dupe <- uni_AU_ID_Dupe[i]
    ##	Also have to check here to pull all tagged with the same AU_ID_Dupe
    ##		and the original AU_ID matching it!
    authors_dupes <- authors[ (!is.na(authors$AU_ID_Dupe) &
                                (authors$AU_ID_Dupe == AU_ID_Dupe) |
                                 authors$AU_ID == AU_ID_Dupe), ]
    
    authors_removed[i, "AU_ID"] <- AU_ID_Dupe
    
    authors_removed[i, "AU_ID_Dupe"] <- NA
    
    authors_removed[i, "Similarity"] <- NA
    
    EM <- character(0)
    
    C1 <- character(0)
    
    RP <- character(0)
    
    for (j in 1:length(authors_dupes$AU_ID)) {
      for(k in 1:length(authors_removed$AU_ID)){
        ##	Take the longest author name with first name:
        if (nchar(authors_dupes[j, "AF"]) > 
            nchar(authors_removed[k, "AF"])) {
          
          authors_removed[k, "AF"] <- authors_dupes[j, "AF"]
        } # if statement
        
        ##	Take the longest author name with initials:
        if (nchar(authors_dupes[j, "AU"]) > 
            nchar(authors_removed[k, "AU"])) {
          
          authors_removed[k, "AU"] <- authors_dupes[j, "AU"]
        } # if statement
        
        ##	Take the longest Researcher ID:
        if (nchar(authors_dupes[j, "RI"]) > 
            nchar(authors_removed[k, "RI"])) {
          
          authors_removed[k, "RI"] <- authors_dupes[j, "RI"]
        } # if statement
        
        ##	Append all addresses together and we'll check for and remove
        ##		duplicates later on:
        if (!(is.na(authors_dupes[j, "EM"]) | 
              authors_dupes[j, "EM"] == "")) {
          
          EM <- append(EM, unlist(strsplit(authors_dupes[j, "EM"], "\n")))
        } # if statement
        if (!(is.na(authors_dupes[j, "C1"]) | 
              authors_dupes[j, "C1"] == "")) {
          
          C1 <- append(C1, unlist(strsplit(authors_dupes[j, "C1"], "\n")))
        } # if statement
        if (!(is.na(authors_dupes[j, "RP"]) | 
              authors_dupes[j, "RP"] == "")) {
          
          RP <- append(RP, unlist(strsplit(authors_dupes[j, "RP"], "\n")))
        } # if statement
        
        ##	Finally, update the authors__references AU_ID to reflect the newly merged
        ##		records:
        authors_references[(authors_references$AU_ID ==
                        authors_dupes[j, "AU_ID"]),"AU_ID"] <- AU_ID_Dupe
        
        ##	If addresses were supplied, update them:
        if (addresses != "") {
          addresses[ (addresses$AU_ID == 
                        authors_dupes[j, "AU_ID"]), 
                     "AU_ID"] <- AU_ID_Dupe
        }# if statement
        
        
        authors_removed[k,"EM"] <- paste0(EM, collapse="\n")
        
        authors_removed[k,"C1"] <- paste0(C1, collapse="\n")
        
        authors_removed[k,"RP"] <- paste0(RP, collapse="\n")
        
        
      } # k for loop
    } # j for loop
    
    ##	Remove any duplicates from EM, C1 and RP records:
    EM <- unique(EM)
    
    C1 <- unique(C1)
    
    RP <- unique(RP)
    
    
  } # AU_ID_Dupe for loop 
  
  
  if(filename_root != "") {
    write.csv(authors_removed, 
              file=paste(filename_root, 
                         "_authors.csv", sep=""), 
              row.names=FALSE)
    
    write.csv(authors_references,
              file=paste(filename_root, 
                         "_authors_references.csv", sep=""), 
              row.names=FALSE)
  }            
  
  return(list("authors"=authors_removed, 
              "authors__references"=authors_references))
}

##	END: remove_duplicates():
########################################
########################################