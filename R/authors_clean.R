#' Extracts the lat and long for each address
#'
#' \code{authors_clean} This function takes the output from `references_read()` and cleans the author information.
#'
#' @param references output from `references_read()`
#' @param sim_score default is 0.88
#' @param filename_root the filename root, can include relative or absolute
#' @param write_out_data TRUE or FALSE, do you want a .csv file written out?
#' @importFrom RecordLinkage jarowinkler
#' @examples 
#'  references <-data.frame(filename=NA, AB=NA,
#'  AF=c('Smith, Jon J.','Thompson, Bob B.','Smith,J'), 
#'  AU=c('Smith, Jon J.','Thompson, Bob','Smith, J'), 
#'  BP=NA , C1=c("Univ Florida, Gainesville, FL USA",
#'  "University of Texas, Austin, TX, USA",NA),
#'  CR=NA,DE=NA, DI=NA, EM=c("j.smithufl.edu",NA,'jsmithusgs.gov'), 
#'  EN=NA, FN=NA, FU=NA, PD=NA, PG=NA, PT=NA, PU=NA, 
#'  PY=NA, RI=NA, OI=NA,PM=NA, 
#'  RP=c("Univ Florida, Gainesville, FL USA",
#'  "University of Texas, Austin, TX, USA",NA), 
#'  SC=NA, SN=NA, SO=NA, TC=NA, TI=NA, UT=NA, VL=NA, 
#'  WC=NA,Z9=NA,refID=c(1,2,3) ,stringsAsFactors=FALSE)
#'  references[]<-lapply(references, as.character)
#'  authors_clean(references=references)
#' @export authors_clean



authors_clean <- function(references,
                          sim_score = 0.88,
                          filename_root = "./",
                          write_out_data = FALSE) {
  # address parsing
  ###############################
  print("Parsing addresses")
  final<-authors_parse(references)
  
  
  address.df<-authors_address(final$address,final$authorID)

  final <- merge(final, address.df[, c("university", "country", "state", 
        "postal_code", "city", "department", "adID")], 
        by.x = "authorID", by.y = "adID", all.x = TRUE)
  
  ##################################
  # Now start Author Matching
  ##################################
  # colnames(final)
  print("Matching authors")
  
  novel.names<-authors_match(final,sim_score=sim_score)
  
  final <- merge(final, novel.names[, c("ID", "groupID", "match_name", 
                  "similarity")], by.x = "authorID", by.y = "ID", all.x = TRUE)
  
  final <- final[, c("authorID", "AU", "AF", "groupID", "match_name", 
        "similarity", colnames(final)[!colnames(final) %in% c("authorID", 
                        "AU", "AF", "groupID", "match_name", "similarity")])]

sub.authors <- final[final$groupID %in% final$groupID[!is.na(final$similarity)],
              c("authorID", "AU", "AF", "groupID", "match_name", "similarity", 
            "author_order", "university", "department", "postal_code", 
            "country", "address", "RP_address", "RI", "OI", "EM", "UT",
            "refID", "PT", "PY", "PU")]
  
  sub.authors <- sub.authors[order(sub.authors$groupID, sub.authors$similarity, 
                                   sub.authors$authorID), ]

  # write it out
  if( write_out_data == TRUE & nrow(sub.authors)>0){
  utils::write.csv(subset(final, select = -c(match_name, similarity)),
    file = paste0(filename_root, "_authors_prelim.csv"),
    row.names = FALSE
  )
  }
  
  if( write_out_data == TRUE){
  utils::write.csv(sub.authors,
    file = paste0(filename_root, "_authors_review.csv"),
    row.names = FALSE
  )
  }
  #
  return(list(prelim = final, review = sub.authors))
} # end function
