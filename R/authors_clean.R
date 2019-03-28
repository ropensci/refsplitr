#' Seperates author information in references files from \code{references_read}
#' 
#' \code{authors_clean} This function takes the output from \code{references_read} and cleans the author information.
#' Information on addresses, emails, ORCIDs, etc are matched.
#' It then attempts to match same author entries together into likely author groups based on common full names, addresses, emails, ORCIDs etc.
#' 
#' Records that are not matched this way have a jaro winkler similiarty analysis metric calculated
#' for all possible matching author names.
#' This calculates the amount of character similarities based on distance of similar character.
#' You can set a similarity score cut off 0 to 1.0 (default is 0.88), higher numbers are more conservative. 
#' Numbers below 0.75 are very unlikely to be similar, and we do not recommend using.
#' 
#' @param references output from \code{references_read}
#' @param sim_score Jaro-winkler similarity cut off, default is 0.88.
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
#'  
#'  references[]<-lapply(references, as.character)
#'  
#'  authors_clean(references=references)
#' @export authors_clean
#' 
authors_clean <- function(references,
                          sim_score = 0.88) {
  ###############################
  # Seperate authors and attempt to match author info

  final <- authors_parse(references)

  ###############################
  # Split address information into relevant fields

  address.df <- authors_address(final$address,final$authorID)

  final <- merge(final, address.df[, c("university", "country", "state",
        "postal_code", "city", "department", "adID")],
        by.x = "authorID", by.y = "adID", all.x = TRUE)

  ##################################
  # Now start Author Matching
  ##################################

  novel.names <- authors_match(final, sim_score = sim_score)

  final <- merge(final, novel.names[, c("ID", "groupID", "match_name",
                  "similarity","confidence")], by.x = "authorID", by.y = "ID", all.x = TRUE)

  final <- final[, c("authorID", "AU", "AF", "groupID", "match_name",
        "similarity","confidence", colnames(final)[!colnames(final) %in% c("authorID",
                        "AU", "AF", "groupID", "match_name", "similarity","confidence")])]

  sub.authors <- final[final$groupID %in% final$groupID[!is.na(final$similarity)],
              c("authorID", "AU", "AF", "groupID", "match_name", "similarity","confidence",
            "author_order", "university", "department", "postal_code",
            "country", "address", "RP_address", "RI", "OI", "EM", "UT",
            "refID", "PT", "PY", "PU")]

  sub.authors <- sub.authors[order(sub.authors$groupID, sub.authors$similarity,
                                   sub.authors$authorID), ]

  return(list(prelim = final, review = sub.authors))

}