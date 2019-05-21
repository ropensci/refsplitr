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
#' 
#' data(BITR)
#'  
#'  authors_clean(references = BITR)
#' @export authors_clean
#' 
authors_clean <- function(references) {
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
  final_backup<-final
  novel.names <- authors_match(final)

  final <- merge(final, novel.names[, c("ID", "groupID", "match_name",
                  "matchID","similarity","confidence","flagged")], 
                 by.x = "authorID", by.y = "ID", all.x = TRUE)

  final <- final[, c("authorID", "AU", "AF", "groupID", "match_name", "matchID",
        "similarity","confidence","flagged" ,colnames(final)[!colnames(final) %in% c("authorID",
                        "AU", "AF", "groupID", "match_name", "similarity","confidence")])]

  sub.authors <- final[final$groupID %in% final$groupID[!is.na(final$similarity) | final$flagged==1],
              c("authorID", "AU", "AF", "groupID", "match_name","matchID" ,"similarity","confidence",
             "university", "department", "postal_code",
            "country", "address", "RP_address", "RI", "OI", "EM", "UT",
            "author_order","refID", "PT", "PY", "PU")]

  sub.authors <- sub.authors[order(sub.authors$groupID, sub.authors$similarity,
                                   sub.authors$authorID), ]

  return(list(prelim = final, review = sub.authors))

}