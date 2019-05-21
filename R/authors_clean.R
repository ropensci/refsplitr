#' Seperates author information in references files from \code{references_read}
#' 
#' \code{authors_clean} This function takes the output from \code{references_read} and cleans the author information.
#' Information on addresses, emails, ORCIDs, etc are matched.
#' It then attempts to match same author entries together into likely author groups based on common full names, addresses, emails, ORCIDs etc.
#' 
#' Records that are not matched this way have a jaro winkler similiarty analysis metric calculated
#' for all possible matching author names.
#' This calculates the amount of character similarities based on distance of similar character.
#' 
#' @param references output from \code{references_read}
#' @importFrom RecordLinkage jarowinkler
#' @importFrom dplyr filter select arrange
#' @examples 
#' 
#' data(BITR)
#'  
#'authors_clean(references = BITR)
#' @export authors_clean
#' 
authors_clean <- function(references) {
  ###############################
  # Seperate authors and attempt to match author info

  final <- authors_parse(references)

  ###############################
  # Split address information into relevant fields

  address_df <- authors_address(final$address, final$authorID)

  final <- merge(final, address_df[, c("university", "country", "state",
    "postal_code", "city", "department", "adID")],
    by.x = "authorID", by.y = "adID", all.x = TRUE)

  ##################################
  # Now start Author Matching
  ##################################

  novel_names <- authors_match(final)

  cols <-  c("ID", "groupID", "match_name", "matchID",
    "similarity", "confidence", "flagged")

  final <- merge(final, novel_names[, cols],
    by.x = "authorID", by.y = "ID", all.x = TRUE)
  cols <-  c("authorID", "groupID", "match_name", "matchID",
    "similarity", "confidence", "flagged")
  final <- final[, c(cols,
    colnames(final)[!colnames(final) %in% cols])]

  sub_authors <- final %>%
    filter( groupID %in% final$groupID[!is.na(similarity) | flagged == 1]) %>%
    select(authorID, AU, AF, groupID, match_name, matchID,
      similarity, confidence, university, department,
      postal_code, country, address, RP_address, RI,
      OI, EM, UT, author_order, refID, PT, PY, PU) %>%
    arrange(groupID, similarity, authorID)

  return(list(prelim = final, review = sub_authors))

}
