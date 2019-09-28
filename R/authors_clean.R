#' Seperates author information in references files from \code{references_read}
#' 
#' \code{authors_clean} This function takes the output from \code{references_read} and cleans the author information.
#' Information on addresses, emails, ORCIDs, etc are matched.
#' It then attempts to match same author entries together into likely author groups based on common full names, addresses, emails, ORCIDs etc.
#' 
#' Records that are not matched this way have a Jaro-Winkler similiarty analysis metric calculated
#' for all possible matching author names.
#' This calculates the amount of character similarities based on distance of similar character.
#' 
#' @param references output from \code{references_read}
#' @importFrom RecordLinkage jarowinkler
#' @examples 
#' ## Load the refsplitr sample dataset "BITR" 
#' data(BITR) 
#' BITR_clean <- authors_clean(BITR)
#' 
#' ## The output of authors_clean is a list with two elements, which can be assigend to dataframes.
#' BITR_review_df <- BITR_clean$review
#' BITR_prelim_df <- BITR_clean$prelim
#' 
#' ## Users can save the these dataframes outside of R as .csv files.
#' ## The "review_df.csv" is then used to review the groupID or authorID assignments and make
#' ## any necessary corrections. The function "authors_refine" is used to load and merge the changes 
#' ## into R and create a dataframe used for analyses. 
#' 
#' @export authors_clean
#' 
authors_clean <- function(references) {
  ###############################
  # If there are NAs in AU and AF, authors_clean() throws an error, so the first thing we have to do is confirm that there are no NAs in 
  # the AU and AF fields. If there are, this most often when papers are written by groups (="Cosortia", field = CA); 
  # in these cases WOS does not include tha AU and AF fields. However, there are cases where there is a Consortium listed in CA, 
  # but there are also the names of individuals in AF and AU. Rather than make decisions for users about who the authors are
  # (e.g., replace AU and AF with CA), we instead stop this function and give an error message to the users to replace the NAs in AU and AF. 
  refID_missingAUAF <- which(is.na(references$AU)==TRUE)  # finds NAs in AU
  missingAUAF
  missingAUAF <- references[references$refID==refID_missingAUAF,]
  refid_sum <- sum(refID_missingAUAF)
  missingAUAF$refID
  # Stop condition and message
  if( any(refid_sum > 0) ) stop('The following references have no authors
(i.e., there are NAs in the AU and AF fields):\n\n',
    'refID = ',paste(missingAUAF$refID, collapse=", "),
    '\n\nBefore using authors_clean() you MUST:
\n(1) remove these references from the dataframe.
\nOR\n
(2) Correct the NAs in the AU and AF fields
for these references.
They do not have an author, in which case
you can use "None", "Anonymous", "Unknown", etc.
They may have been written by an Author Consortium
(see Column "CA");
If so you can replace the NAs in AU and AF
with the contents of column CA.',
    sep=" ")
  
  ###############################
  
  
  ###############################
  # Seperate authors and attempt to match author info
  #requireNamespace(dplyr, quietly = TRUE)
  final <- authors_parse(references[1:1000,])

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

  # sub_authors <- final %>%
  #   filter(
  #     groupID %in% final$groupID[!is.na(similarity) | flagged == 1]
  #   ) %>%
  #   select(authorID, AU, AF, groupID, match_name, matchID,
  #     similarity, confidence, university, department,
  #     postal_code, country, address, RP_address, RI,
  #     OI, EM, UT, author_order, refID, PT, PY, PU) %>%
  #   arrange(groupID, similarity, authorID)
  #
  sub_authors <- subset(final,
    groupID %in% groupID[!is.na(similarity) | flagged == 1],
    select = c(
      authorID, AU, AF, groupID, match_name, matchID,
      similarity, confidence, university, department,
      postal_code, country, address, RP_address, RI,
      OI, EM, UT, author_order, refID, PT, PY, PU
    )
  )

  sub_authors <- sub_authors[
    order(sub_authors$groupID,
      sub_authors$similarity,
      sub_authors$authorID),
    ]

  return(list(prelim = final, review = sub_authors))

}
