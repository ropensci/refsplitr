#' Refines the authors code output from authors_clean()
#'
#' \code{authors_refine} This function takes the author list output after the output has been synthesized for incorrect author matches. It contains a similarity score cutoff like read_authors. This however is to further constrain the list. New values ARE NOT created, instead it filters by the sim_score column in the output file. An output file is created using the 'root' argument that specifies the folder/file prefix for the output. The final file will be appended with '_final.csv'.
#'
#' @param review the `review` element from the list outputted by \code{authors_clean}
#' @param prelim the `prelim` element from the list outputted by \code{authors_clean}
#' @param  sim_score similarity score cut off point. Number between 0-1.
#' @export authors_refine

authors_refine <- function(review, prelim, sim_score = NULL) {

  if (length(review) == 0 || is.null(review) ||
     nrow(review) == 0){
warning("Authors data.frame is empty.
This likely means there are no authors that need to be handchecked.
Outputting the prelim file.")
    output <- prelim
    return(output)
  }
 #If sim score was empty than we just default to lowest value
  if (is.null(sim_score)) {
    sim_score <- min(review$similarity, na.rm = TRUE)
  }

  ##########################################
  review$groupID[!is.na(review$similarity) & review$similarity < sim_score] <-
    review$authorID[!is.na(review$similarity) & review$similarity < sim_score]

  for (i in unique(review$authorID)) {
    if (length(review$authorID[review$authorID == i]) > 1) {
      stop(paste0("Author ID: ", i, " is duplicated please change
                   in the author file and re run"))
    }

    prelim$groupID[prelim$authorID == i] <- review$groupID[review$authorID == i]
  }

  for (p in unique(prelim$groupID)) {
    prelim$AF[prelim$groupID == p] <-
      prelim$AF[sort(prelim$authorID[prelim$groupID == p])[1]]
  }

  data1 <- prelim[, c("authorID", "groupID", "AF", "author_order",
        "address", "university", "department", "postal_code", "city", "state",
    "country", "RP_address", "RI", "OI", "UT", "refID", "PT", "PY", "PU")]
  colnames(data1)[colnames(data1) == "AF"] <- "author_name"

  return(data1)
  }