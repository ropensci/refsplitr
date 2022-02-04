#' Refines the authors code output from authors_clean()
#'
#' \code{authors_refine} This function takes the author list output after the 
#' output has been synthesized for incorrect author matches. It contains a 
#' similarity score cutoff like read_authors. This however is to further 
#' constrain the list. New values ARE NOT created, instead it filters by the
#' sim_score column in the output file. 
#' 
#'
#' @param review the `review` element from list output by \code{authors_clean}
#' @param prelim the `prelim` element from list output by \code{authors_clean}
#' @param  sim_score similarity score cut off point. Number from 0-1.
#' @param confidence confidence score cut off point. Number from 0 - 10.
#' 
#' @examples 
#' ## First gather the authors data.frame from authors_clean
#' data(BITR)
#' BITR_authors <- authors_clean(BITR)
#' BITR_review_df <- BITR_authors$review 
#' BITR_prelim_df <- BITR_authors$prelim
#' 
#' ## If accepting prelim disambiguation from authors_clean() without review:
#' 
#' refine_df <- authors_refine(BITR_review_df, BITR_prelim_df, 
#' sim_score = 0.90, confidence = 5)
#' 
#' ## Note that 'sim_score' and 'confidence' are optional arguments and are
#' ## only required if changing the default values. 
#' refine_df <- authors_refine(BITR_review_df, BITR_prelim_df)
#' 
#' 
#' ## If changes were made to groupID or authorID in the "_review.csv" file: 
#' ## then incorporate those changes in a text editor, save the corrections as
#' ## a new file name, load in to R and run `authors_refine()` with the 
#' ## new corrections as the review arguement.
#'  
#' @export authors_refine
#' 
authors_refine <- function(review, prelim,
                           sim_score = NULL,
                           confidence = NULL) {
 # if(is.data.frame(review)){ stop('review object is not a data.frame') }
 # if(is.data.frame(prelim)){ stop('prelim object is not a data.frame') }
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

  if (is.null(confidence)) {
    confidence <- 0
  }
  ##########################################
  review$groupID[!is.na(review$similarity) & review$similarity < sim_score &
      !is.na(confidence) & review$confidence > confidence ] <-
    review$authorID[!is.na(review$similarity) & review$similarity < sim_score &
        !is.na(confidence) & review$confidence > confidence]

  for (i in unique(review$authorID)) {
    if (length(review$authorID[review$authorID == i]) > 1) {
      error <- paste0(
        "Author ID: ",
        i,
        " is duplicated please change in the author file and re run")
      stop(error)
    }

    prelim$groupID[prelim$authorID == i] <- review$groupID[review$authorID == i]
  }

  for (p in unique(prelim$groupID)) {
    prelim$AF[prelim$groupID == p] <-
      prelim$AF[sort(prelim$authorID[prelim$groupID == p])[1]]
  }

  prelim <- prelim[, c("authorID", "groupID", "AF", "author_order",
    "address", "university", "department", "postal_code", "city", "state",
    "country", "RP_address", "RI", "OI", "UT", "refID", "PT",
    "PY", "PU")
    ]

  colnames(prelim)[colnames(prelim) == "AF"] <- "author_name"

  return(prelim)
}
