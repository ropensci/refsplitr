##################################################
##################################################
## 	BEGIN: authors_refine():
#' Refines the authors code output from authors_clean()
#'
#' \code{authors_refine} This function takes the author list output after the output has been synthesized for incorrect author matches. It contains a similarity score cutoff like read_authors. This however is to further constrain the list. New values ARE NOT created, instead it filters by the sim_score column in the output file. An output file is created using the 'root' argument that specifies the folder/file prefix for the output. The final file will be appended with '_final.csv'.
#'
#' @param authors the `authors` element from the list outputted by `authors_clean()`
#' @param master the `master` element from the list outputted by `authors_clean()`
#' @param  sim_score similarity score cut off point. Number between 0-1.
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_authors.csv" and "_authors_references.csv" will be appended and the
## so what you need to do is take the authors file and change the names (no au and af), with addresses, title, and authors
authors_refine <- function(authors, master, sim_score = NULL, filename_root = "") {
  if (is.null(sim_score)) {
    sim_score <- min(authors$similarity, na.rm = T)
  }
  ##########################################
  # Beginning Checks
  ##########################################
  if(nrow(authors)==0){  print('Authors data.frame is empty. This likely means there are no authors that need to be handchecked, outputting the master file'); return(output<-master)}
  
  ##########################################
  authors$groupID[!is.na(authors$similarity) & authors$similarity < sim_score] <- authors$authorID[!is.na(authors$similarity) & authors$similarity < sim_score]

  for (i in unique(authors$authorID)) {
    # i<-unique(authors$authorID)[2]

    if (length(authors$authorID[authors$authorID == i]) > 1) {
      print(paste0("Author ID: ", i, " is duplicated please change in the author file and re run"))
      break
    }

    master$groupID[master$authorID == i] <- authors$groupID[authors$authorID == i]
  }

  for (p in unique(master$groupID)) {
    master$AF[master$groupID == p] <- master$AF[sort(master$authorID[master$groupID == p])[1]]
  }
  colnames(master)
  data1 <- master[, c("authorID", "groupID", "AF", "author_order", "address", "university", "department", "postal_code", "country", "RP_address", "RI", "OI", "UT", "refID", "PT", "PY", "PU")]
  colnames(data1)[colnames(data1) == "AF"] <- "author_name"

  # write it out
  if (filename_root != "") {
    write.csv(data1,
      file = paste0(filename_root, "_authors_final.csv"),
      row.names = FALSE
    )
  }
  #
  return(data1)
}
