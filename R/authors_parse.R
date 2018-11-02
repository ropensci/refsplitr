#' Parses out each individual authors information from the reference information created by references_read. This is the first step in parsing out author information.
#'
#' \code{authors_clean} This function takes the output from `references_read()` and cleans the author information.
#'
#' @param references output from `references_read()`
.authors_parse<-function(references){
list1 <- list()
############################################

for (ref in seq_along(references$refID)) {
  #ref<-1
  if (ref == 1) {
    print("Splitting author records")
  }
  
  # Split out authors and author emails
  authors_AU <- as.character(unlist(strsplit(references[ref, ]$AU, "\n")))
  authors_AF <- as.character(unlist(strsplit(references[ref, ]$AF, "\n")))
  
  authors_EM <- unlist(strsplit(references[ref, ]$EM, "\n"))
  authors_EM_strip <- substr(authors_EM, 1, regexpr("@", authors_EM) - 1)
  
  ###########
  # makes a datframe of authors as they will be used as a reference later
  
  authors_df <- data.frame(AU = authors_AU, AF = authors_AF, author_order = seq_along(authors_AU), stringsAsFactors = FALSE)
  ###########
  # Split out Addresses
  C1 <- unlist(strsplit(references[ref, ]$C1, "/"))
  if (length(authors_AU) == 1) {
    C1 <- paste0("[", authors_AU, "] ", C1)
  }
  
  C1 <- C1[grepl("^\\[.*\\]", C1)]
  # Split names from the addresses they're associated with
  C1_names <- regmatches(C1, regexpr("^\\[.*\\]", C1))
  C1_names <- substr(C1_names, 2, nchar(C1_names) - 1)
  if (length(authors_AU) == 1) {
    C1_names <- authors_AU
  }
  
  # Split out the addresses and not the names assocated
  C1_address <- gsub("^\\[.*\\] (.*)$", "\\1", C1)
  
  # create a dataframe of all unique addresses and their matching affiliations
  dd <- data.frame(C1_names, C1_address, stringsAsFactors = FALSE)
  dd1 <- data.frame(
    names = unique(unlist(strsplit(
      C1_names,
      "; "
    ))),
    address = vapply(
      unique(unlist(strsplit(
        C1_names,
        "; "
      ))),
      function(x)
        dd$C1_address[grepl(x, dd$C1_names)][1], character(1)
    )
  )
  if (nrow(dd1) == 0 & length(C1_address) == length(authors_AU)) {
    dd1 <- data.frame(names = authors_AU, address = C1_address)
  }
  if (nrow(dd1) == 0) {
    dd1 <- data.frame(names = authors_df$AF, address = "Could not be extracted")
  }
  dd1$address[dd1$address == "NA"] <- NA
  ###########
  # Split out Reprint Author information
  RP <- unlist(strsplit(references[ref, ]$RP, "\n"))
  RP_address <- gsub("^.*\\(reprint author\\), (.*)$", "\\1", RP)
  RP_df <- data.frame(AU = substr(RP, 1, regexpr("(reprint author)", RP)[1] - 3), RP_address)
  
  ##########
  # Split out RI information
  RI <- unlist(strsplit(references[ref, ]$RI, ";"))
  
  if (length(strsplit(RI, "/")) < 2) {
    RI_df <- data.frame(RI_names = "", RI = "", matchname = "")
  }
  
  if (length(strsplit(RI, "/")) > 1) {
    RI_df <- data.frame(do.call(rbind, lapply(strsplit(RI, "/"), function(x) x[1:2])), stringsAsFactors = FALSE)
    # Match the author names to the RIs affiliations, this is not as exact as you think it would be
    colnames(RI_df) <- c("RI_names", "RI")
    
    match_ri <- vapply(RI_df[, 1], function(x) {
      jw <- RecordLinkage::jarowinkler(x, authors_AU)
      jw == max(jw) & jw > 0.8
    },logical(length(authors_AU)))
    
    
    if (length(authors_AU) > 1) {
      RI_df$matchname <- unlist(apply(match_ri, 2, function(x) ifelse(sum(x) == 0, "", authors_AU[x])))
    }
    if (length(authors_AU) == 1) {
      RI_df$matchname <- ifelse(sum(match_ri) == 0, "", authors_AU[match_ri])
    }
  }
  
  # RI
  ##########
  # split out the OI and do the same thing we did with the RI
  OI <- unlist(strsplit(references[ref, ]$OI, ";"))
  
  if (sum(is.na(OI)) == 0) {
    OI_df <- as.data.frame(do.call(rbind, strsplit(OI, "/")), stringsAsFactors = FALSE)
    colnames(OI_df) <- c("OI_names", "OI")
    match_OI <- vapply(OI_df[, 1], function(x) {
      jw <- RecordLinkage::jarowinkler(x, authors_AU)
      jw == max(jw) & jw > 0.8
    },logical(length(authors_AU)))
    
    OI_df$matchname <- unlist(apply(data.frame(match_OI), 2, function(x) ifelse(sum(x) == 0, "", authors_AU[x])))
  }
  
  if (sum(is.na(OI)) > 0) {
    OI_df <- data.frame(OI_names = "", OI = "", matchname = "")
  }
  ############
  # merge all this information together by author name, some journals use the full name some the shortend name
  #########
  new <- merge(authors_df, dd1, by.x = "AU", by.y = "names", all.x = TRUE)
  if (sum(is.na(new$address)) == length(new$address)) {
    new <- merge(authors_df, dd1, by.x = "AF", by.y = "names", all.x = TRUE)
  }
  new <- merge(new, RP_df, by = "AU", all.x = TRUE)
  new <- merge(new, RI_df[, c("RI", "matchname")], by.x = "AU", by.y = "matchname", all.x = TRUE)
  new <- merge(new, OI_df[, c("OI", "matchname")], by.x = "AU", by.y = "matchname", all.x = TRUE)
  new$EM <- NA
  new$refID <- references$refID[ref]
  new$TA <- references$TI[ref]
  new$SO <- references$SO[ref]
  new$UT <- references$UT[ref]
  new$PT <- references$PT[ref]
  new$PU <- references$PU[ref]
  new$PY <- references$PY[ref]
  
  ######################### 3
  #
  # Matching emails is an imprecise science, as emails dont have to match names in any reliable way or at all
  # I feel its better to leave these alone as much as possible, analyzing the resulting matches will lead to issues
  match_em <- vapply(authors_EM_strip, function(x) apply(data.frame(RecordLinkage::jarowinkler(x, new$AU), RecordLinkage::jarowinkler(x, new$AF)), 1, max),double(length(new$AU)))
  
  for (i in seq_along(authors_EM)) {
    # i<-1
    new$EM[as.data.frame(apply(as.matrix(match_em), 2, function(x) x > 0.7 & max(x) == x))[, i]] <- authors_EM[i]
  }
  
  if (nrow(new) == 1) {
    new$EM <- authors_EM[1]
  }
  list1[[ref]] <- new
  
  ############################### Clock#############################################
  
  total <- nrow(references)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  utils::setTxtProgressBar(pb, ref)
  utils::flush.console()
  #################################################################################
}

# Bind all author iterations together into one large sheet that should be used for base analysis from here on out
final <- do.call(rbind, list1)
final$authorID <- seq_len(nrow(final))
final$EM <- tolower(final$EM)
final$address <- as.character(final$address)
final$RP_address <- as.character(final$RP_address)
final$address[!is.na(final$address) & final$address == "Could not be extracted" & !is.na(final$RP_address)] <- final$RP_address[!is.na(final$address) & final$address == "Could not be extracted" & !is.na(final$RP_address)]
final$address <- as.character(final$address)
final$address[is.na(final$address)] <- "Could not be extracted"
return(final)
}
##########################
###############################