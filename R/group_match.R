## new function for group matching

for (p in unique_groupid) {
  matched <- FALSE
  default_frame <- data.frame(ID = NA, first = NA, middle = NA, last = NA,
                              university = NA, email = NA, f_i = 0,
                              address = NA, country = NA)
  match1 <- NA
  match2 <- NA
  match3 <- NA
  match4 <- NA
  name_df <- n_n[n_n$ID == p, ]
  # We need to create a dataframe of possible matching authors,
  # so we dont run comparissons on obviously incorrect people
  n_n1 <- subset(n_n, (m_c > 0 |
                         !is.na(university) |
                         !is.na(email) |
                         !is.na(address)) &
                   ID != p)
  n_n1<-subset(n_n, ID != p)
  
  n_n1 <- n_n1[substr(name_df$first, 1, 1) ==
                 n_n1$f_i & name_df$last == n_n1$last, ]
  if (nrow(n_n1) == 0) {
    n_n1 <- default_frame
  }
  if (!is.na(name_df$first) & nchar(name_df$first) == 1) {
    n_n1 <- n_n1[n_n1$f_i == name_df$first, ]
  }
  if (!is.na(name_df$first) & nchar(name_df$first) > 1) {
    n_n1 <- n_n1[n_n1$f_c == 1 |
                   n_n1$first == name_df$first, ]
  }
  # if middle initial is 1 letter
  if (!is.na(name_df$middle) & nchar(name_df$middle) == 1) {
    n_n1 <- n_n1[is.na(n_n1$middle) |
                   n_n1$m_i == name_df$middle, ]
  }
  # if has a middle name, must match
  if (!is.na(name_df$middle) & nchar(name_df$middle) > 1) {
    n_n1 <- n_n1[is.na(n_n1$middle) |
                   n_n1$m_i == name_df$m_i, ]
  }
  # if has a country, it must match
  if (!is.na(name_df$country)) {
    n_n1 <- n_n1[is.na(n_n1$country) |
                   n_n1$country == name_df$country, ]
  }
  
  if (nrow(n_n1) == 0) {
    n_n1 <- default_frame
  }
  if (!anyNA(n_n1$ID)) {
    # match full middle name
    match1 <- !is.na(name_df$middle) & n_n1$middle == name_df$middle
    # match addresses
    match2 <- (!is.na(n_n1$university) & !is.na(name_df$university)) &
      name_df$university == n_n1$university
    # match middle initial
    match3 <- !is.na(name_df$m_i) & n_n1$m_i == name_df$m_i
    # match by address
    match4 <- is.na(name_df$address) & n_n1$address == name_df$address
    if (sum(ifelse(is.na(c(match1, match2, match3, match4)), FALSE,
                   c(match1, match2, match3, match4))) > 0) {
      matched <- TRUE
      choice <- c(which(match1), which(match2), which(match3), which(match4))
      if (sum(!is.na(n_n1$groupID[choice])) > 0) {
        groupid <- min(n_n1$groupID[choice], na.rm = TRUE)
      } else {
        groupid <- min(n_n1$ID[choice], na.rm = TRUE)
      }
      n_n$groupID[n_n$ID == p] <- groupid
    }
  }
  
  # Remaining names are run with a Jaro_winkler similarity score
  if (!matched & nrow(n_n1) > 0 & !anyNA(n_n1$ID)) {
    jw_m <- RecordLinkage::jarowinkler(paste0(n_n1$last,
                                              n_n1$first, n_n1$middle),
                                       paste0(name_df$last, name_df$first, name_df$middle)
    )
    choice <- which(max(jw_m) == jw_m)[1]
    
    if (sum(!is.na(n_n1$groupID[choice])) > 0) {
      groupid <- min(n_n1$groupID[choice], na.rm = TRUE)
      groupname <- n_n1$unique_name[n_n1$groupID == groupid][1]
      # and now to decide a confidence score
      
      
    } else {
      groupid <- min(n_n1$ID[choice], na.rm = TRUE)
      groupname <- n_n1$unique_name[n_n1$ID == groupid]
    }
    
    n_n$groupID[n_n$ID == p] <- groupid
    n_n$match_name[n_n$ID == p] <- as.character(groupname)
    
    if (sum(choice) > 0) {
      n_n$similarity[n_n$ID == p] <- jw_m[choice][1]
    }
  }
  ############################### Clock#################################
  total <- length(unique_groupid)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  utils::setTxtProgressBar(pb, which(p == unique_groupid))
  utils::flush.console()
  #######################################################################
}
# Fixes a small issue where sometimes matched names using Jaro_winkler
# will get a groupID distinction but that the author
# it matched with will get a different groupID if it has
# a more perfect match criteria.
n_n$groupID[is.na(n_n$groupID)] <- n_n$ID[is.na(n_n$groupID)]
quick_check <- n_n$ID[!is.na(n_n$similarity)]
for (m in quick_check) {
  n_n$groupID[n_n$ID == m] <- n_n$groupID[
    n_n$ID == n_n$groupID[n_n$ID == m]
    ]
}