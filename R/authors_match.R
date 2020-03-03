#' Matches authors together by considering common last names, 
#' university affiliations, and emails.
#' This is an internal function used by \code{authors_clean}
#' 
#' \code{authors_match} This function requires a data.frame with 8 required 
#' columns: ID, full names, address, university, country, RI, OI, and email. 
#' It uses this information to match up common names and identify groups of 
#' people. Jaro-winkler scores are calcualted for non-easily matched names
#' @param data data input
#' @importFrom stats na.omit
#' @noRd
 
authors_match <- function(data){
  message("\nMatching authors\n")
  # Get the data.frame ready to be analyzed. Convert, change to NAs, etc
  n_n <- data.frame(ID = data$authorID,
    unique_name = data$AF, groupID = NA,
    address = data$address, university = data$university,
    country = data$country, RI = data$RI, OI = data$OI,
    email = data$EM, first = NA, middle = NA, last = NA,
    jarowsAsFactors = FALSE)
  n_n[, c("first", "middle", "last")] <-
    t(vapply(as.character(n_n$unique_name), split_names, character(3)))
  n_n$first <- tolower(n_n$first)
  n_n$middle <- tolower(n_n$middle)
  n_n$last <- tolower(n_n$last)
  n_n$university[n_n$university %in% c("No Affiliation",
    "Could not be extracted")] <- NA
  n_n$university <- tolower(as.character(n_n$university))
  n_n$address[n_n$address %in% c("No Affiliation",
    "Could not be extracted")] <- NA
  n_n$address <- tolower(as.character(n_n$address))

  n_n$email <- gsub("\\n| ", "", n_n$email)
  # Now time to match by orcID first. This seems most logical
  unique_oi <- n_n$OI[!is.na(n_n$OI) & is.na(n_n$groupID)]
  unique_oi <- names(table(unique_oi))[table(unique_oi) > 1]
  if (is.null(unique_oi)) unique_oi <- NA
  unique_oi <- unique_oi[!is.na(unique_oi)]
  unique_oi <- as.character(unique_oi)

  for (l in unique_oi) {
    n_n$groupID[which(n_n$OI == l)] <-
      min(n_n$ID[which(n_n$OI == l)])
  }
  # Now match by much less used RI
  unique_ri <- n_n$RI[!is.na(n_n$RI) & is.na(n_n$groupID)]
  unique_ri <- names(table(unique_ri))[table(unique_ri) > 1]
  if (is.null(unique_ri)) unique_ri <- NA
  unique_ri <- unique_ri[!is.na(unique_ri)]
  unique_ri <- as.character(unique_ri)
  for (l in unique_ri) {
    choice <- which(n_n$RI == l)
    groupid <- n_n$groupID[choice]
    groupid <- groupid[!is.na(groupid)]
    if (length(groupid) > 0) {
      groupid <- min(groupid)
    } else {
      groupid <- min(n_n$ID[choice], na.rm = TRUE)
    }

    n_n$groupID[which(n_n$RI == l)] <- groupid
  }
  # Maybe be moved later to be included as a minor grouping variable
  # On the same level as a middle initial etc. Currently is major variable
  unique_em <- n_n$email[!is.na(n_n$email) & is.na(n_n$groupID)]
  unique_em <- names(table(unique_em))[table(unique_em) > 1]
  if ( is.null(unique_em) ) unique_em <- NA
  unique_em <- unique_em[!is.na(unique_em)]
  unique_em <- as.character(unique_em)

  for (l in unique_em) {
    choice <- which(n_n$email == l)
    groupid <- n_n$groupID[choice]
    groupid <- groupid[!is.na(groupid)]

    if (length(groupid) > 0) {
      groupid <- min(groupid)
    } else {
      groupid <- min(n_n$ID[choice], na.rm = TRUE)
    }
    n_n$groupID[which(n_n$email == l)] <- groupid
  }
  # Now run character counts to account for
  # the fact names are sometimes stored as only initials
  n_n$similarity <- NA
  n_n$match_name <- NA
  n_n$f_c <- nchar(n_n$first)
  n_n$m_c <- nchar(n_n$middle)
  n_n$f_i <- substr(n_n$first, 1, 1)
  n_n$m_i <- substr(n_n$middle, 1, 1)
  n_n$m_c[is.na(n_n$m_c)] <- 0

  # match authors with the same first, last, and middle name

  remain <- subset(n_n, !is.na(m_i) & f_c > 1)[,
    c("ID", "groupID", "first", "middle", "last")
    ]
  remain <- merge(subset(remain, is.na(groupID)),
    remain, by = c("first", "middle", "last"))
  remain <- subset(remain, ID.x != ID.y)

  if (nrow(remain) > 0) {
    dd <- data.frame(
      g_n = unique(paste(remain$first, remain$middle, remain$last, sep = ";")),
      first = NA,
      middle = NA,
      last = NA,
      stringsAsFactors = FALSE
    )

    dd$id <- seq_len(nrow(dd))

    dd[, c("first", "middle", "last")] <- do.call(rbind, strsplit(dd$g_n, ";"))

    remain <- merge(remain, subset(dd, select = -g_n),
      by = c("first", "middle", "last"))

    for (n in dd$id) {
      sub <- subset(remain, id == n)
      unique_id <- unique(sub$ID.x)
      if (!sum(is.na(sub$groupID.y)) == nrow(sub)) {
        groupid <- min(unique(sub$groupID.y), na.rm = TRUE)
      } else {
        groupid <- min(unique_id, na.rm = TRUE)
      }

      n_n$groupID[n_n$ID %in% unique_id] <- groupid
    }
  }
  # Need to prune the groups a bit and merge common names
  matched_df <- subset(n_n, !is.na(groupID))
  if (nrow(matched_df) > 1) {
    matched_df$squash <- paste(matched_df$last, matched_df$f_i, matched_df$m_i)
    matched_df$merged <- FALSE

    for (q in na.omit(unique(n_n$groupID))){

      if (any(matched_df$merged[matched_df$groupID == q])) next

      sub <- matched_df[matched_df$groupID == q, ]
      # common_df <- matched_df %>%
      #   dplyr::filter(
      #     squash %in% sub$squash &
      #       ( (f_c %in% 1) | (f_c > 1 & first %in% sub$first) ) &
      #       groupID != q
      #   )
      common_df <- subset(matched_df,
        squash %in% sub$squash &
          ( (f_c %in% 1) | (f_c > 1 & first %in% sub$first) ) &
          groupID != q
        )

      change <- unique(common_df$groupID)
      n_n$groupID[n_n$groupID %in% change] <- q
      matched_df$merged[matched_df$groupID %in% change] <- TRUE
    }
  } #end if
  # For the remaining names we'll use a grouping criteria
  # Where we need one more piece of information besides first and last name
  unique_groupid <- n_n$ID[(n_n$m_c > 0 |
      !is.na(n_n$university) |
      !is.na(n_n$email)) &
      is.na(n_n$groupID)]
  unique_groupid <- n_n$ID[is.na(n_n$groupID)]
  n_n$matchID <- NA
  for (p in unique_groupid) {
    #for (p in unique_groupid[1:(which(unique_groupid==441)-1)]) {
    matched <- FALSE
    default_frame <- data.frame(ID = NA, first = NA, middle = NA, last = NA,
      university = NA, email = NA, f_i = 0, f_c = 0,
      m_c = 0, address = NA, country = NA)
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
    n_n1 <- subset(n_n, (is.na(groupID) | groupID != name_df$ID) & ID != p)

    n_n1 <- n_n1[name_df$f_i ==
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

    if (nrow(n_n1) == 0) {
      n_n1 <- default_frame
    }
    if (!anyNA(n_n1$ID)) {
      # match full middle name
      match1 <- !is.na(name_df$middle) &
        n_n1$middle == name_df$middle &
        n_n1$m_c > 1
      # match addresses
      match2 <- (!is.na(n_n1$university) &
          !is.na(name_df$university)) &
        name_df$university == n_n1$university
      # match middle initial
      match3 <- !is.na(name_df$m_i) &
        n_n1$m_i == name_df$m_i &
        n_n1$f_c > 1 & name_df$f_c > 1
      # match by address
      match4 <- !is.na(name_df$address) & n_n1$address == name_df$address
      if (sum(ifelse(is.na(c(match1, match2, match3, match4)), FALSE,
        c(match1, match2, match3, match4))) > 0) {
        matched <- TRUE
        choice <- c(which(match1), which(match2),
          which(match3), which(match4))
        if (sum(!is.na(n_n1$groupID[choice])) > 0) {
          groupid <- min(n_n1$groupID[choice], na.rm = TRUE)
          n_n$matchID[n_n$ID == p] <- min(n_n1$ID[choice], na.rm = TRUE)
        } else {
          groupid <- min(n_n1$ID[choice], na.rm = TRUE)
          n_n$matchID[n_n$ID == p] <- groupid
        }
        n_n$groupID[n_n$ID == p] <- groupid
      }
    }

    # Remaining names are run with a Jaro_winkler similarity score
    if (!matched & nrow(n_n1) > 0 & !anyNA(n_n1$ID)) {
      if (!is.na(name_df$m_i) & any(na.omit(n_n1$m_i) == name_df$m_i)) {
        n_n1 <- n_n1[!is.na(n_n1$m_i) & n_n1$m_i == name_df$m_i, ]
      }

      jw_m <- stringdist::stringsim(
        paste0(n_n1$last, n_n1$first, n_n1$middle),
        paste0(name_df$last, name_df$first, name_df$middle,
               method = "jw", useBytes = TRUE, p=0.1)
      )

      cc <- rep(TRUE, length(jw_m))

      if (!is.na(name_df$country) & name_df$country %in% n_n1$country) {
        cc <- !is.na(n_n1$country) & n_n1$country == name_df$country
      }

      # Need to prioritize this match down the line
      # country -> full names -> jw
      choice <- seq_along(jw_m)[cc]
      nninf <- apply(n_n1[choice, c("address", "RI", "OI", "email")],
        1, function(x) sum(!is.na(x))
      ) +
        as.numeric(n_n1$f_c[choice] > 1) + as.numeric(n_n1$m_c[choice] > 0)

      if (length(choice) > 1) {
        choice <- choice[nninf == max(nninf)]
      }
      if (length(choice) > 1) {
        choice <- choice[jw_m[choice] == max(jw_m[choice])][1]
      }
      jw_m <- jw_m[choice]

      if (sum(!is.na(n_n1$groupID[choice])) > 0) {
        groupid <- min(n_n1$groupID[choice], na.rm = TRUE)
        groupname <- n_n1$unique_name[n_n1$groupID == groupid][1]
        matchID <- n_n1$ID[n_n1$groupID == groupid][1]
      } else {
        groupid <- min(n_n1$ID[choice], na.rm = TRUE)
        groupname <- n_n1$unique_name[n_n1$ID == groupid]
        matchID <- n_n1$ID[n_n1$ID == groupid]
      }

      n_n$groupID[n_n$ID == p] <- groupid
      n_n$match_name[n_n$ID == p] <- as.character(groupname)
      n_n$matchID[n_n$ID == p] <- matchID

      if (sum(choice) > 0) {
        n_n$similarity[n_n$ID == p] <- jw_m
      }
    }
    ############################### Clock#################################
    total <- length(unique_groupid)
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    utils::setTxtProgressBar(pb, which(p == unique_groupid))
    utils::flush.console()
    #######################################################################
  }
  #########################################################################
  # Time to prune the results. Weve used the vast network of knowledge to
  # match up author complexes, now we'll trim them by splitting any complexes
  # with non matching first and last initials. As well as group complexes
  # Fixes a small issue where sometimes matched names using Jaro_winkler
  # will get a groupID distinction but that the author
  # it matched with will get a different groupID if it has
  # a more perfect match criteria.
  n_n$groupID[is.na(n_n$groupID)] <- n_n$ID[is.na(n_n$groupID)]
  message("\nPruning groupings...\n")
  quick_check <- n_n$ID[!is.na(n_n$similarity)]
  for (m in quick_check) {
    n_n$groupID[n_n$ID == m] <- n_n$groupID[
      n_n$ID == n_n$groupID[n_n$ID == m]]
  }

  # group complexes
  c_t <- data.frame(table(subset(n_n, ID != groupID)$groupID))
  c_t$Var1 <- as.numeric(as.character(c_t$Var1))
  n_n$merged <- FALSE
  for (r in c_t$Var1[c_t$Freq > 0]){

    if (!any(n_n$groupID == r )) next
    l1 <- n_n$ID == r | n_n$groupID == r
    l2 <- n_n$groupID %in% n_n$groupID[l1] | l1
    l3 <- n_n$ID %in% n_n$matchID[l2] | l2
    l4 <- n_n$groupID %in% n_n$ID[n_n$groupID == r] | l3
    n_n$groupID[l4] <- min(n_n$groupID[l4])

  }
  n_n$flagged <- 0
  unique_names_over1 <- unique(n_n$groupID)[table(n_n$groupID) > 1]
  for (n in unique_names_over1) {

    sub <- subset(n_n, groupID == n & is.na(similarity))
    # check first names
    # including the NAs
    fi_check <- unique(sub$f_i)
    newfi <- NA
    if (length(fi_check) > 1) {
      newfi <- fi_check[2:length(fi_check)]
      newGroupID <- vapply(newfi, function(x) {
        sub$ID[sub$f_i == x][1]
      }, numeric(1))

      for (p in seq_len(length(newfi))) {
        n_n$groupID[n_n$ID %in%
            sub$ID & n_n$f_i == newfi[p] & n_n$groupID == n ] <- newGroupID[p]
      }
    }
    # now handle Middle initials without NA.
    sub <- subset(sub, !is.na(m_i))
    mi_check <- unique(paste(sub$f_i, sub$m_i))
    if (length(mi_check) > 1) {
      newmi <- mi_check[2:length(mi_check)]

      newGroupID <- vapply(newmi, function(x){
        sub$ID[
          sub$f_i == substr(x, 1, 1) & sub$m_i == substr(x, 3, 3)
          ][1]
      }, numeric(1))

      for (q in seq_len(length(newfi))) {
        n_n$groupID[n_n$ID %in% sub$ID & n_n$f_i ==
            substr(newmi[q], 1, 1) & n_n$m_i ==
            substr(newmi[q], 3, 3)] <- newGroupID[q]
      }
    }
    # While we're here, find groups with FI,MI that
    # do not match and flag for review
    sub <- subset(n_n, groupID == n & m_c > 0)
    sub$squash <- paste0(sub$f_i, sub$m_i)
    if ( length(unique(sub$squash)) > 1) {
      n_n$flagged[ n_n$groupID == n ] <- 1
    }
  }
  ########
  # #Make grouping criteria
  n_n$confidence <- NA
  unique_names_over2 <- n_n$ID[!is.na(n_n$similarity)]
  for (q in unique_names_over2) {
    author1 <- subset(n_n, ID == q)
    group2 <- subset(n_n, groupID == author1$groupID & ID != q )
    author_lc <- nchar(author1$last)
    sc1 <- sum(author1$f_c > 1) * 1
    sc2 <- sum(author_lc > 6) * 1
    sc3 <- sum(author_lc > 10) * 2
    sc4 <- sum(any(group2$f_c > 1))
    sc5 <- sum(!is.na(author1$postal_code) &
        author1$postal_code %in% group2$postal_code) * 4
    sc6 <- sum(!is.na(author1$country) &
        author1$country %in% group2$country) * 2
    sc7 <- sum(grepl("-", author1$unique_name)) * 2
    sc8 <- sum(any(group2$m_c > 0)) * 1
    sc9 <- sum(author1$m_c > 0) * 1
    sc10 <- sum(c(
      any(!is.na(author1$univeristy)),
      any(!is.na(author1$email)),
      any(!is.na(group2$univeristy)),
      any(!is.na(group2$email))))
    confidence <- sc1 + sc2 + sc3 + sc4 + sc5 + sc6 + sc7 + sc8 + sc9 + sc10
    n_n$confidence[n_n$ID == q] <- ifelse(confidence > 10, 10, confidence)
  }
  return(n_n)
}
