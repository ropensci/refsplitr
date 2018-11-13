#' Matches authors together by considering common last names, university affiliations, and emails.
#'
#' \code{authors_match} This function requires a data.frame with 8 required columns: ID, full names, address, university, country, RI, OI, and email. It uses this information to match up common names and identify groups of people
#'
#' @param data data inpiut
#' @param sim_score sim score input

#' 
authors_match<-function(data, sim_score){

n.n <- data.frame(ID = data$authorID, 
                          unique.name = data$AF, groupID = NA, 
                          address = data$address, university = data$university, 
                          country = data$country, RI = data$RI, OI = data$OI, 
                          email = data$EM, first = NA, middle = NA, last = NA,
                          stringsAsFactors=FALSE)
n.n[, c("first", "middle", "last")] <- 
  t(vapply(as.character(n.n$unique.name), .split_names,character(3)))
n.n$first <- tolower(n.n$first)
n.n$middle <- tolower(n.n$middle)
n.n$last <- tolower(n.n$last)
n.n$university[n.n$university %in% c("No Affiliation", 
                                              "Could not be extracted")] <- NA
n.n$university <- tolower(as.character(n.n$university))
n.n$address[n.n$address %in% c("No Affiliation",
                                               "Could not be extracted")] <- NA
n.n$address <- tolower(as.character(n.n$address))
length(unique(n.n$groupID))
unique.oi <- n.n$OI[!is.na(n.n$OI) & is.na(n.n$groupID)]
unique.oi <- names(table(unique.oi))[table(unique.oi) > 1]
unique.oi <- unique.oi[!is.na(unique.oi)]
unique.oi <- as.character(unique.oi)

for (l in unique.oi) {
n.n$groupID[which(n.n$OI == l)] <- 
  min(n.n$ID[which(n.n$OI == l)])
}

unique.ri <- n.n$RI[!is.na(n.n$RI) & is.na(n.n$groupID)]
unique.ri <- names(table(unique.ri))[table(unique.ri) > 1]
unique.ri <- unique.ri[!is.na(unique.ri)]
unique.ri <- as.character(unique.ri)
for (l in unique.ri) {
  choice <- which(n.n$RI == l)
  groupid <- n.n$groupID[choice]
  groupid <- groupid[!is.na(groupid)]
  if (length(groupid) > 0) {
    groupid <- min(groupid)
  } else {
    groupid <- min(n.n$ID[choice], na.rm = TRUE)
  }
  
  n.n$groupID[which(n.n$RI == l)] <- groupid
}

unique.em <- n.n$email[!is.na(n.n$email) & 
                                 is.na(n.n$groupID)]
unique.em <- names(table(unique.em))[table(unique.em) > 1]
unique.em <- unique.em[!is.na(unique.em)]
unique.em <- as.character(unique.em)
for (l in unique.em) {
  choice <- which(n.n$email == l)
  groupid <- n.n$groupID[choice]
  groupid <- groupid[!is.na(groupid)]
  
  if (length(groupid) > 0) {
    groupid <- min(groupid)
  } else {
    groupid <- min(n.n$ID[choice], na.rm = TRUE)
  }
  n.n$groupID[which(n.n$email == l)] <- groupid
}



n.n$similarity <- NA
n.n$match_name <- NA
n.n$f.c <- nchar(n.n$first)
n.n$m.c <- nchar(n.n$middle)
n.n$f.i <- substr(n.n$first, 1, 1)
n.n$m.i <- substr(n.n$middle, 1, 1)
n.n$m.c[is.na(n.n$m.c)] <- 0
# match authors with the same first, last, and middle name
remain <- subset(n.n, !is.na(m.i) & f.c >= 1)[, c("ID", 
                                      "groupID", "first", "middle", "last")]
remain <- merge(subset(remain, is.na(groupID)), remain, by = c("first", 
                                                            "middle", "last"))
remain <- subset(remain, ID.x != ID.y)

if(nrow(remain)>0){
  dd <- data.frame(
    g.n = unique(paste(remain$first, remain$middle, remain$last, sep = ";")),
    first = NA,
    middle = NA,
    last = NA,
    stringsAsFactors = FALSE
  )
  
  dd$id <- seq_len(nrow(dd))
  
  dd[, c("first", "middle", "last")] <- do.call(rbind, strsplit(dd$g.n, ";"))
  
  remain <- merge(remain, subset(dd, select = -g.n), by = c("first", 
                                                            "middle", "last"))
  
  for (n in dd$id) {
    sub <- subset(remain, id == n)
    unique.id <- unique(sub$ID.x)
    if (!sum(is.na(sub$groupID.y)) == nrow(sub)) {
      groupid <- min(unique(sub$groupID.y), na.rm = TRUE)
    } else {
      groupid <- min(unique.id, na.rm = TRUE)
    }
    
    n.n$groupID[n.n$ID %in% unique.id] <- groupid
  }
  length(unique(n.n$groupID))
  #
}
unique.groupid <- n.n$ID[(n.n$m.c > 0 | 
                                    !is.na(n.n$university) | 
                                    !is.na(n.n$email)) & 
                                    is.na(n.n$groupID)]

for (p in unique.groupid) {
  #p<-1
  matched <- FALSE
  default.frame <- data.frame(ID = NA, first = NA, middle = NA, last = NA, 
             university = NA, email = NA, f.i = 0, address = NA, country = NA)
  match1 <- NA
  match2 <- NA
  match3 <- NA
  match4 <- NA
  name.df <- n.n[n.n$ID == p, ]
  # We need to create a dataframe of possible matching authors, 
  # so we dont run comparissons on obviously incorrect people
  n.n1 <- subset(n.n, (m.c > 0 | 
                                         !is.na(university) | 
                                         !is.na(email) | 
                                         !is.na(address)) & ID != p)
  
  n.n1 <- n.n1[substr(name.df$first, 1, 1) == 
                      n.n1$f.i & name.df$last == n.n1$last, ]
  if (nrow(n.n1) == 0) {
    n.n1 <- default.frame
  }
  if (!is.na(name.df$first) & nchar(name.df$first) == 1) {
    n.n1 <- n.n1[n.n1$f.i == name.df$first, ]
  }
  if (!is.na(name.df$first) & nchar(name.df$first) > 1) {
    n.n1 <- n.n1[n.n1$f.c == 1 | 
                                   n.n1$first == name.df$first, ]
  }
  # if middle initial is 1 letter
  if (!is.na(name.df$middle) & nchar(name.df$middle) == 1) {
    n.n1 <- n.n1[is.na(n.n1$middle) | 
                                   n.n1$m.i == name.df$middle, ]
  }
  # if has a middle name, must match
  if (!is.na(name.df$middle) & nchar(name.df$middle) > 1) {
    n.n1 <- n.n1[is.na(n.n1$middle) | 
                                   n.n1$m.i == name.df$m.i, ]
  }
  # if has a country, it must match
  if (!is.na(name.df$country)) {
    n.n1 <- n.n1[is.na(n.n1$country) | 
                                   n.n1$country == name.df$country, ]
  }
  
  if (nrow(n.n1) == 0) {
    n.n1 <- default.frame
  }
  if (!anyNA(n.n1$ID)) {
    # match full middle name
    match1 <- !is.na(name.df$middle) & n.n1$middle == name.df$middle
    # match addresses
    match2 <- (!is.na(n.n1$university) & !is.na(name.df$university)) & 
      name.df$university == n.n1$university
    # match middle initial
    match3 <- !is.na(name.df$m.i) & n.n1$m.i == name.df$m.i
    match4 <- is.na(name.df$address) & n.n1$address == name.df$address
    if (sum(ifelse(is.na(c(match1, match2, match3, match4)), FALSE, 
                   c(match1, match2, match3, match4))) > 0) {
      matched <- TRUE
      choice <- c(which(match1), which(match2), which(match3), which(match4))
      if (sum(!is.na(n.n1$groupID[choice])) > 0) {
        groupid <- min(n.n1$groupID[choice], na.rm = TRUE)
      } else {
        groupid <- min(n.n1$ID[choice], na.rm = TRUE)
      }
      n.n$groupID[n.n$ID == p] <- groupid
    }
  }
  
  # Jaro_winkler
  if (!matched & nrow(n.n1) > 0 & !anyNA(n.n1$ID)) {
    jw_m <- RecordLinkage::jarowinkler(paste0(n.n1$last, 
                                      n.n1$first, n.n1$middle),
                          paste0(name.df$last, name.df$first, name.df$middle))
    choice <- which(max(jw_m) == jw_m)[1]
    
    if (sum(!is.na(n.n1$groupID[choice])) > 0) {
      groupid <- min(n.n1$groupID[choice], na.rm = TRUE)
      groupname <- n.n1$unique.name[n.n1$groupID == groupid][1]
    } else {
      groupid <- min(n.n1$ID[choice], na.rm = TRUE)
      groupname <- n.n1$unique.name[n.n1$ID == groupid]
    }
    
    n.n$groupID[n.n$ID == p] <- groupid
    n.n$match_name[n.n$ID == p] <- as.character(groupname)

    if (sum(choice) > 0) {
      n.n$similarity[n.n$ID == p] <- jw_m[choice][1]
    }
  }
  ############################### Clock#################################
  total <- length(unique.groupid)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  utils::setTxtProgressBar(pb, which(p == unique.groupid))
  utils::flush.console()
  #######################################################################
}
# Fixes a small issue where sometimes matched names using Jaro_winkler 
# will get a groupID distinction but that the author
# it matched with will get a different groupID if it has 
# a more perfect match criteria.
n.n$groupID[is.na(n.n$groupID)] <- n.n$ID[is.na(n.n$groupID)]
quick.check <- n.n$ID[!is.na(n.n$similarity)]
for (m in quick.check) {
  # m<-quick.check[1]
n.n$groupID[n.n$ID == m] <-n.n$groupID[n.n$ID == 
                          n.n$groupID[n.n$ID == m]]
}

length(unique(n.n$groupID))

# Time to prune the results. Weve used the vast network of knowledge to 
# match up complexes, now we'll trim them by splitting any complexes with 
# non matching 
# First and last initials
# n.n<-n.n1
unique.names.over1 <- unique(n.n$groupID)[table(n.n$groupID) >1]
print("Pruning groupings...")
for (n in unique.names.over1) {
   #n<-140
  sub <- subset(n.n, groupID == n & is.na(similarity))
  # check first names
  # including the NAs
  fi.check <- unique(sub$f.i)
  if (length(fi.check) > 1) {
    newfi <- fi.check[2:length(fi.check)]
    newGroupID <- vapply(newfi, function(x) sub$ID[sub$f.i == x][1],numeric(1))
    
    for (p in seq_len(length(newfi))) {
      n.n$groupID[n.n$ID %in% sub$ID & n.n$f.i == 
                 newfi[p] & n.n$groupID == n] <- newGroupID[p]
    }
  }
  # now handle Middle initials without NA.
  sub <- subset(sub, !is.na(m.i))
  
  mi.check <- unique(paste(sub$f.i, sub$m.i))
  if (length(mi.check) > 1) {
    newmi <- mi.check[2:length(mi.check)]
    newGroupID1 <- vapply(newmi, function(x) sub$ID[sub$f.i == substr(x, 1, 1) &
                                            sub$m.i == substr(x, 3, 3)][1],
                          numeric(1))
    for (q in seq_len(length(newfi))) {
      n.n$groupID[n.n$ID %in% sub$ID & n.n$f.i == 
      substr(newmi[q], 1, 1) & n.n$m.i == 
        substr(newmi[q], 3, 3)] <-  newGroupID[q]
    }
  }
}



return(n.n)
}
