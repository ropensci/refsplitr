#' Matches authors together by considering common last names, university affiliations, and emails.
#'
#' \code{authors_match} This function requires a data.frame with 8 required columns: ID, full names, address, university, country, RI, OI, and email. It uses this information to match up common names and identify groups of people
#'
#' @param data data inpiut
#' @param sim_score sim score input

#' 
authors_match<-function(data, sim_score){
  #data=final;sim_score=0.88
novel.names <- data.frame(ID = data$authorID, unique.name = data$AF, groupID = NA, 
                          address = data$address, university = data$university, 
                          country = data$country, RI = data$RI, OI = data$OI, 
                          email = data$EM, first = NA, middle = NA, last = NA,
                          stringsAsFactors=F)
novel.names[, c("first", "middle", "last")] <- 
  t(vapply(as.character(novel.names$unique.name), .split_names,character(3)))
novel.names$first <- tolower(novel.names$first)
novel.names$middle <- tolower(novel.names$middle)
novel.names$last <- tolower(novel.names$last)
novel.names$university[novel.names$university %in% c("No Affiliation", "Could not be extracted")] <- NA
novel.names$university <- tolower(as.character(novel.names$university))
novel.names$address[novel.names$address %in% c("No Affiliation", "Could not be extracted")] <- NA
novel.names$address <- tolower(as.character(novel.names$address))
length(unique(novel.names$groupID))
unique.oi <- novel.names$OI[!is.na(novel.names$OI) & is.na(novel.names$groupID)]
unique.oi <- names(table(unique.oi))[table(unique.oi) > 1]
unique.oi <- unique.oi[!is.na(unique.oi)]
unique.oi <- as.character(unique.oi)
# unique.oi[nchar(unique.oi)!=19]
# unique.oi[unique.oi=="0000-0001-9549-4178"]
for (l in unique.oi) {
  # l<-"0000-0003-2589-826X"
  novel.names$groupID[which(novel.names$OI == l)] <- min(novel.names$ID[which(novel.names$OI == l)])
}
# length(unique(novel.names$groupID))

unique.ri <- novel.names$RI[!is.na(novel.names$RI) & is.na(novel.names$groupID)]
unique.ri <- names(table(unique.ri))[table(unique.ri) > 1]
unique.ri <- unique.ri[!is.na(unique.ri)]
unique.ri <- as.character(unique.ri)
# unique.ri[nchar(unique.ri)!=11]
for (l in unique.ri) {
  # l<-"E-7093-2013"
  choice <- which(novel.names$RI == l)
  groupid <- novel.names$groupID[choice]
  groupid <- groupid[!is.na(groupid)]
  if (length(groupid) > 0) {
    groupid <- min(groupid)
  } else {
    groupid <- min(novel.names$ID[choice], na.rm = TRUE)
  }
  
  novel.names$groupID[which(novel.names$RI == l)] <- groupid
}
# length(unique(novel.names$groupID))
# Need to place this later. Its currently a weak point, or atleast force a last name check.
unique.em <- novel.names$email[!is.na(novel.names$email) & is.na(novel.names$groupID)]
unique.em <- names(table(unique.em))[table(unique.em) > 1]
unique.em <- unique.em[!is.na(unique.em)]
unique.em <- as.character(unique.em)
for (l in unique.em) {
  choice <- which(novel.names$email == l)
  groupid <- novel.names$groupID[choice]
  groupid <- groupid[!is.na(groupid)]
  
  if (length(groupid) > 0) {
    groupid <- min(groupid)
  } else {
    groupid <- min(novel.names$ID[choice], na.rm = TRUE)
  }
  novel.names$groupID[which(novel.names$email == l)] <- groupid
}


# length(unique(novel.names$groupID))

novel.names$similarity <- NA
novel.names$match_name <- NA
novel.names$f.c <- nchar(novel.names$first)
novel.names$m.c <- nchar(novel.names$middle)
novel.names$f.i <- substr(novel.names$first, 1, 1)
novel.names$m.i <- substr(novel.names$middle, 1, 1)
novel.names$m.c[is.na(novel.names$m.c)] <- 0
# match authors with the same first, last, and middle name
remain <- subset(novel.names, !is.na(m.i) & f.c >= 1)[, c("ID", "groupID", "first", "middle", "last")]
remain <- merge(subset(remain, is.na(groupID)), remain, by = c("first", "middle", "last"))
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
  
  remain <- merge(remain, subset(dd, select = -g.n), by = c("first", "middle", "last"))
  
  for (n in dd$id) {
    sub <- subset(remain, id == n)
    unique.id <- unique(sub$ID.x)
    if (!sum(is.na(sub$groupID.y)) == nrow(sub)) {
      groupid <- min(unique(sub$groupID.y), na.rm = TRUE)
    } else {
      groupid <- min(unique.id, na.rm = TRUE)
    }
    
    novel.names$groupID[novel.names$ID %in% unique.id] <- groupid
  }
  length(unique(novel.names$groupID))
  #
}
unique.groupid <- novel.names$ID[(novel.names$m.c > 0 | !is.na(novel.names$university) | !is.na(novel.names$email)) & is.na(novel.names$groupID)]

for (p in unique.groupid) {
  #p<-1
  matched <- F
  default.frame <- data.frame(ID = NA, first = NA, middle = NA, last = NA, 
                              university = NA, email = NA, f.i = 0, address = NA, country = NA)
  match1 <- NA
  match2 <- NA
  match3 <- NA
  match4 <- NA
  name.df <- novel.names[novel.names$ID == p, ]
  # We need to create a dataframe of possible matching authors, so we dont run comparissons on obviously incorrect people
  novel.names1 <- subset(novel.names, (m.c > 0 | !is.na(university) | !is.na(email) | !is.na(address)) & ID != p)
  
  novel.names1 <- novel.names1[substr(name.df$first, 1, 1) == novel.names1$f.i & name.df$last == novel.names1$last, ]
  if (nrow(novel.names1) == 0) {
    novel.names1 <- default.frame
  }
  if (!is.na(name.df$first) & nchar(name.df$first) == 1) {
    novel.names1 <- novel.names1[novel.names1$f.i == name.df$first, ]
  }
  if (!is.na(name.df$first) & nchar(name.df$first) > 1) {
    novel.names1 <- novel.names1[novel.names1$f.c == 1 | novel.names1$first == name.df$first, ]
  }
  # if middle initial is 1 letter
  if (!is.na(name.df$middle) & nchar(name.df$middle) == 1) {
    novel.names1 <- novel.names1[is.na(novel.names1$middle) | novel.names1$m.i == name.df$middle, ]
  }
  # if has a middle name, must match
  if (!is.na(name.df$middle) & nchar(name.df$middle) > 1) {
    novel.names1 <- novel.names1[is.na(novel.names1$middle) | novel.names1$m.i == name.df$m.i, ]
  }
  # if has a country, it must match
  if (!is.na(name.df$country)) {
    novel.names1 <- novel.names1[is.na(novel.names1$country) | novel.names1$country == name.df$country, ]
  }
  
  if (nrow(novel.names1) == 0) {
    novel.names1 <- default.frame
  }
  if (!anyNA(novel.names1$ID)) {
    # match full middle name
    match1 <- !is.na(name.df$middle) & novel.names1$middle == name.df$middle
    # match addresses
    match2 <- (!is.na(novel.names1$university) & !is.na(name.df$university)) & name.df$university == novel.names1$university
    # match middle initial
    match3 <- !is.na(name.df$m.i) & novel.names1$m.i == name.df$m.i
    match4 <- is.na(name.df$address) & novel.names1$address == name.df$address
    # match emails
    # if(nrow(novel.names1)==0){match1<-F;match2<-F;match3<-F}
    if (sum(ifelse(is.na(c(match1, match2, match3, match4)), FALSE, c(match1, match2, match3, match4))) > 0) {
      matched <- T
      choice <- c(which(match1), which(match2), which(match3), which(match4))
      if (sum(!is.na(novel.names1$groupID[choice])) > 0) {
        groupid <- min(novel.names1$groupID[choice], na.rm = TRUE)
      } else {
        groupid <- min(novel.names1$ID[choice], na.rm = TRUE)
      }
      novel.names$groupID[novel.names$ID == p] <- groupid
    }
  }
  
  # Jaro_winkler
  if (!matched & nrow(novel.names1) > 0 & !anyNA(novel.names1$ID)) {
    jw_m <- RecordLinkage::jarowinkler(paste0(novel.names1$last, novel.names1$first, novel.names1$middle), paste0(name.df$last, name.df$first, name.df$middle))
    choice <- which(max(jw_m) == jw_m)[1]
    
    if (sum(!is.na(novel.names1$groupID[choice])) > 0) {
      groupid <- min(novel.names1$groupID[choice], na.rm = TRUE)
      groupname <- novel.names1$unique.name[novel.names1$groupID == groupid][1]
    } else {
      groupid <- min(novel.names1$ID[choice], na.rm = TRUE)
      groupname <- novel.names1$unique.name[novel.names1$ID == groupid]
    }
    
    novel.names$groupID[novel.names$ID == p] <- groupid
    novel.names$match_name[novel.names$ID == p] <- as.character(groupname)
    # changeID<-final$authorID[novelids][choice][1]
    if (sum(choice) > 0) {
      novel.names$similarity[novel.names$ID == p] <- jw_m[choice][1]
    }
  }
  ############################### Clock#############################################
  total <- length(unique.groupid)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  utils::setTxtProgressBar(pb, which(p == unique.groupid))
  utils::flush.console()
  #################################################################################
}
# Fixes a small issue where sometimes matched names using Jaro_winkler will get a groupID distinction but that the author
# it matched with will get a different groupID if it has a more perfect match criteria.
novel.names$groupID[is.na(novel.names$groupID)] <- novel.names$ID[is.na(novel.names$groupID)]
quick.check <- novel.names$ID[!is.na(novel.names$similarity)]
for (m in quick.check) {
  # m<-quick.check[1]
  novel.names$groupID[novel.names$ID == m] <- novel.names$groupID[novel.names$ID == novel.names$groupID[novel.names$ID == m]]
}

length(unique(novel.names$groupID))

# Time to prune the results. Weve used the vast network of knowledge to match up complexes, now we'll trim them by splitting any complexes with non matching First and last initials
# novel.names<-novel.names1
unique.names.over1 <- unique(novel.names$groupID)[table(novel.names$groupID) > 1]
print("Pruning groupings...")
for (n in unique.names.over1) {
   #n<-140
  sub <- subset(novel.names, groupID == n & is.na(similarity))
  # check first names
  # including the NAs
  fi.check <- unique(sub$f.i)
  if (length(fi.check) > 1) {
    newfi <- fi.check[2:length(fi.check)]
    newGroupID <- vapply(newfi, function(x) sub$ID[sub$f.i == x][1],numeric(1))
    
    for (p in seq_len(length(newfi))) {
      novel.names$groupID[novel.names$ID %in% sub$ID & novel.names$f.i == newfi[p] & novel.names$groupID == n] <- newGroupID[p]
    }
  }
  # now handle Middle initials without NA.
  sub <- subset(sub, !is.na(m.i))
  
  mi.check <- unique(paste(sub$f.i, sub$m.i))
  if (length(mi.check) > 1) {
    newmi <- mi.check[2:length(mi.check)]
    newGroupID1 <- vapply(newmi, function(x) sub$ID[sub$f.i == substr(x, 1, 1) & sub$m.i == substr(x, 3, 3)][1],numeric(1))
    for (q in seq_len(length(newmi))) {
      novel.names$groupID[novel.names$ID %in% sub$ID & novel.names$f.i == substr(newmi[q], 1, 1) & novel.names$m.i == substr(newmi[q], 3, 3)] <- newGroupID[q]
    }
  }
}



return(novel.names)
}
