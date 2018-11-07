#' Parses out address information and splits it into its respective parts.
#'
#' \code{authors_address} This function takes the output from `references_read()` and cleans the author information.
#'
#' @param references output from `references_read()`
#' #' @examples authors_address()

authors_address<-function(addresses, ID){
  
#addresses=final$address; ID=final$authorID
list.address <- strsplit(addresses, ",")
university.list <- vapply(list.address, function(x) x[1],character(1))
country.list <- vapply(list.address, function(x) gsub("\\.", "", x[length(x)]),character(1))
country.list <- trimws(country.list, which = "both")
postal_code.list <- trimws(substr(country.list, 1, (vapply(regexpr("USA", country.list), function(x) x[1],numeric(1))) - 1), which = "right")
state.list <- postal_code.list

state.list[nchar(state.list) > 0] <- regmatches(state.list[nchar(state.list) > 0], regexpr("[[:upper:]]{2}", state.list[nchar(state.list) > 0]))


postal_code.list[nchar(postal_code.list) > 2] <- regmatches(postal_code.list[nchar(postal_code.list) > 2], regexpr("[[:digit:]]{5}", postal_code.list[nchar(postal_code.list) > 2]))
postal_code.list[nchar(postal_code.list) < 3] <- ""
country.list <- ifelse(grepl("USA", country.list), "USA", country.list)

list.address1 <- lapply(list.address, function(x) x[-c(1, length(x))])


second.tier.list <- lapply(list.address1, function(x) x[length(x)])
second.tier.list <- trimws(second.tier.list, which = "both")
second.tier.list[second.tier.list == "character(0)"] <- NA

list.address2 <- lapply(list.address1, function(x) x[-c(length(x))])

third.tier.list <- lapply(list.address2, function(x) x[length(x)])
third.tier.list <- trimws(third.tier.list, which = "both")
third.tier.list[third.tier.list == "character(0)"] <- NA

remain.list <- lapply(list.address2, function(x) x[-c(length(x))][1])
remain.list <- trimws(remain.list, which = "both")
remain.list[remain.list == "character(0)"] <- NA

address.df <- data.frame(adID = ID, university = university.list, country = country.list,
                         state = state.list, postal_code = postal_code.list, city = NA,
                         department = NA, second.tier = second.tier.list, third.tier = third.tier.list,
                         remain = remain.list, address = addresses, stringsAsFactors = FALSE)

# try to fix the USA spots
address.df$city[nchar(address.df$state) > 0] <- address.df$second.tier[nchar(address.df$state) > 0]
address.df$state[nchar(address.df$state) == 0] <- NA
address.df$postal_code[nchar(address.df$postal_code) == 0] <- NA
address.df$department[!is.na(address.df$state) & !is.na(address.df$postal_code) & !is.na(address.df$state)] <- 
  address.df$third.tier[!is.na(address.df$state) & !is.na(address.df$postal_code) & !is.na(address.df$state)]
# address.df$adID<-1:nrow(address.df)
##########################
# reg expression postal_code search
int <- "[[:alpha:]]{2}[[:punct:]]{1}[[:digit:]]{1,8}|[[:space:]][[:upper:]][[:digit:]][[:upper:]][[:space:]][[:digit:]][[:upper:]][[:digit:]]|[[:alpha:]][[:punct:]][[:digit:]]{4,7}|[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"

UK <- "[[:upper:]]{1,2}[[:digit:]]{1,2}[[:space:]]{1}[[:digit:]]{1}[[:upper:]]{2}"

# SO14 3ZH
# L69 7ZB
# NR33 OHT
Mexico <- "[[:space:]]{1}[[:digit:]]{5}" # technically US as well

zip.search <- paste0(int, "|", UK, "|", Mexico)

###########################
id.run <- address.df$adID[is.na(address.df$state) & is.na(address.df$postal_code) & address.df$address != "Could not be extracted"]
###########################


for (i in id.run) {
  # i<-13998
  found <- F
  row <- which(address.df$adID == i)
  second.tier <- address.df$second.tier[row]
  third.tier <- address.df$third.tier[row]
  remain <- address.df$remain[row]
  city <- NA
  state <- NA
  postal_code <- NA
  department <- NA
  grepl(zip.search, second.tier)
  grepl(zip.search, third.tier)
  # 2nd tier
  if (grepl(zip.search, second.tier)) {
    found <- T
    postal_code <- regmatches(second.tier, regexpr(zip.search, second.tier))
    city <- gsub(zip.search, "", second.tier)
    department <- ifelse(is.na(remain), third.tier, remain)
  }
  
  
  if (grepl(zip.search, third.tier) & !found) {
    found <- T
    postal_code <- regmatches(third.tier, regexpr(zip.search, third.tier))
    city <- gsub(zip.search, "", third.tier)
    state <- second.tier
    department <- remain
  }
  
  if (!found) {
    state <- second.tier
    city <- third.tier
    department <- remain
  }
  address.df$city[row] <- gsub("[[:digit:]]", "", city)
  address.df$state[row] <- gsub("[[:digit:]]", "", state)
  address.df$postal_code[row] <- postal_code
  address.df$department[row] <- department
  ############################### Clock#############################################
  total <- length(id.run)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  utils::setTxtProgressBar(pb, which(id.run == i))
  utils::flush.console()
  #################################################################################
}

city.fix <- is.na(address.df$city) & !is.na(address.df$state)
address.df$city[city.fix] <- address.df$state[city.fix]
address.df$state[city.fix] <- NA
address.df$university[address.df$university == "Could not be extracted"] <- NA
address.df$country[address.df$country == "Could not be extracted"] <- NA
address.df$country[address.df$country=='Peoples R China']<-'China'
address.df$postal_code[grepl("[[:alpha:]]{1,2}-",address.df$postal_code)]<-
  vapply(strsplit(address.df$postal_code[grepl("[[:alpha:]]{1,2}-",address.df$postal_code)],
                  '-'),function(x)x[2],character(1))

return(address.df)
}
