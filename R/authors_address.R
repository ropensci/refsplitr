#' Parses out address information and splits it into its respective parts.
#' This is an internal function used by \code{authors_clean}
#' 
#' \code{authors_address} This function takes the output from 
#' \code{references_read} and pulls out address information. Splitting it into
#' university, department, city, state, etc. 
#' @param addresses the addresses
#' @param ID the authorID
#' @noRd
authors_address <- function(addresses, ID){
message("\nSplitting addresses\n")

list_address <- strsplit(addresses, ",")
university_list <- vapply(list_address, function(x) x[1], character(1))
country_list <- vapply(list_address, function(x) gsub("\\_", "", x[length(x)]),
                       character(1))
country_list <- trimws(country_list, which = "both")
pc_list <- trimws(substr(country_list, 1, (vapply(regexpr("USA",
            country_list), function(x) x[1], numeric(1))) - 1), which = "right")
state_list <- pc_list

state_list[nchar(state_list) > 0] <- regmatches(
  state_list[nchar(state_list) > 0],
  regexpr("[[:upper:]]{2}", state_list[nchar(state_list) > 0])
  )

pc_list[nchar(pc_list) > 2] <- regmatches(pc_list[nchar(pc_list) > 2],
      regexpr("[[:digit:]]{5}", pc_list[nchar(pc_list) > 2]))
pc_list[nchar(pc_list) < 3] <- ""
country_list <- ifelse(grepl("USA", country_list), "USA", country_list)

list_address1 <- lapply(list_address, function(x) x[-c(1, length(x))])

# Because formats of address printing is different across platforms
# We are going to split using a tier system assuming first and last
# info is somewhat reliable and guess the other info from the
# remaining position of the info

second_tier_list <- lapply(list_address1, function(x) x[length(x)])
second_tier_list <- trimws(second_tier_list, which = "both")
second_tier_list[second_tier_list == "character(0)"] <- NA

list_address2 <- lapply(list_address1, function(x) x[-c(length(x))])

third_tier_list <- lapply(list_address2, function(x) x[length(x)])
third_tier_list <- trimws(third_tier_list, which = "both")
third_tier_list[third_tier_list == "character(0)"] <- NA

# All remaining info is just shoved in this category
remain_list <- lapply(list_address2, function(x) x[-c(length(x))][1])
remain_list <- trimws(remain_list, which = "both")
remain_list[remain_list == "character(0)"] <- NA

a_df <- data.frame(adID = ID, university = university_list,
                         country = country_list,
                         state = state_list, postal_code = pc_list, city = NA,
                         department = NA, second_tier = second_tier_list,
                         third_tier = third_tier_list,
                         remain = remain_list, address = addresses,
                         stringsAsFactors = FALSE)

# try to fix the USA spots, which vary in format than other countries
a_df$city[nchar(a_df$state) > 0] <- a_df$second_tier[nchar(a_df$state) > 0]
a_df$state[nchar(a_df$state) == 0] <- NA
a_df$postal_code[nchar(a_df$postal_code) == 0] <- NA
a_df$department[!is.na(a_df$state) & !is.na(a_df$postal_code) &
  !is.na(a_df$state)] <- a_df$third_tier[!is.na(a_df$state) &
  !is.na(a_df$postal_code) & !is.na(a_df$state)]
# fix a US problem when USA is not tacked onto the end
a_df$state[grepl("[[:alpha:]]{2}[[:space:]]{1}[[:digit:]]{5}",a_df$country)]<-
  substr(a_df$country[grepl("[[:alpha:]]{2}[[:space:]]{1}[[:digit:]]{5}",a_df$country)],1,2)
a_df$postal_code[grepl("[[:alpha:]]{2}[[:space:]]{1}[[:digit:]]{5}",a_df$country)]<-
  substr(a_df$country[grepl("[[:alpha:]]{2}[[:space:]]{1}[[:digit:]]{5}",a_df$country)],4,8)
a_df$country[grepl("[[:alpha:]]{2}[[:space:]]{1}[[:digit:]]{5}",a_df$country)]<-'USA'


##########################
# We'll use regular expression to pull zipcodes
# These formats differ by region
int <- "[[:alpha:]]{2}[[:punct:]]{1}[[:digit:]]{1,8}|[[:space:]][[:upper:]][[:digit:]][[:upper:]][[:space:]][[:digit:]][[:upper:]][[:digit:]]|[[:alpha:]][[:punct:]][[:digit:]]{4,7}|[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"

UK <- "[[:upper:]]{1,2}[[:digit:]]{1,2}[[:space:]]{1}[[:digit:]]{1}[[:upper:]]{2}"

Mexico <- "[[:space:]]{1}[[:digit:]]{5}" # technically US as well
0843-03092
Panama <- "[[:digit:]]{4}-[[:digit:]]{5}"
zip_search <- paste0(int, "|", UK, "|", Mexico,"|",Panama)

###########################
id_run <- a_df$adID[is.na(a_df$state) & is.na(a_df$postal_code) &
                      a_df$address != "Could not be extracted"]
###########################

# We now iteratively run through the addresses using the concept that
# certain information always exists next to each other.
# Ex. city, state, country tend to exist next to each other.
# We use the position of the zipcode also to help guide us
# in where the information lies as well as how many fields were
# given to us.
for (i in id_run) {
  found <- FALSE
  row <- which(a_df$adID == i)
  university <- a_df$university[row]
  second_tier <- a_df$second_tier[row]
  third_tier <- a_df$third_tier[row]
  remain <- a_df$remain[row]
  city <- NA
  state <- NA
  postal_code <- NA
  department <- NA
  grepl(zip_search, second_tier)
  grepl(zip_search, third_tier)
  # 2nd tier
  if (grepl(zip_search, second_tier)) {
    found <- TRUE
    postal_code <- regmatches(second_tier, regexpr(zip_search, second_tier))
    city <- gsub(zip_search, "", second_tier)
    department <- ifelse(is.na(remain), third_tier, remain)
  }
  # 3RD tiers
  if (grepl(zip_search, third_tier) & !found) {
    found <- TRUE
    postal_code <- regmatches(third_tier, regexpr(zip_search, third_tier))
    city <- gsub(zip_search, "", third_tier)
    state <- second_tier
    department <- remain
  }

  if (!found) {
    state <- second_tier
    city <- third_tier
    department <- remain
  }
  # To make university searching more efficient we'll override values
  # based on if it has university/college in the name, where university overides college
  override.univ<-grepl('\\buniv\\b|\\buniversi',c(second_tier,third_tier,remain,city,university),ignore.case=T)
  if(any(override.univ)){university<-c(second_tier,third_tier,remain,city,university)[override.univ][1]
  assign(c('second_tier','third_tier','remain','city','university')[override.univ][1],NA)}
  # only if university doesnt already exist
  override.univ.col<-grepl('\\bcol\\b|college|\\bcoll\\b',c(second_tier,third_tier,remain,city,university),ignore.case=T)
  if(!any(override.univ) & any(override.univ.col)){university<-c(second_tier,third_tier,remain,city,university)[override.univ.col][1]
  assign(c('second_tier','third_tier','remain','city','university')[override.univ.col][1],NA)}
  # more risky, but institutions as well, just incase its not a university
  override.univ.inst<-grepl('\\binst\\b|\\binstitut',c(second_tier,third_tier,remain,city,university),ignore.case=T)
  if(!any(override.univ) &!any(override.univ.col)  & any(override.univ.inst)){department<-c(second_tier,third_tier,remain,city,university)[override.univ.inst][1]
  assign(c('second_tier','third_tier','remain','city','university')[override.univ.inst][1],NA)}
  
  
  a_df$city[row] <- gsub("[[:digit:]]", "", city)
  a_df$state[row] <- gsub("[[:digit:]]", "", state)
  a_df$postal_code[row] <- postal_code
  a_df$department[row] <- department

  #########################Clock###############################
  total <- length(id_run)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  utils::setTxtProgressBar(pb, which(id_run == i))
  #############################################################
}

city_fix <- is.na(a_df$city) & !is.na(a_df$state)
a_df$city[city_fix] <- a_df$state[city_fix]
a_df$state[city_fix] <- NA
a_df$university[a_df$university == "Could not be extracted"] <- NA
a_df$country[a_df$country == "Could not be extracted"] <- NA
a_df$country[a_df$country == "Peoples R China"] <- "China"
a_df$postal_code[grepl("[[:alpha:]]{1,2}-", a_df$postal_code)] <-
  vapply(strsplit(a_df$postal_code[grepl("[[:alpha:]]{1,2}-", a_df$postal_code)], "-"),
         function(x) x[2], character(1)
         )
#strip periods from the ends of city,state,country
a_df$city <- gsub("\\.", "", a_df$city)
a_df$state <- gsub("\\.", "", a_df$state)
a_df$country <- gsub("\\.", "", a_df$country)

return(a_df)
}
