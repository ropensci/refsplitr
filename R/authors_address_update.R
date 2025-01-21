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

  # DELETE  
  # library(tidyverse)
  # library(refsplitr)
  final<-read.csv("./data/wos_txt/final.csv")
  addresses<-final$address
  ID<-final$authorID
  addresses<-tolower(addresses)
  message("\nSplitting addresses\n")
  list_address <- strsplit(addresses, ",")


# trim ws -----------------------------------------------------------------

  list_address <- lapply(list_address, trimws)

  
  
  
# COUNTRIES ---------------------------------------------------------------

# remove punctuation ----------------------------------------
  ## First remove periods and trim white space from countries.
  ## helps avoids mistakes later on
  remove_period_from_last <- function(list_address) {
    lapply(list_address, function(x) {
      if (length(x) > 0) {
        x[length(x)] <- gsub("\\.$", "", x[length(x)])
        x[length(x)] <- trimws(x[length(x)], which = "both") 
      }
      return(x)
    })
  }

  list_address <- remove_period_from_last(list_address)

  
# correct names
  # Define the function
  correct_countries <- function(my_list, replacements) {
    # Loop through each element of the list
    for(i in 1:length(my_list)) {
      # Get the length of the current element
      len <- length(my_list[[i]])
      
      # Check if the last item matches any of the target words
      if(len > 0 && my_list[[i]][len] %in% names(replacements)) {
        # Replace the last item with the corresponding replacement word
        my_list[[i]][len] <- replacements[[my_list[[i]][len]]]
      }
    }
    return(my_list)
  }
  # czechia = new name for czech republic
  # TBD: united arab rep, 
  replacements <- c("austl" = "australia", 
                    "c z" = "czechia", 
                    "cz" = "czechia",
                    "czech republic" = "czechia",
                    "fed rep ger" = "germany",
                    "columbia" = "colombia",
                    "peoples r china" = "china",
                    "u arab emirates" = "united arab emirates",
                    "mongol peo rep" = "mongolia",
                    "dominican rep" = "dominican republic",
                    "fr polynesia" = "french polynesia",
                    "neth antilles" = "netherland antilles",
                    "trinid & tobago" = "trinidad & tobago",
                    "rep congo" = "congo",
                    "north ireland" = "northern ireland",
                    "syrian arab rep" = "syria"
                    ) 
  

  
  list_address <- correct_countries(list_address, replacements)
  
  

# Extract University ------------------------------------------------------

  university_list <- vapply(list_address, function(x) x[1], character(1))
  
  
# Extract the Department --------------------------------------------------

  # If department is listed it is typically second
  # this will be 2x checked later
  ## EB: seond only if 4+ slots
  dept_extract <- function(x) {
    if (length(x) < 4) {
      return(NA)
    } else {
      return(trimws(x[[2]]))
    }
  }
  # 
  dept_list <- unlist(lapply(list_address, dept_extract))
  
  # dept_list <- vapply(list_address, function(x) x[2], character(1))
  dept_list <- trimws(dept_list, which = "both")
  
  

# Extract City ------------------------------------------------------------

  # If there is only one element, then it can't have both city and country'
  city_list <- vapply(list_address, function(x) {
    n <- length(x)
    if (n == 1) {
      return("no city") # placeholder to replace with NA after function
    }
    
    # In some cases city is next-to-last element, in others next-to-next-to-last
    last_element <- x[[n]]
    second_last <- if (n > 1) x[[n - 1]] else NA
    third_last <- if (n > 2) x[[n - 2]] else NA
    
    # Check for India, China, & brazil, canada, australia, and UK. 
    # These countries' city is in multiple places
    # This puts ina placeholder, which will be replaced later in 
    # the function that checks India and China
    if(last_element %in% c("india", "china", 
                           "brazil", "canada", "australia",
                           "scotland", "england", "wales",
                           "northern ireland"))
      {
      return("icb") # placeholder to replace with NA after function
    }
    
    
    if (grepl("usa",last_element)) {
      return("icb") # placeholder to replace with NA after function
    }
    
    # And of course a few other odd ones. This will check for 
    # other countries with specific rules.
    if ((last_element == "australia" && second_last != "liverpool") ||
        last_element %in% c("wales") ||
        (last_element == "mexico" && second_last != "iztapalapa") ||
        (last_element == "argentina" && second_last == "df")) {
      return(third_last)
    }
    
    # Default case
    return(second_last)
  }, character(1))
  
  # Cleanup
  city_list <- trimws(city_list, which = "both")
  city_list[city_list == "no city"] <- NA
  city_list[city_list == "icb"] <- NA
  

# Extract Country ---------------------------------------------------------


  country_list <- vapply(list_address, function(x) {
    gsub("\\_", "", x[length(x)]) },
    character(1))
  
# Extracts zip & state from usa addresses ---------------------------------

    pc_list <- trimws(substr(country_list, 1, (vapply(regexpr("usa",
    country_list), function(x) x[1], numeric(1))) - 1), which = "right")
  state_list <- pc_list

  state_list[nchar(state_list) > 0] <- regmatches(
    state_list[nchar(state_list) > 0],
    regexpr("[[:lower:]]{2}", state_list[nchar(state_list) > 0])
  )
  state_list[state_list == ""] <- NA

  pc_list[nchar(pc_list) > 2] <- regmatches(pc_list[nchar(pc_list) > 2],
    regexpr("[[:digit:]]{5}", pc_list[nchar(pc_list) > 2]))
  pc_list[nchar(pc_list) < 3] <- ""
  pc_list[pc_list == ""] <- NA
  
  

# USA  --------------------------------------------------------------------

  process_usa_address <- function(x) {
    if (length(x) == 1) {
      return(c(NA, NA)) # Not usa
    }
    if (grepl(" usa", x[[length(x)]])) {
      if (length(x) == 4) {
        return(c(trimws(x[[length(x) - 1]]), trimws(x[length(x)])))
      }
      if (length(x) == 5) {
          return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x)]])))
      }
        if (length(x) == 3) {
          return(c(trimws(x[[length(x) - 1]]), trimws(x[length(x)])))
      } else {
        return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x)]])))
      }
    }
    
    return(c(NA, NA)) # Not usa
      }
      
  
  # Apply the function across all addresses using `mapply`
  results <- t(mapply(process_usa_address, list_address))
  colnames(results) <- c("usa_city", "usa_state")
  
  results<-as.data.frame(results)
  results$pc<-NA
  results$country<-NA
  extract_usa_postcodes <- function(df, usa_state, pc,country) {
    # 6 digits
    pattern <- "[0-9]{5}"
    
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Find all matches of the pattern in the source column
      matches <- gregexpr(pattern, df[i, usa_state])
      # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
      # Extract the matches
      extracted_codes <- regmatches(df[i, usa_state], matches)
      # If there's at least one match and the target column is NA, copy the first match to the target column
      if(length(extracted_codes[[1]]) > 0 && is.na(df[i, pc])) {
        df[i, pc] <- extracted_codes[[1]][1]
        df[i, country] <- "usa"
        # df[i, city_col] <- df[i, source_col]
        
      }
    }
    return(df)
  }
  
  
  results <- extract_usa_postcodes(results, "usa_state", "pc","country")
  
  
  # any without numbers gets NA'd
  results$pc[!grepl("\\d", results$pc)] <- NA
  
  # keep portions with numbers / remove city names
  results$usa_city<-sub("[0-9]{5}","",results$usa_city)
  results$usa_state<-sub("[0-9]{5}","",results$usa_state)
  results$usa_state<-sub("usa","",results$usa_state)
  results$usa_state<-trimws(results$usa_state, which = "both")
  
  results$country<-ifelse(is.na(results$usa_city)==FALSE,"usa",results$country)
  
  
  
  
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), results$usa_city, city_list)
  # 
  # # Update `state_list` if necessary
  state_list<-ifelse(is.na(state_list),results$usa_state, state_list)
  # 
  # # Update `pc_list` if necessary
  pc_list<-ifelse(is.na(pc_list),results$pc, pc_list)
  # Update `country_list` if necessary
  
  country_list<-ifelse((grepl("usa",country_list)),"usa", country_list)
  # remove any with "state_abbrev zip code" but no USA
  country_list <- ifelse(grepl("[A-Za-z]{2} [0-9]{5}", country_list, ignore.case = TRUE), "usa", country_list)
  
  
  
  us_state_abbreviations_lower <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga",
                                    "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md",
                                    "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj",
                                    "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc",
                                    "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")
  
  country_list<-ifelse(country_list%in%us_state_abbreviations_lower,"usa", country_list)
  rm(results)
# AUSTRALIA ---------------------------------------------------------------


  ## Australia postal codes also separated
  # sometimes in the next-to-last, sometimes in the next-to-next-to-last (with state)
  
  # First need to "fix" the city
  # Function to check for three characters or numbers in the city-list and replace with NA
  process_aus_address <- function(x) {
    if (length(x) == 1) {
      return(c(NA, NA)) # Not Australia
    }
    
    if (x[[length(x)]] == "australia") {
      if (length(x) == 3) {
        return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 1]])))
      } else {
        return(c(trimws(x[[length(x) - 2]]), trimws(x[[length(x) - 1]])))
      }
    }
    
    return(c(NA, NA)) # Not Australia
  }
  
  # Apply the function across all addresses using `mapply`
  results <- t(mapply(process_aus_address, list_address))
  colnames(results) <- c("aus_city", "aus_state")
  
  # Clean up results
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  results$aus_city[results$aus_city == "not australia"] <- NA
  results$aus_state[results$aus_state == "not australia"] <- NA
  
  # take the PC+state and assign to PC
  results$aus_pc<-results$aus_state
  # remove all digits from state
  results$aus_state <- trimws(gsub("\\d", "", results$aus_state))
  # remove all letters from pc
  results$aus_pc <- trimws(gsub("[a-z]", "", results$aus_pc))
  results$aus_pc[results$aus_pc == ""] <- NA
  
  # if na in PC, assign the city (some of which have PC)
  results$aus_pc <- ifelse(is.na(results$aus_pc), results$aus_city, results$aus_pc)
  # remove all metters from pc, leaving any new pc
  results$aus_pc <- trimws(gsub("[a-z]", "", results$aus_pc))
  results$aus_pc[results$aus_pc == ""] <- NA
  # now remove any PC from city
  results$aus_city <- trimws(gsub("\\d", "", results$aus_city)) #alternative
  
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), results$aus_city, city_list)
  
  # Update `state_list` if necessary
  state_list<-ifelse(is.na(state_list),results$aus_state, state_list)
  
  # Update `pc_list` if necessary
  pc_list<-ifelse(is.na(pc_list),results$aus_pc, pc_list)

  rm(results)
  


# CANADA ------------------------------------------------------------------

  ## Canada postal codes also separated
  # sometimes in the next-to-last, sometimes in the next-to-next-to-last (with state)
  
  process_can_address <- function(x) {
    if (length(x) == 1) {
      return(c(NA, NA)) # Not Canada
    }
    
    if (x[[length(x)]] == "canada") {
      if (length(x) == 3) {
        return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 1]])))
      } else {
        return(c(trimws(x[[length(x) - 2]]), trimws(x[[length(x) - 1]])))
      }
    }
    
    return(c(NA, NA)) # Not canada
  }
  
  # Apply the function across all addresses using `mapply`
  results <- t(mapply(process_can_address, list_address))
  colnames(results) <- c("can_city", "can_state")
  
  # Clean up results
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  results$can_city[results$can_city == "not canada"] <- NA
  results$can_state[results$can_state == "not canada"] <- NA
  
  # take the PC+state and assign to PC
  results$can_pc<-results$can_state

  # any without numbers gets NA'd
  results$can_pc[!grepl("\\d", results$can_pc)] <- NA
  # removes state and removes ltr at start of PC
  results$can_pc<-sub("[a-z]\\s+[a-z]", "", results$can_pc) #+[a-z] 
  
  
  # if na in PC, assign the city (some of which have PC)
  results$can_pc <- ifelse(is.na(results$can_pc), results$can_city, results$can_pc)
  results$can_pc[!grepl("\\d", results$can_pc)] <- NA
  
  # keep portions with numbers / remove city names
  # .*\\b(\\w{3})\\b.*\\b(\\w{3})\\b$: This regex pattern matches any
  # characters (.*) followed by a word boundary (\\b) and exactly three word
  # characters (\\w{3}), capturing this as the first group ((\\w{3})). It
  # then matches any characters again (.*) followed by another word boundary
  # and exactly three word characters, capturing this as the second
  # group ((\\w{3})), and ensures this is at the end of the string ($).
  # sub(".*\\b(\\w{3})\\b.*\\b(\\w{3})\\b$", "\\1 \\2", string): Replaces the
  # entire string with the two captured groups separated by a space.
  results$can_pc<-sub(".*\\b(\\w{3})\\b.*\\b(\\w{3})\\b$", "\\1 \\2", results$can_pc)

  # results$can_pc<-gsub("\\b[^\\d\\s]+\\b", "", results$can_pc) #+[a-z] removes 2nd ltr

  results$can_pc[results$can_pc == ""] <- NA
  # now remove any PC from city
  results$can_city<-sub("\\s([a-z0-9]+\\s[a-z0-9]+)$", "", results$can_city) # all after first space
  
  
  # fix state
  results$can_state <- trimws(gsub("\\d", "", results$can_state))
  results$can_state <-trimws(sub(" .*", "", results$can_state)) # all after first space
  results$can_state <- gsub("british", "bc", results$can_state)
  results$can_state <- gsub("nova", "ns", results$can_state)
  # fix city
  results$can_city <- trimws(gsub("\\d", "", results$can_city))
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), results$can_city, city_list)
  
  # Update `state_list` if necessary
  state_list<-ifelse(is.na(state_list),results$can_state, state_list)
  
  # Update `pc_list` if necessary
  pc_list<-ifelse(is.na(pc_list),results$can_pc, pc_list)
  
  rm(results)
  
# INDIA  ------------------------------------------------------------------

  # India states are almost always listed but New Delhi is complicated,
  # as are any with only three entries
  # Function to process addresses for both city and state
  process_india_address <- function(x) {
    if (length(x) == 1) {
      return(c(NA, NA)) # Not India
    }
    
    if (x[[length(x)]] == "india") {
      if (length(x) == 3) {
        return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 1]])))
      } else {
        return(c(trimws(x[[length(x) - 2]]), trimws(x[[length(x) - 1]])))
      }
    }
    
    if (length(x) != 3 && grepl("delhi", x[[length(x) - 1]])) {
      return(c(trimws(x[[length(x) - 1]]), NA))
    }
    
    return(c(NA, NA)) # Not India
  }
  
  # Apply the function across all addresses using `mapply`
  results <- t(mapply(process_india_address, list_address))
  colnames(results) <- c("india_city", "india_state")
  
  # Clean up results
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  results$india_city[results$india_city == "not india"] <- NA
  results$india_state[results$india_state == "not india"] <- NA
  
  # Remove numeric parts from state names and trim whitespace
  results$india_state <- trimws(gsub("\\s([0-9]+)", "", results$india_state))
  
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), results$india_city, city_list)
  ####
  
  state_list<-ifelse(is.na(state_list),results$india_state, state_list)
  
  rm(results)
  

# BRAZIL ------------------------------------------------------------------


  # Function to process addresses for both city and state
  process_brazil_address <- function(x) {
    if (length(x) == 1) {
      return(c(NA, NA)) # Not brl
    }
    if (x[[length(x)]] == "brazil") {
      if (length(x) == 3) {
        return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 1]])))
      } else {
        return(c(trimws(x[[length(x) - 2]]), trimws(x[[length(x) - 1]])))
      }
    }
    
    
    return(c(NA, NA)) # Not brl
  }
  
  
    
  # Apply the function across all addresses
  results <- as.data.frame(t(mapply(process_brazil_address, list_address)), 
                            stringsAsFactors = FALSE)
  colnames(results) <- c("brl_city", "brl_state")
  
  
  
  
  # Define words indicating this is actually a dept not state or postal code
  # will use this list to delete the ones that don't apply
  to_check <- c("dept","ctr","inst","ppg","andar","empresas",
                "programa", "ciencias", "unidade", "lab ")
  
  results$brl_city <- ifelse(grepl(paste(to_check, collapse = "|"), results$brl_city),
                             results$brl_state,
                             results$brl_city)
  
  
  
  results$brl_state <- ifelse(grepl(paste(to_check, collapse = "|"), results$brl_state),
                             results$brl_city,
                             results$brl_state)
  
  
  # Define the function
  extract_brl_postcodes <- function(df, source_col, target_col,brl_new_city) {
    # 6 digits
   
    pattern <- "br-[0-9]{5,8}"
    
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Find all matches of the pattern in the source column
      matches <- gregexpr(pattern, df[i, source_col])
      # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
      # Extract the matches
      extracted_codes <- regmatches(df[i, source_col], matches)
      # If there's at least one match and the target column is NA, copy the first match to the target column
      if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
        df[i, target_col] <- extracted_codes[[1]][1]
        df[i, brl_new_city] <- df[i, source_col]
        # df[i, city_col] <- df[i, source_col]
        
      }
    }
    return(df)
  }
  
  results$brl_pc<-NA
  results$brl_new_city<-NA
  # df, source_col, target_col,city_col
  results <- extract_brl_postcodes(results, "brl_city","brl_pc", "brl_new_city")
  results <- extract_brl_postcodes(results, "brl_state","brl_pc", "brl_new_city")
  
  
  results$brl_new_city <- ifelse(is.na(results$brl_new_city),
                                 results$brl_state, 
                                 results$brl_new_city)
  
  
  results$brl_state <- ifelse(results$brl_new_city==results$brl_state,
                              results$brl_city, 
                              results$brl_state)
  
  
  
  
  results$brl_new_city <- ifelse(is.na(results$brl_new_city),
                              results$brl_city, 
                              results$brl_new_city)
  
  
  
  
  results$brl_city<-gsub("br-","",results$brl_city)
  results$brl_city<-sub("[0-9]{2,8}","",results$brl_city)
  results$brl_city<-sub("-","",results$brl_city)
  
  
  results$brl_new_city<-gsub("br-","",results$brl_new_city)
  results$brl_new_city<-sub("[0-9]{2,8}","",results$brl_new_city)
  results$brl_new_city<-sub("-","",results$brl_new_city)
  
  
  results$brl_pc<-gsub("br-","",results$brl_pc)
  results$brl_pc<-sub("[0-9]{2,8}","",results$brl_pc)
  results$brl_pc<-sub("[0-9]{2,8}","",results$brl_pc)
  
  results$brl_state<-gsub("br-","",results$brl_state)
  results$brl_state<-sub("[0-9]{2,8} ","",results$brl_state)
  results$brl_state<-sub(" [0-9]{2,8} ","",results$brl_state)
  
  # 
  # 
  # 
  # 
  # 
  # results$brl_city<-sub("[0-9]{2,8} ","",results$brl_city)
  # results$brl_new_city<-gsub("gavea rio de janeiro","rio de janeiro",results$brl_new_city)
  # results$brl_new_city<-gsub("gavea","rio de janeiro",results$brl_new_city)
  # # 
  # results$brl_city<-gsub("-","",results$brl_city)
  # 
  # results$brl_state<-gsub("br-","",results$brl_state)
  # results$brl_state<-gsub("-","",results$brl_state)
  
  
  
  
  
  # any without numbers gets NA'd
  results$brl_pc[!grepl("\\d", results$brl_pc)] <- NA
  
  # keep portions with numbers / remove city names
  
  results$brl_city<-sub("[0-9]{2,8} ","",results$brl_city)
  results$brl_city<-sub(" [0-9]{2,8}","",results$brl_city)
  # 
  # # Specific replacements
  # results$brl_city <- ifelse(grepl("rio de janeiro", results$brl_city),
  #                            "rio de janeiro", results$brl_city)
  
  results$brl_city <- ifelse(grepl("museu nacl", results$brl_city),
                             "rio de janeiro", results$brl_city)
  results$brl_state <- ifelse(grepl("rio de janeiro", results$brl_state, ignore.case = TRUE),
                              "rj", results$brl_state)
  results$brl_state <- ifelse(grepl("sao paulo", results$brl_state, ignore.case = TRUE),
                              "sp", results$brl_state)
  
  
  
  results$brl_city[results$brl_city==results$brl_state]<-NA
  
  
  # Clean up and adjust columns
  results[] <- lapply(results, trimws)
  
  
  # Define city-to-state mapping
  city_state_mapping <- data.frame(
    city = c("ribeirao preto", "sao carlos", "rio claro", "sorocaba", "seropedica", "rio de janeiro", "rio janeiro", "sao paulo"),
    state = c("sp", "sp", "sp", "sp", "rj", "rj", "rj", "sp"),
    stringsAsFactors = FALSE
  )
  
  # Match cities and states
  for (i in 1:nrow(city_state_mapping)) {
    results$brl_city <- ifelse(grepl(city_state_mapping$city[i], results$brl_state, ignore.case = TRUE),
                                  city_state_mapping$city[i], results$brl_city)
    results$brl_state <- ifelse(grepl(city_state_mapping$city[i], results$brl_state, ignore.case = TRUE),
                                   city_state_mapping$state[i], results$brl_state)
  }
  
  
  # Match cities and states
  for (i in 1:nrow(city_state_mapping)) {
    
    results$brl_state <- ifelse(grepl(city_state_mapping$city[i], results$brl_state, ignore.case = TRUE),
                                city_state_mapping$state[i], results$brl_state)
  }
  
  
  
  results$brl_state <- ifelse(results$brl_new_city==results$brl_state,
                              results$brl_city,
                              results$brl_state)
  
  
  results$brl_city <- trimws(results$brl_city, which = "both")
  results$brl_state <- trimws(results$brl_state, which = "both")
  # Define words indicating this is actually a dept not state or postal code
  # will use this list to delete the ones that don't apply
  to_check <- c("dept","ctr","inst","ppg",
                "programa", "ciencias", "unidade", "lab ")
  
  results$brl_city <- ifelse(grepl(paste(to_check, collapse = "|"), results$brl_city),
                                results$brl_state,
                                results$brl_city)
  
  
    # Final trimming
  results[] <- lapply(results, trimws)
  
  results$brl_city <- ifelse(results$brl_new_city==results$brl_city,
                              NA,
                              results$brl_city)
  
  
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), results$brl_city, city_list)
  # Update `state_list` if necessary
  state_list<-ifelse(is.na(state_list),results$brl_state, state_list)
  # Update `pc_list` if necessary
  pc_list<-ifelse(is.na(pc_list),results$brl_pc, pc_list)
  
  
  rm(results,city_state_mapping)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Handle postal codes (BR-[0-9])
  results$brazil_pc <- ifelse(grepl("br-[0-9]", results$brazil_state), results$brazil_state, NA)
  results$brazil_pc <- ifelse(grepl("br-[0-9]", results$brazil_city) & is.na(results$brazil_pc), 
                               results$brazil_city, results$brazil_pc)
  
  # Remove BR codes from city and state
  results$brazil_city <- gsub("br-[0-9]+", "", results$brazil_city)
  results$brazil_state <- gsub("br-[0-9]+", "", results$brazil_state)
  
  # Define city-to-state mapping
  city_state_mapping <- data.frame(
    city = c("ribeirao preto", "sao carlos", "rio claro", "sorocaba", "seropedica", "rio de janeiro", "sao paulo"),
    state = c("sp", "sp", "sp", "sp", "rj", "rj", "sp"),
    stringsAsFactors = FALSE
  )
  
  # Match cities and states
  for (i in 1:nrow(city_state_mapping)) {
    results$brazil_city <- ifelse(grepl(city_state_mapping$city[i], results$brazil_state, ignore.case = TRUE),
                                   city_state_mapping$city[i], results$brazil_city)
    results$brazil_state <- ifelse(grepl(city_state_mapping$city[i], results$brazil_state, ignore.case = TRUE),
                                    city_state_mapping$state[i], results$brazil_state)
  }
  
  # Specific replacements
  results$brazil_city <- ifelse(grepl("museu nacl", results$brazil_city),
                                 "rio de janeiro", results$brazil_city)
  results$brazil_state <- ifelse(grepl("rio de janeiro", results$brazil_state, ignore.case = TRUE),
                                  "rj", results$brazil_state)
  results$brazil_state <- ifelse(grepl("sao paulo", results$brazil_state, ignore.case = TRUE),
                                  "sp", results$brazil_state)
  
  # cleanup
  results$brazil_city[results$brazil_city==results$brazil_state]<-NA
  results$brazil_city <- trimws(results$brazil_city, which = "both")
  results$brazil_state <- trimws(results$brazil_state, which = "both")
  # Define words indicating this is actually a dept not state or postal code
  # will use this list to delete the ones that don't apply
  to_check <- c("dept","ctr","inst","ppg",
                "programa", "ciencias", "unidade", "lab ")
  
  results$brazil_city <- ifelse(grepl(paste(to_check, collapse = "|"), results$brazil_city),
                                results$brazil_state,
                               results$brazil_city)
  
  # Clean postal codes
  results$brazil_pc <- gsub("[A-Za-z-]", "", results$brazil_pc)
  
  # Final trimming
  results[] <- lapply(results, trimws)
  
  
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), results$brazil_city, city_list)
  # Update `state_list` if necessary
    state_list<-ifelse(is.na(state_list),results$brazil_state, state_list)
    # Update `pc_list` if necessary
    pc_list<-ifelse(is.na(pc_list),results$brazil_pc, pc_list)
  
  
  rm(results,city_state_mapping)
  

# CHINA -------------------------------------------------------------------
  chn_extract <- function(x) {
    if (length(x) == 1) {
      return(c(NA, NA))
    } else if (x[[length(x)]] == "china") {
      return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 2]])))
    } else {
      return(c(NA, NA))
    }
  }
  
  chn_pc <- data.frame(do.call(rbind, lapply(list_address, chn_extract)))
  names(chn_pc) <- c("chn_city", "chn_state")
  
  # Define the function
  extract_china_postcodes <- function(df, source_col, target_col,chn_new_city) {
    # 6 digits
    pattern <- "[0-9]{6}"
    
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Find all matches of the pattern in the source column
      matches <- gregexpr(pattern, df[i, source_col])
      # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
      # Extract the matches
      extracted_codes <- regmatches(df[i, source_col], matches)
      # If there's at least one match and the target column is NA, copy the first match to the target column
      if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
        df[i, target_col] <- extracted_codes[[1]][1]
        df[i, chn_new_city] <- df[i, source_col]
        # df[i, city_col] <- df[i, source_col]
        
      }
    }
    return(df)
  }
  
  chn_pc$chn_pc<-NA
  chn_pc$chn_new_city<-NA
  # df, source_col, target_col,city_col
  chn_pc <- extract_china_postcodes(chn_pc, "chn_city","chn_pc", "chn_new_city")
  chn_pc <- extract_china_postcodes(chn_pc, "chn_state","chn_pc", "chn_new_city")
  
  
  
  # any without numbers gets NA'd
  chn_pc$chn_pc[!grepl("\\d", chn_pc$chn_pc)] <- NA
  
  # keep portions with numbers / remove city names
  chn_pc$chn_new_city<-sub("[0-9]{6}","",chn_pc$chn_new_city)
  chn_pc$chn_city<-sub("[0-9]{6}","",chn_pc$chn_city)
  chn_pc$chn_state<-sub("[0-9]{6}","",chn_pc$chn_state)
  
  
  # Define words indicating this is actually a dept not state or postal code
  # will use this list to delete the ones that don't apply
  to_delete <- c("&", "inst", "ctr", "med", "chem", "lab", "biol", 
                 "dept", "div", "univ", "hosp", "coll", "sci", "rd", 
                 "program","minist", "educ", "sch ", "grad ", "fac ",
                 "assoc","forest")
  
  
  
  chn_pc[, c("chn_city","chn_state", "chn_pc", "chn_new_city")]<-lapply(chn_pc[, c("chn_city","chn_state", "chn_pc", "chn_new_city")], trimws)
  
  
  clean_column <- function(column, delete_terms) {
    column <- gsub(paste(delete_terms, collapse = "|"), NA, column)
    column <- gsub("[0-9]", "", column) # Remove digits
    trimws(column) # Remove leading/trailing whitespace
  }
  
  
  # Clean chn_pc1 and chn_pc2
  chn_pc$chn_city <- clean_column(chn_pc$chn_city, to_delete)
  chn_pc$chn_new_city <- clean_column(chn_pc$chn_new_city, to_delete)
  chn_pc$chn_state <- clean_column(chn_pc$chn_state, to_delete)
  
  
  chn_pc$chn_new_city <- ifelse(is.na(chn_pc$chn_new_city),
                                chn_pc$chn_state,
                                chn_pc$chn_new_city)
  
  
  chn_pc$chn_state <- ifelse(((chn_pc$chn_new_city==chn_pc$chn_state)==TRUE),
                                NA,chn_pc$chn_state)
  
  
  
  chn_pc$chn_state <- ifelse(((chn_pc$chn_new_city==chn_pc$chn_city)==FALSE),
                             chn_pc$chn_city,chn_pc$chn_state)
  
  
  
  chn_pc$chn_state <- ifelse(((chn_pc$chn_new_city==chn_pc$chn_state)==TRUE),
                             NA,chn_pc$chn_state)
  
  chn_pc$chn_state <- gsub(" province", "",chn_pc$chn_state)
  
  # Update `city_list` if necessary
  city_list <- ifelse(is.na(city_list), chn_pc$chn_new_city, city_list)
  
  # Update `state_list` if necessary
  state_list<-ifelse(is.na(state_list),chn_pc$chn_state, state_list)
  
  # Update `pc_list` if necessary
  pc_list<-ifelse(is.na(pc_list),chn_pc$chn_pc, pc_list)
  
  rm(chn_pc)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
# ### China has some where the postal code is with the city, so fix those here
#   # Extract postal code information from the list
#   chn_extract <- function(x) {
#     if (length(x) == 1) {
#       return(c(NA, NA))
#     } else if (x[[length(x)]] == "china") {
#       return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 2]])))
#     } else {
#       return(c(NA, NA))
#     }
#   }

# Apply extraction to list_address
# chn_missing_pc <- data.frame(do.call(rbind, lapply(list_address, chn_extract)))
# names(chn_missing_pc) <- c("chn_pc1", "chn_pc2")
# 
# # Define words indicating this is actually a dept not state or postal code
# # will use this list to delete the ones that don't apply
# to_delete <- c("&", "inst", "ctr", "med", "chem", "lab", "biol", 
#                "dept", "div", "univ", "hosp", "coll", "sci", "rd","program" 
#                "minist", "educ", "sch ", "grad ", "fac ","assoc")
# 
# 
# 
# # Extract numeric postal codes
# chn_missing_pc$chn_pc_from1 <- as.numeric(gsub("[A-Za-z]", "", chn_missing_pc$chn_pc1))
# chn_missing_pc$chn_pc_from2 <- as.numeric(gsub("[A-Za-z]", "", chn_missing_pc$chn_pc2))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# clean_column <- function(column, delete_terms) {
#   column <- gsub(paste(delete_terms, collapse = "|"), NA, column)
#   column <- gsub("[0-9]", "", column) # Remove digits
#   trimws(column) # Remove leading/trailing whitespace
# }
# 
# # Clean chn_pc1 and chn_pc2
# chn_missing_pc$chn_pc1 <- clean_column(chn_missing_pc$chn_pc1, to_delete)
# chn_missing_pc$chn_pc2 <- clean_column(chn_missing_pc$chn_pc2, to_delete)
# 
# # Initialize empty columns for final outputs
# chn_missing_pc$chn_pc <- NA
# chn_missing_pc$chn_city <- NA
# chn_missing_pc$chn_state <- NA
# 
# # Assign postal codes, cities, and states based on conditions
# assign_chn_data <- function(from1, from2, pc1, pc2) {
#   list(
#     chn_pc = ifelse(is.na(from1) & !is.na(from2), from2, from1),
#     chn_city = ifelse(is.na(from1) & !is.na(from2), pc2, pc1),
#     chn_state = ifelse(is.na(from1) & !is.na(from2), pc1, pc2)
#   )
# }
# 
# chn_result <- assign_chn_data(chn_missing_pc$chn_pc_from1, 
#                               chn_missing_pc$chn_pc_from2, 
#                               chn_missing_pc$chn_pc1, 
#                               chn_missing_pc$chn_pc2)
# 
# chn_missing_pc$chn_pc <- chn_result$chn_pc
# chn_missing_pc$chn_city <- gsub("[0-9]", "", chn_result$chn_city)
# chn_missing_pc$chn_state <- gsub("[0-9]", "", chn_result$chn_state)

# # Define Chinese states and cities
# chn_states <- c("guangdong", "shandong", "henan", "jiangsu", "sichuan",
#                 "hebei", "hunan", "zhejiang", "anhui", "hubei", "guangxi", 
#                 "yunnan", "jiangxi", "liaoning", "fujian", "shaanxi", 
#                 "guizhou", "shanxi", "chongqing", "heilongjiang", "xinjiang", 
#                 "gansu", "inner mongolia", "jilin", "hainan", "ningxia", 
#                 "qinghai", "tibet", "macao")
# 
# # All the cities in the addresses, add as needed. 
# chn_cities <- unique(c(chn_missing_pc$chn_city, "lhasa"))

# Update states and cities based on matching conditions
# chn_missing_pc$chn_state <- ifelse(is.na(chn_missing_pc$chn_state) & chn_missing_pc$chn_pc1 %in% chn_states,
#                                    chn_missing_pc$chn_pc1, chn_missing_pc$chn_state)
# chn_missing_pc$chn_state <- ifelse(is.na(chn_missing_pc$chn_state) & chn_missing_pc$chn_pc2 %in% chn_states,
#                                    chn_missing_pc$chn_pc2, chn_missing_pc$chn_state)
# 
# chn_missing_pc$chn_city <- ifelse(is.na(chn_missing_pc$chn_city) & !(chn_missing_pc$chn_pc1 %in% chn_states),
#                                   chn_missing_pc$chn_pc1, chn_missing_pc$chn_city)
# chn_missing_pc$chn_city <- ifelse(is.na(chn_missing_pc$chn_city) & !(chn_missing_pc$chn_pc2 %in% chn_states),
#                                   chn_missing_pc$chn_pc2, chn_missing_pc$chn_city)
# 
# # put the postal codes and cities in the pc_list, state_list
# pc_list<-ifelse(is.na(pc_list),chn_missing_pc$chn_pc, pc_list)
# city_list<-ifelse(is.na(city_list),chn_missing_pc$chn_city, city_list)
# state_list<-ifelse(is.na(state_list),chn_missing_pc$chn_state, state_list)
#   
# 
# rm(chn_cities,chn_states,to_delete,chn_result,chn_missing_pc)    









# UK ------------------------------------------------------------------

process_uk_address <- function(x) {
  if (length(x) == 1) {
    return(c(NA, NA)) # Not uk
  }
  
  if ((x[[length(x)]] == "england")|
      (x[[length(x)]] == "scotland")|
      (x[[length(x)]] == "wales")|
      (x[[length(x)]] == "northern ireland")) {
    if (length(x) == 3) {
      return(c(trimws(x[[length(x) - 1]]), trimws(x[[length(x) - 1]])))
    } else {
      return(c(trimws(x[[length(x) - 2]]), trimws(x[[length(x) - 1]])))
    }
  }
  
  return(c(NA, NA)) # Not uk
}

# Apply the function across all addresses using `mapply`
results <- t(mapply(process_uk_address, list_address))
colnames(results) <- c("uk_city", "uk_state")




# Clean up results
results <- as.data.frame(results, stringsAsFactors = FALSE)
results$uk_city[results$uk_city == "not uk"] <- NA
results$uk_state[results$uk_state == "not uk"] <- NA



results$uk_pc<-NA

# Define the function
extract_uk_postcodes <- function(df, source_col, target_col,city_col) {
  # Regular expression pattern for UK postal codes
  # One or two initial letters.
  # One or two digits (and possibly a letter).
  # A mandatory space.
  # One digit followed by two letters.
  pattern <- "[A-Za-z]{1,2}[0-9R][0-9A-Za-z]? [0-9][A-Za-z]{2}"
  pattern2 <- "[A-Za-z]{1,2}[0-9R][0-9A-Za-z]? [A-Za-z]{2}"
  pattern3 <- "[A-Za-z]{1,2}[0-9R]{3} [A-Za-z]{3}"
  
  
  # Loop through each row of the dataframe
  for(i in 1:nrow(df)) {
    # Find all matches of the pattern in the source column
    matches <- gregexpr(pattern, df[i, source_col]) 
    # Extract the matches
    extracted_codes <- regmatches(df[i, source_col], matches)
    # If there's at least one match and the target column is NA, copy the first match to the target column
    if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
      df[i, target_col] <- extracted_codes[[1]][1]
      df[i, city_col] <- df[i, source_col]
      
    }
  }
  return(df)
}

# Example usage

results <- extract_uk_postcodes(results, "uk_city","uk_pc", "uk_city")
results <- extract_uk_postcodes(results, "uk_state","uk_pc", "uk_city")

# any without numbers gets NA'd
results$uk_pc[!grepl("\\d", results$uk_pc)] <- NA



results$new_city<-NA


# Define the function
uk_city_id <- function(df, source_col, target_col) {
  # Regular expression pattern for UK postal codes
  # One or two initial letters.
  # One or two digits (and possibly a letter).
  # A mandatory space.
  # One digit followed by two letters.
  pattern <- "[A-Za-z]{1,2}[0-9R][0-9A-Za-z]? [0-9][A-Za-z]{2}"
  
  # Loop through each row of the dataframe
  for(i in 1:nrow(df)) {
    # Find all matches of the pattern in the source column
    matches <- gregexpr(pattern, df[i, source_col])
    # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
    # Extract the matches
    extracted_codes <- regmatches(df[i, source_col], matches)
    # If there's at least one match and the target column is NA, copy the first match to the target column
    if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
      df[i, target_col] <- df[i, source_col]
    }
  }
  return(df)
}

# Example usage

results <- uk_city_id(results, "uk_city","new_city")
results <- uk_city_id(results, "uk_state","new_city")


results$new_city<-sub("[A-Za-z]{1,2}[0-9R][0-9A-Za-z]? [0-9][A-Za-z]{2}","",results$new_city)




# Define the function
uk_city_id <- function(df, source_col, target_col) {
  # Regular expression pattern for UK postal codes
  # One or two initial letters.
  # One or two digits (and possibly a letter).
  # A mandatory space.
  # One digit followed by two letters.
  pattern <- "\\s[A-Za-z0-9]{2,4}\\s[A-Za-z0-9]{2,4}"
  
  # Loop through each row of the dataframe
  for(i in 1:nrow(df)) {
    # Find all matches of the pattern in the source column
    matches <- gregexpr(pattern, df[i, source_col])
    # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
    # Extract the matches
    extracted_codes <- regmatches(df[i, source_col], matches)
    # If there's at least one match and the target column is NA, copy the first match to the target column
    if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
      df[i, target_col] <- df[i, source_col]
    }
  }
  return(df)
}

# Example usage
results <- uk_city_id(results, "uk_state","new_city")



results$uk_state<-ifelse(results$uk_state==results$uk_city,
                              "",results$uk_state)

results$new_city<-ifelse(is.na(results$new_city),
                               results$uk_city,
                         results$new_city)
# remove zip codes from new city
results$new_city<-gsub("\\s[A-Za-z0-9]{3}\\s[A-Za-z0-9]{3}","",results$new_city)
results$new_city<-gsub("\\s[A-Za-z0-9]{4}\\s[A-Za-z0-9]{2,3}","",results$new_city)


results$new_city<-ifelse(results$uk_state=="london",
                         "london",
                         results$new_city)


results$uk_state<-ifelse(results$uk_state=="london",
                         NA,
                      results$uk_state)










# keep portions with numbers / remove city names
# results$uk_city<-sub("[A-Za-z]{1,2}[0-9R][0-9A-Za-z]? [0-9][A-Za-z]{2}","",results$uk_city)
# results$uk_state<-sub("[A-Za-z]{1,2}[0-9R][0-9A-Za-z]? [0-9][A-Za-z]{2}","",results$uk_state)
# results$uk_pc<-sub(".*\\b(\\w{3})\\b.*\\b(\\w{3})\\b$", "\\1 \\2", results$uk_pc)

# results$can_pc<-gsub("\\b[^\\d\\s]+\\b", "", results$can_pc) #+[a-z] removes 2nd ltr

results$uk_pc[results$uk_pc == ""] <- NA
# now remove any PC from city

# Update `city_list` if necessary
city_list <- ifelse(is.na(city_list), results$new_city, city_list)

# Update `state_list` if necessary
state_list<-ifelse(is.na(state_list),results$uk_state, state_list)

# Update `pc_list` if necessary
pc_list<-ifelse(is.na(pc_list),results$uk_pc, pc_list)

rm(results)




















# Extracts postal code when combined with city name -----------------------

city_list <- trimws(city_list, which = "both")



  city_clean<-data.frame(
    authorID=ID,
    addresses=addresses,
    original_city=city_list,
    city_list=city_list,
    state_list=state_list,
    country_list=country_list,
    extract_pc=pc_list)
    

  # # England, Scotland, Wales ---------------------------------------------
  # 
  # city_clean$extract_pc<-ifelse(is.na(city_clean$extract_pc)& (city_clean$country_list=="england"|
  #                                                                city_clean$country_list=="wales" |                               
  #                                                                city_clean$country_list=="scotland"), 
  #                               gsub(".*\\s([a-z0-9]+\\s[a-z0-9]+)$", "\\1", city_clean$city_list), 
  #                               city_clean$extract_pc)
  # 
  # # This then deletes the postal code from the city name
  # city_clean$city_list<-ifelse((city_clean$country_list=="england"|
  #                                 city_clean$country_list=="wales" |
  #                                 city_clean$country_list=="scotland"),
  #                              (gsub("([A-Za-z0-9]+\\s[A-Za-z0-9]+)$", "", city_clean$city_list)), 
  #                              city_clean$city_list)
  # 
  city_clean[city_clean == ""] <- NA
  
  # Define the function
  extract_postcodes <- function(df, source_col, target_col) {
    # One or two initial letters.
    # mandatory dash
    # several numbers
    pattern <- "\\b[A-Za-z]{1,2}[-][0-9]{4,8}\\b"
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Find all matches of the pattern in the source column
      matches <- gregexpr(pattern, df[i, source_col])
      # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
      # Extract the matches
      extracted_codes <- regmatches(df[i, source_col], matches)
      # If there's at least one match and the target column is NA, copy the first match to the target column
      if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
        df[i, target_col] <- extracted_codes[[1]][1]
        }
    }
    return(df)
  }
  
  # Example usage
  
  city_clean <- extract_postcodes(city_clean, "city_list","extract_pc")
  
  

  # Define the function
  extract_postcodes <- function(df, source_col, target_col) {
    # One or two initial letters.
    # mandatory dash
    # several numbers
    pattern <- " [0-9]{3,9}"
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Find all matches of the pattern in the source column
      matches <- gregexpr(pattern, df[i, source_col])
      # matches <- gregexpr(paste(pattern1,collapse = "|"), df[i, source_col])
      # Extract the matches
      extracted_codes <- regmatches(df[i, source_col], matches)
      # If there's at least one match and the target column is NA, copy the first match to the target column
      if(length(extracted_codes[[1]]) > 0 && is.na(df[i, target_col])) {
        df[i, target_col] <- extracted_codes[[1]][1]
        }
    }
    return(df)
  }
  
  
  # Example usage
  
  city_clean <- extract_postcodes(city_clean, "city_list","extract_pc")
  
  
  
  # Define the function
  delete_matching_text <- function(df, col_a, col_b) {
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Check if the value in column A is not NA and is found within the text in column B
      if(!is.na(df[i, col_a]) && grepl(df[i, col_a], df[i, col_b])) {
        # Remove the matching text from column B by replacing it with an empty string
        df[i, col_b] <- gsub(df[i, col_a], "", df[i, col_b])
      }
    }
    return(df)
  }
  
  city_clean <- delete_matching_text(city_clean, "extract_pc","city_list")
  
  city_clean <- delete_matching_text(city_clean, "extract_pc","state_list")
  
  
  # remove state if same as city
  
  city_clean$state_list<-ifelse(city_clean$state_list==city_clean$city_list,NA, city_clean$state_list)
  
  
  # there are some usa ones that had state zip buyt no country
  
  
  # Define the function
  extract_postcodes <- function(df, country_list, extract_pc, state_list) {
    # One or two initial letters.
    # mandatory dash
    # several numbers
    pattern <- "\\b[A-Za-z]{2} [0-9]{5}\\b"
    # Loop through each row of the dataframe
    for(i in 1:nrow(df)) {
      # Find all matches of the pattern in the source column
      matches <- gregexpr(pattern, df[i, country_list])
      # Extract the matches
      extracted_codes <- regmatches(df[i, country_list], matches)
      # If there's at least one match and the target column is NA, copy the first match to the target column
      if(length(extracted_codes[[1]]) > 0 && is.na(df[i, extract_pc])) {
        df[i, extract_pc] <- extracted_codes[[1]][1]
        df[i, state_list] <- extracted_codes[[1]][1]
        df[i, country_list] <- "usa"
      }
    }
    return(df)
  }
  
  
  # Example usage
  
  city_clean <- extract_postcodes(city_clean, 
                                  "country_list", "extract_pc", "state_list")
  
  
  # remove zip codes from states
  city_clean$state_list<-ifelse(city_clean$country_list=="usa",
                                gsub("[0-9]","",city_clean$state_list),
                                city_clean$state_list)
  
  # remove state from zipcode 
  city_clean$extract_pc<-ifelse(city_clean$country_list=="usa",
                                gsub("[a-z]","",city_clean$extract_pc),
                                city_clean$extract_pc)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  city_clean$extract_pc <- trimws(gsub("&", "", city_clean$extract_pc))
  city_clean[city_clean == ""] <- NA
  
#   
#   
#   
#   
#   
head(city_clean)
#   
#   # Cleaning up city names with zip codes in them
#   # take the zip code and put it in a new column before deleting
#   
# 
#   
# 
# # 2) Countries with postal code AFTER city --------------------------------
# 
#   
#   city_zip<-c("ireland","qatar","kazakhstan","peru","turkey","south korea",
#               "japan","costa rica","mexico","new zealand","iran","thailand",
#               "russia","spain","india","singapore","indonesia","chile",
#               "finland","colombia","taiwan","saudi arabia","uruguay",
#               "slovenia","spain")
#   
#     
#   
#   city_clean$extract_pc<-ifelse((is.na(city_clean$extract_pc)& (city_clean$country_list %in% city_zip)),
#                                                                 (gsub(".*[A-Za-z]+", "", city_clean$city_list)),
#                                                                 city_clean$extract_pc)
#   # 
#    city_clean$city_list<-ifelse((city_clean$country_list %in% city_zip),
#                                 gsub("\\s([0-9]+)", "", city_clean$city_list), 
#                                 city_clean$city_list)
#   
#     city_clean[city_clean == ""] <- NA
#   
# 
# # 3) Postal code and dash BEFORE city name --------------------------------
# 
#     
#     zip_dash<-c("finland","slovakia","austria","portugal","belgium",
#                 "spain","israel","czech republic","argentina","france",
#                 "sweden","switzerland","turkey","germany","italy",
#                 "lithuania","hungary","denmark","poland","norway", "iceland",
#                 "greece", "ukraine","estonia","latvia","luxembourg","lativa",
#                 "south africa","bulgaria","brazil")
#     
#     
#     
#     
#     city_clean$extract_pc <- ifelse((is.na(city_clean$extract_pc) & 
#                                       (city_clean$country_list %in% zip_dash)),
#                                     # gsub("\\s([A-Za-z]+)", "", city_clean$city_list),
#                                     sub(" .*", "", city_clean$city_list),
#                                     city_clean$extract_pc)
#     
#     
#     city_clean$city_list<-ifelse((city_clean$country_list %in% zip_dash),
#                                  gsub("[a-zA-Z]+-[0-9]+", "", city_clean$city_list),
#                                   city_clean$city_list)
#     
#   
#   city_clean[city_clean == ""] <- NA
# 
# # 4) Netherlands Postal Code ----------------------------------------------
# 
#   # Netherlands has Postal Code before
#   # it is a combination of 2-3 blocks of letters and numbers 
#   city_clean$extract_pc<-ifelse(is.na(city_clean$extract_pc) & (city_clean$country_list=="netherlands"),
#                                 (gsub("^(([^ ]+ ){2}).*", "\\1", city_clean$city_list)), 
#                                 city_clean$extract_pc)
#   city_clean$city_list<-ifelse((city_clean$country_list=="netherlands"),
#                                (gsub(".*\\s", "", city_clean$city_list)),
#                                city_clean$city_list)
#   city_clean[city_clean == ""] <- NA
# 
# # 5) Venezuela  -----------------------------------------------------------
# 
#   # Venezuela has postal code after, it is combo of letters and numbers
#   city_clean$extract_pc<-ifelse(is.na(city_clean$extract_pc) & (city_clean$country_list=="venezuela"),
#                                 gsub(".*\\s([a-z0-9]+)$", "\\1", city_clean$city_list), 
#                                 city_clean$extract_pc)
#   
#   # This then deletes the postal code from the city name
#   city_clean$city_list<-ifelse(city_clean$country_list=="venezuela",
#                                (gsub("(\\s[A-Za-z0-9]+)$", "", city_clean$city_list)), 
#                                city_clean$city_list)
#   city_clean[city_clean == ""] <- NA
#   
#   
#   
#   # trim ws
#   city_clean$extract_pc <- trimws(city_clean$extract_pc, which = "both")
#   city_clean$city_list <- trimws(city_clean$city_list, which = "both")
#   # This removes any that don't have numbers in them
#   city_clean$extract_pc[!grepl("[0-9]", city_clean$extract_pc)] <- NA
#   city_clean$city_list <- gsub("[0-9]+", "", city_clean$city_list)
#   
#   
# Final Clean Up ----------------------------------------------------------

  # Russia
  city_clean$city_list<-ifelse(city_clean$country_list=="russia",
                               (gsub("st Petersburg p", "st petersburg", city_clean$city_list)),
                               city_clean$city_list)
  
  
  
  # India
  India_delete<- c("dept")
  
  city_clean$city_list <- ifelse(city_clean$country_list=="india",
                                 gsub(paste(India_delete, collapse = "|"), NA, city_clean$city_list),
                                 city_clean$city_list)
  city_clean$city_list<-trimws(city_clean$city_list) # Remove leading/trailing whitespace
  
  # brazil
  city_clean$extract_pc<-ifelse((city_clean$country_list=="brazil" & nchar(city_clean$extract_pc) < 5),
                                "",city_clean$extract_pc)
  city_clean[city_clean == ""] <- NA
  
  brazil_delete<- c("barretos canc hosp","univ fed sao paulo",
                    "escola filosofia letras & ciencias humanas",
                    "hosp sirio libanes","perola byington hosp",
                    "sp","univ fed sao paulo","nacl","pesquisa", "museu","dept",
                    "lab","zoologia", "inst", "programa","ppg", "ppg")
  

    city_clean$city_list <- ifelse(city_clean$country_list=="brazil",
      gsub(paste(brazil_delete, collapse = "|"), NA, city_clean$city_list),
      city_clean$city_list)
    
    city_clean$city_list<-trimws(city_clean$city_list, which="both") # Remove leading/trailing whitespace
    city_clean$state_list<-trimws(city_clean$state_list, which="both") # Remove leading/trailing whitespace
    city_clean$country_list<-trimws(city_clean$country_list, which="both") # Remove leading/trailing whitespace) # Remove leading/trailing whitespace
    # City Abbreviations
    
    city_clean$city_list<-ifelse(city_clean$city_list=="university pk",
                                 "university park",
                                 city_clean$city_list)
    
    
    city_clean$city_list<-ifelse(city_clean$city_list=="n chicago",
                                 "north chicago",
                                 city_clean$city_list)
    
    city_clean$city_list<-ifelse(city_clean$city_list=="college pk",
                                 "college park",
                                 city_clean$city_list)
    
    city_clean$city_list<-ifelse(city_clean$city_list=="research triangle pk",
                                 "research triangle park",
                                 city_clean$city_list)
    
    city_clean$city_list<-ifelse(city_clean$city_list=="state coll",
                                 "state college",
                                 city_clean$city_list)
    
    # city corrections
    city_clean$city_list<-ifelse((city_clean$city_list=="dehra dun" & city_clean$country_list == "india"),
                                 "dehradun",
                                 city_clean$city_list)
    
    city_clean$city_list<-ifelse((city_clean$city_list=="st john" & city_clean$country_list == "canada"),
                                 "st. john's",
                                 city_clean$city_list)
    
    city_clean$state_list<-ifelse(city_clean$state_list=="london ",
                                 "london",
                                 city_clean$state_list)
    
    city_clean$city_list<-ifelse((city_clean$state_list=="london" & city_clean$country_list == "england"),
                                 "london",
                                 city_clean$city_list)
    
    
    city_clean$state_list<-ifelse(city_clean$state_list=="london",
                                 NA,
                                 city_clean$state_list)
    
    
    
  city_list<-city_clean$city_list
  state_list<-city_clean$state_list
  pc_list<-city_clean$extract_pc
  country_list<-city_clean$country_list
  
  
  rm(brazil_delete,India_delete)
  

  # rm(city_clean)
  
  
  pc_list[pc_list == ""] <- NA
  city_list[city_list == ""] <- NA
  state_list[state_list == ""] <- NA
  dept_list[dept_list == ""] <- NA
  country_list[country_list == ""] <- NA
  # Create the df that will be returned
  cleaned_ad<-data.frame(ID,
                         addresses,
                         university_list,
                         dept_list,
                         city_list,
                         country_list,
                         state_list,
                         pc_list)
  
  
  
  # names(cleaned_ad)
  
  
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

  
  # original
  # a_df <- data.frame(
  #   adID = ID, university = university_list,
  #   country = country_list,
  #   state = state_list, postal_code = pc_list, city = NA,
  #   department = NA, second_tier = second_tier_list,
  #   third_tier = third_tier_list,
  #   remain = remain_list, address = addresses,
  #   stringsAsFactors = FALSE
  # )
  
  # EB EDIT
  a_df_1 <- data.frame(
    adID = ID, 
    university_1 = university_list,
    university = university_list,
    country_1 = country_list,
    country = country_list,
    state_1 = state_list, 
    state = state_list, 
    postal_code_1 = pc_list, 
    postal_code = pc_list, 
    city_1 = city_list,
    city = city_list,
    department_1 = dept_list, 
    department = dept_list, 
    second_tier = second_tier_list,
    third_tier = third_tier_list,
    remain = remain_list, 
    address = addresses,
    stringsAsFactors = FALSE
  )
  
  a_df_1$city_list<-ifelse(is.na(a_df_1$city_1),
                            a_df_1$city, 
                            a_df_1$city_1)
   
  a_df_1$postal_code_1<-ifelse(is.na(a_df_1$postal_code_1),
                            a_df_1$postal_code, 
                            a_df_1$postal_code_1)
  
  
  # Quebec Postal Code is PQ (Fr) or QC (En) but only QC is georeferenced
  a_df_1$state_1<-ifelse(a_df_1$state_1=="pq" & a_df_1$country_1 == "canada",
                                "qc",
                            a_df_1$state_1)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
a_df<-a_df_1

rm(a_df_1)



  # try to fix the usa spots, which vary in format than other countries
  a_df$state<-ifelse(is.na(a_df$state),"",a_df$state) # added by eb to deal with index problem
  a_df$city[nchar(a_df$state) > 0] <- a_df$second_tier[nchar(a_df$state) > 0]
  a_df$state[nchar(a_df$state) == 0] <- NA
  a_df$postal_code[nchar(a_df$postal_code) == 0] <- NA
  a_df$department[!is.na(a_df$state) & !is.na(a_df$postal_code) &
      !is.na(a_df$state)] <- a_df$third_tier[!is.na(a_df$state) &
          !is.na(a_df$postal_code) & !is.na(a_df$state)]
  # fix a US problem when usa is not tacked onto the end

  us_reg <- "[[:alpha:]]{2}[[:space:]]{1}[[:digit:]]{5}"
  a_df$state[ grepl(us_reg, a_df$country) ] <-
    substr(a_df$country[ grepl(us_reg, a_df$country) ], 1, 2)

  a_df$postal_code[ grepl(us_reg, a_df$country) ] <-
    substr(a_df$country[grepl(us_reg, a_df$country)], 4, 8)

  a_df$country[grepl(us_reg, a_df$country)] <- "usa"

  
  a_df$state_1 <- ifelse(a_df$country=="usa" & a_df$country_1!="usa", 
                           a_df$state, 
                           a_df$state_1)
  
  a_df$postal_code_1 <- ifelse(a_df$country=="usa" & a_df$country_1!="usa", 
                         a_df$postal_code, 
                         a_df$postal_code_1)
  
  
  a_df$country_1 <- ifelse(a_df$country=="usa" & a_df$country_1!="usa", 
                           a_df$country, 
                           a_df$country_1)
  
  
  ##########################
  # We'll use regular expression to pull zipcodes
  # These formats differ by region
  int1 <- "[[:alpha:]]{2}[[:punct:]]{1}[[:digit:]]{1,8}"
  int2 <- paste("[[:space:]][[:upper:]][[:digit:]][[:upper:]]",
                 "[[:space:]][[:digit:]][[:upper:]][[:digit:]]", sep="")
  int3 <- "[[:alpha:]][[:punct:]][[:digit:]]{4,7}"
  int4 <- "[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"
  int <- paste(int1, int2, int3, int4, sep = "|")

  uk <- paste("[[:upper:]]{1,2}[[:digit:]]{1,2}[[:space:]]",
              "{1}[[:digit:]]{1}[[:upper:]]{2}", sep="")

  mexico <- "[[:space:]]{1}[[:digit:]]{5}" # technically US as well

  panama <- "[[:digit:]]{4}-[[:digit:]]{5}"

  zip_search <- paste0(int, "|", uk, "|", mexico, "|", panama)

  
  
  
  
  # ADDRD EB INSTEAD OF 
  a_df$city_1 <- ifelse(is.na(a_df$city_1),a_df$third_tier,a_df$city_1)
  a_df$state_1 <- ifelse(is.na(a_df$state_1),a_df$second_tier,a_df$state_1)
  a_df$postal_code_1 <- ifelse(is.na(a_df$postal_code_1),a_df$second_tier,a_df$postal_code_1)
  
  
  # fix country - usa 
  # Function to remove everything before " usa"
  remove_before_usa <- function(x) {
    if (grepl(" usa", x)) {
      return(sub(".*(?= usa)", "", x, perl = TRUE))
    } else {
      return(x)
    }
  }
  
  # Apply the function to each element in the vector
  a_df$country_1 <- sapply(a_df$country_1, remove_before_usa)
  a_df$country_1 <- trimws(a_df$country_1, which = "both")
  
  
  a_df$state_1 <- ifelse(a_df$country_1=="usa", 
                       (trimws(gsub("[0-9]", "", a_df$state_1), which="both")),
                       a_df$state_1)
  
  a_df$postal_code_1 <- ifelse(a_df$country_1=="usa", 
                             (trimws(gsub("[a-z]", "", a_df$postal_code_1), which="both")),
                             a_df$postal_code_1)
  
  
  
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
    city <- a_df$city[row]
    state <- a_df$state[row]
    postal_code <- a_df$postal_code[row]
    department <- a_df$department[row]
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
    # based on if it has university/college in the name,
    # where university overides college
    override_univ <- grepl("\\buniv\\b|\\buniversi",
      c(second_tier, third_tier, remain, city, university),
      ignore.case = TRUE) &
      !grepl("\\bdrv\\b|\\bdrive\\b",
        c(second_tier, third_tier, remain, city, university),
        ignore.case = TRUE)

    if (any(override_univ)) {
      university <-
        c(second_tier, third_tier, remain, city, university)[override_univ][1]
      assign(
        c("second_tier", "third_tier", "remain", "city", "university")[
          override_univ][1],
        NA
      )
    }
    # only if university doesnt already exist
    override_univ_col <-
      grepl("\\bcol\\b|college|\\bcoll\\b",
        c(second_tier, third_tier, remain, city, university),
        ignore.case = TRUE) &
      !grepl("\\bdrv\\b|\\bdrive\\b",
        c(second_tier, third_tier, remain, city, university),
        ignore.case = TRUE)

    if (!any(override_univ) & any(override_univ_col)) {
      university <-
        c(second_tier, third_tier, remain, city, university )[
          override_univ_col][1]

      assign(
        c("second_tier", "third_tier", "remain", "city", "university")[
          override_univ_col][1],
        NA
      )
    }
    # more risky, but institutions as well, just incase its not a university
    override_univ_inst <- grepl("\\binst\\b|\\binstitut",
      c(second_tier, third_tier, remain, city, university),
      ignore.case = TRUE)
    if (
      !any(override_univ) & !any(override_univ_col) & any(override_univ_inst)
      ) {
      department <- c(second_tier, third_tier, remain, city, university )[
        override_univ_inst][1]

      assign(
        c("second_tier", "third_tier", "remain", "city", "university")[
          override_univ_inst][1],
        NA
      )
    }

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
  # a_df$country[a_df$country == "peoples r china"] <- "China"
  # a_df$country[a_df$country == "U Arab Emirates"] <- "United Arab Emirates"
  # a_df$country[a_df$country == "Mongol Peo Rep"] <- "Mongolia"
  
  a_df$postal_code[grepl("[[:alpha:]]{1,2}-", a_df$postal_code)] <-
    vapply(strsplit(
      a_df$postal_code[ grepl("[[:alpha:]]{1,2}-", a_df$postal_code)],
      "-"),
      function(x) x[2], character(1)
    )
  #strip periods from the ends of city,state,country
  a_df$city <- gsub("\\.", "", a_df$city)
  a_df$state <- gsub("\\.", "", a_df$state)
  a_df$country <- gsub("\\.", "", a_df$country)
  a_df$country[a_df$country == ""] <- NA
  a_df$university[a_df$university == ""] <- NA
  a_df$postal_code[a_df$postal_code == ""] <- NA
  #convert to lower
  for (l in 2:ncol(a_df)){
    a_df[, l] <- tolower(a_df[, l])
  }
  
  # Select columns 
  a_df <- a_df[, c("adID", 
                     "university_1", 
                     "country_1", 
                     "state_1",
                     "postal_code_1", 
                     "city_1", 
                     "department_1",
                     "second_tier", 
                     "third_tier", 
                     "remain", 
                     "address")
                 ]
  
  # Rename columns
  colnames(a_df) <- c("adID", 
                      "university", 
                      "country", 
                      "state", 
                      "postal_code",
                      "city", 
                      "department", 
                      "second_tier", 
                      "third_tier", 
                      "remain",
                      "address")
  
  
  # sometimes the postal code fails to prse out of state. canm use this 
  # when postal code is missing, but then need to remove
    # Function to extract numbers from one column and copy them to another column
    extract_numbers <- function(df, source_col, target_col) {
      if (is.na(target_col)) {
        
        
        df[[target_col]] <- gsub(".*?(\\d+).*", "\\1", df[[source_col]])
        df[[target_col]][!grepl("\\d", df[[source_col]])] <- NA
        return(df)
        
      } else {
        return(df)
      }
      
    }
    
    # Apply the function to the dataframe
    a_df <- extract_numbers(a_df, "state", "postal_code")
    
  
    
    # ther postal code and city are sometimes in tier3 
    a_df$city <- ifelse(is.na(a_df$city), a_df$third_tier, a_df$city)
    a_df$postal_code <- ifelse(is.na(a_df$postal_code), a_df$third_tier, a_df$postal_code)
    
    
    
  
  
  # Function to remove matching characters from col1 based on col2
  remove_matching <- function(col1, col2) {
    pattern <- paste0("\\b", gsub("([\\W])", "\\\\\\1", col2), "\\b")
    result <- sub(pattern, "", col1)
    trimws(result)
  }
  
  # Apply the function to each row
  a_df$city <- mapply(remove_matching, a_df$city, a_df$postal_code)
  a_df$state <- mapply(remove_matching, a_df$state, a_df$postal_code)
  
  
  
  library(tidyverse)
  
  
  country<- a_df %>% 
    # mutate(city_match=(city==second_tier)) %>% 
    # filter(city_match==FALSE) %>% 
    distinct(country) %>%
    mutate(summary=nchar(country)) %>% 
    arrange(country) 
  
  
  country_city<- a_df %>% 
    # mutate(city_match=(city==second_tier)) %>% 
    # filter(city_match==FALSE) %>% 
    distinct(country,city) %>%
    mutate(city_char=nchar(city)) %>% 
    arrange(country,city) 
  
  
  
  country_state<- a_df %>% 
    # mutate(city_match=(city==second_tier)) %>% 
    # filter(city_match==FALSE) %>% 
    distinct(country,state) %>%
    mutate(city_char=nchar(state)) %>% 
    arrange(country,state) 
  
  country_state_city<- a_df %>% 
    # mutate(city_match=(city==second_tier)) %>% 
    # filter(city_match==FALSE) %>% 
    distinct(country ,state,city) %>%
    mutate(city_char=nchar(city)) %>% 
    arrange(country,state,city) 
  
  
  country_state_city_pc<- a_df %>% 
    # mutate(city_match=(city==second_tier)) %>% 
    # filter(city_match==FALSE) %>% 
    distinct(country ,state,postal_code,city) %>%
    mutate(city_char=nchar(city)) %>% 
    arrange(country,state,postal_code,city) 
  
                return(a_df)
}
