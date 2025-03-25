#' Parses out address information and splits it into its respective parts.
#' This is an internal function used by \code{authors_clean}. Note that parsing
#' addresses is surprisingly difficult, largely because there is no standard
#' format across journals/countries for how there are reported. For example:
#' Ex 1) some journals use dept, univ, city, state, postal code, country
#' Ex 2) others use univ, dept, country, postal code.
#' Ex 3) Postal code is sometimes in the same cell as country, other times not.
#'
#' \code{authors_address} This function takes the output from
#' \code{references_read} and pulls out address information. Splitting it into
#' university, department, city, state, etc.
#' @param addresses the addresses
#' @param ID the authorID
#' @noRd
authors_address <- function(addresses, ID) {
  addresses <- tolower(addresses)

  message("\nSplitting addresses.\n")
  
  list_address <- strsplit(addresses, ",")



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

  # trim ws -----------------------------------------------------------------

  list_address <- lapply(list_address, trimws)

  # correct countries -------------------------------------------------------


  # format or update names of some countries to make it possible to georef
  # the WOS often uses abbreviations, this standardizes them in a way that
  # `tidygeocoder` can use them. It also updates country names that have changed
  # (e.g., czechia is new name for czech republic). In some cases no changes
  # were made (e.g., united arab rep = current country name depends on city).

  # Define the function
  correct_countries <- function(my_list, replacements) {
    # Loop through each element of the list
    for (i in 1:length(my_list)) {
      # Get the length of the current element
      len <- length(my_list[[i]])

      # Check if the last item matches any of the target words
      if (len > 0 && my_list[[i]][len] %in% names(replacements)) {
        # Replace the last item with the corresponding replacement word
        my_list[[i]][len] <- replacements[[my_list[[i]][len]]]
      }
    }
    return(my_list)
  }

  replacements <- c(
    "austl" = "australia",
    "c z" = "czechia",
    "cz" = "czechia",
    "czech republic" = "czechia",
    "fed rep ger" = "germany",
    "columbia" = "colombia", # a depressingly common mistake
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

  message("\nstandardizing country names...\n")
  
  list_address <- correct_countries(list_address, replacements)

  # extract university ------------------------------------------------------

  message("\nextracting the names of institutions...\n")
  
  university_list <- vapply(list_address, function(x) x[1], character(1))

  # extract department ------------------------------------------------------

  #   If department is listed it is typically second
  #   (EB note: only if 4+ slots)
  #   this will be 2x checked later

  dept_extract <- function(x) {
    if (length(x) < 4) {
      return(NA)
    } else {
      return(trimws(x[[2]]))
    }
  }

  dept_list <- unlist(lapply(list_address, dept_extract))

  dept_list <- trimws(dept_list, which = "both")


  # Extract City ------------------------------------------------------------

  message("\nextracting cities...\n")
  
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

    # Default case
    return(second_last)
  }, character(1))

  # Cleanup
  city_list <- trimws(city_list, which = "both")
  city_list[city_list == "no city"] <- NA


  # extract state -----------------------------------------------------------
  message("\nextracting states/provinces...\n")
  
  # If there is only one element, then it can't have both city and country'
  state_list <- vapply(list_address, function(x) {
    n <- length(x)
    if (n == 1) {
      return("no state") # placeholder to replace with NA after function
    }

    # In some cases city is next-to-last element, in others next-to-next-to-last
    last_element <- x[[n]]
    second_last <- if (n > 1) x[[n - 1]] else NA
    third_last <- if (n > 2) x[[n - 2]] else NA

    # Default case
    return(third_last)
  }, character(1))

  # Cleanup
  state_list <- trimws(state_list, which = "both")
  state_list[state_list == "no state"] <- NA

  # this is used to double check later - sometimes city is extracted as state
  city_list2 <- trimws(state_list, which = "both")

  # Extract Country ---------------------------------------------------------

  message("\nextracting country...\n")
  
  country_list <- vapply(
    list_address, function(x) {
      gsub("\\_", "", x[length(x)])
    },
    character(1)
  )


  # postal code (pc) list ---------------------------------------------------

  message("\nprocessing postal codes...\n")
  
  # pc often with city

  pc_list <- city_list

  # bind all into df --------------------------------------------------------
  
  message("\nreview, correction, and clean-up...\n")
  message("\nPlease be patient - this might take a bit.\n")

  a_df <- data.frame(
    adID = ID,
    university = university_list,
    country = country_list,
    state = state_list,
    postal_code = pc_list,
    city = city_list,
    city2 = city_list2,
    department = dept_list,
    address = addresses,
    stringsAsFactors = FALSE
  )



  # any PC without numbers gets NA'd
  a_df$postal_code[!grepl("\\d", a_df$postal_code)] <- NA

  # copy over PC and state
  a_df$state <- ifelse(grepl("usa", a_df$country) & nchar(a_df$state) > 2,
    NA,
    a_df$state
  )


  a_df$postal_code <- ifelse(grepl("[a-z]{2} [0-9]{5} usa", a_df$country),
    a_df$country, a_df$postal_code
  )

  a_df$state <- ifelse(grepl("[a-z]{2} [0-9]{5} usa", a_df$country),
    a_df$country, a_df$state
  )

  a_df$state <- ifelse(grepl("[a-z]{2} [0-9]{5}", a_df$city),
    a_df$city, a_df$state
  )


  a_df$state <- ifelse(grepl("[a-z]{2} usa", a_df$country),
    a_df$country, a_df$state
  )

  # remove the numbers and letters as appropriate


  a_df$country <- ifelse(grepl(" usa", a_df$country),
    "usa", a_df$country
  )

  a_df$state <- ifelse(a_df$country == "usa" & grepl(
    "[a-z]{2} [0-9]{5}",
    a_df$state
  ),
  gsub("[[:digit:]]{5}", "", a_df$state),
  a_df$state
  )

  a_df$state <- ifelse(a_df$country == "usa" & grepl(" usa", a_df$state),
    gsub(" usa", "", a_df$state),
    a_df$state
  )


  a_df$postal_code <- ifelse(a_df$country == "usa",
    gsub(
      "[[:alpha:]]{2} ", "",
      a_df$postal_code
    ), a_df$postal_code
  )

  a_df$postal_code <- ifelse(a_df$country == "usa",
    gsub(
      " usa", "",
      a_df$postal_code
    ), a_df$postal_code
  )




  a_df$city <- ifelse(a_df$country == "usa" & grepl(
    "[a-z]{2} [0-9]{5}",
    a_df$city
  ),
  a_df$city2,
  a_df$city
  )


  pattern <- "[a-z]{2} [0-9]{5}"

  a_df$postal_code <- ifelse(grepl(pattern, a_df$country),
    a_df$country, a_df$postal_code
  )
  a_df$state <- ifelse(grepl(pattern, a_df$country),
    a_df$country, a_df$state
  )
  a_df$country <- ifelse(grepl(pattern, a_df$country),
    "usa", a_df$country
  )
  a_df$postal_code <- ifelse(a_df$country == "usa" & grepl(pattern, a_df$postal_code),
    gsub("[a-z]", "", a_df$postal_code),
    a_df$postal_code
  )
  a_df$state <- ifelse(a_df$country == "usa" & grepl(pattern, a_df$state),
    gsub(
      "[0-9]",
      "",
      a_df$postal_code
    ),
    a_df$state
  )



  # BRAZIL clean-up ---------------------------------------------------------

  a_df$state <- ifelse(a_df$country == "brazil" & nchar(a_df$city) == 2,
    a_df$city,
    a_df$state
  )
  a_df$city <- ifelse(a_df$country == "brazil" & nchar(a_df$city) == 2,
    a_df$city2,
    a_df$city
  )
  a_df$city2 <- ifelse(a_df$country == "brazil" & a_df$city == a_df$city2,
    NA,
    a_df$city2
  )
  a_df$postal_code <- ifelse(a_df$country == "brazil" & is.na(a_df$postal_code),
    a_df$city,
    a_df$postal_code
  )
  a_df$state <- ifelse(a_df$country == "brazil" & nchar(a_df$state) > 2,
    NA,
    a_df$state
  )

  a_df$postal_code <- ifelse(a_df$country == "brazil",
    gsub(
      "[A-Za-z]",
      "",
      a_df$postal_code
    ),
    a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country == "brazil",
    gsub(
      "[-]",
      "",
      a_df$postal_code
    ),
    a_df$postal_code
  )

  a_df$city <- ifelse(a_df$country == "brazil",
    gsub(
      "br-",
      "",
      a_df$city
    ),
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "brazil",
    gsub(
      "[0-9]",
      "",
      a_df$city
    ),
    a_df$city
  )


  a_df$state <- ifelse(a_df$country == "brazil" & grepl("br-", a_df$city2),
    a_df$city,
    a_df$state
  )
  a_df$postal_code <- ifelse(a_df$country == "brazil" & grepl("br-", a_df$city2),
    a_df$city2,
    a_df$postal_code
  )
  a_df$city <- ifelse(a_df$country == "brazil" & grepl("br-", a_df$city2),
    a_df$city2,
    a_df$city
  )

  message("\n(still working on it...)\n")
  
  # repeat the clean of city
  a_df$city <- ifelse(a_df$country == "brazil",
    gsub("br-", "", a_df$city),
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "brazil",
    gsub("[0-9]", "", a_df$city),
    a_df$city
  )


  a_df$postal_code <- ifelse(a_df$country == "brazil",
    gsub("[A-Za-z]", "", a_df$postal_code),
    a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country == "brazil",
    gsub("[-]", "", a_df$postal_code),
    a_df$postal_code
  )

  a_df[] <- lapply(a_df, trimws)


  # Define city-to-state mapping
  city_state_mapping <- data.frame(
    city = c(
      "ribeirao preto", "sao carlos", "rio claro", "sorocaba",
      "seropedica", "rio de janeiro", "rio janeiro", "sao paulo"
    ),
    state = c("sp", "sp", "sp", "sp", "rj", "rj", "rj", "sp"),
    stringsAsFactors = FALSE
  )

  # Match cities and states
  for (i in 1:nrow(city_state_mapping)) {
    a_df$city <- ifelse(a_df$country == "brazil" &
      grepl(city_state_mapping$city[i],
        a_df$state,
        ignore.case = TRUE
      ),
    city_state_mapping$city[i], a_df$city
    )
    a_df$state <- ifelse(a_df$country == "brazil" &
      grepl(city_state_mapping$city[i],
        a_df$state,
        ignore.case = TRUE
      ),
    city_state_mapping$state[i], a_df$state
    )
  }

  # Match cities and states
  for (i in 1:nrow(city_state_mapping)) {
    a_df$state <- ifelse(a_df$country == "brazil" &
      grepl(city_state_mapping$city[i], a_df$city, ignore.case = TRUE),
    city_state_mapping$state[i],
    a_df$state
    )
  }

  # AUSTRALIA clean-up---------------------------------------------------------------

  a_df$state <- ifelse(a_df$country == "australia",
    a_df$city, a_df$state
  )
  a_df$postal_code <- ifelse(a_df$country == "australia",
    a_df$city, a_df$postal_code
  )
  a_df$city <- ifelse(a_df$country == "australia",
    a_df$city2, a_df$city
  )
  a_df$city2 <- ifelse(a_df$country == "australia",
    NA, a_df$city2
  )


  a_df$postal_code <- ifelse(a_df$country == "australia",
    gsub("[A-Za-z]", "", a_df$postal_code),
    a_df$postal_code
  )
  a_df$state <- ifelse(a_df$country == "australia",
    gsub("[0-9]", "", a_df$state),
    a_df$state
  )

  a_df[] <- lapply(a_df, trimws)



  # CANADA clean-up ---------------------------------------------------------

  a_df$state <- ifelse(a_df$country == "canada" & nchar(a_df$city) == 2,
    a_df$city,
    a_df$state
  )

  a_df$city <- ifelse(a_df$country == "canada" & nchar(a_df$city) == 2,
    NA,
    a_df$city
  )

  a_df$postal_code <- ifelse(a_df$country == "canada" & grepl("\\b(\\w{3})\\b \\b(\\w{3})\\b", a_df$city2),
    a_df$city2,
    a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country == "canada" & grepl("\\b(\\w{3})\\b \\b(\\w{3})\\b", a_df$city2),
    a_df$city2,
    a_df$postal_code
  )

  a_df$state <- ifelse(a_df$country == "canada" & a_df$city2 == a_df$state,
    NA, a_df$state
  )

  a_df$city <- ifelse(a_df$country == "canada",
    a_df$city2,
    a_df$city
  )
  a_df$city2 <- ifelse(a_df$country == "canada",
    NA,
    a_df$city2
  )


  a_df$city <- ifelse(a_df$country == "canada" & grepl("\\b(\\w{3})\\b \\b(\\w{3})\\b", a_df$city),
    gsub(
      "\\b(\\w{3})\\b \\b(\\w{3})\\b",
      "",
      a_df$city
    ),
    a_df$city
  )

  a_df$state <- ifelse(a_df$country == "canada" & is.na(a_df$state),
    a_df$postal_code,
    a_df$state
  )

  a_df$state <- ifelse(a_df$country == "canada" & grepl("\\b(\\w{3})\\b \\b(\\w{3})\\b", a_df$state),
    gsub(
      "\\b(\\w{3})\\b \\b(\\w{3})\\b",
      "",
      a_df$state
    ),
    a_df$state
  )

  a_df$postal_code <- ifelse(a_df$country == "canada" &
    grepl(
      "\\b(\\w{2,20})\\b \\b(\\w{3})\\b \\b(\\w{3})\\b",
      a_df$postal_code
    ),
  gsub(
    "\\b(\\w{1,2}|\\w{4,})\\b",
    "",
    a_df$postal_code
  ),
  a_df$postal_code
  )

  a_df[] <- lapply(a_df, trimws)

  # TODO: a few postal codes still have letters from city


  a_df$postal_code <- ifelse(a_df$country == "canada",
    gsub(" ", "", a_df$postal_code),
    a_df$postal_code
  )


  # UK clean-up -------------------------------------------------------------

  uk <- c("scotland", "england", "wales", "northern ireland")
  pattern <- "[a-z0-9]{2,4} [a-z0-9]{3,4}"
  #
  # a_df$postal_code <- ifelse(a_df$country %in% uk &
  #                              grepl(pattern, a_df$city2),a_df$city2,
  #                            a_df$postal_code)

  a_df$postal_code <- ifelse(a_df$country %in% uk & grepl(pattern, a_df$city2),
    a_df$city2,
    a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country %in% uk & grepl(pattern, a_df$state),
    a_df$state,
    a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country %in% uk & grepl(pattern, a_df$city),
    a_df$city,
    a_df$postal_code
  )

  a_df$postal_code <- ifelse(a_df$country %in% uk,
    ifelse(!grepl("\\d", a_df$postal_code), NA, a_df$postal_code),
    a_df$postal_code
  )

  a_df$city <- ifelse(a_df$country %in% uk & a_df$city == a_df$postal_code,
    NA, a_df$city
  )

  a_df$state <- ifelse(a_df$country %in% uk & a_df$state == a_df$postal_code,
    NA, a_df$state
  )


  a_df$state <- ifelse(a_df$country == "england",
    a_df$city,
    a_df$state
  )
  a_df$city <- ifelse(a_df$country == "england",
    NA,
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "england",
    a_df$postal_code,
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "england",
    gsub("\\b\\w*\\d\\w*\\b", "", a_df$city),
    a_df$city
  )

  message("\n(getting closer...)\n")
  
  # TODO: england still needs work

  a_df$state <- ifelse(a_df$country == "scotland" |
    a_df$country == "northern ireland" |
    a_df$country == "wales",
  NA,
  a_df$state
  )
  a_df$state <- ifelse(a_df$country == "scotland" |
    a_df$country == "northern ireland" |
    a_df$country == "wales" &
      is.na(a_df$state),
  a_df$city,
  a_df$state
  )
  a_df$city <- ifelse(a_df$country == "scotland" |
    a_df$country == "northern ireland" |
    a_df$country == "wales",
  a_df$postal_code,
  a_df$city
  )
  a_df$city <- ifelse(a_df$country == "scotland" |
    a_df$country == "northern ireland" |
    a_df$country == "wales" &
      is.na(a_df$city),
  a_df$city2,
  a_df$city
  )
  a_df$city <- ifelse(a_df$country == "scotland" |
    a_df$country == "northern ireland" |
    a_df$country == "wales",
  gsub(
    "\\b\\w*\\d\\w*\\b",
    "",
    a_df$city
  ),
  a_df$city
  )


  # postal codes clean uk ---------------------------------------------------


  # Define the function
  keep_numerical_parts <- function(df, control_col, country, target_col) {
    # Apply the function to each row using sapply or a loop
    df[[target_col]] <- sapply(1:nrow(df), function(i) {
      if (df[[control_col]][i] == country) {
        # Use gregexpr to find all parts of the string that include a numeral
        matches <- gregexpr("\\b\\S*\\d\\S*\\b", df[[target_col]][i])
        # Extract the matched parts
        result <- regmatches(df[[target_col]][i], matches)
        # Combine the matched parts into a single string
        result <- unlist(result)
        result <- paste(result, collapse = " ")
        result <- gsub(" ", "", result)
        return(result)
      } else {
        return(df[[target_col]][i])
      }
    })

    return(df)
  }


  a_df <- keep_numerical_parts(a_df, "country", "scotland", "postal_code")
  a_df <- keep_numerical_parts(a_df, "country", "england", "postal_code")
  a_df <- keep_numerical_parts(a_df, "country", "northern ireland", "postal_code")
  a_df <- keep_numerical_parts(a_df, "country", "wales", "postal_code")







  # INDIA clean-up ----------------------------------------------------------


  a_df$postal_code <- ifelse(a_df$country == "india" & grepl("[0-9]{5,10}", a_df$city2),
    a_df$city2,
    a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country == "india" & grepl("[0-9]{5,10}", a_df$city),
    a_df$city,
    a_df$postal_code
  )


  a_df$city2 <- ifelse(a_df$country == "india" & a_df$state == a_df$city2,
    a_df$state,
    a_df$city2
  )
  a_df$state <- ifelse(a_df$country == "india", NA, a_df$state)
  a_df$state <- ifelse(a_df$country == "india" & is.na(a_df$postal_code),
    a_df$city,
    a_df$state
  )
  a_df$city <- ifelse(a_df$country == "india" & a_df$state == a_df$city,
    NA,
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "india" & grepl("[0-9]{4,10}", a_df$postal_code),
    a_df$postal_code,
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "india" & is.na(a_df$city),
    a_df$city2,
    a_df$city
  )


  a_df$postal_code <- ifelse(a_df$country == "india",
    gsub("[A-Za-z]", "", a_df$postal_code),
    a_df$postal_code
  )
  a_df$city <- ifelse(a_df$country == "india",
    gsub("[0-9]", "", a_df$city),
    a_df$city
  )

  a_df$city <- ifelse(a_df$country == "india" &
    (grepl("delhi", a_df$city) | grepl("delhi", a_df$state)),
  "new delhi",
  a_df$city
  )


  # CHINA clean-up ----------------------------------------------------------





  a_df$postal_code <- ifelse(a_df$country == "china" &
    grepl("[0-9]{5,10}", a_df$city2),
  a_df$city2,
  a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country == "china" &
    grepl("[0-9]{5,10}", a_df$city),
  a_df$city,
  a_df$postal_code
  )
  a_df$postal_code <- ifelse(a_df$country == "china" &
    grepl("[0-9]{5,10}", a_df$state),
  a_df$state,
  a_df$postal_code
  )


  a_df$city2 <- ifelse(a_df$country == "china" & a_df$state == a_df$city2,
    a_df$state,
    a_df$city2
  )
  a_df$state <- ifelse(a_df$country == "china",
    NA,
    a_df$state
  )
  a_df$state <- ifelse(a_df$country == "china" & is.na(a_df$postal_code),
    a_df$city,
    a_df$state
  )
  a_df$city <- ifelse(a_df$country == "china" & a_df$state == a_df$city,
    NA,
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "china" & grepl("[0-9]{4,10}", a_df$postal_code),
    a_df$postal_code,
    a_df$city
  )
  a_df$city <- ifelse(a_df$country == "china" & is.na(a_df$city),
    a_df$city2,
    a_df$city
  )


  a_df$postal_code <- ifelse(a_df$country == "china",
    gsub("[A-Za-z]", "", a_df$postal_code),
    a_df$postal_code
  )
  a_df$city <- ifelse(a_df$country == "china",
    gsub("[0-9]", "", a_df$city),
    a_df$city
  )



  a_df$city <- ifelse(a_df$country == "china" & grepl("beijing", a_df$state),
    "beijing",
    a_df$city
  )


  # Define words indicating this is actually a dept not state or postal code
  # will use this list to delete the ones that don't apply
  to_delete <- c(
    "&", "inst", "ctr", "med", "chem", "lab", "biol",
    "dept", "div", "univ", "hosp", "coll", "sci", "rd",
    "program", "minist", "educ", "sch ", "grad ", "fac ",
    "assoc", "forest"
  )


  pattern <- paste(to_delete, collapse = "|")
  # Apply the ifelse function to update
  a_df$city <- ifelse(a_df$country == "china" &
    grepl(pattern, a_df$city, ignore.case = TRUE, perl = TRUE),
  NA, a_df$city
  )
  a_df$city <- ifelse(a_df$country == "china" & is.na(a_df$city),
    a_df$state, a_df$city
  )



  a_df[] <- lapply(a_df, trimws)


  message("\n(not much longer...)\n")

  # This verifies that what is in `city` is actually a city
  # (or at least that what is in `city` is NOT a province)

  chn_states <- c(
    "guangdong", "shandong", "henan", "jiangsu", "sichuan",
    "hebei", "hunan", "zhejiang", "anhui", "hubei", "guangxi",
    "yunnan", "jiangxi", "liaoning", "fujian", "shaanxi",
    "guizhou", "shanxi", "chongqing", "heilongjiang", "xinjiang",
    "gansu", "inner mongolia", "jilin", "hainan", "ningxia",
    "qinghai", "tibet", "macao"
  )
  pattern <- paste(to_delete, collapse = "|")
  a_df$city <- ifelse(a_df$country == "china" &
    grepl(pattern, a_df$city, ignore.case = TRUE, perl = TRUE),
  NA, a_df$city
  )



  # pc is letters dash numbers ----------------------------------------------


  pattern <- "\\b[A-Za-z]{1,3}[-][0-9]{3,8}\\b"

  a_df$postal_code <- ifelse(grepl(pattern, a_df$city),
    a_df$city, a_df$postal_code
  )

  a_df$postal_code <- ifelse(grepl(pattern, a_df$state),
    a_df$state,
    a_df$postal_code
  )

  a_df$state <- ifelse((grepl(pattern, a_df$postal_code) & a_df$city2 == a_df$postal_code),
    a_df$city,
    a_df$state
  )


  a_df$city <- ifelse((grepl(pattern, a_df$postal_code) & a_df$city2 == a_df$postal_code),
    a_df$postal_code,
    a_df$city
  )

  a_df$city2 <- ifelse((grepl(pattern, a_df$postal_code) & a_df$city2 == a_df$city),
    NA, a_df$city2
  )



  a_df$city <- ifelse(grepl(pattern, a_df$city),
    gsub("[0-9]", "", a_df$city),
    a_df$city
  )
  a_df$city <- gsub("[a-z]{1,2}- ", "", a_df$city)


  a_df$city <- gsub("[-]", "", a_df$city)
  a_df[] <- lapply(a_df, trimws)


  pattern <- "\\b[A-Za-z]{1,3}[-][0-9]{3,8}\\b"

  a_df$postal_code <- ifelse(grepl(pattern, a_df$postal_code),
    gsub("[a-z]", "", a_df$postal_code),
    a_df$postal_code
  )

  a_df$postal_code <- gsub("[-]", "", a_df$postal_code)
  a_df[] <- lapply(a_df, trimws)


  # final check of postal codes (consecutive nos.) --------------------------


  # Define the function
  extract_consecutive_numbers <- function(df, source, destination) {
    df[[destination]] <- sapply(1:nrow(df), function(i) {
      # Use gregexpr to find sequences of 4 or more consecutive numbers
      if (is.na(df[[destination]][i])) {
        matches <- gregexpr("\\d{4,}", df[[source]][i])
        # Extract the matched sequences
        result <- regmatches(df[[source]][i], matches)
        # Flatten the list of matches into a character vector
        result <- unlist(result)
        # Combine the matched sequences into a single string
        result <- paste(result, collapse = " ")
        return(result)
      } else {
        return(df[[destination]][i])
      }
    })
    return(df)
  }

  a_df <- extract_consecutive_numbers(a_df, "state", "postal_code")



  # clean the city ----------------------------------------------------------

  # remove any digits

  a_df$city <- gsub("[0-9]", "", a_df$city)



  # clean up postal code ----------------------------------------------------


  a_df$postal_code <- ifelse(grepl("\\b[a-zA-Z]+\\s+[0-9]+\\b", a_df$postal_code),
    gsub("\\b[a-zA-Z]+\\s", "", a_df$postal_code),
    a_df$postal_code
  )

  # NETHERLANDS clean-up ----------------------------------------------------
  # cities often have two characters at start (ascii version of ligature/dipthong)

  a_df[] <- lapply(a_df, trimws)
  a_df$city <- ifelse(a_df$country == "netherlands" & grepl("^[a-zA-Z]{2} ", a_df$city),
    (sub("^[a-zA-Z]{2} ", "", a_df$city)), a_df$city
  )

  a_df[] <- lapply(a_df, trimws)



  # Final clean-up of some US cities and states -----------------------------

  a_df$city <- ifelse(a_df$city == "university pk",
    "university park",
    a_df$city
  )

  a_df$city <- ifelse(a_df$city == "college stn",
    "college station",
    a_df$city
  )

  a_df$city <- ifelse(a_df$city == "n chicago",
    "north chicago",
    a_df$city
  )

  a_df$city <- ifelse(a_df$city == "college pk",
    "college park",
    a_df$city
  )

  a_df$city <- ifelse(a_df$city == "research triangle pk" | a_df$city == "res triangle pk",
    "research triangle park",
    a_df$city
  )

  a_df$city <- ifelse(a_df$city == "state coll",
    "state college",
    a_df$city
  )



  a_df$city <- ifelse(grepl("sioux ctr", a_df$city),
    (sub("sioux ctr", "sioux city", a_df$city)), 
    a_df$city
  )



  a_df$city <- ifelse(grepl("sioux ctr", a_df$city),
    (sub("sioux ctr", "sioux city", a_df$city)), 
    a_df$city
  )


  # Final clean-up of some Brazil cities and states -------------------------

  message("\n(almost done...)\n")
  
  a_df$city <- ifelse(a_df$country=="brazil" & grepl("seropedica", a_df$city),
                      "seropedica", 
                      a_df$city
  )

  a_df$city <- ifelse(a_df$country == "brazil" & a_df$city == "gavea rio de janeiro",
    "rio de janeiro",
    a_df$city
  )


  a_df$city <- ifelse((a_df$country == "brazil" & a_df$city == "s jose campos"),
    "sao jose dos campos",
    a_df$city
  )

  a_df$city <- ifelse((a_df$country == "brazil" & (a_df$city == "rio de janerio" |
    a_df$city == "rio de janiero" |
    a_df$city == "rio der janeiro" |
    a_df$city == "rio janeiro" |
    a_df$city == "rio janiero")),
  "rio de janeiro",
  a_df$city
  )

  # Final clean-up of some INDIA cities and states --------------------------



  a_df$city <- ifelse((a_df$city == "dehra dun" & a_df$country == "india"),
    "dehradun",
    a_df$city
  )


  # Final clean-up of some CANADA cities and states -------------------------


  a_df$city <- ifelse((a_df$city == "st john" & a_df$country == "canada"),
    "st. john's",
    a_df$city
  )


  # Final clean-up of some UK cities and states -----------------------------


  a_df$state <- ifelse(a_df$state == "london ",
    "london",
    a_df$state
  )

  a_df$city <- ifelse((a_df$state == "london" & a_df$country == "england"),
    "london",
    a_df$city
  )


  # final clean-up of some MEXICO cities and states -------------------------


  a_df$city <- ifelse(a_df$country == "mexico" & a_df$city == "df",
    "mexico city",
    a_df$city
  )


  # final clean-up of some ARGENTINA cities and states ----------------------



  a_df$city <- ifelse(a_df$country == "argentina" & a_df$city == "df",
    "buenos aires", a_df$city
  )


  # final clean up of some ABBREVIATIONS in city names ----------------------


  a_df$city <- ifelse(grepl("^st ", a_df$city),
    (sub("^st ", "saint ", a_df$city)), a_df$city
  )

  a_df$city <- ifelse(grepl(" st ", a_df$city),
    (sub(" st ", " saint ", a_df$city)), a_df$city
  )

  a_df$city <- ifelse(grepl("^ste ", a_df$city),
    (sub("^ste ", "saint ", a_df$city)), a_df$city
  )



  # removing departments etc allocated to city or state ---------------------

  # use strings of words typical of institutions or departmewnts to remove

  tech_words <- c(
    " lab ", "lab ", " lab", "dept", "hosp", " inst", "inst ", "ctr",
    "unit", "ltd", "minist", "educ", "grad ", " sch ", "sch ", " sch",
    "coll ", " sci ", "natl", "&", " med", "med ",
    "publ", "dept", "biomed", "phys", "technol",
    "engn"
  )
  pattern <- paste(tech_words, collapse = "|")

  a_df$city <- ifelse((a_df$city != "esch sur alzette" & grepl(pattern, a_df$city, ignore.case = TRUE, perl = TRUE)),
    a_df$state, a_df$city
  )


  a_df$state <- ifelse(a_df$state == a_df$city2, NA, a_df$state)

  a_df$state <- ifelse(grepl("[[:digit:]]", a_df$state),
    NA, a_df$state
  )

  a_df$state <- ifelse(a_df$state == "", NA, a_df$state)

  a_df$postal_code <- ifelse(a_df$postal_code == "", NA, a_df$postal_code)



  # still some us states not extracting properly but fixed here -------------

  message("\n(so close...the end is in sight!)\n")

  us_state_abbreviations_lower <- c(
    "al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga",
    "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md",
    "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj",
    "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc",
    "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy"
  )
  pattern <- paste(us_state_abbreviations_lower, collapse = "|")
  a_df$country_list <- country_list
  a_df$state <- ifelse((a_df$country == "usa" &
    is.na(a_df$state) &
    grepl(pattern, a_df$address, ignore.case = TRUE, perl = TRUE)),
  a_df$country_list,
  a_df$state
  )


  a_df$state <- ifelse((a_df$country == "usa" & grepl("[[:digit:]]", a_df$state)),
    gsub("[[:digit:]]", "", a_df$state),
    a_df$state
  )
  a_df$state <- ifelse((a_df$country == "usa" & grepl("usa", a_df$state)),
    gsub("usa", "", a_df$state),
    a_df$state
  )
  a_df$state <- trimws(a_df$state, which = "both")


  
  
  # Remove panama canal zone from usa states (for stri)
  a_df$state <- ifelse((a_df$country == "usa" & a_df$state == "cz"),
                       NA, 
                       a_df$state
  )
  
  # armed forces & diplomatic
  a_df$state <- ifelse((a_df$country == "usa" & a_df$state == "aa"),
                       NA, 
                       a_df$state
  )
  
  a_df$city <- ifelse((a_df$country == "usa" & a_df$state == "apo"),
                      NA, 
                      a_df$city
  )
  
  a_df$city <- ifelse((a_df$country == "usa" & a_df$state == "dpo"),
                      NA, 
                      a_df$city
  )


  # Japanese prefectures & cities sometimes swapped in address --------------



  to_delete <- c(
    "&", "inst", "ctr", "med", "chem", "lab", "biol",
    "dept", "div", "univ", "hosp", "coll", "sci", "rd",
    "program", "minist", "educ", "sch ", "grad ", "fac ",
    "assoc", "forest", "corp"
  )
  pattern <- paste(to_delete, collapse = "|")
  a_df$city2 <- ifelse((a_df$country == "japan" &
    grepl(pattern, a_df$city2, ignore.case = TRUE, perl = TRUE)),
  NA,
  a_df$city2
  )

  # Remove any with numbers
  a_df$city2 <- ifelse((a_df$country == "japan" &
    grepl("[[:digit:]]", a_df$city2)),
  NA,
  a_df$city2
  )

  japan_prefectures <- c(
    "hokkaido", "aomori", "iwate", "miyagi", "akita",
    "yamagata", "fukushima", "ibaraki", "tochigi", "gunma",
    "saitama", "chiba", "tokyo", "kanagawa", "niigata",
    "toyama", "ishikawa", "fukui", "yamanashi", "nagano", "gifu",
    "shizuoka", "aichi", "mie", "shiga", "kyoto", "osaka", "gumma",
    "hyogo", "nara", "wakayama", "tottori", "shimane",
    "okayama", "hiroshima", "yamaguchi", "tokushima", "kagawa",
    "ehime", "kochi", "fukuoka", "saga", "nagasaki", "kumamoto",
    "oita", "miyazaki", "kagoshima", "okinawa"
  )
  pattern <- paste(japan_prefectures, collapse = "|")


  a_df$state <- ifelse((a_df$country == "japan" &
    grepl(pattern, a_df$city, ignore.case = TRUE, perl = TRUE)),
  a_df$city,
  a_df$state
  )


  # This removes all special regions of a city like tokyo from city2
  a_df$city2 <- ifelse((a_df$country == "japan" &
    grepl(" ku", a_df$city2,
      ignore.case = TRUE, perl = TRUE
    )),
  NA,
  a_df$city2
  )

  # replace from city with city2 EXCEPT in cases where no state (and therefore
  # city is correct) and where no city2 (otherwise would bring in NA)

  a_df$city <- ifelse((a_df$country == "japan" &
    !(is.na(a_df$state)) & !(is.na(a_df$city2))),
  a_df$city2,
  a_df$city
  )



  # fine-tuning SCOTLAND ----------------------------------------------------



  a_df$city <- ifelse((a_df$country == "scotland" &
    grepl("univ ", a_df$city, ignore.case = TRUE, perl = TRUE)),
  gsub("univ ", "", a_df$city),
  a_df$city
  )

  to_delete <- c(
    " ave", " grp", "hlth", " rd", "mrc", " oba", "plz",
    " dr", "oqb", " quad", "fisheries"
  )

  pattern <- paste(to_delete, collapse = "|")
  a_df$city <- ifelse((a_df$country == "scotland" &
    grepl(pattern, a_df$city, ignore.case = TRUE, perl = TRUE)),
  NA,
  a_df$city
  )



  # fine-tuning ENGLAND -----------------------------------------------------

  message("\n(this is it - the last step!)\n")
  
  
  to_delete <- c(
    "&", "inst", "ctr", "med", "chem", "lab", "biol",
    "dept", "div", "univ", "hosp", "coll", "sci", "rd",
    "program", "minist", "educ", "sch ", "grad ", "fac ",
    " sq", "quarter", " way", " dr", "diagnost", "consultant",
    "microsoft", "diagnost", "[[:digit:]]", "project", "facil", "grp",
    "campus", "expt", " pk", "canc", "assoc", "forest", "corp",
    "consortium", "partners", "lane", "ucl", "street", "trust",
    "business", "inform", "royal", "survey", "drosophila", " st",
    "ndorms", "nat hist", "hlth", " ave", "council", "unit", "nerc", "nat res"
  )
  pattern <- paste(to_delete, collapse = "|")

  a_df$city2 <- ifelse((a_df$country == "england" &
    grepl(pattern, a_df$city2, ignore.case = TRUE, perl = TRUE)),
  NA,
  a_df$city2
  )

  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)),
    a_df$city2,
    a_df$city
  )


  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)) &
    grepl("london", a_df$address, ignore.case = TRUE, perl = TRUE),
  "london",
  a_df$city
  )

  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)) &
    grepl("cambridge", a_df$address, ignore.case = TRUE, perl = TRUE),
  "cambridge", a_df$city
  )

  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)) &
    grepl("oxford", a_df$address, ignore.case = TRUE, perl = TRUE),
  "oxford",
  a_df$city
  )

  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)) &
    grepl("durham", a_df$address, ignore.case = TRUE, perl = TRUE),
  "durham", a_df$city
  )

  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)) &
    grepl("bristol", a_df$address, ignore.case = TRUE, perl = TRUE),
  "bristol",
  a_df$city
  )

  a_df$city <- ifelse((a_df$country == "england" & is.na(a_df$city)) &
    grepl("lancaster", a_df$address, ignore.case = TRUE, perl = TRUE),
  "lancaster",
  a_df$city
  )




# final clean up to return ------------------------------------------------

  
  
  # delete columns used to 2x check

  a_df$city2 <- NULL
  a_df$country_list <- NULL
  
  # replace blank with NA
  
  a_df[a_df == ""] <- NA


  # return output of function -----------------------------------------------


  return(a_df)
}
