#' Extracts the lat and long for each address from authors_clean
#'
#' \code{authors_georef} This function takes the final author list from 
#' refine_authors, and calculates the lat long of the addresses. 
#' It does this by feeding the addresses into data science toolkit. 
#' In order to maximize effectiveness and mitigate errors in parsing addresses
#' We run this multiple times creating addresses in different ways
#' in hopes that the data science toolkit can recognize an address
#' 1st. University, city, zipcode, country
#' 2nd. City, zipcode, country
#' 3rd. city, country
#' 4th. University, country
#' 
#' The output is a list with three data.frames
#' \code{addresses} is a data frame with all information from 
#' refine_authors plus new location columns and calculated lat longs.
#' \code{missing addresses} is a data frame with all addresses could
#' not be geocoded
#' \code{addresses} is a data frame like \code{addresses} except
#' the missing addresses are gone.
#'
#' @param data dataframe from `authors_refine()`
#' @param address_column name of column in quotes where the addresses are
#' @importFrom ggmap geocode
#' 
#' @examples 
#' 
#' data(BITR)
#' authors <- authors_clean(BITR)
#' refined <- authors_refine(authors$review, authors$prelim, 0.9, 5)
#' 
#' \dontrun{
#' georef_df <- authors_georef(refined, 'address')
#' }
#' @export authors_georef 
#' 
authors_georef <- function(data,
  address_column = "address") {

  options(ggmap = list(display_api_key = FALSE))
  if (!is.character(data$address)) {
    stop("Address columns are not characters,
    please change to characters and try again")
  }
  addresses <- data[, c("university", "city", "state", "country",
    "postal_code", "authorID", "address")]
  #Change some formatting to help data science toolkit
  addresses$university[is.na(addresses$university)] <- ""
  addresses$country[is.na(addresses$country)] <- ""
  addresses$postal_code[is.na(addresses$postal_code)] <- ""
  addresses$city[is.na(addresses$city)] <- ""
  addresses$state[is.na(addresses$state)] <- ""
  addresses$country <- trimws(addresses$country, which = "both")
  addresses$city <- trimws(addresses$city, which = "both")
  addresses$state <- trimws(addresses$state, which = "both")
  addresses$university <- trimws(addresses$university, which = "both")

  # create Short form address base to defaul address
  # rougly adheres to universty, city, zipcode, country
  addresses$base <- addresses$country
  addresses$base[addresses$postal_code != ""] <-
    paste0(addresses$base[addresses$postal_code != ""],
      ", ",
      addresses$postal_code[addresses$postal_code != ""])

  addresses$base[addresses$state != ""] <-
    paste0(addresses$state[addresses$state != ""],
      ", ",
      addresses$country[addresses$state != ""])

  # second tier, city > zip > university
  addresses$second <- NA
  addresses$second[addresses$city != ""] <- addresses$city[addresses$city != ""]
  addresses$second[is.na(addresses$second) & addresses$university != ""] <-
    addresses$university[is.na(addresses$second) & addresses$university != ""]

  addresses$short_address <- addresses$base
  addresses$short_address[!is.na(addresses$second)] <-
    paste0(addresses$second[!is.na(addresses$second)],
      ", ",
      addresses$short_address[!is.na(addresses$second)])
  addresses$lat <- NA
  addresses$lon <- NA
  addresses$adID <- seq_len(nrow(addresses))

  # # we'll check if data science toolkit is working, by pinging a known address
  check_ad <- "1600 Pennsylvania Ave NW, Washington, DC 20500"
  check.open <- sum(is.na(ggmap::geocode(check_ad, source = "dsk"))) == 0
  if (!check.open) {
    stop("data science toolkit is down right now, please try again later")
  }

  #Lets try broad strokes first. Our 4 layered address
  for (i in addresses$adID[addresses$short_address != ""]) {
    address <- as.character(addresses$short_address[i])
    #if (address == '') next
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
      output = "latlona",
      source = "dsk",
      messaging = TRUE
    ))
    addresses$lat[addresses$adID == i] <- result[[2]]
    addresses$lon[addresses$adID == i] <- result[[1]]
  }

  # Now lets try using a shorter code (city, state, country)
  remain <- addresses[is.na(addresses$lat), ]
  remain$short_address <-
    ifelse(!(is.na(remain$state) | is.na(remain$country)),
      paste0(remain$city, ", ", remain$state, ", ", remain$country),
      NA)
  remain <- remain[!is.na(remain$short_address) &
      remain$short_address != ", , ", ]

  for (i in remain$adID) {
    address <- as.character(remain$short_address[remain$adID == i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
      output = "latlona",
      source = "dsk",
      messaging = TRUE
    ))
    addresses$lat[addresses$adID == i] <- result[[2]]
    addresses$lon[addresses$adID == i] <- result[[1]]
  }

  # Now try city, country
  remain <- addresses[is.na(addresses$lat), ]
  remain$short_address <-
    ifelse(!(is.na(remain$city) | is.na(remain$country)),
      paste0(remain$city, ", ", remain$country),
      NA)

  remain <- remain[!is.na(remain$short_address) &
      remain$short_address != ", ", ]
  for (i in remain$adID) {
    address <- as.character(remain$short_address[remain$adID == i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
      output = "latlona",
      source = "dsk",
      messaging = TRUE
    ))
    addresses$lat[addresses$adID == i] <- result[[2]]
    addresses$lon[addresses$adID == i] <- result[[1]]
  }

  # Finally try using just university, country
  remain <- addresses[is.na(addresses$lat), ]
  remain$short_address <-
    ifelse(!(is.na(remain$university) | is.na(remain$country)),
      paste0(remain$university, ", ", remain$country),
      NA)

  remain <- remain[!is.na(remain$short_address) &
      remain$short_address != ", ", ]
  for (i in remain$adID) {
    address <- as.character(remain$short_address[remain$adID == i])
    message(paste("Working... ", address))
    suppressWarnings(result <- ggmap::geocode(address,
      output = "latlona",
      source = "dsk",
      messaging = TRUE
    ))
    addresses$lat[addresses$adID == i] <- result[[2]]
    addresses$lon[addresses$adID == i] <- result[[1]]
  }

  ## Change "" back to NA
  addresses$country[addresses$country == ""] <- NA
  addresses$university[addresses$university == ""] <- NA
  addresses$postal_code[addresses$postal_code == ""] <- NA

  addresses <-
    merge(
      addresses[, c("authorID", "university", "postal_code",
        "country", "lat", "lon")],
      data[, c("authorID", "groupID", "author_order", "address",
        "department", "RP_address", "RI", "OI", "UT", "refID")],
      by = "authorID", all.y = TRUE)

  missingaddresses <- addresses[is.na(addresses$lat), ]
  addresses$lat <- unlist(addresses$lat)
  addresses$lon <- unlist(addresses$lon)

  outputlist <- list()
  outputlist$addresses <- addresses
  outputlist$missing_addresses <- missingaddresses
  outputlist$not_missing_addresses <- addresses[!is.na(addresses$lat), ]

  # reset ggmaps option to TRUE. This only until the ggmaps gets fixed
  on.exit(options(ggmap = list(display_api_key = TRUE)))
  return(outputlist)
}