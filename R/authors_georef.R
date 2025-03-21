#' Extracts the lat and long for each address from authors_clean
#'
#' \code{authors_georef} This function takes the final author list from
#' refine_authors, and calculates the lat long of the city, country, and postal
#' code (for USA addresses) or city and country (for addresses outside the USA).
#'
#' The output is a list of three data.frames
#' \code{addresses} All info from 'refine_authors' plus new columns with
#' lat & long. It includes ALL addresses, including those that could not
#' be geocoded.
#' \code{missing_addresses} A data frame of the addresses that could
#' NOT be geocoded.
#' \code{no_missing_addresses} the \code{addresses} data frame with ONLY the
#' addresses that were geocoded.
#'
#' @param data dataframe from `authors_refine()`
#' @param address_column name of column in quotes where the addresses are
#' @param google_api if `google_api = FALSE` georeferencing is carried out with
#' the `tidygeocoder` package (option `geocode()` with  `method = 'osm'`).
#' If `google_api = TRUE`, then geocoding is done with the Google Maps API.
#' Defaults to `FALSE`.
#' @importFrom ggmap geocode
#'
#' @examples
#' \dontrun{
#' BITR_georef_df <- authors_georef(BITR_refined, address_column = "address")
#' }
#' @export authors_georef
#'
authors_georef <- function(
    data,
    address_column = "address",
    google_api = FALSE) {
  if (google_api == TRUE) {
    pt1 <- ("Attention: You have chosen to geocode with the GOOGLE API.\n")
    pt2 <- ("This is NOT a free service.\n")
    pt3 <- ("Please refer to Google's current billing rates & usage limits.\n")

    message(paste(pt1, pt2, pt3, sep = ""))
    rm(pt1, pt2, pt3)


    options(ggmap = list(display_api_key = FALSE))

    if (!is.character(data$address)) {
      stop("Address columns are not characters,
         please change to characters and try again")
    }
    addresses <- data[, c(
      "university", "city", "state", "country",
      "postal_code", "authorID", "address"
    )]
    # Change some formatting to help data science toolkit
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
      paste0(
        addresses$base[addresses$postal_code != ""],
        ", ",
        addresses$postal_code[addresses$postal_code != ""]
      )

    addresses$base[addresses$state != ""] <-
      paste0(
        addresses$state[addresses$state != ""],
        ", ",
        addresses$country[addresses$state != ""]
      )

    # second tier, city > zip > university
    addresses$second <- NA
    addresses$second[addresses$city != ""] <- addresses$city[addresses$city != ""]
    addresses$second[is.na(addresses$second) & addresses$university != ""] <-
      addresses$university[is.na(addresses$second) & addresses$university != ""]

    addresses$short_address <- addresses$base
    addresses$short_address[!is.na(addresses$second)] <-
      paste0(
        addresses$second[!is.na(addresses$second)],
        ", ",
        addresses$short_address[!is.na(addresses$second)]
      )
    addresses$lat <- NA
    addresses$lon <- NA
    addresses$adID <- seq_len(nrow(addresses))

    # # we'll check if data science toolkit is working, by pinging a known address
    check_ad <- "1600 Pennsylvania Ave NW, Washington, DC 20500"
    check.open <- sum(is.na(ggmap::geocode(check_ad, source = "google", urlonly = TRUE))) == 0
    if (!check.open) {
      stop("google geocoding API is down right now, please try again later")
    }

    # Lets try broad strokes first. Our 4 layered address

    ggmap::register_google(
      key = ggmap::google_key(),
      write = TRUE,
      second_limit = 50,
      day_limit = 2500
    )

    for (i in addresses$adID[addresses$short_address != ""]) {
      address <- as.character(addresses$short_address[i])
      # if (address == '') next
      message(paste("Working... ", address))

      suppressWarnings(result <- ggmap::geocode(address,
        output = "latlona",
        source = "google",
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
        NA
      )
    remain <- remain[!is.na(remain$short_address) &
      remain$short_address != ", , ", ]

    for (i in remain$adID) {
      address <- as.character(remain$short_address[remain$adID == i])
      message(paste("Working... ", address))
      suppressWarnings(result <- ggmap::geocode(address,
        output = "latlona",
        source = "google",
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
        NA
      )

    remain <- remain[!is.na(remain$short_address) &
      remain$short_address != ", ", ]
    for (i in remain$adID) {
      address <- as.character(remain$short_address[remain$adID == i])
      message(paste("Working... ", address))
      suppressWarnings(result <- ggmap::geocode(address,
        output = "latlona",
        source = "google",
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
        NA
      )

    remain <- remain[!is.na(remain$short_address) &
      remain$short_address != ", ", ]
    for (i in remain$adID) {
      address <- as.character(remain$short_address[remain$adID == i])
      message(paste("Working... ", address))
      suppressWarnings(result <- ggmap::geocode(address,
        output = "latlona",
        source = "google",
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
        addresses[, c(
          "authorID", "lat", "lon"
        )],
        data[, c(
          "authorID", "groupID", "author_order", "address",
          "department", "RP_address", "RI", "OI", "UT", "refID"
        )],
        by = "authorID", all.y = TRUE
      )

    missingaddresses <- addresses[is.na(addresses$lat), ]
    addresses$lat <- unlist(addresses$lat)
    addresses$lon <- unlist(addresses$lon)

    outputlist <- list()
    outputlist$addresses <- addresses
    outputlist$missing_addresses <- missingaddresses
    outputlist$no_missing_addresses <- addresses[!is.na(addresses$lat), ]

    # reset ggmaps option to TRUE. This only until the ggmaps gets fixed
    on.exit(options(ggmap = list(display_api_key = TRUE)))
    return(outputlist)
  } else {
    pt1 <- ("You are Geocoding with OpenStreetMap.\n")
    pt2 <- ("This proceeds at a rate of 1 address/second.\n")
    pt3 <- ("For large data sets: OSM requests that you consider downloading\n")
    pt4 <- ("the complete database to query locally instead of using the API.\n")
    pt5 <- ("See the Refsplitr vignette for more information.\n")
    message(paste(pt1, pt2, pt3, pt4, pt5, sep = ""))
    rm(pt1, pt2, pt3, pt4, pt5)



    if (!is.character(data$address)) {
      stop("Address columns are not characters,
         please change to characters and try again")
    }
    # a_df <- data[, c(
    #   "university", "city", "state", "country",
    #   "postal_code", "authorID", "address"
    # )]

    a_df <- data[, c(
      "city", "state", "country",
      "postal_code", "authorID"
    )]
    a_df$country[a_df$country == "could not be extracted"] <- NA
    a_df$state[a_df$state == "no state"] <- NA
    a_df <- a_df[!is.na(a_df$country), ]
    # select the following columns from the fll dataframe
    # a_df<-("authorID", "city","state","postal_code","country")
    a_df$addr <- NA


    a_df$addr <- ifelse(a_df$country == "usa",
      ifelse(!is.na(a_df$state),
        ifelse(!is.na(a_df$postal_code),
          paste(a_df$city,
            a_df$state,
            a_df$postal_code,
            a_df$country,
            sep = ","
          ),
          paste(a_df$city,
            a_df$state,
            a_df$country,
            sep = ","
          )
        ),
        ifelse(!is.na(a_df$postal_code),
          paste(a_df$city,
            a_df$postal_code,
            a_df$country,
            sep = ","
          ),
          paste(a_df$city,
            a_df$country,
            sep = ","
          )
        )
      ),
      paste(a_df$city,
        a_df$country,
        sep = ","
      )
    )


    #
    # a_df$addr <- ifelse(is.na(a_df$state),
    #   paste(a_df$city, a_df$country, sep = ","),
    #   paste(a_df$city, a_df$state, a_df$country, sep = ",")
    # )

    a_df$addr <- ifelse(a_df$country == "Could not be extracted",
      NA,
      a_df$addr
    )
    to_georef_df <- a_df$addr

    # Find unique values of the 'id' column and keep all other columns


    to_georef_df <- unique(a_df$addr)
    to_georef_df <- as.data.frame(to_georef_df)
    colnames(to_georef_df) <- c("addr")

    # to_georef_df <- na.omit(to_georef_df)

    to_georef_df <- to_georef_df |> tidygeocoder::geocode(addr,
      method = "osm",
      lat = latitude, long = longitude
    )
    no_latlon <- to_georef_df[is.na(to_georef_df$latitude), ]
    perc_missing <- (nrow(no_latlon) / nrow(to_georef_df)) * 100

    pt1 <- c(paste("Unable to georef ",
      round(perc_missing, 2), "% of author addresses.\n",
      sep = ""
    ))
    pt2 <- c("Check `outputlist$missing_addresses` to see which ones.\n")
    message(paste(pt1, pt2, sep = ""))
    rm(pt1, pt2, perc_missing)

    # These get merged back into the original
    a_df <-
      merge(
        to_georef_df[, c(
          "addr", "latitude", "longitude"
        )],
        a_df,
        by = "addr", all.y = TRUE
      )

    data <-
      merge(
        a_df[, c(
          "authorID", "latitude", "longitude"
        )],
        data,
        by = c("authorID"), all.y = TRUE
      )

    names(data)[names(data) == "latitude"] <- "lat"
    names(data)[names(data) == "longitude"] <- "lon"


    no_georef <- data[is.na(data$lat), ]

    addresses <- data
    missingaddresses <- data[is.na(data$lat), ]
    addresses$lat <- unlist(data$lat)
    addresses$lon <- unlist(data$lon)

    outputlist <- list()
    outputlist$addresses <- addresses
    outputlist$missing_addresses <- missingaddresses
    outputlist$no_missing_addresses <- addresses[!is.na(addresses$lat), ]
    pt1 <- ("The output is a list with three data.frames:\n")
    pt2 <- ("outputlist$addresses: all info from 'refine_authors'
          plus new `lat` & `long` columns. It includes ALL addresses,
          including those that could not be geocoded. \n")
    pt3 <- ("outputlist$missing_addresses: Includes only the addresses that
            could NOT be geocoded.\n")
    pt4 <- ("outputlist$no_missing_addresses: Includes only the addresses
          that WERE geocoded. \n")
    message(paste(pt1, pt2, pt3, pt4, sep = ""))
    rm(pt1, pt2, pt3, pt4)

    return(outputlist)
  }
}
