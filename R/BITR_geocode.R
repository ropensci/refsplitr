#' Georeferenced data from the journal Biotropica (pulled from Web of Science)
#'
#' A dataset containing 41 authors taken from the Biotropica journal. 
#' This dataset represents the typical formatted output 
#' from \code{authors_georef()}
#' in the refsplitr package. It serves as a useful testing data set for
#' spatial functions and
#'
#' @format A data frame with 41 rows and 15 variables:
#' \describe{
#'   \item{authorID}{ID field populated in authors_clean}
#'   \item{university}{also can be considered 
#'   institution for non-universities}
#'   \item{postal_code}{character, international postcode}
#'   \item{country}{country name}
#'   \item{lat}{numeric, latitude populated from authors_georef}
#'   \item{lon}{numeric, longitude populated from authors_georef}
#'   \item{groupID}{ID field for what name group the author 
#'   is identified as from authors_clean()}
#'   \item{author_order}{numeric, order of author from journal article}
#'   \item{address}{address of references pulled from 
#'   the original raw WOS file}
#'   \item{department}{department which is nested within university}
#'   \item{RP_address}{reprint address, pulled from the original raw WOS file}
#'   \item{RI}{ResearcherID number, identifier given by 
#'   web of science only, less common than OrcID}
#'   \item{OI}{OrcID, unique identifier for 
#'   researcher given by https://orcid.org }
#'   \item{UT}{unique identifier to each article, given by WOS}
#'   \item{refID}{unique identifier for each article, 
#'   given by references_read()}
#' }
"BITR_geocode"