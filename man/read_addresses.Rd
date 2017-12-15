\name{read_addresses}
\alias{read_addresses}
\title{Processes multiple records of a data.frame of addresses and attempts to geocode them using refnet_geocode()}
\usage{
read_addresses(x, filename_root = "", verbose = FALSE)
}
\arguments{
  \item{x}{references data.frame that should contain three
  columns, the first of which is assumed to be the AU_ID,
  the second of which can be a code or description of the
  type of addresses being processed (e.g. "C1") and the
  third being the address itself, stripped to something
  Google Maps can accurately locate.}

  \item{filename_root}{the filename root, can include
  relative or absolute path, to which "_addresses.csv" will
  be appended and the output from the function will be
  saved}

  \item{verbose=FALSE}{argument that when set to TRUE will
  output search strings and output from the Google Maps API
  call as it proceeds, useful for troubleshooting
  long-running calls to the function}
}
\description{
\code{read_addresses} This function is based on the geoPlot
addrListLookup() function, and takes an address string and
uses the Google Maps API to geocode the address if
possible.  Though the geoPlot package addrListLookup()
function works, it seems inconsistent in the way it handles
returns, especially in bulk, so I'm going to redefine it
adding in a wait period, re-test, and an ability to
simplify the address down to city/state/country.
}

