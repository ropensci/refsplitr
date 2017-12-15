\name{refnet_geocode}
\alias{refnet_geocode}
\title{Uses the Google Maps API to process a data.frame of addresses and attempts to geocode them}
\usage{
refnet_geocode(x, verbose = FALSE)
}
\arguments{
  \item{x}{references data.frame that should contain three
  columns, the first of which is assumed to be the AU_ID,
  the second of which can be a code or description of the
  type of addresses being processed (e.g. "C1") and the
  third being the address itself, stripped to something
  Google Maps can accurately locate.}

  \item{verbose=FALSE}{argument that when set to TRUE will
  output search strings and output from the Google Maps API
  call as it proceeds, useful for troubleshooting
  long-running calls to the function}
}
\description{
\code{refnet_geocode} This function is based on the geoPlot
geoAddress() function, and takes an address string and uses
the Google Maps API to geocode the address if possible.
Unlike the original function this one saves all of the
first Placemark data returned for further use.
}

