\name{net_plot_coauthor}
\alias{net_plot_coauthor}
\title{Creates a network diagram of coauthors' countries linked by reference}
\usage{
net_plot_coauthor(addresses, authors__references)
}
\arguments{
  \item{addresses}{output from the read_addresses()
  function, containing geocoded address latitude and
  longitude locations.}

  \item{authors__references}{output from the read_authors()
  function, which links author addresses together via
  AU_ID.}
}
\description{
\code{net_plot_coauthor} This function takes an addresses
data.frame, links it to an authors__references dataset and
plots a network diagram generated for co-authorship.
}

