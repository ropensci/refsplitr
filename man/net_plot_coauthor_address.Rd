\name{net_plot_coauthor_address}
\alias{net_plot_coauthor_address}
\title{Creates a network diagram of coauthors' addresses linked by reference, and with nodes arranged geographically}
\usage{
net_plot_coauthor_address(addresses, authors__references)
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
\code{net_plot_coauthor_address} This function takes an
addresses data.frame, links it to an authors__references
dataset and plots a network diagram generated for
individual points of co-authorship.
}

