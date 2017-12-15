\name{read_references}
\alias{read_references}
\title{Reads Thomson Reuters Web of Knowledge/Science and ISI reference export files}
\usage{
read_references(data = ".", dir = TRUE, filename_root = "")
}
\arguments{
  \item{data}{either a directory, used in conjuction with
  dir=TRUE, or a file name to load}

  \item{dir}{if TRUE then data is assumed to be a directory
  name from which all files will be read, but if FALSE then
  data is assumed to be a single file to read}

  \item{filename_root}{the filename root, can include
  relative or absolute path, to which "_references.csv"
  will be appended and the output from the function will be
  saved}
}
\description{
\code{read_references} This function reads Thomson Reuters
Web of Knowledge and ISI format reference data files into
an R friendly data format and can optionally write the
converted data to a friendly CSV format.
}

