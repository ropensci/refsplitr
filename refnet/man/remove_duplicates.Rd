\name{remove_duplicates}
\alias{remove_duplicates}
\title{Processes the output of read_authors() to remove any flagged duplicate authors}
\usage{
remove_duplicates(authors, authors__references, addresses = "",
  filename_root = "")
}
\arguments{
  \item{authors}{the first list item from the output from
  the read_authors() function}

  \item{authors__references}{the second list item from the
  output from the read_authors() function}

  \item{addresses}{an optional argument, that will allow
  you to recode the AU_ID of corresponding, removed
  duplicates, should be a data.frame object output from the
  read_addresses() function}

  \item{filename_root}{the filename root, can include
  relative or absolute path and will be appended to output
  file names function will be saved}
}
\description{
\code{remove_duplicates} This function remove duplicates
from the combined author and authors__references
data.frames that you give it by merging the information
from all records with the same AU_ID_Dupe value and
replacing any of the merged records AU_ID values in the
authors__references list with the AU_ID_Dupe value.
}

