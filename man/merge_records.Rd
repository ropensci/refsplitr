\name{merge_records}
\alias{merge_records}
\title{Merges two sets of references/authors/authors__references/addresses objects}
\usage{
merge_records(references, authors, authors__references, addresses = "",
  references_merge, authors_merge, authors__references_merge,
  addresses_merge = "", filename_root = "")
}
\arguments{
  \item{references}{references data.frame to merge to}

  \item{authors}{authors data.frame to merge to}

  \item{authors__references}{authors__references data.frame
  to merge to}

  \item{addresses}{optional addresses data.frame to merge
  to, if you only have addresses from the second set then
  they will be retained}

  \item{references_merge}{references data.frame to merge
  with references argument}

  \item{authors_merge}{authors data.frame to merge with
  authors argument}

  \item{authors__references_merge}{authors__references
  data.frame to merge with}

  \item{addresses_merge}{optional addresses data.frame to
  merge with}

  \item{filename_root}{the filename root, can include
  relative or absolute path and will be appended to output
  file names function will be saved}
}
\description{
\code{merge_records} This function will merge two sets of
references/authors/authors__references/addresses, making
sure that there are no duplicates in references, no
duplicates AU_IDs in the authors table, renaming the AU_IDs
in the linking table appropriately, and then doing an
automated potential match of authors just like when
processing references for the first time.
}

