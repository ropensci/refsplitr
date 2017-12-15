\name{read_authors}
\alias{read_authors}
\title{Processes the output of read_references() to extract authors}
\usage{
read_authors(references, filename_root = "")
}
\arguments{
  \item{references}{output from the read_references()
  function}

  \item{filename_root}{the filename root, can include
  relative or absolute path, to which "_authors.csv" and
  "_authors__references.csv" will be appended and the
  output from the function will be saved}
}
\description{
\code{read_authors} This function extracts authors from a
read_references format data.frame object, uses a
Jaro-Winkler comparison of first names to try to match
authors with multiple Last Name, Initial combinations,
filling in potential matches using the AU_ID_Dupe and
Similarity fields in the resulting output.  The output is a
list containing two data.frame objects, one named authors
and the other authors__references, which is a linking table
that links authors by the AU_ID field to references data
via the UT (Web of Knowledge ID) field.
}

