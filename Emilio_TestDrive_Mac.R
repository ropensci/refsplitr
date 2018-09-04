library(devtools)
devtools::install_github("embruna/refnet2")
library(Refnet)

################
eb_references<-references_read(data = './data/EBpubs.txt', dir = F,filename_root = './output/EB')
eb_authors<-authors_clean(eb_references, filename_root="./output/eb")
eb_authors <- authors_refine(eb_authors, eb_authors_master, filename_root="./output/eb")

# Error in if (nrow(authors) == 0) { : argument is of length zero
#   In addition: Warning message:
#     In min(authors$similarity, na.rm = T) :
#     no non-missing arguments to min; returning Inf

eb_authors <- authors_refine(eb_authors$authors, eb_authors$master, filename_root="./output/eb")

#same error, probably because authors is blank (no dupes)
authors_georef(eb_authors)
authors_georef(eb_authors$master)


################

WOS2018_references <- references_read(data = './data/WOS2018', dir = T,filename_root = './output/WOS2018')
WOS2018_authors <- authors_clean(WOS2018_references, filename_root="./output/WOS2018")
WOS2018_authors <- authors_refine(WOS2018_authors, WOS2018_authors_master, filename_root="./output/WOS2018")

# Same error as above until...

# Error in if (nrow(authors) == 0) { : argument is of length zero
#   In addition: Warning message:
#     In min(authors$similarity, na.rm = T) :
#     no non-missing arguments to min; returning Inf


# make the following change....
WOS2018_authors <- authors_refine(WOS2018_authors$authors, WOS2018_authors$master, filename_root="./output/WOS2018")
# Note this deletes authors and authors master...is that right?

# Made a smaller file to work from so it would take less time
WOS2018_authors_short<-head(WOS2018_authors,300)
#Gets hung up with this on the screen: [1] "Trying data science toolkit first..."

authors_georef(WOS2018_authors_short)
#Gets hung up with this on the screen: [1] "Trying data science toolkit first..."

