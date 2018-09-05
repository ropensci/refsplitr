library(devtools)
devtools::install_github("embruna/refnet")
library(refnet)


################
eb_references<-references_read(data = './data/EBpubs.txt', dir = F,filename_root = './output/EB')
eb_authors<-authors_clean(eb_references, filename_root="./output/eb")

# This doesn't work
eb_authors <- authors_refine(eb_authors, eb_authors_master, filename_root="./output/eb")

# Error in if (nrow(authors) == 0) { : argument is of length zero
#   In addition: Warning message:
#     In min(authors$similarity, na.rm = T) :
#     no non-missing arguments to min; returning Inf

eb_authors <- authors_refine(eb_authors$authors, eb_authors$master, filename_root="./output/eb")

# Now it works, and I like this message: 

# [1] "Authors data.frame is empty. This likely means there are no authors that need to be handchecked, outputting the master file. You can use the $master output from authors_clean() for your next task."
# 
# BUT 
#
# I would suggest we actually have it create the "final" file by duplicating master to be consistent with other cases
# 
# AND
# it also gives this message and it took me a sec to realize this message was the same thing in R-speak.
# Maybe we can supress this messaage OR put your nice one afterwards to explain it away?
# 
# Warning message:
#   In min(authors$similarity, na.rm = T) :
#   no non-missing arguments to min; returning Inf


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
WOS2018_authors_refined <- authors_refine(WOS2018_authors$authors, WOS2018_authors$master, filename_root="./output/WOS2018")
# Note this deletes authors and authors master...is that right?
# Got this warning after it completed refining.
# Warning message:
#   In names(sys.call()) %in% c("", "exact") :
#   closing unused connection 3 (./data/WOS2018/E.txt)


# Made a smaller file to work from so it would take less time
WOS2018_authors_short<-head(WOS2018_authors,300)
#Gets hung up with this on the screen: [1] "Trying data science toolkit first..."

authors_georef(WOS2018_authors_short)
#Gets hung up with this on the screen: [1] "Trying data science toolkit first..."

#######################################
#######################################

# put your working directory into the folder where your refnet folder is located. 
refnet_path<-path.package("refnet", quiet = FALSE)
refnet_path<-gsub("/refnet","",refnet_path)
setwd(refnet_path)

library(devtools)
logs <- check('refnet')


# Status: 1 ERROR
# See
# ‘/private/var/folders/6c/gxnkrl150x7ddwk0t88t9j6c0000gn/T/Rtmp53HSVY/refnet.Rcheck/00check.log’
# for details.
# 
# Warning message:
#   roxygen2 requires Encoding: UTF-8 

# Change this for your own computer
setwd("~/Dropbox (UFL)/RESEARCH/Refnet")
save(logs, file="mac_logs.Rdata")


# SOME SUGGESTIONS
# 1) Tweak names of saved csv outputs and data frames:
#     XX_references: this is fine
#     `XX_authors_master` -> `XX_authors_prelim` or `XX_authors_initial`.  `Master list` sounds like the supreme, total correct, list (plus it vaguely reminds me of Master Race, which is a  very depressing sign of the times). This file is not the Master, it’s actually the First Pass, and may include some mistakes - that’s why we generate a list of names to review). Sometyhing that still needs to be reviewed is Preliminary or Initial, no?    
#     XX_authors -> XX_authors_review  This is the list of groupings to review (and possibly correct) 
#     XX_authors_final -> XX_authors_refined.  Nothing is ever final (raise your hnd if you have manuscript drafts called "final2, final3, final_really"). This file is not the Master, it’s actually the First Pass, and may include some mistakes - that’s why we generate a list of names to review). Sometyhing that still needs to be reviewed is Preliminary or Initial, no?
# 
# 
# 2) eb_authors <- authors_refine(eb_authors$authors... completely replaces the original eb_authors (2 lists) - both of which are useful (and if you need them you'd have to rerun the whole analysis)')  
#   Can we change this to  "authors_refined <- authors_refine(authors$authors..." so that it saves the oroiginalariginal XX_authors (2 lists) and creates a new dataframe of the refined 
# 
# 3) postal codes with leading zeros:the leading zeros are being deleted: 00100 becomes 100, 00681 becomes 681 
#
# 4) postal codes for Brazil have a BR in front of them that doesn't need to be there BR-01059970 -> 01059970
#
# 5) Use XX_refined when no corrections necessary for consistency in step-by-step (even if it just duplicates the master list file) 