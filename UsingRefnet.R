## This is EB's edit's to the instructions on using refnet Forrest wrote up.
## Don't forget that to add your commits to the fork you need to do the following: 
## In RStudio menus go to "Tools"->"Shell" and type the following: git push origin proposed-updates
## See: http://r-bio.github.io/intro-git-rstudio/ for more info.

## This is Forrest's original instructions for getting started
#	Set this to wherever you unzipped the archive folders (not /src):
# setwd("C:/tmp")
# detach(package:refnet, unload=TRUE)
# remove.packages("refnet")
# Original files
# install.packages("~/Desktop/refnet_0.6.tar.gz", repos = NULL, type="source")
# Unzipped packages
# install.packages("~/Desktop/refnet", repos = NULL, type="source")
# Should only have to do above once.

## TO INSTALL PACKAGE FROM GITHUB (added by EB after forking refnet)
## from http://kbroman.org/pkg_primer/pages/github.html
# Load the devtools package
library(devtools)
library(tidyverse)
library(stringr)
# # This installs the original (install_github points to the master branch)
# install_github("embruna/refnet", subdir="pkg") 

# This installs the package from the "proposed-updates" branch 
# Trying to figure out the correct syntax for dfoing this:
# devtools::install_github("embruna/refnet/pkg@proposed-updates") # OR can do like this

# how to install 'refnet' 
#devtools::install_github("embruna/refnet2", subdir = "refnet")  ###I THINK THIS IS CORRECT, YES?
library(refnet)

# to check can run read_references
# uncomment the following line to see what code is being read in.
# read_references 

?refnet #Package info

##	This reads in single files. Can specify a directory & set dir=TRUE flag to read in entire directory of files.
##	If the filename_root argument is not "" then it is used to create the root filenames for CSV output:

## EB: I have uploaded three sample datafiles: EBdata, Peru, Ecuador, ane Ecuador2 (downloaded jan 2017). They include one that was originally used by FS
## to test-drive refnet, one that got hung up because of the change by Thomson-Reuters in how they coded ResearcherID (Peru)
## and one downloaded from WOS on 11 jan 2016 that has the new ResearchID tag AND has the ORCID ID field code.
## Note that Thomson-Reuters adds ORCID ID all article records retroactively (i.e., even if person didn't have an 
## ORCID ID at the time they had submitted the paper). This and the greater updtake of ORCID than other ID 
## numbers makes it the best option for disambiguating author names.
rm(list=ls())
# This uses some sample datasets posted to guthub Use package RCurl dowload them
# see: https://www.r-bloggers.com/data-on-github-the-easy-way-to-make-your-data-available/ 

# the first argument of "read_references" should point to the folder where your data files are installed. 

# EMB: I have been using the following files to test drive the package: 
    # Ecuador.txt
    # Ecuador_0114.txt: Data from all 55 journals 2001-2014 downloaded 1/20/2017
    # EBpubs.txt: Emilio Bruna's articles (downloaded Jan 2017)
    # peru.txt
    # Ecology.txt: all articles published in Ecology in 2016

ecuador_references <- read_references("./data/Ecuador.txt", dir=FALSE, filename_root="./output/ecuador")
output <- read_authors(ecuador_references, filename_root="./output/ecuador")
ecuador_authors <- output$authors
ecuador_authors__references <- output$authors__references

ecuador2_references <- read_references("./data/Ecuador_0114.txt", dir=FALSE, filename_root="./output/ecuador2") 
output <- read_authors(ecuador2_references, filename_root="./output/ecuador2")
ecuador2_authors <- output$authors
ecuador2_authors__references <- output$authors__references

eb_references <- read_references("./data/EBpubs.txt", dir=FALSE, filename_root="./output/eb")
output <- read_authors(eb_references, filename_root="./output/eb")
eb_authors <- output$authors
eb_authors__references <- output$authors__references

peru_references <- read_references("./data/Peru.txt", dir=FALSE, filename_root="./output/peru")
output <- read_authors(peru_references, filename_root="./output/peru")
peru_authors <- output$authors
peru_authors__references <- output$authors__references

ecology_references <- read_references("./data/Ecology.txt", dir=FALSE, filename_root="./output/ecology")
output <- read_authors(ecology_references, filename_root="./output/ecology")
ecology_authors <- output$authors
ecology_authors__references <- output$authors__references


 
##############################################################################################
##########################           NEXT STEPS (12 jan 2017)             #####################
# 1) the researcherID is not being read in correctly due to T-R changing from RID to RI...
###############################################################################################

## FIX LIST
# Somewhere in going from FOO_references to FOO_authors it is failing to read in the addresses 
# in cells where there are multiple authros in the same institution
# The problem must be in read_authors(function) and how it divides the record to assign to C1
# EG for WOS:000282982300009
# Correctly assigns the address for Killen, TJ but leaves a blank address for Vasquez-Martinez 
# reads in [Killeen, TJ; Vasquez-Martinez, R] Conservat Int, Ctr Appl Biodivers Sci, Washington, DC USA. 
# 
# Can try to include in the code for read_authors, but might be best to just hack and add to file later.
# 
# C1<-eb_references$C1
# foo<-unlist(strsplit(C1, "\n")) 
# C1_address <- gsub("^\\[.*\\] (.*)$", "\\1", foo) #just the address
# foo<-as.data.frame(foo) #makes a dataframe
# foo$C1_address<-C1_address #adds address to column
# x <- data.frame(do.call('rbind', strsplit(as.character(foo$foo),']',fixed=TRUE)))
# names(x)[2]<-"address"
# x$X1 <- unlist(gsub("\\[", "", x$X1))
# z<-x$X1 %>% str_split("\\;", simplify=TRUE)
# x<-select(x,-1)
# q<-cbind(x,z, stringsAsFactors=FALSE)
# q[q==""]<-NA
# str(x)
# str(q)
# z<-q %>% gather("Author_No","AF",-address,na.rm=TRUE)
# all<-full_join(eb_authors,z,by="AF")

# Need to figure out how to get WOS record in there to match them all up./
# test<- eb_references %>% select (C1,UT)
# names(test)[1]<-"foo"
# foo<-full_join(foo,test,by="foo")

############################################################
# BACK TO FORREST'S ORIGINAL INSTRUCTIONS
############################################################
##	After reading the files in you can check the ecuador_authors.csv file
##	and by hand in Excel, using the AU_ID_Dupe and Similarity fields, merge any author records that represent the same author.
##	After doing so you can read these back into R using the following, or if you're not starting from scratch above:

###	Can be read back in without importing from the following three commands:
eb_references <- read.csv("output/eb_references.csv", as.is=TRUE)
eb_authors <- read.csv("output/eb_authors.csv", as.is=TRUE)
eb_authors__references <- read.csv("output/eb_authors__references.csv", as.is=TRUE)



##	Calculate the percentage of author records without contact information:
sum(ecuador_authors$C1 == "" | is.na(ecuador_authors$C1))/length(ecuador_authors$C1)*100
sum(ecuador2_authors$C1 == "" | is.na(ecuador2_authors$C1))/length(ecuador2_authors$C1)*100
sum(peru_authors$C1 == "" | is.na(peru_authors$C1))/length(peru_authors$C1)*100
sum(eb_authors$C1 == "" | is.na(eb_authors$C1))/length(eb_authors$C1)*100

##	Let's remove duplicates from our presumably updated and corrected author lists:

output <- remove_duplicates(authors=ecuador_authors, authors__references=ecuador_authors__references, filename_root="output/ecuador_nodupe")
ecuador_authors <- output$authors
ecuador_authors__references <- output$authors__references

output <- remove_duplicates(authors=ecuador2_authors, authors__references=ecuador2_authors__references, filename_root="output/ecuador2_nodupe")
ecuador2_authors <- output$authors
ecuador2_authors__references <- output$authors__references

output <- remove_duplicates(authors=peru_authors, authors__references=peru_authors__references, filename_root="output/peru_nodupe")
peru_authors <- output$authors
peru_authors__references <- output$authors__references

output <- remove_duplicates(authors=eb_authors, authors__references=eb_authors__references, filename_root="output/eb_nodupe")
eb_authors <- output$authors
eb_authors__references <- output$authors__references


##	Now let's merge references, authors, and authors__references:
output <- merge_records(
	references=ecuador_references, 
	authors=ecuador_authors, 
	authors__references=ecuador_authors__references, 
	
	references_merge=ecuador2_references, 
	authors_merge=ecuador2_authors, 
	authors__references_merge=ecuador2_authors__references,
	
	references_merge=peru_references, 
	authors_merge=peru_authors, 
	authors__references_merge=peru_authors__references,
	
	references_merge=eb_references, 
	authors_merge=eb_authors, 
	authors__references_merge=eb_authors__references,
	
	filename_root = "output/merged"
)

merged_references <- output$references
merged_authors <- output$authors
merged_authors__references <- output$authors__references

##	  And finally after scrolling through and hand-correcting any authors
##		from the merged list that have a high similarity:
#     merged_authors <- read.csv("merged_authors.csv", as.is=TRUE)

output <- remove_duplicates(authors=merged_authors, authors__references=merged_authors__references, filename_root="output/merged_nodupe")
merged_authors <- output$authors
merged_authors__references <- output$authors__references



######
##	Sample geographic plotting of author locations based on RP or C1:

##	How to process addresses:
authors_working <- merged_authors

##	Process a single address at a time:
refnet_geocode(data.frame("AU_ID"=authors_working$AU_ID[1], "type"="RP", "address"=authors_working$RP[1], stringsAsFactors=FALSE))
refnet_geocode(data.frame("AU_ID"=authors_working$AU_ID[2], "type"="RP", "address"=authors_working$RP[2], stringsAsFactors=FALSE), verbose=TRUE)

##	Process a group of addresses:
read_addresses(data.frame("AU_ID"=authors_working$AU_ID[1:10], "type"="RP", "address"=authors_working$RP[1:10], stringsAsFactors=FALSE), verbose=TRUE)


##	Sample using the first C1 address listed for the first 1000 authors, 
##		keyed by author so we can join it back:
##	NOTE:  The string "NA" does get translated to a point so we'll remove it
##		before passing it along:

address_list_working <- sapply(strsplit(authors_working$C1[1:1000], "\n"), FUN=function(x) { return(x[1]) })
address_list_working_au_id <- authors_working$AU_ID[1:1000][!is.na(address_list_working)]
address_list_working <- address_list_working[!is.na(address_list_working)]

##	Let's try to strip off any institutional references which may complicate geocoding:
address_list_working <- gsub("^.* (.*,.*,.*)$", "\\1", address_list_working)
address_list_working <- gsub("[. ]*$", "", address_list_working)

##	Use the full list to create addresses from the C1 records:
addresses_working <- read_addresses(data.frame("id"=address_list_working_au_id, "type"="C1", "address"=address_list_working, stringsAsFactors=FALSE), filename_root="output/merged_nodupe_addresses_C1_first1000")
#addresses_working <- read.csv("output/merged_nodupe_addresses_C1_first1000_addresses.csv")

##	Now we can use those addresses to plot things out:
plot_addresses_country(addresses_working)
plot_addresses_points(addresses_working)

##	Uncomment to save as a PDF, and display the semi-transparent edge color:
#pdf("output/merged_nodupe_first1000_linkages_countries.pdf")
net_plot_coauthor(addresses_working, merged_authors__references)
#dev.off()
net_plot_coauthor_country(addresses_working, merged_authors__references)

##	The default plot area doesn't show semitransparent colors, so we'll output to PDF:
output <- net_plot_coauthor_country(addresses_working, merged_authors__references)
ggsave("output/merged_nodupe_first1000_linkages_countries_world_ggplot.pdf", output, h = 9/2, w = 9)

##	We can subset records any way that makes sense.  For example, if we wanted to only use references from 2012 (note that the way records are read in they are strings and have a hard return character):
ref_index <- merged_references$PY == "2012\n"
summary(ref_index)

##	Pull reference IDs (UT field) for just those from 2012:
UT_index <- merged_references$UT[ref_index]
merged_authors__references_subset <- merged_authors__references[ merged_authors__references$UT %in% UT_index, ]

##	Plot the subset for 2012:
net_plot_coauthor_country(addresses_working, merged_authors__references_subset)

##	Compare to 2011:
ref_index <- merged_references$PY == "2011\n"
UT_index <- merged_references$UT[ref_index]
merged_authors__references_subset <- merged_authors__references[ merged_authors__references$UT %in% UT_index, ]

##	Plot the subset for 2011:
net_plot_coauthor_country(addresses_working, merged_authors__references_subset)
