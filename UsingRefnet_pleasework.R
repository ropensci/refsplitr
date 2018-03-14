
## Don't forget that to add your commits to the fork you need to do the following: 
## In RStudio menus go to "Tools"->"Shell" and type the following: git push origin proposed-updates
## See: http://r-bio.github.io/intro-git-rstudio/ for more info.
# devtools::install_github("embruna/refnet2", subdir = "refnet")  

library(devtools)
library(tidyverse)
library(stringr)
library(refnet)

eb_references <- read_references("./data/EBpubs.txt", 
                                 dir=FALSE, 
                                filename_root="./output/eb")

output <- read_authors(eb_references, 
                       filename_root="./output/eb")

eb_authors <- output$authors

eb_authors__references <- output$authors__references




ecology_references <- read_references("./data/Ecology.txt", 
                                 dir=FALSE, 
                                 filename_root="./output/ecology")

output <- read_authors(ecology_references, 
                       filename_root="./output/ecology")

ecology_authors <- output$authors

ecology_authors_references <- output$authors_references


##	Let's remove duplicates from our presumably updated and corrected author lists:

output <- remove_duplicates(authors=ecology_authors, 
                 authors_references=ecology_authors_references,
                   filename_root="output/ecology_nodupe")

ecology_authors <- output$authors
ecology_authors_references <- output$authors_references


output <- remove_duplicates(authors=eb_authors, 
                  authors_references=eb_authors_references,
                  filename_root="output/eb_nodupe")

eb_authors <- output$authors
eb_authors_references <- output$authors_references


##	Now let's merge references, authors, and authors__references:
output <- merge_records(
	references=ecology_references, 
	authors=ecology_authors, 
	authors_references=ecology_authors_references, 
	# 
	# references_merge=ecuador2_references, 
	# authors_merge=ecuador2_authors, 
	# authors__references_merge=ecuador2_authors__references,
	# 
	# references_merge=peru_references, 
	# authors_merge=peru_authors, 
	# authors__references_merge=peru_authors__references,
	
	references_merge=eb_references, 
	authors_merge=eb_authors, 
	authors_references_merge=eb_authors_references,
	
	filename_root = "output/merged"
)

save(output, file=paste0(Sys.Date(),"_merged_records_ecology_eb.Rdata"))

merged_references <- output$references
merged_authors <- output$authors
merged_authors_references <- output$authors_references

##	  And finally after scrolling through and hand-correcting any authors
##		from the merged list that have a high similarity:
#     merged_authors <- read.csv("merged_authors.csv", as.is=TRUE)

output <- remove_duplicates(authors=merged_authors, 
                authors_references=merged_authors_references,
                filename_root="output/merged_nodupe")

merged_authors <- output$authors
merged_authors_references <- output$authors_references



######
##	Sample geographic plotting of author locations based on RP or C1:

##	How to process addresses:
authors_working <- merged_authors


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


addressdf <- data.frame(
  "id"=address_list_working_au_id, 
  "type"="C1", 
  "address"=address_list_working, 
  stringsAsFactors=FALSE)
##	Use the full list to create addresses from the C1 records:
addresses_working <- read_addresses(x=addressdf, 
      filename_root="output/merged_nodupe_addresses_C1_first1000")

##	Now we can use those addresses to plot things out:
plot_addresses_country(addresses_working)
plot_addresses_points(addresses_working)

##	Uncomment to save as a PDF, and display the semi-transparent edge color:
#pdf("output/merged_nodupe_first1000_linkages_countries.pdf")
net_plot_coauthor(addresses_working, merged_authors_references)
#dev.off()
net_plot_coauthor_country(addresses_working, merged_authors_references)

##	The default plot area doesn't show semitransparent colors, so we'll output to PDF:
output <- net_plot_coauthor_country(addresses_working, merged_authors_references)


ggsave("output/merged_nodupe_first1000_linkages_countries_world_ggplot.pdf", output, h = 9/2, w = 9)

##	We can subset records any way that makes sense.  For example, if we wanted to only use references from 2012 (note that the way records are read in they are strings and have a hard return character):
ref_index <- merged_references$PY == "2016\n"|merged_references$PY == "2016"
summary(ref_index)

##	Pull reference IDs (UT field) for just those from 2012:
UT_index <- merged_references$UT[ref_index]

merged_authors_references_subset <- merged_authors_references[ merged_authors_references$UT %in% UT_index, ]

##	Plot the subset for 2012:
net_plot_coauthor_country(addresses_working, merged_authors_references_subset)

