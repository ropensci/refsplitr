####
## 12 Janury 2017: These are updates to the refnet project by Emilio Bruna
## including the use of ORCID ID nunmbers to disambiguate author names.

#####
##	BEGIN: .onLoad():

.onLoad <- function(libname, pkgname){
	#.libPaths("~/RLibrary")
	require(maptools)
	require(maps)
	require(rworldmap)
	require(RecordLinkage)
	require(Matrix)
	require(plyr)  # need to update to dplyr
	require(igraph)
	require(network)
	require(sna)
	require(Hmisc)
	require(reshape2)
	require(ggplot2)
}

##	END: .onLoad():
#####



#####
##	BEGIN: read_references():

#' Reads Thomson Reuters Web of Knowledge/Science and ISI reference export files
#' 
#' \code{read_references} This function reads Thomson Reuters Web of Knowledge
#' and ISI format reference data files into an R friendly data format and can
#' optionally write the converted data to a friendly CSV format.
#' 
#' @param data either a directory, used in conjuction with dir=TRUE, or a file
#'   name to load
#' @param dir if TRUE then data is assumed to be a directory name from which all
#'   files will be read, but if FALSE then data is assumed to be a single file
#'   to read
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_references.csv" will be appended and the output from the
#'   function will be saved

read_references <- function(data=".", dir=TRUE, filename_root="") {
	##	NOTE: The fields stored in our output table are a combination of the
	##		"Thomson Reuters Web of Knowledge" FN format and the "ISI Export
	##		Format" both of which are version 1.0:
	output <- data.frame(
		"filename" = character(0),
		"AB" = character(0),
		"AF" = character(0),
		"AU" = character(0),
		"BP" = character(0),
		"C1" = character(0),
		"CC" = character(0),
		"CH" = character(0),
		"CL" = character(0),
		"CR" = character(0),
		"CT" = character(0),
		"CY" = character(0),
		"DE" = character(0),
		"DI" = character(0),
		"DT" = character(0),
	#	"EF" = character(0),	##	End file
		"EM" = character(0),
		"EP" = character(0),
	#	"ER" = character(0),	##	End record
		"FN" = character(0),
		"FU" = character(0),
		"FX" = character(0),
		"GA" = character(0),
		"GE" = character(0),
		"ID" = character(0),
		"IS" = character(0),
		"J9" = character(0),
		"JI" = character(0),
		"LA" = character(0),
		"LT" = character(0),
		"MC" = character(0),
		"MI" = character(0),
		"NR" = character(0),
		"PA" = character(0),
		"PD" = character(0),
		"PG" = character(0),
		"PI" = character(0),
		"PN" = character(0),
		"PS" = character(0),
		"PT" = character(0),
		"PU" = character(0),
		"PY" = character(0),
		"RI" = character(0),  # NEW field code for Thomson-Reuters ResearcherID
	  "RID" = character(0), # OLD field code for Thomson-Reuters ResearcherID Older searchers will have RID, not RI ACTUALLY LOOK SL IKE NOT
	  "OI"= character(0),   # New field code for ORCID ID (added EB Jan 2017)
	  "PM"= character(0),   # Pubmed ID Number (added by EB 3 dec 2017)
	  "RP" = character(0),
		"SC" = character(0),
		"SI" = character(0),
		"SN" = character(0),
		"SO" = character(0),
		"SU" = character(0),
		"TA" = character(0),
		"TC" = character(0),
		"TI" = character(0),
		"UT" = character(0),
		"VR" = character(0),
		"VL" = character(0),
		"WC" = character(0),
		"Z9" = character(0),
		
		stringsAsFactors=FALSE
	)
	
	
	##	This is an index for the current record, it gets iterated for each record we advance through:
	i <- 1
	
	if (dir) {
		file_list = dir(path=data)
	}	else {
		file_list = data
	}
	
	##	Strip out any files in the directory that aren't Web of Knowledge files:
	file_list = file_list[ grep(".ciw|.txt", file_list) ]
	
	if (length(file_list) == 0) {
		close(in_file)
		stop("ERROR:  The specified file or directory does not contain any Web of Knowledge or ISI Export Format records!")
	}
	
	filename <- file_list[1]
	for (filename in file_list) {
		
		in_file <- file(filename, "r")
	
		field <- ""
	
		##	Process the first line to determine what file type it is:
		##		NOTE:  We could add the encoding="UTF-8" flag to the readLines in
		##		order to remove the byte-order mark (BOM) from some exported
		##		files coming out of ISI, but there seems to be a bug in the 
		##		readLines() function after bringing a UTF-8 file in, in that
		##		it doesn't respsect the BOM characters.  So we'll just read 
		##		the files in with no encoding specified and strip the BOM if
		##		it's there:
		#read_line <- readLines(in_file, n=1, warn=FALSE, encoding="UTF-8")
		read_line <- readLines(in_file, n=1, warn=FALSE)
		
		if (length(read_line) > 0) {
			###	Check for UTF-8 encoding:
			#if (substr(read_line, 1, 3) == "ï»¿") {
			#	read_line <- substr(read_line, 4, nchar(read_line))
			#}
			###	Check for alternate UTF-8 encoding:
			#if (substr(read_line, 1, 3) == "�") {
			#	read_line <- substr(read_line, 4, nchar(read_line))
			#}

			##	NOTE: The above is inconsistent across files and download
			##		types, so we could try the following:
			#read_line <- enc2native(read_line)
			
			##	The enc2native() function doesn't work all the time either, 
			##		so we'll have to strip the leading text.  To strip the leading 
			##		characters from the line we can't just pull white space as the 
			##		BOM will cause us problems for certain formatted files, so 
			##		we'll use a regular expression to just pull the line after 
			##		eliminating the preceding non-capital letters:
			read_line <- gsub("^[^A-Z]*([A-Z]+)(.*)$", "\\1\\2", read_line)
			

			##	Strip the first two characters from the text line, skip the third (should be a space) and store the rest:
			pre_text <- substr(read_line, 1, 2)
			line_text <- substr(read_line, 4, nchar(read_line))
	
			if (pre_text != "FN") {
				close(in_file)
				stop("ERROR:  The file ", filename, " doesn't appear to be a valid ISI or Thomson Reuters reference library file!\n\nThe first line is:\n", pre_text," ",line_text)
			}
	
			##	Check to see if this is a "ISI Export Format" file, in which
			##		case we need to parse out the first line into three fields:
			if ( substr(line_text, 1, 3) == "ISI" ) {
				field <- pre_text
	
				##	Pull apart the FN, VR and PT fields all contained on the first
				##		line of the ISI file format:
				matches <- regexec("^(.*) VR (.*) PT (.*)", line_text)
				match_strings <- regmatches(line_text, matches)
	
				##	Store those fields:
				output[i, "FN"] <- paste(match_strings[[1]][2], "\n", sep="")
				output[i, "VR"] <- paste(match_strings[[1]][3], "\n", sep="")
				output[i, "PT"] <- paste(match_strings[[1]][4], "\n", sep="")
			} else {
				
				##	If this is not an ISI export format then just parse the first
				##		line normally into the FN field:
				field <- pre_text
				if (field %in% names(output)) {
					output[i, field] <- ""
					output[i, field] <- paste(output[i, field], line_text, "\n", sep="")
				}
			}
	
	
		} else {
			print("WARNING:  Nothing contained in the specified file!")
			flush.console()
			break
		}
		
	
		##	Process the remaining lines in the file (see the note above about
		##		the encoding= flag and necessity for it, but why we didn't use it:
		while ( length( read_line <- readLines(in_file, n=1, warn=FALSE)) > 0 ) {
			##	Strip the first three characters from the text line:
			pre_text <- substr(read_line, 1, 2)
			line_text <- substr(read_line, 4, nchar(read_line))
	
			
			##	Check to see if this is a new field:
			if (pre_text != "  ") {
				field <- pre_text
				##	If the field is in our file and in our data structure then
				##		initialize it to an empty string:
				if (field %in% names(output)) {
					output[i, field] <- ""
				}
	
			}
	
			
			# --------------
			# EMB 2 December 2017: THE FOLLOWING ISSUES WERE RESOLVED BY EMB. 
			#   OI See notes below in the section on reading in OI
			#   Names: See the solution in this section
			#
			#     EMB 14 jan 2017: RI and OI fields aren't being read in properly because the text files include extra spaces 
			#     after they carriage return the longer records to the next line. In ebpubs record WOS: "WOS:000269700500018
			#     OI should be Dattilo, Wesley/0000-0002-4758-4379; Bruna, Emilio/0000-0003-3381-8477; Vasconcelos, Heraldo/0000-0001-6969-7131; Izzo, Thiago/0000-0002-4613-3787
			#   
			#     The same is true of names - only first one was being read in.
			#     I somehow got it to read all the names and orcids in by replacing this line below:
			#     output[i, field] <- paste(output[i, field], line_text, "\n", sep="")
			#     with this line: 
		  #     output[i, field] <- paste(output[i, field], line_text, "\\s+", sep="")
			#     but this left \s+ in between. After reading and experimenting I just deleted the "\n"
			#     or "\\s+" completeley...and...voila!
			#     Then realized wasn't being read into *_authors, so added a space as seperator after ;
			# --------------
			
			##	Check to see if the current field is one we are saving to output:
			if (field %in% names(output)) {
				##	... if it is then append this line's data to the field in our output:
				 output[i, field] <- paste(output[i, field], line_text, "\n", sep="")
				# output[i, field] <- paste(output[i, field], line_text, sep=" ")
			}
			
		
			##	If this is the end of a record then add any per-record items and
			##		advance our row:
			if (field == "ER") {
				output[i,"filename"] <- filename
				
				##	These fields are not repeated for every record, so we set them
				##		from the first record where they were recorded:
				output[i, "FN"] <- output[1, "FN"]
				output[i, "VR"] <- output[1, "VR"]
	
				i <- i + 1
			}
		
		}
		
		close(in_file)
		
	}
				
	if(filename_root != "") {
		write.csv(output, file=paste(filename_root, "_references.csv", sep=""), row.names=FALSE)
	}
	
	return(output)
}	

##	END: read_references():
##################################################
##################################################


##################################################
##################################################
##	BEGIN: read_authors():

#' Processes the output of read_references() to extract authors
#' 
#' \code{read_authors} This function extracts authors from a read_references format data.frame object, uses a Jaro-Winkler comparison of first names to try to match authors with multiple Last Name, Initial combinations, filling in potential matches using the AU_ID_Dupe and Similarity fields in the resulting output.  The output is a list containing two data.frame objects, one named authors and the other authors__references, which is a linking table that links authors by the AU_ID field to references data via the UT (Web of Knowledge ID) field.
#' 
#' @param references output from the read_references() function
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_authors.csv" and "_authors__references.csv" will be appended and the output from the
#'   function will be saved

read_authors <- function(references, filename_root="") {
	##	NOTE: The fields stored in our output table are a combination of the
	##		"Thomson Reuters Web of Knowledge" FN format and the "ISI Export
	##		Format" both of which are version 1.0:
	authors <- data.frame(
	  "AU" = character(0),
	  "AU_ID" = character(0),
		"AU_ID_Dupe" = character(0),
		"Similarity" = character(0),
		"AF" = character(0),
		"EM" = character(0),
		"C1" = character(0),
		"RP" = character(0),
		"RID" = character(0),
		"RI" = character(0),
		"OI"= character(0),  # added by EB
		stringsAsFactors=FALSE
	)
	
	authors__references <- data.frame(
	  "AU_ID" = character(0),
		"UT" = character(0),
		"C1" = character(0),
		"RP" = character(0),
		"RID" = character(0),
		"RI" = character(0), # added by EB 2 dec 2017
		"OI" = character(0), #ADDED EB 18Feb17
		"Author_Order" = numeric(0),
		stringsAsFactors=FALSE
	)

	##	This is an index for the current author record, it gets iterated for 
	##		record we advance through:
	i <- 0

	##	Iterate through each reference record:
	for (ref in 1:length(references$UT)) {
		authors_AU <- unlist(strsplit(references[ref,]$AU, "\n"))
		authors_AF <- unlist(strsplit(references[ref,]$AF, "\n"))
		
		##	The new CIW format does not actually fill the AF field, instead
		##		using the AU field for the full name.  Therefore we'll check it
		##		and fill AF from AU if it's all NA:
		if (length(authors_AF)==1) {
			if (is.na(authors_AF)) {
				authors_AF <- authors_AU
			}
		}

		##	The email list is interesting because it seems it has line
		##		breaks and is "; " delimited.  So we have to clean the line
		##		a bit to start:
		references[ref,]$EM <- gsub(" ", "", references[ref,]$EM)
		references[ref,]$EM <- gsub(";", "\n", references[ref,]$EM)
		authors_EM <- unlist(strsplit(references[ref,]$EM, "\n"))
		
		##	Process contact addresses, the first will be the C1 value
		##		itself, the second is the address without the names, stripped:
		C1 <- unlist(strsplit(references[ref,]$C1, "\n")) 
		C1_address <- gsub("^\\[.*\\] (.*)$", "\\1", C1)  #This remives the [author 1, author 2, author 3] and leaves just the address.
		
		##	Process reprint author address, the first will be the RP value
		##		itself, the second is the address without the name, stripped:
		RP <- unlist(strsplit(references[ref,]$RP, "\n"))
		RP_address <- gsub("^.*\\(reprint author\\), (.*)$", "\\1", RP)
		
		##	Process author Researcher ID fields:
		  
		  # RI <- unlist(strsplit(references[ref,]$RI, "; ")) EMB 2 decvember 2017: CAN DELETE THIS LINE.
		      # Note: this was challenging because there were carriage returns and uneven spacing in the files downloaded from WOS. 
		      # Then I realized that the problem with OI was the same as the problem with the emails, so I modified that code 
	      	# for processing that field to get the spacing along which to split the string right.
		  
		  references[ref,]$RI <- gsub(" ", "", references[ref,]$RI, fixed=TRUE)
		  references[ref,]$RI <- gsub("\n"," ", references[ref,]$RI, fixed=TRUE)
		  references[ref,]$RI <- gsub("; ", ";", references[ref,]$RI, fixed=TRUE)
		  references[ref,]$RI <- trimws(references[ref,]$RI,which = "both")
		  RI <- unlist(strsplit(references[ref,]$RI, ";"))  

	  ## Process author ORCID ID fields to add to the *_authors.csv file (Added by EMB 2 dec 2017)
      
		  # Note: this was challenging because there were carriage returns and uneven spacing in the files downloaded from WOS. 
		  # Then I realized that the problem with OI was the same as the problem with the emails, so I modified that code 
		  # for processing that field to get the spacing along which to split the string right.
		  #
		  references[ref,]$OI <- gsub(" ", "", references[ref,]$OI, fixed=TRUE)
		  references[ref,]$OI <- gsub("\n"," ", references[ref,]$OI, fixed=TRUE)
		  references[ref,]$OI <- gsub("; ", ";", references[ref,]$OI, fixed=TRUE)
		  references[ref,]$OI <- trimws(references[ref,]$OI,which = "both")
		  OI <- unlist(strsplit(references[ref,]$OI, ";"))  
		  ########################################################
		  
		  
		  
		  
		  		##	Now add all authors to our author_list and author_refdata_link 
		##		tables:
		for (aut in 1:length(authors_AU)) {
			i <- i + 1
		
			##	Check to see how many identical AU records we have and add
			##		one to the iterator for the ID:
			ID_sum <- sum(authors_AU[aut] == authors[,"AU"])
		
			##	Eventually, we probably want to use a numeric primary key for
			##		both AU_ID and for the reference (instead of UT), but for
			##		now let's keep it this way:
			authors[i,"AU_ID"] <- paste(authors_AU[aut], "_", (ID_sum + 1), sep="")

			authors[i,"AU"] <- authors_AU[aut]
			authors[i,"AF"] <- authors_AF[aut]

			authors[i,"EM"] <- ""
			
			##	If we have email addresses, we'll try to match it to
			##		individual authors, there is not a one-to-one relationship:
			if (!is.na(authors_EM[1])) {
				Similarity <- 0
				em_match <- ""

				for (emid in 1:length(authors_EM)) {
					##	More sophisticated similarity measures could be devised here
					##		but we'll use a canned distance composite from the 
					##		RecordLinkage package:
					newSimilarity <- jarowinkler(authors_EM[emid], authors[i,"AU"])
					
					if ( (newSimilarity > 0.6) & (newSimilarity > Similarity) ) {
						Similarity <- newSimilarity
						em_match <- authors_EM[emid]
					}
				}
				authors[i,"EM"] <- em_match
			}

			authors[i,"C1"] <- paste0(C1_address[ grep(authors_AF[aut], C1) ], collapse="\n")
			authors[i,"C1"] <- paste0(C1_address[ grep(authors_AF[aut], C1) ], collapse="\n")
			##	For first authors, and the case where names are not listed with 
			##		multiple C1 addresses, pull the first one:
			if (authors[i,"C1"] == "" & (length(C1_address) == 1 | aut == 1)) {
				authors[i,"C1"] <- C1_address[1]
			}

			authors[i,"RP"] <- paste0(RP_address[ grep(authors_AU[aut], RP) ], collapse="\n")

			authors[i,"RID"] <- ""# Added EB
			authors[i,"RI"] <- ""
			authors[i,"OI"] <- "" # Added EB


			##	If we have Researcher ID information, we'll try to match it to
			##		individual authors:
			
			
			##	If we have Researcher ID information, we'll try to match it to
			##		individual authors:
			if (!is.na(RI[1])) {
			  Similarity <- 0
			  rid_match <- ""
			  
			  for (rid in 1:length(RI)) {
			    ##	More sophisticated similarity measures could be devised here
			    ##		but we'll use a canned distance composite from the 
			    ##		RecordLinkage package:
			    newSimilarity <- jarowinkler(RI[rid], authors[i,"AF"])
			    
			    if ( (newSimilarity > 0.8) & (newSimilarity > Similarity) ) {
			      Similarity <- newSimilarity
			      rid_match <- RI[rid]
			    }
			  }
			  authors[i,"RI"] <- rid_match
			}
			
			
			# Copied the above for OI (by EB)
			
			if (!is.na(OI[1])) {
				Similarity <- 0
				oid_match <- ""

				for (oid in 1:length(OI)) {
					##	More sophisticated similarity measures could be devised here
					##		but we'll use a canned distance composite from the 
					##		RecordLinkage package:
					newSimilarity <- jarowinkler(OI[oid], authors[i,"AF"])
					
					if ( (newSimilarity > 0.8) & (newSimilarity > Similarity) ) {
						Similarity <- newSimilarity
						oid_match <- OI[oid]
					}
				}
				authors[i,"OI"] <- oid_match
			}
			
			# 
			# # Copied the above for RID (by EB) BUT COMMENTED OUT LATER L:OOKS LIKE UNECESSARY
			# 
			# if (!is.na(RID[1])) {
			#   Similarity <- 0
			#   oid_match <- ""
			#   
			#   for (oid in 1:length(RID)) {
			#     ##	More sophisticated similarity measures could be devised here
			#     ##		but we'll use a canned distance composite from the 
			#     ##		RecordLinkage package:
			#     newSimilarity <- jarowinkler(RID[oid], authors[i,"AF"])
			#     
			#     if ( (newSimilarity > 0.8) & (newSimilarity > Similarity) ) {
			#       Similarity <- newSimilarity
			#       oid_match <- RID[oid]
			#     }
			#   }
			#   authors[i,"RID"] <- oid_match
			# }
			# 
			##	Country is no longer stored with an author, we'll pull it during
			##		analyses from any existing addresses attached to the author
			##		record for with he authors__references link:

			###	To set the country for the author we'll first pull the reprint
			###		address country and if it's blank then pull the address from the
			###		C1 address listed:
			#country <- gsub("^.* ([A-z]*)\\.*$", "\\1", authors[i, "RP"], perl=TRUE)
			#if (country == "") {
			#	##	Have to jump through some hoops to pull the first of the possibly
			#	##		multiple addresses stored in C1 for each:
			#	country <- gsub("^.* ([A-z]*)\\.*$", "\\1", unlist(strsplit(authors[i, "C1"], "\n"))[1], perl=TRUE)
			#}
			#authors[i,"Country"] <- country


			authors__references[i,"AU_ID"] <- authors[i,"AU_ID"]
			authors__references[i,"UT"] <- references[ref,"UT"]
			authors__references[i,"C1"] <- authors[i,"C1"]
			authors__references[i,"RP"] <- authors[i,"RP"]
			authors__references[i,"RID"] <- authors[i,"RID"] # still not parsing out the multuple RI (EB 17 feb 2017). FIXED IN DEC 2017 by EB
			authors__references[i,"RI"] <- authors[i,"RI"]   # still not parsing out the multuple RI (EB 17 feb 2017). FIXED IN DEC 2017 by EB
			authors__references[i,"OI"] <- authors[i,"OI"]
			authors__references[i,"Author_Order"] <- aut
			
		}
	}


	##	See note above about not storing Country within author
	##		records.  Authors can belong to more than one country so
	##		we'll parse these during analyses:

	###	Fix country names to match country names in rworldmap:
	#indices <- authors$Country == "USA"
	#indices[is.na(indices)] <- FALSE
	#authors[indices, "Country"] <- "United States"

	#indices <- authors$Country == "England"
	#indices[is.na(indices)] <- FALSE
	#authors[indices, "Country"] <- "United Kingdom"

	#indices <- authors$Country == "Scotland"
	#indices[is.na(indices)] <- FALSE
	#authors[indices, "Country"] <- "United Kingdom"

	#indices <- authors$Country == "Wales"
	#indices[is.na(indices)] <- FALSE
	#authors[indices, "Country"] <- "United Kingdom"
	#
	#indices <- authors$Country == "Zealand"
	#indices[is.na(indices)] <- FALSE
	#authors[indices, "Country"] <- "New Zealand"
	
	
	##	Calculate the maximum similarity for any subsets of AU that match
	##		across authors.  Note that we sort by country in decreasing order
	##		so that we can prioritize equally matching duplicates that have
	##		a country extracted:
	#for (aut in order(authors[,"Country"], decreasing=FALSE, na.last=FALSE)) {

	##	The above doesn't work, so we'll do the simpler thing and just 
	##		any author with a country listed by extra, at least before
	##		we removed the country from the author data.
	##	The matching works, and will only match author records that have
	##		the same AU value, and will join records with an AF value that's
	##		similar.  These records will then be cleaned, merged into single
	##		author records by the remove_duplicates() function:
	for (aut in 1:length(authors[,"AU"])) {
		indices <- grepl(authors[aut,"AU"], authors[,"AU"])
		if (sum(indices) > 1) {
			similar <- authors[indices,]
			AU_ID_Dupe <- NA 
			Similarity <- 0
			for (i in length(similar$AF)) {
				
				##	More sophisticated similarity measures could be devised here
				##		but we'll use a canned distance composite from the 
				##		RecordLinkage package:
				newSimilarity <- jarowinkler(authors[aut,"AF"], similar[i,"AF"])

				##	See note above about not including country directly in the
				##		author record:
				#if (!is.na(similar[i,"Country"])) {
				#	newSimilarity <- newSimilarity + 1.0
				#}

				if (newSimilarity > Similarity) {
					Similarity <- newSimilarity
					AU_ID_Dupe <- similar[i,"AU_ID"]
				}
			}
			authors[aut,"AU_ID_Dupe"] <- AU_ID_Dupe
			authors[aut,"Similarity"] <- Similarity
		}
	}

				
	if(filename_root != "") {
		write.csv(authors, file=paste(filename_root, "_authors.csv", sep=""), row.names=FALSE)
		write.csv(authors__references, file=paste(filename_root, "_authors__references.csv", sep=""), row.names=FALSE)
	}            
	             
	return(list("authors"=authors, "authors__references"=authors__references))
}

##	END: read_authors():
#############################################
#############################################



#############################################
#############################################
##	BEGIN: remove_duplicates():

#' Processes the output of read_authors() to remove any flagged duplicate authors
#' 
#' \code{remove_duplicates} This function remove duplicates from the combined author and authors__references data.frames that you give it by merging the information from all records with the same AU_ID_Dupe value and replacing any of the merged records AU_ID values in the authors__references list with the AU_ID_Dupe value.
#' 
#' @param authors the first list item from the output from the read_authors() function
#' @param authors__references the second list item from the output from the read_authors() function
#' @param addresses an optional argument, that will allow you to recode the AU_ID of corresponding, removed duplicates, should be a data.frame object output from the read_addresses() function
#' @param filename_root the filename root, can include relative or absolute
#'   path and will be appended to output file names function will be saved

remove_duplicates <- function(authors, authors__references, 
                              addresses="", filename_root = "") {
	##	First we will create a new author list with all of those authors removed that have not been tagged as a duplicate:
	authors_removed <- authors[ (is.na(authors$AU_ID_Dupe) & 
	                 !(authors$AU_ID %in% authors$AU_ID_Dupe)), ]

	##	Next we will iterate over the unique values of the AU_ID_Dupe field and create a single record for each set of duplicates:
	
	i <- 1

uni_AU_ID_Dupe <- na.omit(unique(authors[,"AU_ID_Dupe"]))
	
	for (i in 1:length(uni_AU_ID_Dupe)) {
AU_ID_Dupe <- uni_AU_ID_Dupe[i]
	  
		##	Also have to check here to pull all tagged with the same AU_ID_Dupe
		##		and the original AU_ID matching it!
		authors_dupes <- authors[ (!is.na(authors$AU_ID_Dupe) &
		                        (authors$AU_ID_Dupe == AU_ID_Dupe) |
                             authors$AU_ID == AU_ID_Dupe), ]
	
		authors_removed[i, "AU_ID"] <- AU_ID_Dupe
		authors_removed[i, "AU_ID_Dupe"] <- NA
		authors_removed[i, "Similarity"] <- NA

		EM <- character(0)
		C1 <- character(0)
		RP <- character(0)

		for (j in 1:length(authors_dupes$AU_ID)) {
			##	Take the longest author name with first name:
			if (nchar(authors_dupes[j, "AF"]) > 
			    nchar(authors_removed[i, "AF"])) {
				authors_removed[i, "AF"] <- authors_dupes[j, "AF"]
			} # if statement

			##	Take the longest author name with initials:
			if (nchar(authors_dupes[j, "AU"]) > 
			    nchar(authors_removed[i, "AU"])) {
				authors_removed[i, "AU"] <- authors_dupes[j, "AU"]
			} # if statement
			
			##	Take the longest Researcher ID:
			if (nchar(authors_dupes[j, "RI"]) > 
			    nchar(authors_removed[i, "RI"])) {
				authors_removed[i, "RI"] <- authors_dupes[j, "RI"]
			} # if statement

			##	Append all addresses together and we'll check for and remove
			##		duplicates later on:
			if (!(is.na(authors_dupes[j, "EM"]) | 
			      authors_dupes[j, "EM"] == "")) {
				EM <- append(EM, unlist(strsplit(authors_dupes[j, "EM"], "\n")))
			} # if statement
			if (!(is.na(authors_dupes[j, "C1"]) | 
			      authors_dupes[j, "C1"] == "")) {
				C1 <- append(C1, unlist(strsplit(authors_dupes[j, "C1"], "\n")))
			} # if statement
			if (!(is.na(authors_dupes[j, "RP"]) | 
			      authors_dupes[j, "RP"] == "")) {
				RP <- append(RP, unlist(strsplit(authors_dupes[j, "RP"], "\n")))
			} # if statement

			##	Finally, update the authors__references AU_ID to reflect the newly merged
			##		records:
			authors__references[(authors__references$AU_ID ==
			                     authors_dupes[j, "AU_ID"]),"AU_ID"] <- AU_ID_Dupe
			
			##	If addresses were supplied, update them:
			if (addresses != "") {
				addresses[ (addresses$AU_ID == 
				              authors_dupes[j, "AU_ID"]), 
				           "AU_ID"] <- AU_ID_Dupe
			}# if statement
		} # j for loop

		##	Remove any duplicates from EM, C1 and RP records:
		EM <- unique(EM)
		C1 <- unique(C1)
		RP <- unique(RP)

		authors_removed[i,"EM"] <- paste0(EM, collapse="\n")
		authors_removed[i,"C1"] <- paste0(C1, collapse="\n")
		authors_removed[i,"RP"] <- paste0(RP, collapse="\n")
	} # AU_ID_Dupe for loop 
	
	
	if(filename_root != "") {
		write.csv(authors_removed, file=paste(filename_root, "_authors.csv", sep=""), row.names=FALSE)
		write.csv(authors__references, file=paste(filename_root, "_authors__references.csv", sep=""), row.names=FALSE)
	}            
	             
	return(list("authors"=authors_removed, "authors__references"=authors__references))
}

##	END: remove_duplicates():
########################################
########################################



########################################
########################################
##	BEGIN: merge_records():

#' Merges two sets of references/authors/authors__references/addresses objects
#' 
#' \code{merge_records} This function will merge two sets of references/authors/authors__references/addresses, making sure that there are no duplicates in references, no duplicates AU_IDs in the authors table, renaming the AU_IDs in the linking table appropriately, and then	doing an automated potential match of authors just like when processing references for the first time.
#' 
#' @param references references data.frame to merge to
#' @param authors authors data.frame to merge to
#' @param authors__references authors__references data.frame to merge to
#' @param addresses optional addresses data.frame to merge to, if you only have addresses from the second set then they will be retained
#' @param references_merge references data.frame to merge with references argument
#' @param authors_merge authors data.frame to merge with authors argument
#' @param authors__references_merge authors__references data.frame to merge with 
#' @param addresses_merge optional addresses data.frame to merge with
#' @param filename_root the filename root, can include relative or absolute
#'   path and will be appended to output file names function will be saved

merge_records <- function(references, authors, authors__references, addresses="", references_merge, authors_merge, authors__references_merge, addresses_merge="", filename_root = "") {
	##	For testing:
	#references <- brazil_references
	#authors <- brazil_authors
	#authors__references <- brazil_authors__references
	#references_merge <- ecuador_references
	#authors_merge <- ecuador_authors
	#authors__references_merge <- ecuador_authors__references

	##	First we merge references lists, making sure that we have no duplicates
	##		to mess with:
	remove_indices <- (references_merge$UT %in% references$UT)

	##	If we have duplicate references then we need to remove them from the 
	##		references table and the linking table:
	if (sum(remove_indices) > 0) {
		##	Pull IDs for references to be removed:
		UT <- references_merge$UT[remove_indices]

		##	Remove the references that will be duplicates:
		references_merge <- references_merge[!remove_indices,]

		##	Remove the links matching those references that were removed:
		authors__references_merge <- authors__references_merge[!(authors__references_merge$UT %in% UT),]

		##	Remove any authors from this list that no longer have records in
		##		the link table:
		linked <- merge(x=authors_merge, y=authors__references_merge, by.x="AU_ID", by.y="AU_ID", all.x=TRUE, all.y=FALSE)

		remove_ids <- linked$AU_ID[ is.na(linked$UT) ]
		authors_merge <- authors_merge[ authors_merge$AU_ID %in% remove_ids, ]
		
		if (addresses_merge != "") {
			addresses_merge <- addresses_merge[ addresses_merge$AU_ID %in% remove_ids, ]
		}
	}


	##	Now we check each author in the author list to be merged to see if it's AU_ID is already in the list to be merged and increment it:
	for (aut in 1:length(authors_merge$AU_ID)) {
		while (sum(authors_merge[aut, "AU_ID"] %in% authors$AU_ID) > 0) {
			AU_ID_old <- authors_merge[aut, "AU_ID"]
			ID_num <- gsub(".*_([0-9]+)$", "\\1", AU_ID_old)
			ID_num <- as.numeric(ID_num) + 1
			AU_ID_new <- gsub("(.*_)[0-9]+$", paste("\\1", ID_num, sep=""), AU_ID_old)
			##	Now check to make sure we aren't overwriting an existing AU_ID
			##		in the authors_merge table:
			while (sum(authors_merge$AU_ID == AU_ID_new) > 0) {
				ID_num <- gsub(".*_([0-9]+)$", "\\1", AU_ID_new)
				ID_num <- as.numeric(ID_num) + 1
				AU_ID_new <- gsub("(.*_)[0-9]+$", paste("\\1", ID_num, sep=""), AU_ID_new)
			}
			
			authors_merge[aut, "AU_ID"] <- AU_ID_new
			authors__references_merge[ authors__references_merge$AU_ID == AU_ID_old, "AU_ID"] <- AU_ID_new

			##	And if we have addresses replaced old AU_ID values with the new:
			if (addresses_merge != "") {
				addresses_merge[ addresses_merge$AU_ID == AU_ID_old, "AU_ID"] <- AU_ID_new
			}

			##	Return to the top to check to see if we've found a unique AU_ID
			##		with the authors table...
		}
	}

	##	Now we can merge all of the tables:
	references <- rbind(references, references_merge)
	authors <- rbind(authors, authors_merge)
	authors__references <- rbind(authors__references, authors__references_merge)
	if (addresses_merge != "") {
		if (addresses == "") {
			addresses <- addresses_merge
		} else {
			addresses <- rbind(addresses, addresses_merge)
		}

	}

	##	Now we check all authors in the merged dataset to see whether there are
	##		possible duplicates:
	for (aut in 1:length(authors[,"AU"])) {
		indices <- grepl(authors[aut,"AU"], authors[,"AU"])
		if (sum(indices) > 1) {
			similar <- authors[indices,]
			AU_ID_Dupe <- NA 
			Similarity <- 0
			for (i in length(similar$AF)) {
				
				##	More sophisticated similarity measures could be devised here
				##		but we'll use a canned distance composite from the 
				##		RecordLinkage package:
				newSimilarity <- jarowinkler(authors[aut,"AF"], similar[i,"AF"])

				##	See note above about not including country directly in the
				##		author record:
				#if (!is.na(similar[i,"Country"])) {
				#	newSimilarity <- newSimilarity + 1.0
				#}

				if (newSimilarity > Similarity) {
					Similarity <- newSimilarity
					AU_ID_Dupe <- similar[i,"AU_ID"]
				}
			}
			authors[aut,"AU_ID_Dupe"] <- AU_ID_Dupe
			authors[aut,"Similarity"] <- Similarity
		}
	}

	
	if(filename_root != "") {
		write.csv(references, file=paste(filename_root, "_references.csv", sep=""), row.names=FALSE)
		write.csv(authors, file=paste(filename_root, "_authors.csv", sep=""), row.names=FALSE)
		write.csv(authors__references, file=paste(filename_root, "_authors__references.csv", sep=""), row.names=FALSE)
	}            
	             
	return(list("references"=references, "authors"=authors, "authors__references"=authors__references))
}

##	END: merge_records():
########################################
########################################



########################################
########################################
##	BEGIN: refnet_geocode():

#' Uses the Google Maps API to process a data.frame of addresses and attempts to geocode them
#' 
#' \code{refnet_geocode} This function is based on the geoPlot geoAddress() function, and takes an address string and uses the Google Maps API to geocode the address if possible.  Unlike the original function this one saves all of the first Placemark data returned for further use.
#' 
#' @param x references data.frame that should contain three columns, the first of which is assumed to be the AU_ID, the second of which can be a code or description of the type of addresses being processed (e.g. "C1") and the third being the address itself, stripped to something Google Maps can accurately locate.
#' @param verbose=FALSE argument that when set to TRUE will output search strings and output from the Google Maps API call as it proceeds, useful for troubleshooting long-running calls to the function

refnet_geocode <- function (x, key="", verbose=FALSE) {
	##	x in this case is assumed to be a data.frame object with two 
	##		columns, the first being the ID to tag the record with, the second
	##		being a descriptive code (e.g. C1, RP, etc.) and the third being the
	##		search_string:

	columns <- c("AU_ID", "type", "search_string", "status_code", "accuracy", "address", "country_name_code", "country_name", "administrative_area", "locality", "postal_code", "latitude", "longitude", "box_north", "box_south", "box_east", "box_west")

	y <- data.frame(t(rep("", length(columns))), stringsAsFactors=FALSE)
	y <- y[-1, ]
	colnames(y) <- columns

	##	Pull address info from the x data.frame argument:
	y[1, "AU_ID"] <- x[1]
	y[1, "type"] <- x[2]
	temp00 <- data.frame(lapply(x[3], as.character), stringsAsFactors = FALSE)

	##	This is the old Google Geocoding API format:
	#temp01 <- paste("http://maps.google.com/maps/geo?q=", gsub(" ", "+", temp00), "&output=xml&key=", key, sep = "", collapse = NULL)

	##	We're replacing it with the new Geocoding API format:
	if (key == "") {
		temp01 <- paste("https://maps.googleapis.com/maps/api/geocode/xml?address=", gsub(" ", "+", temp00), "&sensor=false", sep = "", collapse = NULL)
	} else {
		temp01 <- paste("https://maps.googleapis.com/maps/api/geocode/xml?address=", gsub(" ", "+", temp00), "&sensor=false&key=", key, sep = "", collapse = NULL)
	}

	if (verbose) {
		print(temp01)
		flush.console()
	}

	y[1, "search_string"] <- paste(temp00, collapse = " ")
	
	##	Check to see if the search_string is all spaces, and if not then 
	##		do the search:
	if (length(grep("^[ \\n]*$", y[1, "search_string"])) == 0) {
		temp02 <- readLines(temp01)
	
		temp03 <- grep("<location>", temp02)[1]
		temp04 <- gsub("^.*<lat>(.*)</lat>.*$", "\\1", temp02[temp03+1])
		
		if (!is.na(temp04[1])) {
			temp05 <- gsub("^.*<lng>(.*)</lng>.*$", "\\1", temp02[temp03+2])
		}	else {
			temp05 <- temp04
		}
		
		y[1, "latitude"] <- temp04
		y[1, "longitude"] <- temp05
		

		search_string <- grep("<status>.*</status>", temp02)
		if (length(search_string) > 1) search_string <- min(search_string)
		if (length(search_string) == 1) y[1, "status_code"] <- gsub("^.*<status>(.*)</status>.*$", "\\1", temp02[search_string])

		search_string <- grep("<partial_match>.*</partial_match>", temp02)
		if (length(search_string) > 1) search_string <- max(search_string)
		if (length(search_string) == 1) y[1, "accuracy"] <- gsub("^.*<partial_match>(.*)</partial_match>.*$", "\\1", temp02[search_string])
		
		search_string <- grep("<formatted_address>.*</formatted_address>", temp02)
		if (length(search_string) > 1) search_string <- max(search_string)
		if (length(search_string) == 1) y[1, "address"] <- gsub("^.*<formatted_address>(.*)</formatted_address>.*$", "\\1", temp02[search_string])

		search_string <- grep("<type>country</type>", temp02)
		if (length(search_string) > 1) search_string <- max(search_string)
		if (length(search_string) == 1) y[1, "country_name_code"] <- gsub("^.*<short_name>(.*)</short_name>.*$", "\\1", temp02[search_string-1])
		if (length(search_string) == 1) y[1, "country_name"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])
		
		search_string <- grep("<type>administrative_area_level_1</type>", temp02)
		if (length(search_string) > 1) search_string <- max(search_string)
		if (length(search_string) == 1) y[1, "administrative_area"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])

		search_string <- grep("<type>locality</type>", temp02)
		if (length(search_string) > 1) search_string <- max(search_string)
		if (length(search_string) == 1) y[1, "locality"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])
		
		search_string <- grep("<type>postal_code</type>", temp02)
		if (length(search_string) > 1) search_string <- max(search_string)
		if (length(search_string) == 1) y[1, "postal_code"] <- gsub("^.*<long_name>(.*)</long_name>.*$", "\\1", temp02[search_string-2])

		##	NOTE: I stripped out the code that saves off the viewport and/or 
		##		bounding box for approximate matches...
	}

	return(y)
}

##	END: refnet_geocode():
########################################
########################################


########################################
########################################
##	BEGIN: read_addresses():

#' Processes multiple records of a data.frame of addresses and attempts to geocode them using refnet_geocode()
#' 
#' \code{read_addresses} This function is based on the geoPlot addrListLookup() function, and takes an address string and uses the Google Maps API to geocode the address if possible.  Though the geoPlot package addrListLookup() function works, it seems inconsistent in the way it handles returns, especially in bulk, so I'm going to redefine it adding in a wait period, re-test, and an	ability to simplify the address down to city/state/country.
#' 
#' @param x references data.frame that should contain three columns, the first of which is assumed to be the AU_ID, the second of which can be a code or description of the type of addresses being processed (e.g. "C1") and the third being the address itself, stripped to something Google Maps can accurately locate.
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_addresses.csv" will be appended and the output from the
#'   function will be saved
#' @param verbose=FALSE argument that when set to TRUE will output search strings and output from the Google Maps API call as it proceeds, useful for troubleshooting long-running calls to the function

read_addresses <- function(x, filename_root="", key="", verbose=FALSE) {
	columns <- c("AU_ID", "type", "search_string", "status_code", "accuracy", "address", "country_name_code", "country_name", "administrative_area", "locality", "postal_code", "latitude", "longitude", "box_north", "box_south", "box_east", "box_west")

	y <- data.frame(t(rep("", length(columns))), stringsAsFactors=FALSE)
	y <- y[-1, ]
	colnames(y) <- columns

	for (i in 1:nrow(x)) {
		input <- x[i, ]
		output <- refnet_geocode(input, key=key, verbose=verbose)

		counter <- 0
		
		while ((length(grep("^[ \\n]*$", output$search_string)) == 0) & is.na(output["latitude"]) & counter < 5) {
			##	Something potentially went wrong so we'll try 5 more times
			##		before giving up:
			if (verbose) {
				print(output)
				flush.console()
			}
			Sys.sleep(0.5)
			counter <- counter + 1
			output <- refnet_geocode(input, key=key, verbose=verbose)
		}

		##	If it's still a fail, let's try stripping the city, or the first
		##		item before a comma from the address:
		counter <- 2
		while ((length(grep("^[ \\n]*$", output$search_string)) == 0) & is.na(output["latitude"]) & counter >= 0) {
			##	Something potentially went wrong so we'll try 3 more times
			##		stripping an item each time:
			input$address <- gsub("^[^,]*, (.*)$", "\\1", input$address)

			if (verbose) {
				print(input)
				flush.console()
				Sys.sleep(0.5)
			}

			counter <- counter - 1
			output <- refnet_geocode(input, key=key, verbose=verbose)
		}

		y <- rbind(y, output)
	}
	
	if(filename_root != "") {
		write.csv(y, file=paste(filename_root, "_addresses.csv", sep=""), row.names=FALSE)
	}
	
	return(y)
}

##	END: read_addresses():
########################################
########################################


########################################
########################################
##	BEGIN: plot_addresses_points():

#' Plot address point locations on world map
#' 
#' \code{plot_addresses_points} This function plots an addresses data.frame object by point overlaid on the countries of the world.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.

plot_addresses_points <- function(addresses) {
	##	Remove any addresses that have NA values:
	addresses <- addresses[!is.na(addresses$longitude) | !is.na(addresses$latitude),]

	##	Convert our lat/long coordinates to a SpatialPointsDataFrame:
	spatial_points <- SpatialPoints(cbind(as.numeric(addresses$longitude), as.numeric(addresses$latitude)))
	spatial_authors <- SpatialPointsDataFrame(spatial_points, addresses)


	##	Get the world map from rworldmap package:
	world_map <- getMap()

	plot(world_map)
	plot(spatial_points, add=T, col="red")
}

##	END: plot_addresses_points():
########################################
########################################



########################################
########################################
##	BEGIN: plot_addresses_country():
##		

#' Plot addresses, the number of which are summed by country_name
#' 
#' \code{plot_addresses_country} This function plots an addresses data.frame object by country name.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.

plot_addresses_country <- function(addresses) {
	###	Exapmle plot by country (using just reprint author):
	#country_name <- gsub("^.* ([A-z]*)\n$", "\\1", addresses$RP, perl=TRUE)
	country_name <- addresses$country_name
	
	country_name[country_name == "USA"] <- "United States"
	country_name[country_name == "England"] <- "United Kingdom"
	country_name[country_name == "Scotland"] <- "United Kingdom"
	country_name[country_name == "Wales"] <- "United Kingdom"
	country_name[country_name == "The Netherlands"] <- "Netherlands"
	#country_name[country_name == "Zealand"] <- "New Zealand"
	
	country_name_table <- as.data.frame(table(country_name))
	
	#install.packages("rworldmap")
	require(rworldmap)
	
	mapdata <- joinCountryData2Map(country_name_table, joinCode="NAME", nameJoinColumn="country_name", verbose=TRUE)
	
	##	Don't know why, but in the latest version of mapCountryData() they changed "title" to "mapTitle":
	par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
	mapParams <- mapCountryData( mapdata, nameColumnToPlot="Freq", addLegend=FALSE, mapTitle="Authors Records by Country", catMethod="pretty")
	do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2))
}

##	END: plot_addresses_country():
########################################
########################################



########################################
########################################
##	BEGIN: net_plot_coauthor():

#' Creates a network diagram of coauthors' countries linked by reference
#' 
#' \code{net_plot_coauthor} This function takes an addresses data.frame, links it to an authors__references dataset and plots a network diagram generated for co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors__references output from the read_authors() function, which links author addresses together via AU_ID.

net_plot_coauthor <- function(addresses, authors__references) {
	references_addresses_linked <- merge(x=authors__references, y=addresses, by.x="AU_ID", by.y="AU_ID", all.x=FALSE, all.y=FALSE)

	##	For now, we'll just drop any that don't have a Country Name:
	references_addresses_linked <- references_addresses_linked[complete.cases(references_addresses_linked[c("UT", "country_name_code")]),]

	
	###	Aggregate into a weighted two-mode list:
	#require(plyr)
	#linkages <- count(references_addresses_linked, vars=c("UT", "country_name_code"))


	##	Or, we could use a sparse matrix representation:
	require(Matrix)

	linkages <- spMatrix(nrow=length(unique(references_addresses_linked$country_name_code)),
		ncol=length(unique(references_addresses_linked$UT)),
		i = as.numeric(factor(references_addresses_linked$country_name_code)),
		j = as.numeric(factor(references_addresses_linked$UT)),
		x = rep(1, length(references_addresses_linked$country_name_code)) 
	)
	row.names(linkages) <- levels(factor(references_addresses_linked$country_name_code))
	colnames(linkages) <- levels(factor(references_addresses_linked$UT))

	##	Convert to a one-mode representation of countries:
	linkages_countries <- linkages %*% t(linkages)

	##	Convert to a one-mode representation of references:
	linkages_references <- t(linkages) %*% linkages


	##	Create an igraph object from our countries linkages:
	require(igraph)


	#linkages_countries_net <- graph.adjacency(linkages_countries, mode="undirected")
	linkages_countries_net <- graph.adjacency(linkages_countries, mode="undirected", weighted=TRUE)

	#summary(linkages_countries_net)

	V(linkages_countries_net)$label <- V(linkages_countries_net)$name
	V(linkages_countries_net)$label.color <- rgb(0,0,.2,.5)
	V(linkages_countries_net)$label.cex <- 0.5
	V(linkages_countries_net)$size <- 12
	V(linkages_countries_net)$frame.color <- NA
	V(linkages_countries_net)$color <- rgb(0, 0.6, 0, 0.7)

	#plot(linkages_countries_net, layout=layout.fruchterman.reingold)


	##	This would only be necessary if we hadn't already computed weights from
	##		above:
	#E(linkages_countries_net)$weight <- count.multiple(linkages_countries_net)

	##	Simplify the network edges by removing the diagonal and other half (assuming it's symmetric/undirected:
	linkages_countries_net <- simplify(linkages_countries_net)
	#plot(linkages_countries_net, layout=layout.fruchterman.reingold)

	#E(linkages_countries_net)$weight
	egam <- (log(E(linkages_countries_net)$weight)+.3)/max(log(E(linkages_countries_net)$weight)+.3)
	11
	E(linkages_countries_net)$color <- rgb(0,0,0,egam)

	#pdf("../output/merged_nodupe_first1000_linkages_countries_net.pdf")
	plot(linkages_countries_net, layout=layout.fruchterman.reingold)
	#plot(linkages_countries_net, layout=layout.kamada.kawai)
	#dev.off()


	###	Or we might be interested in using the network package instead of igraph:
	#detach(package:igraph)
	#require(network)
	#require(sna)

	###	This loads our adjacency matrix into a network object, and we 
	###		specify directed as FALSE, and because we use the ignore.eval=FALSE
	###		and names.eval="value" arguments it will load our edge counts in as
	###		an edge attribute named "value" which we can then use as a weighting
	###		or plotting attribute:
	#linkages_countries_net <- network(as.matrix(linkages_countries), directed=FALSE, loops=FALSE, ignore.eval=FALSE, names.eval="value")
	#summary(linkages_countries_net)
	#plot(linkages_countries_net)
}

##	END: net_plot_coauthor():
########################################
########################################



########################################
########################################
##	BEGIN: net_plot_coauthor_country():

#' Creates a network diagram of coauthors' countries linked by reference, and with nodes arranged geographically
#' 
#' \code{net_plot_coauthor_country} This function takes an addresses data.frame, links it to an authors__references dataset and plots a network diagram generated for countries of co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors__references output from the read_authors() function, which links author addresses together via AU_ID.

net_plot_coauthor_country <- function(addresses, authors__references) {
	references_addresses_linked <- merge(x=authors__references, y=addresses, by.x="AU_ID", by.y="AU_ID", all.x=FALSE, all.y=FALSE)

	##	For now, we'll just drop any that don't have a Country Name:
	references_addresses_linked <- references_addresses_linked[complete.cases(references_addresses_linked[c("UT", "country_name_code")]),]

	
	###	Aggregate into a weighted two-mode list:
	#require(plyr)
	#linkages <- count(references_addresses_linked, vars=c("UT", "country_name_code"))


	##	Or, we could use a sparse matrix representation:
	require(Matrix)

	linkages <- spMatrix(nrow=length(unique(references_addresses_linked$country_name_code)),
		ncol=length(unique(references_addresses_linked$UT)),
		i = as.numeric(factor(references_addresses_linked$country_name_code)),
		j = as.numeric(factor(references_addresses_linked$UT)),
		x = rep(1, length(references_addresses_linked$country_name_code)) 
	)
	rownames(linkages) <- levels(factor(references_addresses_linked$country_name_code))
	colnames(linkages) <- levels(factor(references_addresses_linked$UT))

	##	Convert to a one-mode representation of countries:
	linkages_countries <- linkages %*% t(linkages)

	##	Convert to a one-mode representation of references:
	linkages_references <- t(linkages) %*% linkages

	
	##	Or we might be interested in using the network package instead of igraph:
	require(network)
	require(sna)

	##	This loads our adjacency matrix into a network object, and we 
	##		specify directed as FALSE, and because we use the ignore.eval=FALSE
	##		and names.eval="value" arguments it will load our edge counts in as
	##		an edge attribute named "value" which we can then use as a weighting
	##		or plotting attribute:
	linkages_countries_net <- network(as.matrix(linkages_countries), directed=FALSE, loops=FALSE, ignore.eval=FALSE, names.eval="value")
	#summary(linkages_countries_net)
	#plot(linkages_countries_net)

	##	Quick way:
	#gplot(as.matrix(linkages_countries))

	
	vertex_names <- (linkages_countries_net %v% "vertex.names")
	
	
	##	Get the world map from rworldmap package:
	world_map <- getMap()

	coords_df <- merge(data.frame("ISO_A2"=vertex_names, stringsAsFactors=FALSE), world_map[c("ISO_A2", "LON", "LAT")]@data, by="ISO_A2")

	##	It seems there are two "AU" codes, so we'll aggregate and mean them:
	coords_df <- aggregate(coords_df[c("LON", "LAT")], by=list(factor(coords_df$ISO_A2)), FUN=mean)
	names(coords_df) <- c("ISO_A2", "LON", "LAT")

	
	#pdf("../output/merged_nodupe_linkages_countries_world.pdf")
	#plot(world_map, xlim=c(-180, 180), ylim=c(-90, 90))
	#plot(linkages_countries_net, coord=coords_df[c("LON", "LAT")], displaylabels = TRUE, boxed.labels = FALSE, suppress.axes = FALSE, label.col = rgb(0., 0.5, 0.0, 0.7), edge.col=rgb(0.0,0.0,0.0,0.3), vertex.col=rgb(0.,0.7,0.,0.5), label.cex = 0.5, xlab = "Longitude", ylab = "Latitude", main="Co-Author Locations", vertex.cex=1+10*degree(linkages_countries_net, cmode="outdegree", rescale=TRUE), xlim=c(-180, 180), ylim=c(-90, 90), usecurve=TRUE, edge.curve=.75, new=FALSE)
	#dev.off()


	##	One could also use ggplot to plot out the network geographically:
	require(maptools)
	gpclibPermit()
	require(maps)

	##	From my "Plotting Social Networks With ggplot.r" code:
	doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
	toInstall <- c("sna", "ggplot2", "Hmisc", "reshape2")
	if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
	lapply(toInstall, library, character.only = TRUE)
	 

	# Function to generate paths between each connected node
	edgeMaker <- function(whichRow, len = 100, curved = TRUE){
		fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
		toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus

		# Add curve:
		graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
		bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
		distance1 <- sum((graphCenter - bezierMid)^2)
		if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
			bezierMid <- c(toC[1], fromC[2])
		}  # To select the best Bezier midpoint
		bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
		if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve

		edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
			c(fromC[2], bezierMid[2], toC[2]),  # X & y
			evaluation = len)
		)  # Bezier path coordinates
		edge$Sequence <- 1:len  # For size and colour weighting in plot
		edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
		return(edge)
	}
	 

	adjacencyMatrix <- as.matrix(linkages_countries)

	##	Instead of getting layoutCoordinates here, we get them from LON/LAT:
	#layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
	layoutCoordinates <- as.matrix(coords_df[,2:3])
	
	##	It seems that the melt function behaves badly when row and column
	##		names are "NA" which happens to be a legitimate country code.  So
	##		we'll first fix this and then convert back:
	rownames(adjacencyMatrix)[rownames(adjacencyMatrix) == "NA"] <- "NAstr"
	colnames(adjacencyMatrix)[colnames(adjacencyMatrix) == "NA"] <- "NAstr"

	adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
	adjacencyList <- adjacencyList[adjacencyList$value > 0, ]

	##	Repair names:
	rownames(adjacencyMatrix)[rownames(adjacencyMatrix) == "NAstr"] <- "NA"
	colnames(adjacencyMatrix)[colnames(adjacencyMatrix) == "NAstr"] <- "NA"
	levels(adjacencyList$Var1)[levels(adjacencyList$Var1) == "NAstr"] <- "NA"
	levels(adjacencyList$Var2)[levels(adjacencyList$Var2) == "NAstr"] <- "NA"
	
	 
	# Generate a (curved) edge path for each pair of connected nodes
	allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE)
	allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^


	# Empty ggplot2 theme
	new_theme_empty <- theme_bw()
	new_theme_empty$line <- element_blank()
	new_theme_empty$rect <- element_blank()
	new_theme_empty$strip.text <- element_blank()
	new_theme_empty$axis.text <- element_blank()
	new_theme_empty$plot.title <- element_blank()
	new_theme_empty$axis.title <- element_blank()
	new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")


	##	Create the world outlines:
	world_map@data$id = rownames(world_map@data)
	world_map.points <- fortify(world_map, region="id")
	world_map.df <- join(world_map.points, world_map@data, by="id")

	zp1 <- ggplot() + 
		geom_polygon(data=world_map.df, aes(long,lat,group=group), fill=gray(8/10)) +
		geom_path(data=world_map.df, aes(long,lat,group=group), color=gray(6/10)) +
		coord_equal()

	#zp1 <- ggplot()  # Pretty simple plot code
	zp1 <- zp1 + geom_path(
		data=allEdges,
		aes(x = x, y = y, group = Group,  # Edges with gradient
		colour = Sequence, size = -Sequence), alpha=0.1
	)  # and taper

	zp1 <- zp1 + geom_point(
		data = data.frame(layoutCoordinates),  # Add nodes
		aes(x = LON, y = LAT), size = 5+100*degree(linkages_countries_net, cmode="outdegree", rescale=TRUE), pch = 21,
		#colour = gray(4/10), fill = gray(6/10)
		colour = rgb(8/10, 2/10, 2/10, alpha=5/10), fill = rgb(9/10, 6/10, 6/10, alpha=5/10)
	)  # Customize gradient v

	#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
	#zp1 <- zp1 + scale_colour_gradient(low = gray(8/10), high = gray(8/10), guide = "none")
	#zp1 <- zp1 + scale_colour_gradient(low = gray(4/10), high = gray(4/10), guide = "none")
	zp1 <- zp1 + scale_colour_gradient(low = rgb(8/10, 2/10, 2/10, alpha=5/10), high = rgb(8/10, 2/10, 2/10, alpha=5/10), guide = "none")
	#zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
	zp1 <- zp1 + scale_size(range = c(1, 1), guide = "none")  # Customize taper

	zp1 <- zp1 + geom_text(
		data = coords_df,
		aes(x = LON, y = LAT, label=ISO_A2), size=2, color=gray(2/10)
	)

	zp1 <- zp1 + new_theme_empty  # Clean up plot

	# Looks better when saved as a PNG:
	#ggsave("../output/merged_nodupe_linkages_countries_world_ggplot.png", zp1, h = 9/2, w = 9, type = "cairo-png")
	#ggsave("../output/merged_nodupe_linkages_countries_world_ggplot.pdf", zp1, h = 9/2, w = 9)
	
	return(zp1)


	##	Or, we can get the fanciest and plot great circle networks:
	##		http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
}

##	END: net_plot_coauthor_country():
########################################
########################################



########################################
########################################
##	BEGIN: net_plot_coauthor_address():

#' Creates a network diagram of coauthors' addresses linked by reference, and with nodes arranged geographically
#' 
#' \code{net_plot_coauthor_address} This function takes an addresses data.frame, links it to an authors__references dataset and plots a network diagram generated for individual points of co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors__references output from the read_authors() function, which links author addresses together via AU_ID.

net_plot_coauthor_address <- function(addresses, authors__references) {
	references_addresses_linked <- merge(x=authors__references, y=addresses, by.x="AU_ID", by.y="AU_ID", all.x=FALSE, all.y=FALSE)

	##	For now, we'll just drop any that don't have lat/long information:
	references_addresses_linked <- references_addresses_linked[complete.cases(references_addresses_linked[c("UT", "latitude")]),]

	##	Link all authors at a particular point location:
	references_addresses_linked$latlon <- paste(references_addresses_linked$latitude, ",", references_addresses_linked$longitude, sep="") 

	
	##	Or, we could use a sparse matrix representation:
	require(Matrix)

	linkages <- spMatrix(nrow=length(unique(references_addresses_linked$latlon)),
		ncol=length(unique(references_addresses_linked$UT)),
		i = as.numeric(factor(references_addresses_linked$latlon)),
		j = as.numeric(factor(references_addresses_linked$UT)),
		x = rep(1, length(references_addresses_linked$latlon)) 
	)
	rownames(linkages) <- levels(factor(references_addresses_linked$latlon))
	colnames(linkages) <- levels(factor(references_addresses_linked$UT))

	##	Convert to a one-mode representation of countries:
	linkages_points <- linkages %*% t(linkages)

	##	Convert to a one-mode representation of references:
	linkages_references <- t(linkages) %*% linkages

	
	##	Or we might be interested in using the network package instead of igraph:
	require(network)
	require(sna)

	##	This loads our adjacency matrix into a network object, and we 
	##		specify directed as FALSE, and because we use the ignore.eval=FALSE
	##		and names.eval="value" arguments it will load our edge counts in as
	##		an edge attribute named "value" which we can then use as a weighting
	##		or plotting attribute:
	linkages_points_net <- network(as.matrix(linkages_points), directed=FALSE, loops=FALSE, ignore.eval=FALSE, names.eval="value")
	#summary(linkages_points_net)
	#plot(linkages_points_net)

	##	Quick way:
	#gplot(as.matrix(linkages_points))

	
	vertex_names <- (linkages_points_net %v% "vertex.names")
	
	
	##	Get the world map from rworldmap package:
	world_map <- getMap()

	coords_df <- data.frame("latlon"=vertex_names, "LON"=as.numeric(gsub("^.*,", "", vertex_names)), "LAT"=as.numeric(gsub(",.*$", "", vertex_names)), stringsAsFactors=FALSE)

	
	#pdf("../output/merged_nodupe_linkages_addresses_world.pdf")
	#plot(world_map, xlim=c(-180, 180), ylim=c(-90, 90))
	#plot(linkages_countries_net, coord=coords_df[c("LON", "LAT")], displaylabels = FALSE, boxed.labels = FALSE, suppress.axes = FALSE, label.col = rgb(0., 0.5, 0.0, 0.7), edge.col=rgb(0.0,0.0,0.0,0.3), vertex.col=rgb(0.,0.7,0.,0.5), label.cex = 0.5, xlab = "Longitude", ylab = "Latitude", main="Co-Author Locations", vertex.cex=1+10*degree(linkages_countries_net, cmode="outdegree", rescale=TRUE), xlim=c(-180, 180), ylim=c(-90, 90), usecurve=TRUE, edge.curve=.75, new=FALSE)
	#dev.off()


	##	One could also use ggplot to plot out the network geographically:
	require(maptools)
	gpclibPermit()
	require(maps)

	##	From my "Plotting Social Networks With ggplot.r" code:
	doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
	toInstall <- c("sna", "ggplot2", "Hmisc", "reshape2")
	if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
	lapply(toInstall, library, character.only = TRUE)
	 

	# Function to generate paths between each connected node
	edgeMaker <- function(whichRow, len = 100, curved = TRUE){
		fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
		toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus

		# Add curve:
		graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
		bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
		distance1 <- sum((graphCenter - bezierMid)^2)
		if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
			bezierMid <- c(toC[1], fromC[2])
		}  # To select the best Bezier midpoint
		bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
		if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve

		edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
			c(fromC[2], bezierMid[2], toC[2]),  # X & y
			evaluation = len)
		)  # Bezier path coordinates
		edge$Sequence <- 1:len  # For size and colour weighting in plot
		edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
		return(edge)
	}
	 

	adjacencyMatrix <- as.matrix(linkages_points)

	##	Instead of getting layoutCoordinates here, we get them from LON/LAT:
	#layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
	layoutCoordinates <- as.matrix(coords_df[,2:3])
	
	adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
	adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
	
	 
	# Generate a (curved) edge path for each pair of connected nodes
	allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE)
	allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^


	# Empty ggplot2 theme
	new_theme_empty <- theme_bw()
	new_theme_empty$line <- element_blank()
	new_theme_empty$rect <- element_blank()
	new_theme_empty$strip.text <- element_blank()
	new_theme_empty$axis.text <- element_blank()
	new_theme_empty$plot.title <- element_blank()
	new_theme_empty$axis.title <- element_blank()
	new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")


	##	Create the world outlines:
	world_map@data$id = rownames(world_map@data)
	world_map.points <- fortify(world_map, region="id")
	world_map.df <- join(world_map.points, world_map@data, by="id")

	zp1 <- ggplot() + 
		geom_polygon(data=world_map.df, aes(long,lat,group=group), fill=gray(8/10)) +
		geom_path(data=world_map.df, aes(long,lat,group=group), color=gray(6/10)) +
		coord_equal()

	#zp1 <- ggplot()  # Pretty simple plot code
	zp1 <- zp1 + geom_path(
		data=allEdges,
		aes(x = x, y = y, group = Group,  # Edges with gradient
		colour = Sequence, size = -Sequence), alpha=0.1
	)  # and taper

	##	TODO: We can change the scale from being for the paths to the points
	##		which will allow us to provide a legend on centrality, whereas we
	##		don't need a scale for the edges as we are not scaling by direction
	##		as this is an undirected network...
	zp1 <- zp1 + geom_point(
		data = data.frame(layoutCoordinates),  # Add nodes
		aes(x = LON, y = LAT), size = 3+100*degree(linkages_points_net, cmode="outdegree", rescale=TRUE), pch = 21,
		#colour = gray(4/10, alpha=5/10), fill = gray(6/10, alpha=5/10)
		colour = rgb(8/10, 2/10, 2/10, alpha=5/10), fill = rgb(9/10, 6/10, 6/10, alpha=5/10)
	)  # Customize gradient v
	
	#zp1 <- zp1 + scale_shape_discrete(name  ="Prop. Centrality", breaks=c(3+100*0, 3+100*0.05, 3+100*0.1, 3+100*0.2), labels=c("0%", "5%", "10%", "20%"))
	#zp1 <- zp1 + theme(legend.justification=c(1,0), legend.position=c(1,0))


	#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
	#zp1 <- zp1 + scale_colour_gradient(low = gray(8/10), high = gray(8/10), guide = "none")
	#zp1 <- zp1 + scale_colour_gradient(low = gray(4/10), high = gray(4/10), guide = "none")
	zp1 <- zp1 + scale_colour_gradient(low = rgb(8/10, 2/10, 2/10, alpha=5/10), high = rgb(8/10, 2/10, 2/10, alpha=5/10), guide = "none")
	#zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
	zp1 <- zp1 + scale_size(range = c(5/10, 5/10), guide = "none")  # Customize taper

	#zp1 <- zp1 + geom_text(
	#	data = coords_df,
	#	aes(x = LON, y = LAT, label=""), size=2, color=gray(2/10)
	#)


	zp1 <- zp1 + new_theme_empty  # Clean up plot

	# Looks better when saved as a PNG:
	#ggsave("../output/merged_nodupe_linkages_addresses_world_ggplot.png", zp1, h = 9/2, w = 9, type = "cairo-png")
	#ggsave("../output/merged_nodupe_linkages_addresses_world_ggplot.pdf", zp1, h = 9/2, w = 9)
	
	return(zp1)


	##	Or, we can get the fanciest and plot great circle networks:
	##		http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
}

##	END: net_plot_coauthor_address():
########################################
########################################
