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
  
  authors_references <- data.frame(
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
  
  ##	The email list is interesting because it seems it has line
  ##		breaks and is "; " delimited.  So we have to clean the line
  ##		a bit to start:
  references$EM <- gsub(" ", "", references$EM)
  references$EM <- gsub(";", "\n", references$EM)
  
  ##	Iterate through each reference record:
  for (ref in 1:length(references$UT)) {
    authors_AU <- unlist(strsplit(references$AU, "\n"))
    authors_AF <- unlist(strsplit(references$AF, "\n"))
    
    ##	The new CIW format does not actually fill the AF field, instead
    ##		using the AU field for the full name.  Therefore we'll check it
    ##		and fill AF from AU if it's all NA:
    
    fill_af_from_au <- which(nchar(authors_AF)==1|is.na(authors_AF))
    
    authors_AF[fill_af_from_au] <- authors_AU[fill_af_from_au]
    

    authors_EM <- unlist(strsplit(references[ref,]$EM, "\n"))
    
    ##	Process contact addresses, the first will be the C1 value
    ##		itself, the second is the address without the names, stripped:
    #C1 <- unlist(strsplit(references[ref,]$C1, "\n")) 
    C1 <- unlist(strsplit(references$C1, "\n")) 
    
    C1_address <- gsub("^\\[.*\\] (.*)$", "\\1", C1)  #This removes the [author 1, author 2, author 3] and leaves just the address.
    
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
      authors[i,"AU_ID"] <- paste(authors_AU[aut], "_", 
                                  (ID_sum + 1), sep="")
      
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
          newSimilarity <- jarowinkler(authors_EM[emid], 
                                       authors[i,"AU"])
          
          if ( (newSimilarity > 0.6) & 
               (newSimilarity > Similarity) ) {
            Similarity <- newSimilarity
            
            em_match <- authors_EM[emid]
          }
        }
        authors[i,"EM"] <- em_match
      }
      
      authors[i,"C1"] <- paste0(C1_address[ grep(authors_AF[aut], C1) ],
                                collapse="\n")
      
      authors[i,"C1"] <- paste0(C1_address[ grep(authors_AF[aut], C1) ],
                                collapse="\n")
      ##	For first authors, and the case where names are not listed with 
      ##		multiple C1 addresses, pull the first one:
      if (authors[i,"C1"] == "" & 
          (length(C1_address) == 1 | aut == 1)) {
        
        authors[i,"C1"] <- C1_address[1]
        
      }
      
      authors[i,"RP"] <- paste0(RP_address[grep(authors_AU[aut], RP) ],
                                collapse="\n")
      
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
          newSimilarity <- jarowinkler(RI[rid], 
                                       authors[i,"AF"])
          
          if ( (newSimilarity > 0.8) & 
               (newSimilarity > Similarity) ) {
            
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
          newSimilarity <- jarowinkler(OI[oid], 
                                       authors[i,"AF"])
          
          if ( (newSimilarity > 0.8) & 
               (newSimilarity > Similarity) ) {
            
            Similarity <- newSimilarity
            
            oid_match <- OI[oid]
          }
        }
        authors[i,"OI"] <- oid_match
      }
      
      authors_references[i,"AU_ID"] <- authors[i,"AU_ID"]
      
      authors_references[i,"UT"] <- references[ref,"UT"]
      
      authors_references[i,"C1"] <- authors[i,"C1"]
      
      authors_references[i,"RP"] <- authors[i,"RP"]
      
      authors_references[i,"RID"] <- authors[i,"RID"] # still not parsing out the multuple RI (EB 17 feb 2017). FIXED IN DEC 2017 by EB
      
      authors_references[i,"RI"] <- authors[i,"RI"]   # still not parsing out the multuple RI (EB 17 feb 2017). FIXED IN DEC 2017 by EB
      
      authors_references[i,"OI"] <- authors[i,"OI"]
      
      authors_references[i,"Author_Order"] <- aut
    }
  }
  
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
    
    indices <- grepl(authors[aut,"AU"], 
                     authors[,"AU"])
    
    if (sum(indices) > 1) {
      
      similar <- authors[indices,]
      
      AU_ID_Dupe <- NA 
      
      Similarity <- 0
      
      for (i in length(similar$AF)) {
        
        ##	More sophisticated similarity measures could be devised here
        ##		but we'll use a canned distance composite from the 
        ##		RecordLinkage package:
        newSimilarity <- jarowinkler(authors[aut,"AF"], 
                                     similar[i,"AF"])
        
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
    write.csv(authors, 
              file=paste(filename_root, "_authors.csv", sep=""), 
              row.names=FALSE)
    
    write.csv(authors_references, 
              file=paste(filename_root, "_authors_references.csv", sep=""), 
              row.names=FALSE)
  }            
  
  return(list("authors"=authors, 
              "authors_references"=authors_references))
}

##	END: read_authors():
#############################################
#############################################