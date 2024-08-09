#' Reads SCOPUS download Output 
##' Thomson Reuters Web of Knowledge/Science and ISI reference export files (both .txt or .ciw format accepted)
#'
#' \code{references_read_scopus_download} This function reads Scopus 
#' reference data files downloaded directly as csv into an R-friendly data format. The resulting dataframe
#' is the argument for the refsplitr function `authors_clean()`.    
#'
#' @param data the location of the file or files to be imported. This can be either the absolute or 
#' relative name of the file (for a single file) or folder (for multiple files stored in the same folder; 
#' used in conjunction with `dir = TRUE``). If left blank it is assumed the location is the working directory.
#' @param dir if FALSE it is assumed a single file is to be imported. 
#' Set to TRUE if importing multiple files (the path to the folder in which files are stored is set with `data=``; 
#' all files in the folder will be imported). Defaults to FALSE. 
#' @param include_all if FALSE only a subset of commonly used fields from references records are imported. 
#' If TRUE then all fields from the reference records are imported. Defaults to FALSE.  
#' The additional data fields included if `include_all=TRUE`: CC, CH, CL, CT, CY, DT, FX, GA, GE, ID, IS, J9, JI, 
#' LA, LT, MC, MI, NR, PA, PI, PN, PS, RID, SU, TA, VR.
#' @export references_read_scopus_api
#' 
#' @examples 
#' ## If a single files is being imported from a folder called "data" located in an RStudio Project: 
#' ## imported_refs<-references_read_scopus_api(data = './data/refs.txt', dir = FALSE, include_all=FALSE)
#' 
#' ## If multiple files are being imported from a folder named "heliconia" nested within a folder
#' ## called "data" located in an RStudio Project: 
#' ## heliconia_refs<-references_read_scopus_api(data = './data/heliconia', dir = TRUE, include_all=FALSE)
#' 
#' ## To load the Scopus API records used in the examples in the documentation  
#' scopus_api_data <- system.file('extdata', 'BITR_test.txt', package = 'refsplitr')
#' scopus_api_example <- references_read_scopus_api(scopus_api_data)
#' 
#' 

# SEE IMPORTANT NOTES ON ORIGINAL 265 and 295 IN TROPICAL SCIENTOMETRIX


library(tidyverse)
# read & standardize: SCOPUS papers ---------------------------------------

file_list <- list.files(path='./data/for_testing_updates/scopus_csv',
                            full.names = TRUE)

references_read <- function(data = ".", dir = FALSE, include_all=FALSE) {
  ## 	NOTE: The fields stored in our output table are a combination of the
  ## 	"Thomson Reuters Web of Knowledge" FN format and the "ISI Export
  ## 	Format" both of which are version 1.0:
  output <- data.frame(
    "filename" = character(0),
    "AB" = character(0),
    "AF" = character(0),
    "AU" = character(0),
    "CA" = character(0),
    "BP" = character(0),
    "C1" = character(0),
    "C3" = character(0),
    "CC" = character(0),
    "CH" = character(0),
    "CL" = character(0),
    "CR" = character(0),
    "CT" = character(0),
    "CY" = character(0),
    "DE" = character(0),
    "DI" = character(0),
    "DT" = character(0),
    # 	"EF" = character(0),	##	End file
    "EM" = character(0),
    "EP" = character(0),
    # 	"ER" = character(0),	##	End record
    "FN" = character(0),
    "FU" = character(0),
    "FX" = character(0),
    "GA" = character(0),
    # "GE" = character(0), (removed by EB Sept 2024)
    "ID" = character(0),
    "IS" = character(0),
    "J9" = character(0),
    "JI" = character(0),
    "LA" = character(0),
    # "LT" = character(0),
    # "MC" = character(0),
    # "MI" = character(0),
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
    "RI" = character(0), # New field code for Thomson-Reuters ResearcherID
    "RID" = character(0), # Original field code for Thomson-Reuters ResearcherID
    # Older searchers will have RID (added by EB Sept 2024)
    "OI" = character(0), # Field code for ORCID ID (added by EB Jan 2017)
    "PM" = character(0), # Pubmed ID Number (added by EB Dec 2017)
    "RP" = character(0),
    "SC" = character(0),
    "SI" = character(0),
    "SN" = character(0),
    "EI" = character(0),
    "SO" = character(0),
    "SU" = character(0),
    # "TA" = character(0), (removed by EB Sept 2024)
    "TC" = character(0),
    "TI" = character(0),
    "UT" = character(0),
    "VR" = character(0),
    "VL" = character(0),
    "WC" = character(0),
    "Z9" = character(0),
    "AR" = character(0),
    "WE" = character(0),
    stringsAsFactors = FALSE
  )

  
  ## 	This is an index for the current record, it gets iterated for each
  # record we advance through:
  i <- 1
  if (dir) {
    file_list <- dir(path = data)
  } else {
    file_list <- data
  }
  
  
  
  ## 	Strip out any files in the directory that aren't Web of Knowledge files:
  file_list <- file_list[ grep(".ciw|.csv", file_list) ]
  
  if (length(file_list) == 0) {
    stop("ERROR:  The specified file or directory does not contain any
      Scopus download records in .csv format!")
  }
  message("Now processing all references files")
  
  
  
  # for (filename in file_list) {
  #   if (dir) {
  #     in_file <- file(paste0(data, "/", filename), "r")
  #   }
  #   if (!dir) {
  #     in_file <- file(filename, "r")
  #   }
  #   
  #   field <- ""
  #   
  #   ##    Process the first line to determine what file type it is:
  #   ## 		NOTE:  We could add the encoding="UTF-8" flag to the readLines in
  #   ## 		order to remove the byte-order mark (BOM) from some exported
  #   ## 		files coming out of ISI, but there seems to be a bug in the
  #   ## 		readLines() function after bringing a UTF-8 file in, in that
  #   ## 		it doesn't respsect the BOM characters.  So we'll just read
  #   ## 		the files in with no encoding specified and strip the BOM if
  #   
  #   read_line <- readLines(in_file, n = 1, warn = FALSE)
  #   
  #   if (length(read_line) > 0) {
  #     
  #     read_line <- gsub("^[^A-Z]*([A-Z]+)(.*)$", "\\1\\2", read_line)
  #     
  #     ##  Strip the first two characters from the text line,
  #     #   skip the third (should be a space) and store the rest:
  #     pre_text <- substr(read_line, 1, 2)
  #     line_text <- substr(read_line, 4, nchar(read_line))
  #     
  #     if (pre_text != "FN") {
  #       close(in_file)
  #       error <- paste0("ERROR:  The file ",
  #                       filename,
  #                       " doesn't appear to be a valid ISI or
  #         Thomson Reuters reference library file!")
  #       stop(error)
  #     }
  #     
  #     ## 	Check to see if this is a "ISI Export Format" file, in which
  #     ## 		case we need to parse out the first line into three fields:
  #     if (substr(line_text, 1, 3) == "ISI") {
  #       field <- pre_text
  #       
  #       ## 	Pull apart the FN, VR and PT fields all contained on the first
  #       ## 		line of the ISI file format:
  #       matches <- regexec(
  #         "^(.*) VR (.*) PT (.*)",
  #         line_text
  #       )
  #       
  #       match_strings <- regmatches(
  #         line_text,
  #         matches
  #       )
  #       
  #       ## 	Store those fields:
  #       output[i, "FN"] <- paste(match_strings[[1]][2], "\n", sep = "")
  #       
  #       output[i, "VR"] <- paste(match_strings[[1]][3], "\n", sep = "")
  #       
  #       output[i, "PT"] <- paste(match_strings[[1]][4], "\n", sep = "")
  #     } else {
  #       ## 	If this is not an ISI export format then just parse the first
  #       ## 		line normally into the FN field:
  #       field <- pre_text
  #       if (field %in% names(output)) {
  #         output[i, field] <- ""
  #         output[i, field] <- trimws(
  #           ifelse(length(line_text) == 1,
  #                  paste(output[i, field], line_text,
  #                        sep = "\n")),
  #           "both")
  #       }
  #     }
  #   } else {
  #     utils::flush.console()
  #     stop("WARNING:  Nothing contained in the specified file!")
  #   }
  #   
  #   ## 	Process the remaining lines in the file (see the note above about
  #   ## 		the encoding= flag and necessity for it, but why we didn't use it):
  #   while (length(read_line <- readLines(in_file, n = 1, warn = FALSE)) > 0) {
  #     ## 	Strip the first three characters from the text line:
  #     pre_text <- substr(read_line, 1, 2)
  #     
  #     line_text <- substr(read_line, 4, nchar(read_line))
  #     
  #     ## 	Check to see if this is a new field:
  #     if (pre_text != "  ") {
  #       field <- pre_text
  #       ## 	If the field is in our file and in our data structure then
  #       ## 		initialize it to an empty string:
  #       if (field %in% names(output)) {
  #         output[i, field] <- ""
  #       }
  #     }
  #     
  #     ## 	Check to see if the current field is one we are saving to output:
  #     if (field %in% names(output)) {
  #       ##... if it is then append this line's data to the field in our output:
  #       
  #       output[i, field] <- trimws(
  #         ifelse(length(line_text) == 1,
  #                paste(output[i, field], line_text, sep = "\n")),
  #         "both")
  #     }
  #     
  #     # 	If this is the end of a record then add any per-record items and
  #     # 		advance our row:
  #     if (field == "ER") {
  #       output[i, "filename"] <- filename
  #       
  #       ## 	These fields are not repeated for every record, so we set them
  #       ## 		from the first record where they were recorded:
  #       
  #       output[i, "FN"] <- output[1, "FN"]
  #       output[i, "VR"] <- output[1, "VR"]
  #       
  #       i <- i + 1
  #     }
  #   }
  #   
  #   close(in_file)
  #   ############################### Clock######################################
  #   total <- length(file_list)
  #   pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  #   utils::setTxtProgressBar(pb, counter)
  #   counter <- counter + 1
  #   utils::flush.console()
  #   ###########################################################################
  # }
  # 
  # 
output <- file_list %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "csv_id") %>% # bind all tables into one object, and give id for each
  rename_all(~str_replace_all(.,"prism","")) %>% 
  rename_all(~str_replace_all(.,"dc","")) %>% 
  rename_all(~str_replace_all(.,"\\:","")) %>% 
  # mutate(PY=str_sub(coverDate, 1, 4)) %>% 
  rename("SO"="Source title",
         "AB"="Abstract",
         "DI"="DOI",
         "DT"="Document Type",
         "VL"="Volume",
         "filename"="csv_id",
         "TI"="Title",
         "PY"="Year",
         "SO"="Source title",
         "VL"="Volume",
         "IS"="Issue",
         "AR"="Art. No.",
         "BP"="Page start",
         "EP"="Page end",
         "PG"="Page count",
         "DI"="DOI",
         "DE"="Author Keywords",
         "RP"="Correspondence Address",
         "BE"="Editors",
         "PU"="Publisher",
         "SN"="ISSN",
         "BN"="ISBN",
         "PM"="PubMed ID",
         "LA"="Language of Original Document",
         "JI"="Abbreviated Source Title",
         "DT"="Document Type",
         "OA"="Open Access",
         "UT"="EID",
         "TC"="Cited by",
         "ID"="Index Keywords",
         "FU"="Funding Details",
         "FX"="Funding Texts",
         "PubStage"="Publication Stage",
         "CODEN"="CODEN",
         "WE"="Source",
         "URL"="Link",
         "C3"="Affiliations", # in WOS csv download it is also "affiliations"
         "C1"="Authors with affiliations", #"Addresses" in WOS csv downloads
         "AU"="Authors",
         "AF"="Author full names",
         "SID"="Author(s) ID" #scopus ID number
         ) %>% 
  mutate(SO=tolower(SO)) %>% 
  distinct() %>% 
  filter(if_any(everything(), is.na)) %>% 
  filter(!is.na(URL)) %>% 
  mutate(PG=str_replace_all(PG,"�","")) %>% 
  mutate(PG=str_replace_all(PG,"E-","E")) %>% 
  # separate(PG,c("BP","EP"),remove=FALSE,sep="-",extra="merge") %>% 
  unite(FU,FX,sep="-",na.rm=TRUE,remove = TRUE) %>% 
  mutate(refID = row_number(),.before=1) %>% 
  unite("refID",refID:filename,sep="-",na.rm=TRUE,remove=FALSE) %>% 
  mutate(DE=str_replace_all(DE,"\\|",";")) %>% 
  # select(-"@_fa",
  #        -"coverDisplayDate",
  #        -"aggregationType",
  #        -"author-count.@limit",
  #        -"openaccess",
  #        -"freetoread.value.$",
  #        -"freetoreadLabel.value.$",
  #        # -"pii",
  #        -'author-count.$',
  #        -"coverDate",
  #        # -"error",
  #        -"eid",
  #        -"url",
  #        -"pageRange",
  #        -article_type_long,
  # ) %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws)


  output<-output %>% distinct(DI,TI,.keep_all = TRUE)
  output

write_csv(output,file="./data/for_testing_updates/scopus_api/scopus_papers.csv")

# names(scopus_papers)
# unique(scopus_papers$SO)
# names(scopus_papers)
# head(scopus_papers)
# unique(scopus_papers$DE)[999]

# unique(scopus_papers$journal)


scopus_refs<-scopus_papers %>% 
  group_by(SO,PY) %>% 
  tally() 


scopus_papers %>% 
  group_by(SO) %>% 
  tally() 




# read & standardize - SCOPUS affiliations --------------------------------


scopus_affils <- list.files(path='./data/for_testing_updates/scopus_api/affils',
                            full.names = TRUE) %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% 
  bind_rows %>% 
  select(-"@_fa") %>% 
  mutate(affilname =str_replace_all(affilname, "\\,", "")) %>% 
  select(-'affiliation-url',
         # -"entry_number"
         ) %>% 
  distinct() %>%
  mutate(C1=paste(affilname,`affiliation-city`,`affiliation-country`,sep=", ")) %>% 
  # mutate(C1=paste("[", C1,"]",sep="")) %>% 
  relocate(C1, .after = afid) %>% 
  rename("university" = "affilname",
         "city" = "affiliation-city",
         "country" = "affiliation-country") %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws)



# read & standardize: SCOPUS authors --------------------------------------

scopus_authors <- list.files(path='./data/for_testing_updates/scopus_api/authors',
                             full.names = TRUE)

scopus_authors <- scopus_authors %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "csv_id") %>% # bind all tables into one object, and give id for each
  mutate(author_key = dplyr::row_number()) %>% 
  # left_join(scopus_authors2) %>% # join month column created earlier
  select(-"@_fa",
         -"afid.@_fa") %>% 
  rename("author_order"="@seq",
         "afid"="afid.$",
         "first_name"="given-name",
         "author_url"="author-url") %>% 
  # relocate(author_key,SO,PY,entry_number, .before="author_order") %>% 
  relocate(author_key, .before="author_order") %>% 
  mutate(first_name = str_replace_all(first_name, "\\. " ,"")) %>%
  mutate(first_name = str_replace_all(first_name, "\\." ,"")) %>%
  mutate(initials = str_replace_all(initials, "\\. " ,"")) %>%
  mutate(initials = str_replace_all(initials, "\\." ,"")) %>%
  mutate(last_init = paste(surname, initials, sep=" ")) %>% 
  mutate(authname = paste(surname, first_name,sep= ", ")) %>% 
  relocate(authid,authname,last_init, .after="author_key") %>% 
  rename("author_count"="author_order") %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  unite(refID,csv_id,sep="-",na.rm=TRUE,remove=FALSE)

scopus_authors
write_csv(scopus_authors,'./data/for_testing_updates/scopus_api/scopus_authors.csv')
rm(scopus_authors2)


names(scopus_affils)
# add the affiliations to authors -----------------------------------------

# scopus_authors_affils<-scopus_authors %>% left_join(scopus_affils)
names(scopus_authors)
names(scopus_affils)
# Add the name to C1 to make it consistent with WOS
# excludes secondary/current
scopus_authors_affils<-scopus_authors %>% 
  left_join(scopus_affils) %>% 
  select(-author_url) %>% 
  # relocate(SO,.before="PY") %>% 
  mutate(first_name = str_replace_all(first_name, "\\. " ,"\\.")) %>% 
  mutate(last_init = paste(surname, initials, sep=" ")) %>% 
  mutate(authname = paste(surname, first_name,sep= ", ")) %>% 
  mutate(C1 = paste("[",authname,"] ", C1 ,sep= "")) %>% 
  # distinct(SO,PY,entry_number,authid,afid,.keep_all = TRUE)%>% 
  distinct(authid,afid,.keep_all = TRUE)%>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  mutate(C1=gsub("Second Author, ","No Inst Given, ",C1)) 

# unique(scopus_authors_affils$C1)

# are there any remaining?
scopus_authors_affils %>% filter(is.na(author_key)) 

write_csv(scopus_authors_affils,'./data/for_testing_updates/scopus_api/scopus_authors_affils.csv')

# add the authors and affiliations to papers -------------------------------

names(scopus_authors_affils)
# author_affils in WIDE FORMAT (authors for each paper)
scopus_authors_affils_wide<-scopus_authors_affils %>% 
  select(
    -"university", 
    -"city", 
    -"country", 
    -"last_init", 
    -"authid",
    #       -"C1", 
    - "afid", 
    -"surname" ,
    -"first_name" ,
    #       -"entry_number",
    -author_key,
    -authname,
    #        # -SO,
    #        # -PY,
    -"initials") %>%
  pivot_wider(names_from = author_count, 
              id_cols=refID,
              values_from = C1,
              names_prefix = "C") %>% 
  # BE SURE TO CHECK HOW MANY C COLUMNS AND EDIT BELOW
  unite("C1", C1:last(matches("C[[:digit:]]")), remove = TRUE,na.rm=TRUE, sep="./") %>% 
  mutate(C1 = str_replace_all(C1, " \\[na, na, na]" ," \\[missing]"))
# head(scopus_authors_affils_wide$C1,40)
names(scopus_authors_affils_wide)



# select(scopus_authors_affils, matches("C[[:digit:]]"))
# select(scopus_authors_affils, last(matches("C[[:digit:]]")))
# scopus_authors_affils$C2<-NULL
# scopus_authors_affils$C3<-NULL
scopus_article_authors_wide<-scopus_authors_affils %>% 
  distinct() %>% 
  ungroup() %>% 
  select(-"C1", 
         -"university", 
         -"city", 
         -"country", 
         -"last_init", 
         -"authid",
         - "afid", 
         -"surname" ,
         -"first_name" ,
         # -"entry_number",
         # -authname,
         -author_key,
         # -SO,
         # -PY,
         -"initials") %>%
  mutate(orcid=paste(authname,"/",orcid,";",sep="")) %>% 
  pivot_wider(names_from = author_count,
              values_from = c("authname","orcid"),
              id_cols=refID,
              # names_prefix = c("AF","OI")
              ) %>%
  rename_with(~ gsub("authname_", "AF", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("orcid_", "OI", .x, fixed = TRUE)) %>% 
  # CHECK HOW MANY AUTHOR COLUMNS
  unite("AF", AF1:last(matches("AF[[:digit:]]")), remove = TRUE,na.rm=TRUE, sep="; ") %>% 
  unite("OI", OI1:last(matches("OI[[:digit:]]")), remove = TRUE,na.rm=TRUE, sep="") 
# mutate(C1 = str_replace_all(C1, " \\[NA, NA, NA]" ," \\[MISSING]"))
# head(scopus_allauthors_wide$AF,40)
names(scopus_article_authors_wide)




scopus_papers_complete <- scopus_papers %>% 
  left_join(scopus_authors_affils_wide) %>% 
  left_join(scopus_article_authors_wide) %>% 
  rename("filename"="csv_id")


# final tweaks to make it possible to use refsplitr on scopus -------------
names(scopus_papers_complete)
scopus_papers_complete<-scopus_papers_complete %>% 
 # mutate(AU=if_else(is.na(AU), AF, AU)) %>% 
  mutate(AU=AF) %>%
  # mutate(refID=gsub("scopus_", "", refID)) %>% 
  mutate(AF=gsub("\\.", "", AF)) %>% 
  mutate(AU=gsub("\\.", "", AU)) %>% 
  mutate(AF=gsub("\\;", "\n", AF)) %>% 
  mutate(AU=gsub("\\;", "\n", AU)) %>% 
  mutate(AF=gsub("\n ", "\n", AF)) %>% 
  mutate(AU=gsub("\n ;", "\n", AU)) %>% 
  # mutate(orcid=paste0(creator, " /", orcid)) %>% # authors clean requires orcid be in wos format "[name]/orcid" 
  relocate(SO,PY,AF,C1,DI,TI,VL,BP,EP,.after="filename")

# remove the last semicolon from every orcid id (last character)
scopus_papers_complete<-scopus_papers_complete %>% 
  mutate(OI=str_sub(OI,end=-2))
# scopus_papers_complete$EM<-NA
  # mutate(refID=as.numeric(refID)*1000) 



write_csv(scopus_papers_complete,"./data/for_testing_updates/scopus_api/scopus_refs.csv")
# save csv ----------------------------------------------------------------
