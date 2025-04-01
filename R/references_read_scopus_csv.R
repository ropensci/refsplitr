#' Reads SCOPUS download Output reference export files (.csv format)
#' 
#'
#' \code{references_read_scopus_csv} This function reads Scopus reference data files 
#' downloaded directly as csv into an R-friendly data format. The resulting dataframe
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
#' LA, LT, MC, MI, NR, PA, PI, PN, PS, RID, SU, TA, VR.   !!!! DOUBLE CHECK THIS LIST!!!!
#' @export references_read_scopus_download
#' 
#' @examples 
#' ## If a single files is being imported from a folder called "data" located in an RStudio Project: 
#' ## imported_refs<-references_read_scopus_csv(data = './data/refs.txt', dir = FALSE, include_all=FALSE)
#' 
#' ## If multiple files are being imported from a folder named "scopus_csv" nested within a folder
#' ## called "data" located in an RStudio Project: 
#' ## heliconia_refs<-references_read_scopus_csv(data = './data/scopus_csv', dir = TRUE, include_all=FALSE)
#' 
#' ## To load the Scopus API records used in the examples in the documentation  
#' scopus_api_data <- system.file('extdata', 'BITR_test.txt', package = 'refsplitr')
#' scopus_api_example <- references_read_scopus_api(scopus_api_data)
#' 
#' 

# SEE IMPORTANT NOTES ON ORIGINAL 265 and 295 IN TROPICAL SCIENTOMETRIX

# read & standardize: SCOPUS papers ---------------------------------------

## DELETE - THIS IS FOR CODE DEV
# library(refsplitr)
# references_read_scopus_csv <- function(data = ".", dir = FALSE, include_all=FALSE){

dir = TRUE
data = "./data/scopus_csv/scopus_csv"
folder_path<-"./data/scopus_csv/scopus_csv"
# folder_path<-paste(data,"/",sep="")

dir = TRUE
data = "./data/scopus_csv/one_file"
# folder_path<-"./data/scopus_csv"
folder_path<-paste(data,"/",sep="")


dir = FALSE
data = "./data/scopus_csv/scopus_bitr.csv"
folder_path<-"./data/scopus_csv"

  
  if (dir) {
    
    file_list_papers <- dir(folder_path)
    
    
  } else {
    file_list_papers <- data
    
  }
  
  
  ## 	makes sure all files ars .csv and same number of files
  file_list_papers_csv <- length(file_list_papers[grep(".csv", file_list_papers) ])
  file_list_papers_total <- length(file_list_papers)
  
  # 
  if (
    (file_list_papers_total> file_list_papers_csv)
  ) {
    stop("The specified file or directory contains
         files that are not in .csv format.")
  }
  if (
    (file_list_papers_total==file_list_papers_csv)
  ) {
  
      message("Now processing all references files")
  }
  
  combine_csv_files <- function(folder_path) {
    
    # List all CSV files in the folder
    csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    
    
    # 
    # 
    # ############################### Clock######################################
    # total <- length(csv_files)
    # pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    # utils::setTxtProgressBar(pb, counter)
    # utils::flush.console()
    # ###########################################################################
    
    
    # TODO:NEED AN OPTION FOR FOLLOWING FUNCTION IF ONLY ONE FILE CHOSE (ie DIR=FALSE)
    
    # Read all CSV files into a list of data frames, convert target column to integer, and add a column for the file name
    data_list <- lapply(csv_files, function(file) {
      df <- read.csv(file, stringsAsFactors = FALSE)
      # if ("prism.coverDisplayDate" %in% names(df)) {
      #   df[["prism.coverDisplayDate"]] <- as.character(df[["prism.coverDisplayDate"]])
      # }
      # if ("X._fa" %in% names(df)) {
      #   df[["X._fa"]] <- NULL
      # }
      # if ("afid.._fa" %in% names(df)) {
      #   df[["afid.._fa"]] <- NULL
      # }
      
      df$filename <- basename(file)
      df$FN<-"scopus_csv"
      return(df)
    })
    
    # Find all unique column names across all data frames
    all_columns <- unique(unlist(lapply(data_list, names)))
    
    # Function to add missing columns with NA values
    add_missing_columns <- function(df, all_columns) {
      missing_cols <- setdiff(all_columns, names(df))
      df[missing_cols] <- NA
      return(df)
    }
    
    # Apply the function to each data frame in the list
    data_list <- lapply(data_list, add_missing_columns, all_columns)
    
    
    
    
    # Combine all data frames into one
    combined_refs <- do.call(rbind, data_list)
    
    
    
    return(combined_refs)
    
  }
  
  
  scopus_refs <- combine_csv_files(folder_path) 
  
  names_standardizer <- function(refs_df){
     names(refs_df)[names(refs_df) == "Source.title"]<-"SO"
     names(refs_df)[names(refs_df) == "Abstract"]<-"AB"
     names(refs_df)[names(refs_df) == "DOI"]<-"DI"
     names(refs_df)[names(refs_df) == "Document Type"]<-"DT"
     names(refs_df)[names(refs_df) == "Volume"]<-"VL"
     names(refs_df)[names(refs_df) == "Title"]<-"TI"
     names(refs_df)[names(refs_df) == "Year"]<-"PY"
     names(refs_df)[names(refs_df) == "Volume"]<-"VL"
     names(refs_df)[names(refs_df) == "Issue"]<-"IS"
     names(refs_df)[names(refs_df) == "Art..No."]<-"AR"
     names(refs_df)[names(refs_df) == "Page.start"]<-"BP"
     names(refs_df)[names(refs_df) == "Page.end"]<-"EP"
     names(refs_df)[names(refs_df) == "Page.count"]<-"PG"
     names(refs_df)[names(refs_df) == "DOI"]<-"DI"
     names(refs_df)[names(refs_df) == "Author.Keywords"]<-"DE"
     names(refs_df)[names(refs_df) == "Correspondence.Address"]<-"RP"
     names(refs_df)[names(refs_df) == "Editors"]<-"BE"
     names(refs_df)[names(refs_df) == "Publisher"]<-"PU"
     names(refs_df)[names(refs_df) == "ISSN"]<-"SN"
     names(refs_df)[names(refs_df) == "ISBN"]<-"BN"
     names(refs_df)[names(refs_df) == "PubMed.ID"]<-"PM"
     names(refs_df)[names(refs_df) == "Language.of.Original.Document"]<-"LA"
     names(refs_df)[names(refs_df) == "Abbreviated.Source.Title"]<-"JI"
     names(refs_df)[names(refs_df) == "Document.Type"]<-"DT"
     names(refs_df)[names(refs_df) == "Open.Access"]<-"OA"
     names(refs_df)[names(refs_df) == "EID"]<-"UT"
     names(refs_df)[names(refs_df) == "Cited.by"]<-"TC"
     names(refs_df)[names(refs_df) == "Index.Keywords"]<-"ID"
     names(refs_df)[names(refs_df) == "Funding.Details"]<-"FU"
     names(refs_df)[names(refs_df) == "Funding.Texts"]<-"FX"
     names(refs_df)[names(refs_df) == "Publication.Stage"]<-"PubStage"
     names(refs_df)[names(refs_df) == "CODEN"]<-"CODEN"
     names(refs_df)[names(refs_df) == "Source"]<-"WE"
     names(refs_df)[names(refs_df) == "Link"]<-"URL"
     names(refs_df)[names(refs_df) == "Affiliations"]<-"C3" # in WOS csv download it is also "affiliations
     names(refs_df)[names(refs_df) == "Authors.with.affiliations"]<-"C1" #"Addresses" in WOS csv download
    names(refs_df)[names(refs_df) == "Authors"]<-"AU"
    names(refs_df)[names(refs_df) == "Author.full.names"]<-"AF"
    names(refs_df)[names(refs_df) == "Author.s..ID"]<-"SID" #scopus ID number
    
    return(refs_df)
  }  
  
  
  scopus_refs<-names_standardizer(scopus_refs)
  
  names(scopus_refs)
  



# check for duplicates ----------------------------------------------------

  summary(duplicated(scopus_refs))
  # These are the duplicates
  duplicated_pubs<-scopus_refs[duplicated(scopus_refs), ]
  # This removes them
  scopus_refs<-scopus_refs[!duplicated(scopus_refs), ]
  
  
# cleanup -----------------------------------------------------------------
  
  # Convert all "" to NA 
  
  scopus_refs<-replace(scopus_refs, scopus_refs =="", NA)
  
  
  
  # page numbers: for many online-only jrnls, AR is the page number 
  # need to copy over to BP if there is no BP or EP
  
  scopus_refs$BP <- ifelse(is.na(scopus_refs$BP) & is.na(scopus_refs$EP),
                                    scopus_refs$AR,scopus_refs$BP
  )

  
  

# add refID ---------------------------------------------------------------

  scopus_refs$refID <- seq.int(nrow(scopus_refs))  
  
  names(scopus_refs)
  
  
  # #remove text from scopus id
  # scopus_refs$SID <- gsub("SCOPUS_ID:", "", scopus_refs$scopus_article_id)
  # 
# convert to lower case ---------------------------------------------------
  
  
# the split_names functions uses upper and lower case
  # scopus_refs <- data.frame(lapply(scopus_refs, function(x) tolower(as.character(x))))
  
  
  library(tidyverse)
  library(refsplitr)
  # wos<-references_read("data/scopus_csv/wos_bitr.txt", include_all = TRUE)
  # wos<-wos %>% arrange(AF)
  #   names_wos<-names(wos)
  # 
  

  # scopus<-scopus %>% arrange(AF)
  # names_scopus<-names(scopus)
  # names_scopus
  # scopus$VL
  # wos$VL
  # 
  # wos$AU
  # scopus$AU
  # 
  # wos$AF
  # scopus$AF
  # # 
  # wos_cols_not_in_scopus <- setdiff(names_wos,names_scopus)
  # scopus_cols_not_in_wos <- setdiff(names_scopus,names_wos)
  # 
  
  
  # get AU in same format as scopus
  scopus_refs$AU<-gsub(".; ","\n",scopus_refs$AU)
  scopus_refs$AU<-gsub("[.]","",scopus_refs$AU)
  scopus_refs$AU<-gsub(" ",", ",scopus_refs$AU)
  
  # get AF in same format as scopus_refs
  scopus_refs$AF<-gsub("; ","\n",scopus_refs$AF)
  scopus_refs$AF<-gsub("[0-9]","",scopus_refs$AF)
  scopus_refs$AF<-gsub(" [(][)]","",scopus_refs$AF)
  scopus_refs$AF<-gsub(" [(]","",scopus_refs$AF)
  
  scopus_AF_long<-scopus_refs %>% select(refID,AF) %>%
    mutate(to = strsplit(AF, "\n")) %>%
    unnest(to) %>%
    group_by(refID) %>%
    mutate(row = row_number()) %>%
    spread(row, to) %>% 
    select(-AF)
  
  
  scopus_AF_long<-scopus_AF_long %>% 
    pivot_longer(!refID, names_to = "author_order", values_to = "AF") %>% 
    mutate(AF=trimws(AF))
  
  scopus_SID_long<-scopus_refs %>% select(refID,SID) %>%
    mutate(to = strsplit(SID, ";")) %>%
    unnest(to) %>%
    group_by(refID) %>%
    mutate(row = row_number()) %>%
    spread(row, to) %>% 
    select(-SID)
    
  
  scopus_SID_long<-scopus_SID_long %>% 
    pivot_longer(!refID, names_to = "author_order", values_to = "SID") %>% 
    mutate(SID=trimws(SID))
  
  new_scopus_SID<-left_join(scopus_AF_long,scopus_SID_long) %>% 
    drop_na() %>% 
    mutate(SID=paste(AF,SID,sep="/")) %>% 
    select(-AF) %>% 
  pivot_wider(
    names_from = author_order,
    values_from = SID)
  
  max_col<-ncol(new_scopus_SID)
  new_scopus_SID<-new_scopus_SID %>% 
    unite("SID_mod", `1`:max_col, na.rm = TRUE, remove = TRUE,sep=";")
  
  # Replace the original SID with the new one
  
  scopus_refs<-scopus_refs %>% left_join(new_scopus_SID) %>% relocate("SID_mod",.after="SID") %>% 
    select(-SID) %>% 
    rename(SID=SID_mod)
  
  rm(new_scopus_SID,scopus_AF_long,scopus_SID_long)
  
  # wos_c1<-wos[1,]$C1
  # scopus_refs_c1<-scopus_refs[1,]$C1
  # Modify c1 (address)
  scopus_refs$C1<-paste("[",scopus_refs$C1,sep="")
  scopus_refs$C1<-gsub("[;] ","./[",scopus_refs$C1)
  scopus_refs$C1<-gsub("[.][,] ","] ",scopus_refs$C1)
  scopus_refs$C1 <- gsub("\\[(\\w+)\\s(\\w+)", "[\\1, \\2", scopus_refs$C1) # insert a comma in name, which you need for authors_parse 
  scopus_refs$C1 <- gsub("\\.", "", scopus_refs$C1)
  # this causes any addresses that denote authors with same address 
  # eg Emergency Medicine (OA, AMC, MF, NS)
  # to be replaced with 
  # (OA] AMC] MF] NS)
  # this causes problems later on in split names so need to fix those
  # Easiest way was to delete any strings with surrounded with ()
  # chainsaw but works
  scopus_refs$C1  <- gsub(" \\([^\\)]*\\)", "", scopus_refs$C1)
  scopus_refs$C1  <- gsub(" \\([A-Z]+\\]", ",",scopus_refs$C1)
  scopus_refs$C1  <- gsub("\\], ", "] ",scopus_refs$C1)
  # there are some places where institutional abbreviation is given in [---]
  # removed them by looking for any *without* commas and convert to (---).
  # (if they had commas they would be [last name, initia]
  scopus_refs$C1  <- gsub("\\[([^,]+)\\]", "(\\1)", scopus_refs$C1)
  # In other cases it looks like this: (DZL] so need to make (DZL)
  scopus_refs$C1  <- gsub("\\(([^,]+)\\]", "(\\1)", scopus_refs$C1)
  
  
  
  
  
  
  wos_cols<-c("filename","AB","AF","AU","CA","BP","C1","C3","CC",
              "CH","CL","CR","CT","CY","DE","DI","DT","EM",
              "EP","FN","FU","FX","GA","ID","IS","J9","JI",
              "LA","NR","PA","PD","PG","PI","PN","PS","PT",
              "PU","PY","RI","RID","OI","PM","RP","SC","SI",
              "SN","EI","SO","SU","TC","TI","UT","VR","VL",
              "WC","Z9","AR","WE","OA","refID")
  
  scopus_cols<-names(scopus_refs)
  
  # Function to add missing columns with NA values
  add_missing_columns <- function(df, missing_cols) {
    missing_cols <- setdiff(wos_cols, names(df))
    df[missing_cols] <- NA
    return(df)
  }
    scopus_refs<-add_missing_columns(scopus_refs,wos_cols)
  
    
  # TODO: convert the format in each cell to the output of references_read WOS
  # files so can then do merge with references read or go directly to authors_clean.
  # This includes adding any columns exported by WOS but not SCOPUS
    
    # extract email from rerprint address
    # use sub function to match and extract. regex .*email: ([^ ]+).* looks for pattern "email: " 
    # followed by the email address then \\1 captures email address.
    # scopus_RP<-scopus_refs[1,]$RP
    
    scopus_refs$EM<- sub(".*email: ([^ ]+).*", "\\1", scopus_refs$RP)
    
    # Now delete the email address from RP
    
    scopus_refs$RP <- gsub("email: [^ ]+@[^ ]+", "", scopus_refs$RP)

    scopus_refs$RP<-trimws(scopus_refs$RP)
    scopus_refs$RP<-gsub("[;] "," (corresponding author), ",scopus_refs$RP)
    scopus_refs$RP<-gsub("[;]",".",scopus_refs$RP)
    scopus_refs$RP<-gsub("[.]","",scopus_refs$RP)
    corr_author <- sub(" \\(corresponding author\\).*", "", scopus_refs$RP)
    corr_author1<-sub(" .*", "", scopus_refs$RP)
    corr_author2<-sub(".* ", "", corr_author)
    corr_author<-paste(corr_author2,corr_author1,sep=", ")
    corr_author<-gsub("[.]","",corr_author)
    scopus_refs$RP <- sub(".* \\(corresponding author\\)", " \\(corresponding author\\)", scopus_refs$RP)
    scopus_refs$RP <- paste(corr_author,scopus_refs$RP,sep="")
    
    
    
    
    # first_part <- sub("\n.*", "", string)
    
    # scopus_refs$AU
    # wos$RP
    
    
    
    scopus_refs$RI<-scopus_refs$SID #"aditya, vikram/55139593900;ganesh, thyagarajan/57203832886"
    # wos$RI # "T, Ganesh/W-1294-2019;Aditya, Vikram/HSB-3625-2023"
    # scopus_refs$OI<-ifelse(is.na(scopus_refs$OI),"no;orcid",scopus_refs$OI)
    # scopus_refs$OI<-""
    # scopus_refs$OI<-";"
    scopus_refs$OI<-NA
    
    
    
    
    
    references$OI
    
    references<-scopus_refs %>% sample_n(100) %>% filter(!is.na(AU))
    foo<-authors_clean(references)
    
    # authors_clean(wos)
    
    # prelim<-foo$prelim
    # review<-foo$review
    # final<-authors_refine(review,prelim)
  
    # foo2<-authors_georef(final %>% slice(1:20))
    # plot_net_address(foo2)
    # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #   ## 	NOTE: The fields stored in our output table are a combination of the
  # ## 	"Thomson Reuters Web of Knowledge" FN format and the "ISI Export
  # ## 	Format" both of which are version 1.0:
  # output <- data.frame(
  #   "filename" = character(0),
  #  `Authors`= character(0),
  #   `Author full names` = character(0),
  #   `Author(s) ID` = character(0),
  #   `Title` = character(0),
  #   `Year` = character(0),
  #   `Source title` = character(0),
  #   `Volume` = character(0),
  #   `Issue` = character(0),
  #   `Art. No.` = character(0),
  #   `Page start` = character(0),
  #   `Page end` = character(0),
  #   `Page count` = character(0),
  #   `Cited by` = character(0),
  #   `DOI` = character(0),
  #   `Link` = character(0),
  #   `Affiliations` = character(0),
  #   `Authors with affiliations` = character(0),
  #   `Abstract` = character(0),
  #   `Author Keywords` = character(0),
  #   `Index Keywords` = character(0),
  #   `Funding Details` = character(0),
  #   `Funding Texts` = character(0),
  #   `Correspondence Address` = character(0),
  #   `Editors` = character(0),
  #   `Publisher` = character(0),
  #   `ISSN` = character(0),
  #   `ISBN` = character(0),
  #   `CODEN` = character(0),
  #   `PubMed ID` = character(0),
  #   `Language of Original Document` = character(0),
  #   `Abbreviated Source Title` = character(0),
  #   `Document Type` = character(0),
  #   `Publication Stage` = character(0),
  #   `Open Access` = character(0),
  #   `Source` = character(0),
  #   `EID` = character(0),
  #  stringsAsFactors = FALSE
  # )
  
  # 
  #   
  #   "AB" = character(0),
  #   "AF" = character(0),
  #   "AU" = character(0),
  #   "CA" = character(0),
  #   "BP" = character(0),
  #   "C1" = character(0),
  #   "C3" = character(0),
  #   "CC" = character(0),
  #   "CH" = character(0),
  #   "CL" = character(0),
  #   "CR" = character(0),
  #   "CT" = character(0),
  #   "CY" = character(0),
  #   "DE" = character(0),
  #   "DI" = character(0),
  #   "DT" = character(0),
  #   # 	"EF" = character(0),	##	End file
  #   "EM" = character(0),
  #   "EP" = character(0),
  #   # 	"ER" = character(0),	##	End record
  #   "FN" = character(0),
  #   "FU" = character(0),
  #   "FX" = character(0),
  #   "GA" = character(0),
  #   # "GE" = character(0), (removed by EB Sept 2024)
  #   "ID" = character(0),
  #   "IS" = character(0),
  #   "J9" = character(0),
  #   "JI" = character(0),
  #   "LA" = character(0),
  #   # "LT" = character(0),
  #   # "MC" = character(0),
  #   # "MI" = character(0),
  #   "NR" = character(0),
  #   "PA" = character(0),
  #   "PD" = character(0),
  #   "PG" = character(0),
  #   "PI" = character(0),
  #   "PN" = character(0),
  #   "PS" = character(0),
  #   "PT" = character(0),
  #   "PU" = character(0),
  #   "PY" = character(0),
  #   "RI" = character(0), # New field code for Thomson-Reuters ResearcherID
  #   "RID" = character(0), # Original field code for Thomson-Reuters ResearcherID
  #   # Older searchers will have RID (added by EB Sept 2024)
  #   "OI" = character(0), # Field code for ORCID ID (added by EB Jan 2017)
  #   "PM" = character(0), # Pubmed ID Number (added by EB Dec 2017)
  #   "RP" = character(0),
  #   "SC" = character(0),
  #   "SI" = character(0),
  #   "SN" = character(0),
  #   "EI" = character(0),
  #   "SO" = character(0),
  #   "SU" = character(0),
  #   # "TA" = character(0), (removed by EB Sept 2024)
  #   "TC" = character(0),
  #   "TI" = character(0),
  #   "UT" = character(0),
  #   "VR" = character(0),
  #   "VL" = character(0),
  #   "WC" = character(0),
  #   "Z9" = character(0),
  #   "AR" = character(0),
  #   "WE" = character(0),
  #   "OA" = character(0), # Field code for Open Acceess (added by EB Sept 2024)
  #   "SID" = character(0),
  #   "URL"= character(0),
  #   "BE"= character(0),
  #   "BN"= character(0),
  #   "CODEN"= character(0),
  #   "PubStage"= character(0),
  #   stringsAsFactors = FALSE
  # )

  
  # file_list <- list.files(path='./data/scopus_csv', full.names = TRUE)
  
  
  # data <- ('./data/scopus_csv/scopus.csv')
  # (data = './data/scopus_csv', dir = TRUE, include_all=FALSE)
  
  ## 	This is an index for the current record, it gets iterated for each
  # record we advance through:
#   i <- 1
#   if (dir) {
#     file_list <- dir(path = data)
#   } else {
#     file_list <- data
#   }
#   
#   ## 	Strip out any files in the directory that aren't Web of Knowledge files:
#   file_list <- file_list[ grep(".csv", file_list) ]
#   # file<-file_list
#   if (length(file_list) == 0) {
#     stop("ERROR:  The specified file or directory does not contain any
#       Scopus download records in .csv format!")
#   }
#   message("Now processing all references files")
#   
#   
#   # 
#   for (filename in file_list) {
#      if (dir) {
#        in_file <- file(paste0(data, "/", filename), "r")
#      }
#      if (!dir) {
#        in_file <- file(filename, "r")
#      }
# 
#      field <- ""
# 
#     ##    Process the first line to determine what file type it is:
#     ## 		NOTE:  We could add the encoding="UTF-8" flag to the readLines in
#     ## 		order to remove the byte-order mark (BOM) from some exported
#     ## 		files coming out of ISI, but there seems to be a bug in the
#     ## 		readLines() function after bringing a UTF-8 file in, in that
#     ## 		it doesn't respsect the BOM characters.  So we'll just read
#     ## 		the files in with no encoding specified and strip the BOM if
# 
#     read_line <- readLines(in_file, n = 1, warn = FALSE)
# 
#     if (length(read_line) > 0) {
# 
#       read_line <- gsub("^[^A-Z]*([A-Z]+)(.*)$", "\\1\\2", read_line)
# 
#       ##  Strip the first two characters from the text line,
#       #   skip the third (should be a space) and store the rest:
#       pre_text <- substr(read_line, 1, 4)
#       line_text <- substr(read_line, 4, nchar(read_line))
# 
#       if (pre_text != "Auth") {
#         close(in_file)
#         error <- paste0("ERROR:  The file ",
#                         filename,
#                         " doesn't appear to be a valid ISI or
#           Thomson Reuters reference library file!")
#         stop(error)
#       }
# 
#   #     ## 	Check to see if this is a "ISI Export Format" file, in which
#   #     ## 		case we need to parse out the first line into three fields:
#   #     if (substr(line_text, 1, 3) == "ISI") {
#   #       field <- pre_text
#   #       
#   #       ## 	Pull apart the FN, VR and PT fields all contained on the first
#   #       ## 		line of the ISI file format:
#   #       matches <- regexec(
#   #         "^(.*) VR (.*) PT (.*)",
#   #         line_text
#   #       )
#   #       
#   #       match_strings <- regmatches(
#   #         line_text,
#   #         matches
#   #       )
#   #       
#   #       ## 	Store those fields:
#   #       output[i, "FN"] <- paste(match_strings[[1]][2], "\n", sep = "")
#   #       
#   #       output[i, "VR"] <- paste(match_strings[[1]][3], "\n", sep = "")
#   #       
#   #       output[i, "PT"] <- paste(match_strings[[1]][4], "\n", sep = "")
#   #     } else {
#   #       ## 	If this is not an ISI export format then just parse the first
#   #       ## 		line normally into the FN field:
#   #       field <- pre_text
#   #       if (field %in% names(output)) {
#   #         output[i, field] <- ""
#   #         output[i, field] <- trimws(
#   #           ifelse(length(line_text) == 1,
#   #                  paste(output[i, field], line_text,
#   #                        sep = "\n")),
#   #           "both")
#   #       }
#   #     }
#   #   } else {
#   #     utils::flush.console()
#   #     stop("WARNING:  Nothing contained in the specified file!")
#   #   }
#   #   
#   #   ## 	Process the remaining lines in the file (see the note above about
#   #   ## 		the encoding= flag and necessity for it, but why we didn't use it):
#   #   while (length(read_line <- readLines(in_file, n = 1, warn = FALSE)) > 0) {
#   #     ## 	Strip the first three characters from the text line:
#   #     pre_text <- substr(read_line, 1, 2)
#   #     
#   #     line_text <- substr(read_line, 4, nchar(read_line))
#   #     
#   #     ## 	Check to see if this is a new field:
#   #     if (pre_text != "  ") {
#   #       field <- pre_text
#   #       ## 	If the field is in our file and in our data structure then
#   #       ## 		initialize it to an empty string:
#   #       if (field %in% names(output)) {
#   #         output[i, field] <- ""
#   #       }
#   #     }
#   #     
#   #     ## 	Check to see if the current field is one we are saving to output:
#   #     if (field %in% names(output)) {
#   #       ##... if it is then append this line's data to the field in our output:
#   #       
#   #       output[i, field] <- trimws(
#   #         ifelse(length(line_text) == 1,
#   #                paste(output[i, field], line_text, sep = "\n")),
#   #         "both")
#   #     }
#   #     
#   #     # 	If this is the end of a record then add any per-record items and
#   #     # 		advance our row:
#   #     if (field == "ER") {
#   #       output[i, "filename"] <- filename
#   #       
#   #       ## 	These fields are not repeated for every record, so we set them
#   #       ## 		from the first record where they were recorded:
#   #       
#   #       output[i, "FN"] <- output[1, "FN"]
#   #       output[i, "VR"] <- output[1, "VR"]
#   #       
#   #       i <- i + 1
#   #     }
#   #   }
#   #   
#   #   close(in_file)
#   #   ############################### Clock######################################
#   #   total <- length(file_list)
#   #   pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
#   #   utils::setTxtProgressBar(pb, counter)
#   #   counter <- counter + 1
#   #   utils::flush.console()
#   #   ###########################################################################
#   # }
#   # 
#   # 
#   # file_list <- ('./data/for_testing_updates/scopus_csv/scopus.csv')
#   
# output <- file_list %>% 
#   lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
#   bind_rows(.id = "filename") %>% # bind all tables into one object, and give id for each
#   rename_all(~str_replace_all(.,"prism","")) %>% 
#   rename_all(~str_replace_all(.,"dc","")) %>% 
#   rename_all(~str_replace_all(.,"\\:","")) %>% 
#   # mutate(PY=str_sub(coverDate, 1, 4)) %>% 
#   rename("SO"="Source title",
#          "AB"="Abstract",
#          "DI"="DOI",
#          "DT"="Document Type",
#          "VL"="Volume",
#          "TI"="Title",
#          "PY"="Year",
#          "SO"="Source title",
#          "VL"="Volume",
#          "IS"="Issue",
#          "AR"="Art. No.",
#          "BP"="Page start",
#          "EP"="Page end",
#          "PG"="Page count",
#          "DI"="DOI",
#          "DE"="Author Keywords",
#          "RP"="Correspondence Address",
#          "BE"="Editors",
#          "PU"="Publisher",
#          "SN"="ISSN",
#          "BN"="ISBN",
#          "PM"="PubMed ID",
#          "LA"="Language of Original Document",
#          "JI"="Abbreviated Source Title",
#          "DT"="Document Type",
#          "OA"="Open Access",
#          "UT"="EID",
#          "TC"="Cited by",
#          "ID"="Index Keywords",
#          "FU"="Funding Details",
#          "FX"="Funding Texts",
#          "PubStage"="Publication Stage",
#          "CODEN"="CODEN",
#          "WE"="Source",
#          "URL"="Link",
#          "C3"="Affiliations", # in WOS csv download it is also "affiliations"
#          "C1"="Authors with affiliations", #"Addresses" in WOS csv downloads
#          "AU"="Authors",
#          "AF"="Author full names",
#          "SID"="Author(s) ID" #scopus ID number
#          ) %>%
#   mutate(SO=tolower(SO)) %>% 
#   distinct() %>% 
#   filter(if_any(everything(), is.na)) %>% 
#   filter(!is.na(URL)) %>% 
#   mutate(PG=str_replace_all(PG,"�","")) %>% 
#   mutate(PG=str_replace_all(PG,"E-","E")) %>% 
#   # separate(PG,c("BP","EP"),remove=FALSE,sep="-",extra="merge") %>% 
#   unite(FU,FX,sep="-",na.rm=TRUE,remove = TRUE) %>% 
#   mutate(refID = row_number(),.before=1) %>% 
#   unite("refID",refID:filename,sep="-",na.rm=TRUE,remove=FALSE) %>% 
#   mutate(DE=str_replace_all(DE,"\\|",";")) %>% 
#   # select(-"@_fa",
#   #        -"coverDisplayDate",
#   #        -"aggregationType",
#   #        -"author-count.@limit",
#   #        -"openaccess",
#   #        -"freetoread.value.$",
#   #        -"freetoreadLabel.value.$",
#   #        # -"pii",
#   #        -'author-count.$',
#   #        -"coverDate",
#   #        # -"error",
#   #        -"eid",
#   #        -"url",
#   #        -"pageRange",
#   #        -article_type_long,
#   # ) %>% 
#   # mutate_all(tolower) %>% 
#   mutate_all(trimws)  %>% 
#     distinct(DI,TI,.keep_all = TRUE) %>% 
#     mutate("CA" = NA,
#            "CC" = NA,
#            "CH" = NA,
#            "CL" = NA,
#            "CR" = NA,
#            "CT" = NA,
#            "CY" = NA,
#            "EM" = NA,
#            "FN" = NA,
#            "FX" = NA,
#            "GA" = NA,
#            "J9" = NA,
#            "NR" = NA,
#            "PA" = NA,
#            "PD" = NA,
#            "PI" = NA,
#            "PN" = NA,
#            "PS" = NA,
#            "PT" = NA,
#            "RI" = NA,
#            "RID" = NA,
#            "OI" = NA,
#            "SC" = NA,
#            "SI" = NA,
#            "EI" = NA,
#            "SU" = NA,
#            "VR" = NA,
#            "WC" = NA,
#            "Z9" = NA
#     )
#   
#   
#   
#   # 
#   # output
#   # 
#   # 
#   # names(output1)
#   # names(output2)
#   # set_diff<-setdiff(names(output1),names(output2))
#   # all_names<-union(names(output1),names(output2))
#   # missing_in_ref2<-setdiff(all_names,names(output2))
# 
#       # write_csv(output,file="./data/for_testing_updates/scopus_api/scopus_papers.csv")
# 
# # names(scopus_papers)
# # unique(scopus_papers$SO)
# # names(scopus_papers)
# # head(scopus_papers)
# # unique(scopus_papers$DE)[999]
# 
# # unique(scopus_papers$journal)
# 
# 
# scopus_refs<-output %>% 
#   group_by(SO,PY) %>% 
#   tally() 
# 
# 
# output %>% 
#   group_by(SO) %>% 
#   tally() 
# 
# # 
# # names(output2
# #       )
# 
# # final tweaks to make it possible to use refsplitr on scopus -------------
# names(output)
# output<-output %>% 
#  # mutate(AU=if_else(is.na(AU), AF, AU)) %>% 
#   mutate(AU=AF) %>%
#   # mutate(refID=gsub("scopus_", "", refID)) %>% 
#   mutate(AF=gsub("\\.", "", AF)) %>% 
#   mutate(AU=gsub("\\.", "", AU)) %>% 
#   mutate(AF=gsub("\\;", "\n", AF)) %>% 
#   mutate(AU=gsub("\\;", "\n", AU)) %>% 
#   mutate(AF=gsub("\n ", "\n", AF)) %>% 
#   mutate(AU=gsub("\n ;", "\n", AU)) %>% 
#   # mutate(orcid=paste0(creator, " /", orcid)) %>% # authors clean requires orcid be in wos format "[name]/orcid" 
#   relocate(SO,PY,AF,C1,DI,TI,VL,BP,EP,.after="filename")
# 
# #TODO: CAN YOU EXTRACT OI?
# 
# # remove the last semicolon from every orcid id (last character)
# output<-output %>% 
#   mutate(OI=str_sub(OI,end=-2))
# 
# output_clean<-output %>% janitor::remove_empty("cols") 
# 
# # names(output)
# # names(output_clean)
# # set_diff<-setdiff(names(output),names(output_clean))
# # all_names<-union(names(output1),names(output2))
# # missing_in_ref2<-setdiff(all_names,names(output2))
# 
# # write_csv(output,"./data/for_testing_updates/scopus_api/scopus_refs_refreadoutput.csv")
#     }
#   }
#   return(output)
#   
# }
# # save csv ----------------------------------------------------------------
