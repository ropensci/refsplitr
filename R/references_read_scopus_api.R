#' Reads SCOPUS API Output 
##' Thomson Reuters Web of Knowledge/Science and ISI reference export files (both .txt or .ciw format accepted)
#'
#' \code{references_read_scopus_api} This function reads Scopus 
#' reference data files downloaded via API into an R-friendly data format. The resulting dataframe
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

scopus_papers <- list.files(path='./data/for_testing_updates/scopus_api/papers',
                            full.names = TRUE)

scopus_papers <- scopus_papers %>% 
  lapply(read_csv,col_types = cols(.default = "c")) %>% # read all the files at once
  bind_rows(.id = "filename") %>% # bind all tables into one object, and give id for each
  rename_all(~str_replace_all(.,"prism","")) %>% 
  rename_all(~str_replace_all(.,"dc","")) %>% 
  rename_all(~str_replace_all(.,"\\:","")) %>% 
  mutate(PY=str_sub(coverDate, 1, 4)) %>% 
  rename("SO"="publicationName",
         "AB"="description",
         "DI"="doi",
         "DT"="subtype",
         "VL"="volume",
         "article_type_long"="subtypeDescription",
         "author_count"='author-count.@total',
         "IS"='issueIdentifier',
         "SN"="issn",
         "EI"="eIssn",
         "DE"="authkeywords",
         "PM"="pubmed-id",
         "TC"="citedby-count",
         "TI"="title",
         # "UT"="url",
         "fund_acr"="fund-acr",
         "fund_no"="fund-no",
         "fund_sp"="fund-sponsor",
         "UT"="identifier",
         "OA"="openaccessFlag"
  ) %>% 
  mutate(SO=tolower(SO)) %>% 
  distinct() %>% 
  filter(if_any(everything(), is.na)) %>% 
  filter(!is.na(url)) %>% 
  # mutate(pub_number = row_number()) %>% 
  mutate(pageRange=str_replace_all(pageRange,"�","")) %>% 
  mutate(pageRange=str_replace_all(pageRange,"E-","E")) %>% 
  separate(pageRange,c("BP","EP"),remove=FALSE,sep="-",extra="merge") %>% 
  unite(FU,fund_acr,fund_sp,fund_no, sep="-",na.rm=TRUE,remove = TRUE) %>% 
  unite(refID,filename,entry_number,sep="-",na.rm=TRUE,remove=FALSE) %>% 
  mutate(DE=str_replace_all(DE,"\\|",";")) %>% 
  select(-"@_fa",
         -"coverDisplayDate",
         -"aggregationType",
         -"author-count.@limit",
         -"openaccess",
         -"freetoread.value.$",
         -"freetoreadLabel.value.$",
         # -"pii",
         -'author-count.$',
         -"coverDate",
         # -"error",
         -"eid",
         -"url",
         -"pageRange",
         -article_type_long,
  ) %>% 
  # mutate_all(tolower) %>% 
  mutate_all(trimws)


scopus_papers<-scopus_papers %>% distinct(DI,TI,.keep_all = TRUE)
rm(scopus_papers2)

write_csv(scopus_papers,file="./data/for_testing_updates/scopus_api/scopus_papers.csv")

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
  bind_rows(.id = "filename") %>% # bind all tables into one object, and give id for each
  mutate(author_key = dplyr::row_number()) %>% 
  # left_join(scopus_authors2) %>% # join month column created earlier
  select(-"@_fa",
         -"afid.@_fa") %>% 
  rename("author_order"="@seq",
         "afid"="afid.$",
         "first_name"="given-name",
         "author_url"="author-url") %>% 
  # relocate(author_key,SO,PY,entry_number, .before="author_order") %>% 
  relocate(author_key,entry_number, .before="author_order") %>% 
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
  unite(refID,filename,entry_number,sep="-",na.rm=TRUE,remove=FALSE)

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
  distinct(entry_number,authid,afid,.keep_all = TRUE)%>% 
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
  left_join(scopus_article_authors_wide) 


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
