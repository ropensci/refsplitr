##################################################
##################################################
##	BEGIN: read_authors():

#' Processes the output of read_references() to extract authors
#' 
#' \code{read_authors} This function extracts authors from a read_references format data.frame object, uses a Jaro-Winkler comparison of first names to try to match authors with multiple Last Name, Initial combinations, filling in potential matches using the AU_ID_Dupe and Similarity fields in the resulting output.  The output is a list containing two data.frame objects, one named authors and the other authors__references, which is a linking table that links authors by the AU_ID field to references data via the UT (Web of Knowledge ID) field.
#' 
#' @param references output from the read_references() function
#' @param  sim_score similarity score cut off point. Number between 0-1. Default is 88 (liberal)
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_authors.csv" and "_authors__references.csv" will be appended and the output from the
#'   function will be saved
read_authors <- function(references, 
                         sim_score=0.88 ,
                         filename_root="./",
                         write_out_data=FALSE) {
  
  list1<-list()
  #if(sim_score>1){print('Similarity score can not be greater than 1! Using default value (0.88)'); sim_score<-0.88}
  for(ref in 1:nrow(references)){
    #ref<-5209
    if(ref==1){print('Summarizing author records')}
    
    #Split out authors and author emails
    authors_AU <- as.character(unlist(strsplit(references[ref,]$AU, "\n")))
    authors_AF <- as.character(unlist(strsplit(references[ref,]$AF, "\n")))
    
    authors_EM <- unlist(strsplit(references[ref,]$EM, "\n"))
    authors_EM_strip<-substr(authors_EM,1,regexpr('@',authors_EM)-1)
    
    ###########
    #makes a datframe of authors as they will be used as a reference later
    authors_df<-data.frame(AU=authors_AU, AF=authors_AF,author_order=1:length(authors_AU),stringsAsFactors=F)
    ###########
    # Split out Addresses
    C1 <- unlist(strsplit(references[ref,]$C1, "\n"))
    if(length(authors_AU)==1){C1<-paste0('[',authors_AU,'] ',C1)}
    
    C1<-C1[grepl("^\\[.*\\]", C1)]
    # Split names from the addresses they're associated with
    C1_names<-regmatches(C1,regexpr("^\\[.*\\]", C1))
    C1_names<-substr(C1_names,2,nchar(C1_names)-1)
    if(length(authors_AU)==1){C1_names<-authors_AU}
    
    # Split out the addresses and not the names assocated
    C1_address <- gsub("^\\[.*\\] (.*)$", "\\1", C1)
    
    #create a dataframe of all unique addresses and their matching affiliations
    dd<-data.frame(C1_names,C1_address,stringsAsFactors=F)
    dd1<-data.frame(names=unique(unlist(strsplit(C1_names,
                                                 '; '))),
                    address=sapply(unique(unlist(strsplit(C1_names,
                                                          '; '))),
                                   function(x)
      dd$C1_address[grepl(x,dd$C1_names)][1]))
    if(nrow(dd1)==0 & length(C1_address)==length(authors_AU)){dd1<-data.frame(names=authors_AU,address=C1_address)}
    if(nrow(dd1)==0){dd1<-data.frame(names='',address='')}
    ###########
    # Split out Reprint Author information 
    RP <- unlist(strsplit(references[ref,]$RP, "\n"))
    RP_address <- gsub("^.*\\(reprint author\\), (.*)$", "\\1", RP)
    RP_df<-data.frame(AU=substr(RP,1,regexpr('(reprint author)',RP)[1]-3),RP_address)
    
    ##########
    #Split out RI information
    RI <- unlist(strsplit(references[ref,]$RI, ";"))  
    
    if(length(strsplit(RI,'/'))<2){RI_df<-data.frame(RI_names='',RI='',matchname='')}
    
    if(length(strsplit(RI,'/'))>1){
      RI_df<-data.frame(do.call(rbind,lapply(strsplit(RI,'/'), function(x)x[1:2])),stringsAsFactors=F)
      # Match the author names to the RIs affiliations, this is not as exact as you think it would be
      colnames(RI_df)<-c('RI_names','RI')
      
      match_ri<-sapply(RI_df[,1], function(x) {
        jw<-jarowinkler(x,authors_AU)
        jw==max(jw) & jw>0.8})
      if(length(authors_AU)>1){  
        RI_df$matchname<-unlist(apply(match_ri, 2, function(x)ifelse(sum(x)==0,'',authors_AU[x])))
      }
      if(length(authors_AU)==1){
        RI_df$matchname <- ifelse(sum(match_ri)==0,'',authors_AU[match_ri]) 
      }
    }
    
    #RI
    ##########
    #split out the OI and do the same thing we did with the RI
    OI <- unlist(strsplit(references[ref,]$OI, ";"))  
    
    if(sum(is.na(OI))==0){
      OI_df<-as.data.frame(do.call(rbind,strsplit(OI,'/')),stringsAsFactors=F)
      colnames(OI_df)<-c('OI_names','OI')
      match_OI<-sapply(OI_df[,1], function(x) {
        jw<-jarowinkler(x,authors_AU)
        jw==max(jw) & jw>0.8})
      
      OI_df$matchname<-unlist(apply(data.frame(match_OI), 2, function(x)ifelse(sum(x)==0,'',authors_AU[x])))
    }
    
    if(sum(is.na(OI))>0){OI_df<-data.frame(OI_names='',OI='',matchname='')}
    ############
    # merge all this information together by author name, some journals use the full name some the shortend name
    #########
    new<-merge(authors_df,dd1,by.x='AU',by.y='names',all.x=T)
    if(sum(is.na(new$address))==length(new$address)){new<-merge(authors_df,dd1,by.x='AF',by.y='names',all.x=T)}
    new<-merge(new, RP_df, by='AU',all.x=T)
    new<-merge(new, RI_df[,c('RI','matchname')], by.x='AU',by.y='matchname',all.x=T)
    new<-merge(new, OI_df[,c('OI','matchname')], by.x='AU',by.y='matchname',all.x=T)
    new$EM<-NA
    new$refID<-references$refID[ref]
    new$TA<-references$TI[ref]
    new$SO<-references$SO[ref]
    new$UT<-references$UT[ref]
    new$PT<-references$PT[ref]
    new$PU<-references$PU[ref]
    new$PY<-references$PY[ref]
    #########################3
    #
    # Matching emails is an imprecise science, as emails dont have to match names in any reliable way or at all
    # I feel its better to leave these alone as much as possible, analyzing the resulting matches will lead to issues
    match_em<-sapply(authors_EM_strip, function(x) apply(data.frame(jarowinkler(x,new$AU),jarowinkler(x,new$AF)),1,max))
    
    for(i in 1:length(authors_EM)){
      #i<-1
      new$EM[as.data.frame(apply(as.matrix(match_em),2,function(x) x>0.7 & max(x)==x))[,i]]<-authors_EM[i]
    }
    
    if(nrow(new)==1){new$EM<-authors_EM[1]}
    list1[[ref]]<-new
    
    ###############################Clock#############################################
    
    total <- nrow(references)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, ref)
    flush.console()
    #################################################################################
  }
  
  #Bind all author iterations together into one large sheet that should be used for base analysis from her eon out
  final<-do.call(rbind,list1)
  final$authorID<-1:nrow(final)
  final$groupID<-final$authorID
  final$match_name<-NA
  final$similarity<-NA
  ##########################
  ###############################
  # address parsing
  ###############################
  suppressWarnings(origAddress <- separate(data=final, 
                                           col = address,
                                           into=c("university","department","short_address"),
                                           sep=",",extra = "merge", remove=FALSE) %>%
                     mutate(postal_code = str_extract(string=short_address, 
                                                      pattern="[:alpha:]{2}[:punct:]{1}[:digit:]{1,8}|[:space:][:upper:][:digit:][:upper:][:space:][:digit:][:upper:][:digit:]|[:alpha:][:punct:][:digit:]{4}")) %>%
                     mutate(postal_code = ifelse(is.na(postal_code), 
                                                 str_extract(string=short_address,
                                                             pattern="[:space:][:digit:]{5}"), postal_code)) %>%
                     mutate(postal_code = ifelse(is.na(postal_code), 
                                                 str_extract(string=short_address,
                                                             pattern="[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"),
                                                 postal_code)))
  
  origAddress[is.na(origAddress$short_address),"short_address"] <- "No Affiliation" 
  
  
  finalad <- extract_country_name(origAddress)
  finalad$institution <- finalad$university
  finalad<-finalad[,c('authorID','AU','AF','groupID',
                      'match_name','similarity','author_order',
                      'address','institution','department',
                      'short_address','postal_code',"country",
                      'RP_address','RI','OI','EM','UT',
                      'refID',"PT","PY","PU")]
  final<-finalad
  

  
  
  ############################################
  # This is a secondary function to match authors that are likely to be the same person. This could
  # possibly be done in a seperate function, but it seems to make sense to nest it here
  # 
  # Because we are matching names, we're going to split the author names into their first,last,middle component.
  # This does this for the first entry and creates a list of novel names that we will step through later
  novel.df<-data.frame(id=rep(NA,nrow(final)),first=NA,middle=NA,last=NA,institution=NA,email=NA)
  
  ############################################
  # Split Names function
  ###########################################
  split.names<-function(x){
    first<-NA;middle<-NA;last<-NA
    #x<-final$AF[1]
    #split first by commas, as we assume this is atleast seperating the last name from the rest of the information
    first.split<-strsplit(x,',')
    #we are going to assume the very first split before the comma is the last name
    last<-first.split[[1]][1]
    
    #Since we've already split by commas, the next most comma split is by spaces. This can be dangerous if someone has a space in their first name, for now we'll assume it doesnt
    second.split<-strsplit(first.split[[1]][-1],' ')
    #delete trailing spaces (maybe a better way to do that)
    second.split<-unlist(second.split)
    second.split<-second.split[nchar(second.split)>0]
    
    #now we assume that the first name after the first comma is the first name, this is pretty standard so it should be safe enough of an assumption. 
    #However because we often have First initials and middle intials shoved together with no space we have to specify lower case
    
    first<- regmatches(second.split,regexpr("[A-Z][a-z]*",second.split))[1]
    
    # Middle names are messy because they have multiple parts, spaces, names, and jrs srs, for the sake of this analysis
    # we'll just shove the names together. Even though this might not be 'correct'. 
    if(length(second.split)>1){
      third.split<-second.split[-1]
      middle<- gsub('[\\./,]','',paste0(third.split,collapse=''))
    }
    #Check if first and middle names are just initials and not seperated by period
    
    if( length(second.split)>0 && grepl("[A-Z][A-Z]", second.split)){middle<-substr(second.split,2,nchar(second.split))}
    
    return(c(first=first, middle=middle, last=last))
  }
 ###########################################
  
  novel.df[1,c('id','first','middle','last','institution','email')]<- c(1,split.names(final$AF[1]),final$institution[1],final$EM[1])
  
  novel<-list('1'=1) # the novel list is a list of novel authors who get their own GROUPID
  #we start on the 2nd record as we assume the 1st record is novel
  for(i in 2:nrow(final)){
   #for(i in 15659:nrow(final)){
    if(i==2){print('Matching similar authors names')}
    #i<-64
    changeID<-NA
    novelids<-unlist(novel) #every iteration we rebuild the list of novelIDs and authors
    name<-final$AF[i]
    institution<-final$institution[i]
    email<-final$EM[i]
    indicesAF<-final$AF[novelids]
    indicesAU<-final$AU[novelids]
    indicesUni<-final$institution[novelids]
    #Author matching is done heirarchical starting with simple solutions and then leading to matching using jaro winkler methods
    # in this instance changeID will guide us through the hierarchy, if changeID is anything but NA we skip to the next record unitl a match has been found
    #match by exact OI matches
    if(!is.na(final$OI[i])){  
      changeID<-final$groupID[!is.na(final$OI[1:(i-1)]) & final$OI[1:(i-1)]==final$OI[i]][1]
    }
    
    #match by exact RI matches
    if(is.na(changeID) & !is.na(final$RI[i])){  
      changeID<-final$groupID[!is.na(final$RI[1:(i-1)]) & final$RI[1:(i-1)]==final$RI[i]][1]
    }
    

    # split name into its first, middle, last component
    name.df<-split.names(final$AF[i])
    name.df<-data.frame(first=name.df[1],middle=name.df[2],last=name.df[3],institution=institution,email=email,stringsAsFactors=F)
    
    #Check name matches
    #Create a novel names df to compare against
    novel.names<-novel.df[!is.na(novel.df$id),]
    
    #First check for full name matching (first, middle, last)
    name.df
    colnames(novel.names)
    
    novel.names1<-novel.names[substr(name.df$first,1,1)==substr(novel.names$first,1,1) & name.df$last==novel.names$last,]
    if(nrow(novel.names1)==0){novel.names1<-data.frame(id=NA,first=NA,middle=NA,last=NA,institution=NA,email=NA)}
    if(!is.na(name.df$first) & nchar(name.df$first)==1){novel.names1<-novel.names1[substr(novel.names1$first,1,1)==name.df$first,]}
    if(!is.na(name.df$first) & nchar(name.df$first)>1){novel.names1<-novel.names1[nchar(novel.names1$first)==1 | novel.names1$first==name.df$first,]}
    if(!is.na(name.df$middle) & nchar(name.df$middle)==1){novel.names1<-novel.names1[is.na(novel.names1$middle) | substr(novel.names1$middle,1,1)==name.df$middle,]}
    if(nrow(novel.names1)==0){novel.names1<-data.frame(id=NA,first=NA,middle=NA,last=NA,institution=NA,email=NA)}
    if(nrow(novel.names1)>0){
    
    match1<-!is.na(name.df$middle) | novel.names1$middle==name.df$middle
    #match addresses
    match2<-(!is.na(novel.names1$institution)& !is.na(name.df$institution)) & name.df$institution==novel.names1$institution
    #match emails
    match3<-(!is.na(novel.names1$email)& !is.na(name.df$email)) & name.df$email==novel.names1$email
    }
    #if(nrow(novel.names1)==0){match1<-F;match2<-F;match3<-F}
    if(sum(ifelse(is.na(c(match1,match2,match3)),F,c(match1,match2,match3)))>0){
      changeID<-novel.names1$id[c(which(match1),which(match2),which(match3))[1]]
      }
    
    if(is.na(changeID)  & nrow(novel.names1)>0 & !is.na(novel.names1$id)){
      
      
     jw_m<-jarowinkler(paste0(novel.names1$last,novel.names1$first,novel.names1$middle),paste0(name.df$last,name.df$first,name.df$middle))
        #choice<-(max(jw_m)==jw_m & jw_m>=sim_score)
        choice<-(max(jw_m)==jw_m)
        changeID<-novel.names1$id[choice][1]
        #changeID<-final$authorID[novelids][choice][1]
        if(sum(choice)>0){final$similarity[i]<-jw_m[choice][1]}
      }
    
    
    #If we found a match we'll change the groupID to it's match
    if(!is.na(changeID)){
      final$groupID[i]<-final$groupID[changeID==final$authorID]
      final$match_name[i]<-final$AF[changeID==final$authorID]}
    #If we still never found a match, we can assume the author is novel.
    if(is.na(changeID)){novel[[paste0(i)]]<-final$authorID[i]
    novel.df[i,c('id','first','middle','last','institution','email')]<-c(final$authorID[i],name.df)
    }
    
    ###############################Clock#############################################
    total <- nrow(final)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
    #################################################################################
  }
  
  #final$groupID[!is.na(final$similarity)]
  sub<-final[!is.na(final$similarity) | (final$authorID %in% (final$groupID[!is.na(final$similarity)])),c('authorID','AU','AF','groupID','match_name','similarity','address',"country",'RI','OI','EM','UT')]
  
  sub<-sub[order(sub$groupID, sub$authorID),]
  
  
  
  
  
  #write it out
    write.csv(subset(final,select=-c(match_name,similarity)), 
              file=paste0(filename_root, "_authors_master.csv"), 
              row.names=FALSE)
    
    write.csv(sub, 
              file=paste0(filename_root, "_authors.csv"), 
              row.names=FALSE)
  #
  return(list(master=final,authors=sub))
} #end function
