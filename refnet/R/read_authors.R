##################################################
##################################################
##	BEGIN: read_authors():

#' Processes the output of read_references() to extract authors
#' 
#' \code{read_authors} This function extracts authors from a read_references format data.frame object, uses a Jaro-Winkler comparison of first names to try to match authors with multiple Last Name, Initial combinations, filling in potential matches using the AU_ID_Dupe and Similarity fields in the resulting output.  The output is a list containing two data.frame objects, one named authors and the other authors__references, which is a linking table that links authors by the AU_ID field to references data via the UT (Web of Knowledge ID) field.
#' 
#' @param references output from the read_references() function
#' @parm  sim_score similarity score cut off point. Number between 0-1. Default is 88 (liberal)
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_authors.csv" and "_authors__references.csv" will be appended and the output from the
#'   function will be saved
read_authors <- function(references, sim_score=0.88 ,filename_root="") {
  
  list1<-list()
  if(sim_score>1){print('Similarity score can not be greater than 1! Using default value (0.88)'); sim_score<-0.88}
  for(ref in 1:nrow(references)){
    #ref<-2818
    if(ref==1){print('Now summarizing author records')}
    
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
    dd1<-data.frame(names=unique(unlist(strsplit(C1_names,'; '))),address=sapply(unique(unlist(strsplit(C1_names,'; '))),function(x)
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
  ############################################
  # This is a secondary function to match authors that are likely to be the same person. This could
  # possibly be done in a seperate function, but it seems to make sense to nest it here
  novel<-list('1'=1) # the novel list is a list of novel authors who get their own GROUPID
  #we start on the 2nd record as we assume the 1st record is novel
  for(i in 2:nrow(final)){
    #for(i in 2:1000){
    if(i==2){print('Now matching similar authors records')}
    #i<-2
    changeID<-NA
    novelids<-unlist(novel) #every iteration we rebuild the list of novelIDs and authors
    name<-final$AF[i]
    indicesAF<-final$AF[novelids]
    indicesAU<-final$AU[novelids]
    #Author matching is done heirarchical starting with simple solutions and then leading to matching using jaro winkler methods
    # in this instance changeID will guide us through the hierarchy, if changeID is anything but NA we skip to the next record unitl a match has been found
    #match by exact OI matches
    if(is.na(changeID) & !is.na(final$OI[i])){  
      changeID<-final$authorID[novelids][!is.na(final$OI[novelids]) && fina$OI[i]==final$OI[novelids]][1]
    }
        
    #match by exact name matches AF
    if(sum(name==indicesAF)>0){changeID<- final$authorID[novelids][indicesAF==name][1]}
    
    #match by exact name matches AU
    if(sum(name==indicesAU)>0){changeID<- final$authorID[novelids][indicesAU==name][1]}
    
    #match by exact RI matches
    if(is.na(changeID) & !is.na(final$RI[i])){  
      changeID<-final$authorID[novelids][!is.na(final$RI[novelids]) && fina$RI[i]==final$RI[novelids]][1]
    }

    
    #If we still dont have a match, we will attempt to match using Jaro-winkler matching algorithm
    # Currently this is controlled by the sim_score limit. 0.9 seems to match accurately but leave out
    # more difficult matches. 88% seems fine if you want to hand check. Anything less usually doesn't match correctly.
    if(is.na(changeID)){
      jw_m<-jarowinkler(gsub(', ','',name), gsub(', ','',indicesAF))
      choice<-(max(jw_m)==jw_m & jw_m>=sim_score)
      changeID<-final$authorID[novelids][choice][1]
      if(sum(choice)){final$similarity[i]<-jw_m[choice][1]}
    }
    
    #If we still never found a match, we can assume the author is novel.
    if(!is.na(changeID)){
      final$groupID[i]<-final$groupID[changeID==final$authorID]
      final$match_name[i]<-final$AF[changeID==final$authorID]}
    if(is.na(changeID)){novel[[paste0(i)]]<-final$authorID[i]}
    
    
    ###############################Clock#############################################
    
    total <- nrow(final)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
    #################################################################################
  }
  colnames(final)
  final<-final[,c('authorID','AU','AF','groupID','match_name','similarity','author_order','address','RP_address','RI','OI','EM','UT','refID')]
  
  #write it out
  if(filename_root != "") {
    write.csv(final, 
              file=paste0(filename_root, "_authors.csv"), 
              row.names=FALSE)
  }            
  
  #
  return(final)
} #end function
