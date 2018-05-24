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
    #ref<-2818
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
  # 
  # Because we are matching names, we're going to split the author names into their first,last,middle component.
  # This does this for the first entry and creates a list of novel names that we will step through later
  novel.df<-data.frame(id=rep(NA,nrow(final)),first=NA,middle=NA,last=NA)
  
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
  
  novel.df[1,c('id','first','middle','last')]<- c(1,split.names(final$AF[1]))
  
  novel<-list('1'=1) # the novel list is a list of novel authors who get their own GROUPID
  #we start on the 2nd record as we assume the 1st record is novel
  for(i in 2:nrow(final)){
   #for(i in 2:1000){
    if(i==2){print('Matching similar authors names')}
    #i<-2
    changeID<-NA
    novelids<-unlist(novel) #every iteration we rebuild the list of novelIDs and authors
    name<-final$AF[i]
    indicesAF<-final$AF[novelids]
    indicesAU<-final$AU[novelids]
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
    
    #match by exact name matches AF
    if(is.na(changeID) & sum(name==indicesAF)>0){changeID<- final$authorID[novelids][indicesAF==name][1]}
    
    #match by exact name matches AU
    if(is.na(changeID) & sum(name==indicesAU)>0){changeID<- final$authorID[novelids][indicesAU==name][1]}
    
    #There's a minor problem where an RI/OI exists on only certain of the authors files. So this will match RI/OI if it exists in one but not the original author mention (defunct for now)
  
   
    # #If we still dont have a match, we will attempt to match using Jaro-winkler matching algorithm
    # # Currently this is controlled by the sim_score limit. 0.9 seems to match accurately but leave out
    # # more difficult matches. 88% seems fine if you want to hand check. Anything less usually doesn't match correctly.   # Below is defunct until we're sure we want to use the more complicated logic
    # if(is.na(changeID)){
    #   jw_m<-jarowinkler(gsub(', ','',name), gsub(', ','',indicesAF))
    #   choice<-(max(jw_m)==jw_m & jw_m>=sim_score)
    #   changeID<-final$authorID[novelids][choice][1]
    #   if(sum(choice)){final$similarity[i]<-jw_m[choice][1]}
    # }
    # 
    ###################################
    # split name into its first, middle, last component
    name.df<-split.names(final$AF[i])
    name.df<-data.frame(first=name.df[1],middle=name.df[2],last=name.df[3],stringsAsFactors=F)
    
    #Check name matches
    #Create a novel names df to compare against
    novel.names<-novel.df[!is.na(novel.df$id),]
    
    #Check first if full first and last name match but and middle initials (in rare instances where middle is full in one but initials in the other)
    if(is.na(changeID)){
    changeID<-novel.names$id[!is.na(novel.names$middle) && (novel.names$first==name.df$first & novel.names$last==name.df$last & substr(novel.names$middle,1,1)==substr(name.df$middle,1,1))]
    changeID<-ifelse(length(changeID)==0,NA,changeID)
    }
    
    #Check second if full last name exist as well as the first and middle letters. Run JW on these
    # create a subset list to match against where either:
    # Last Name ALWAYS MATCHES + 
    #first name only has 1 letter we match first names of any name of any length:
    # 1. First letter of first and middle match 
    # 2. First letter of first matches and there is no middle name 
    # or
    #first name has more than 1 letter in which case we only match similar names that have only 1 letter and
    # 1. First letter of first and middle match
    # 2. First letter of first matches and there is no middle name
   
    if(is.na(changeID)){
      #To make this easier, lets subset by same last name and same first letter of first name
      name.df$first[is.na(name.df$first)]<-' '
     sub.names<-novel.names[novel.names$last==name.df$last & (substr(novel.names$first,1,1)==substr(name.df$first,1,1)),]
      
     # The reason to do this is I need to match where NAs are involved and this makes the code easier to understand and troubleshoot later. It absolutely can be done with one large logical statement
     # If the authors name is just an initial
     if(nchar(name.df$first)==1 &!is.na(name.df$middle)){
       sub.names<-sub.names[is.na(sub.names$middle) | (!is.na(sub.names$middle)&(substr(sub.names$middle,1,1)==substr(name.df$middle,1,1) )),]
     }
     
     # If the authors name is a full name
     if(nchar(name.df$first)>1){
       sub.names<-sub.names[(sub.names$first==name.df$first & (!is.na(sub.names$middle) & (substr(sub.names$middle,1,1)==substr(name.df$middle,1,1) ) | (is.na(sub.names$middle))))  | (nchar(sub.names$first)==1 & (!is.na(sub.names$middle) & (substr(sub.names$middle,1,1)==substr(name.df$middle,1,1) ) | (is.na(sub.names$middle)))) ,]
       }
     
     # If their were names that matched our scrict criteria we will run Jaro_Winkler matching on them
       if(nrow(sub.names)>0 &&!is.na(sub.names$id) ){                                     
     jw_m<-jarowinkler(paste0(sub.names$last,sub.names$first,sub.names$middle),paste0(name.df$last,name.df$first,name.df$middle))
        #choice<-(max(jw_m)==jw_m & jw_m>=sim_score)
        choice<-(max(jw_m)==jw_m)
        changeID<-sub.names$id[choice][1]
        #changeID<-final$authorID[novelids][choice][1]
        if(sum(choice)>0){final$similarity[i]<-jw_m[choice][1]}
      }
    }
    
    #If we found a match we'll change the groupID to it's match
    if(!is.na(changeID)){
      final$groupID[i]<-final$groupID[changeID==final$authorID]
      final$match_name[i]<-final$AF[changeID==final$authorID]}
    #If we still never found a match, we can assume the author is novel.
    if(is.na(changeID)){novel[[paste0(i)]]<-final$authorID[i]
    novel.df[i,c('id','first','middle','last')]<-c(final$authorID[i],name.df)
    }
    
    ###############################Clock#############################################
    total <- nrow(final)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, i)
    flush.console()
    #################################################################################
  }
  
  final<-final[,c('authorID','AU','AF','groupID','match_name','similarity','author_order','address','RP_address','RI','OI','EM','UT','refID')]
  #final$groupID[!is.na(final$similarity)]
  sub<-final[!is.na(final$similarity) | final$authorID%in%final$groupID[!is.na(final$similarity)],c('authorID','AU','AF','groupID','match_name','similarity','address','RI','OI','EM')]
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
