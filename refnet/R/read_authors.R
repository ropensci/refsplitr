read_authors <- function(references, 
                         sim_score=0.88 ,
                         filename_root="./",
                         write_out_data=FALSE) {
  ptm <- proc.time()
  list1<-list()
  ############################################
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
    
    if( length(second.split)>0 && grepl("[A-Z][A-Z]", second.split)){middle<-substr(second.split,2,nchar(second.split))[1]}
    
    return(c(first=first, middle=middle, last=last))
  }
  
  #if(sim_score>1){print('Similarity score can not be greater than 1! Using default value (0.88)'); sim_score<-0.88}
  for(ref in 1:nrow(references)){
    #ref<-5209
    if(ref==1){print('Splitting author records')}
    
    #Split out authors and author emails
    authors_AU <- as.character(unlist(strsplit(references[ref,]$AU, "/")))
    authors_AF <- as.character(unlist(strsplit(references[ref,]$AF, "/")))
    
    authors_EM <- unlist(strsplit(references[ref,]$EM, "/"))
    authors_EM_strip<-substr(authors_EM,1,regexpr('@',authors_EM)-1)
    
    ###########
    #makes a datframe of authors as they will be used as a reference later
    authors_df<-data.frame(AU=authors_AU, AF=authors_AF,author_order=1:length(authors_AU),stringsAsFactors=F)
    ###########
    # Split out Addresses
    C1 <- unlist(strsplit(references[ref,]$C1, "/"))
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
    if(nrow(dd1)==0){dd1<-data.frame(names=authors_df$AF,address='Could not be extracted')}
    dd1$address[dd1$address=='NA']<-NA
    ###########
    # Split out Reprint Author information 
    RP <- unlist(strsplit(references[ref,]$RP, "/"))
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
  final$EM<-tolower(final$EM)
  final$address<-as.character(final$address)
  final$RP_address<-as.character(final$RP_address)
  final$address[!is.na(final$address) & final$address=='Could not be extracted' & !is.na(final$RP_address)]<-final$RP_address[!is.na(final$address) & final$address=='Could not be extracted' & !is.na(final$RP_address)]
  ##########################
  ###############################
  # address parsing
  ###############################

  final$address<-as.character(final$address)
  final$address[is.na(final$address)]<-'Could not be extracted'
  sub.address<-final
  list.address<-strsplit(sub.address$address,',')
  university.list<-sapply(list.address,function(x)x[1])
  country.list<-sapply(list.address,function(x)gsub('\\.','',x[length(x)]))
  country.list<-trimws(country.list,which='both')
  postal_code.list<-trimws(substr(country.list,1,(sapply(regexpr('USA',country.list),function(x)x[1]))-1),which='right')
  state.list<-postal_code.list
  
  state.list[nchar(state.list)>0]<-regmatches(state.list[nchar(state.list)>0],regexpr("[[:upper:]]{2}",state.list[nchar(state.list)>0]))
  
  
  postal_code.list[nchar(postal_code.list)>2]<-regmatches(postal_code.list[nchar(postal_code.list)>2],regexpr("[[:digit:]]{5}",postal_code.list[nchar(postal_code.list)>2]))
  postal_code.list[nchar(postal_code.list)<3]<-''
  country.list<-ifelse(grepl('USA',country.list),'USA',country.list)
  
  list.address1<-sapply(list.address,function(x)x[-c(1,length(x))])
  
  
  second.tier.list<-sapply(list.address1,function(x)x[length(x)])
  second.tier.list<-trimws(second.tier.list,which='both')
  second.tier.list[second.tier.list=='character(0)']<-NA
  
  list.address2<-sapply(list.address1,function(x)x[-c(length(x))])
  
  third.tier.list<-sapply(list.address2,function(x)x[length(x)])
  third.tier.list<-trimws(third.tier.list,which='both')
  third.tier.list[third.tier.list=='character(0)']<-NA
  
  remain.list<-sapply(list.address2,function(x)x[-c(length(x))][1])
  remain.list<-trimws(remain.list,which='both')
  remain.list[remain.list=='character(0)']<-NA
  
  address.df<-data.frame(adID=final$authorID,university=university.list,country=country.list,state=state.list,postal_code=postal_code.list,city=NA,department=NA,second.tier=second.tier.list,third.tier=third.tier.list,remain=remain.list,address=sub.address$address,stringsAsFactors=F)
  
  #try to fix the USA spots
  address.df$city[nchar(address.df$state)>0]<-address.df$second.tier[nchar(address.df$state)>0]
  address.df$state[nchar(address.df$state)==0]<-NA
  address.df$postal_code[nchar(address.df$postal_code)==0]<-NA
  address.df$department[!is.na(address.df$state)&!is.na(address.df$postal_code)&!is.na(address.df$state)]<-address.df$third.tier[!is.na(address.df$state)&!is.na(address.df$postal_code)&!is.na(address.df$state)]
  #address.df$adID<-1:nrow(address.df)
  ##########################
  # reg expression postal_code search
  int<-"[[:alpha:]]{2}[[:punct:]]{1}[[:digit:]]{1,8}|[[:space:]][[:upper:]][[:digit:]][[:upper:]][[:space:]][[:digit:]][[:upper:]][[:digit:]]|[[:alpha:]][[:punct:]][[:digit:]]{4,7}|[:upper:]{1,2}[:alnum:]{1,3}[:space:][:digit:][:alnum:]{1,3}"
  
  UK<-"[[:upper:]]{1,2}[[:digit:]]{1,2}[[:space:]]{1}[[:digit:]]{1}[[:upper:]]{2}"
  
  #SO14 3ZH
  #L69 7ZB
  #NR33 OHT
  Mexico<-"[[:space:]]{1}[[:digit:]]{5}" #technically US as well
  
  zip.search<-paste0(int,'|',UK,'|',Mexico)
  
  ###########################
  id.run<-address.df$adID[is.na(address.df$state)&is.na(address.df$postal_code) & address.df$address!='Could not be extracted']
  ###########################
  
  print("Parsing addresses")
  for(i in id.run){
    #i<-13998
    found<-F
    row<-which(address.df$adID==i)
    second.tier<-address.df$second.tier[row]
    third.tier<-address.df$third.tier[row]
    remain<-address.df$remain[row]
    city<-NA;state<-NA;postal_code<-NA;department<-NA
    grepl(zip.search,second.tier)
    grepl(zip.search,third.tier)
    #2nd tier
    if(grepl(zip.search,second.tier)){
      found<-T
      postal_code<-regmatches(second.tier,regexpr(zip.search,second.tier) )
      city<-gsub(zip.search,'',second.tier)
      department<-ifelse(is.na(remain),third.tier,remain)
    }
    
    
    if(grepl(zip.search,third.tier)&!found){
      found<-T
      postal_code<-regmatches(third.tier,regexpr(zip.search,third.tier) )
      city<-gsub(zip.search,'',third.tier)
      state<-second.tier
      department<-remain
    }
    
    if(!found){
      state<-second.tier
      city<-third.tier
      department<-remain
    }
    
    postal_code <- gsub("[A-z]-","",postal_code)
    
    address.df$city[row]<-gsub('[[:digit:]]','',city)
    address.df$state[row]<-gsub('[[:digit:]]','',state)
    address.df$postal_code[row]<-postal_code
    address.df$department[row]<-department
    ###############################Clock#############################################
    total <- length(id.run)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, which(id.run==i))
    flush.console()
    #################################################################################
    
  }
  #address.df$department[is.na(address.df$department) & !is.na(address.df$third.tier)]<-address.df$third.tier[is.na(address.df$department) & !is.na(address.df$third.tier)]
  city.fix<-is.na(address.df$city) & !is.na(address.df$state)
  address.df$city[city.fix]<-address.df$state[city.fix]
  address.df$state[city.fix]<-NA
  address.df$university[address.df$university=='Could not be extracted']<-NA
  address.df$country[address.df$country=='Could not be extracted']<-NA
  
  final<-merge(final,address.df[,c('university','country','state','postal_code','city','department','adID')],by.x='authorID',by.y='adID',all.x=T)
  ##################################
  # Now start Author Matching
  ##################################
  #address.df$groupID<-NA
  #final<-final[1:10000,]
  colnames(final)
  print('Matching authors')
  novel.names<-data.frame(ID=final$authorID,unique.name=final$AF, groupID=NA, address=final$address,university=final$university,country=final$country,RI=final$RI,OI=final$OI,email=final$EM,first=NA,middle=NA,last=NA)
  novel.names[,c("first",'middle','last')]<-t(sapply(as.character(novel.names$unique.name), split.names))
  novel.names$first<-tolower(novel.names$first)
  novel.names$middle<-tolower(novel.names$middle)
  novel.names$last<-tolower(novel.names$last)
  novel.names$university[novel.names$university%in%c('No Affiliation',"Could not be extracted")]<-NA
  novel.names$university<-tolower(as.character(novel.names$university))
  novel.names$address[novel.names$address%in%c('No Affiliation',"Could not be extracted")]<-NA
  novel.names$address<-tolower(as.character(novel.names$address))
  length(unique(novel.names$groupID))
  unique.oi<-novel.names$OI[!is.na(novel.names$OI) & is.na(novel.names$groupID)]
  unique.oi<-names(table(unique.oi))[table(unique.oi)>1]
  unique.oi<-unique.oi[!is.na(unique.oi)]
  unique.oi<-as.character(unique.oi)
  unique.oi[nchar(unique.oi)!=19]
  unique.oi[unique.oi=="0000-0001-9549-4178"]
  for(l in unique.oi){
    #l<-"0000-0003-2589-826X"
    novel.names$groupID[which(novel.names$OI==l)]<- min(novel.names$ID[which(novel.names$OI==l)])
  }
  length(unique(novel.names$groupID))
  
  unique.ri<-novel.names$RI[!is.na(novel.names$RI) & is.na(novel.names$groupID)]
  unique.ri<-names(table(unique.ri))[table(unique.ri)>1]
  unique.ri<-unique.ri[!is.na(unique.ri)]
  unique.ri<-as.character(unique.ri)
  unique.ri[nchar(unique.ri)!=11]
  for(l in unique.ri){
    #l<-"E-7093-2013"
    choice<-which(novel.names$RI==l)
    groupid<-novel.names$groupID[choice]
    groupid<-groupid[!is.na(groupid)]
    if(length(groupid)>0){groupid<-min(groupid)} else {groupid<-min(novel.names$ID[choice],na.rm=T)}
    
    novel.names$groupID[which(novel.names$RI==l)]<-groupid
    
    #if(groupid==30){stop()}
  }
  length(unique(novel.names$groupID))
  
  unique.em<-novel.names$email[!is.na(novel.names$email) & is.na(novel.names$groupID)]
  unique.em<-names(table(unique.em))[table(unique.em)>1]
  unique.em<-unique.em[!is.na(unique.em)]
  unique.em<-as.character(unique.em)
  for(l in unique.em){
    # l<-'soto@ege.fcen.uba.ar'
    #l<-'aabril@agro.uncor.edu'
    choice<-which(novel.names$email==l)
    groupid<-novel.names$groupID[choice]
    groupid<-groupid[!is.na(groupid)]
    
    if(length(groupid)>0){groupid<-min(groupid)} else {groupid<-min(novel.names$ID[choice],na.rm=T)}
    novel.names$groupID[which(novel.names$email==l)]<-groupid
    
  }
  
  
  length(unique(novel.names$groupID))
  
  novel.names$similarity<-NA
  novel.names$match_name<-NA
  novel.names$f.c<-nchar(novel.names$first)
  novel.names$m.c<-nchar(novel.names$middle)
  novel.names$f.i<-substr(novel.names$first,1,1)
  novel.names$m.i<-substr(novel.names$middle,1,1)
  #match authors with the same first, last, and middle name
  remain<-subset(novel.names,!is.na(m.i) & f.c>1)[,c('ID','groupID','first','middle','last')]
  remain<-merge(subset(remain,is.na(groupID)), remain, by=c('first','middle','last'))
  remain<-subset(remain, ID.x!=ID.y)
  dd<-data.frame(g.n=unique(paste(remain$first,remain$middle,remain$last,sep=';')),first=NA,middle=NA,last=NA,stringsAsFactors=F)
  dd$id<-1:nrow(dd)
  dd[,c('first','middle','last')]<-do.call(rbind,strsplit(dd$g.n,';'))
  remain<-merge(remain,subset(dd,select=-g.n), by=c('first','middle','last'))
  for(n in dd$id){
    sub<-subset(remain, id==n)
    unique.id<-unique(sub$ID.x)
    if(!sum(is.na(sub$groupID.y))==nrow(sub)){ groupid<-min(unique(sub$groupID.y),na.rm=T)}else{groupid<-min(unique.id,na.rm=T)}
    
    novel.names$groupID[novel.names$ID%in%unique.id]<-groupid  
  }
  length(unique(novel.names$groupID))
  #
  
  unique.groupid<-subset(novel.names,(m.c>0 |!is.na(university) | !is.na(email)) & is.na(groupID))$ID
  which(unique.groupid==9364)
  
  
  for(p in unique.groupid){
    #for(p in unique.groupid[1:5654]){
    #p<-9364
    matched<-F
    default.frame<-data.frame(ID=NA,first=NA,middle=NA,last=NA,university=NA,email=NA,f.i=0,address=NA,country=NA)
    match1<-NA;match2<-NA;match3<-NA; match4<-NA
    name.df<-novel.names[novel.names$ID==p,]
    # We need to create a dataframe of possible matching authors, so we dont run comparissons on obviously incorrect people
    novel.names1<-subset(novel.names,(m.c>0 |!is.na(university) | !is.na(email) | !is.na(address)) & ID!=p)
    
    novel.names1<-novel.names1[substr(name.df$first,1,1)==novel.names1$f.i & name.df$last==novel.names1$last  ,]
    if(nrow(novel.names1)==0){novel.names1<-default.frame}
    if(!is.na(name.df$first) & nchar(name.df$first)==1){novel.names1<-novel.names1[novel.names1$f.i==name.df$first,]}
    if(!is.na(name.df$first) & nchar(name.df$first)>1){novel.names1<-novel.names1[novel.names1$f.c==1 | novel.names1$first==name.df$first,]}
    #if middle initial is 1 letter
    if(!is.na(name.df$middle) & nchar(name.df$middle)==1){novel.names1<-novel.names1[is.na(novel.names1$middle) | novel.names1$m.i==name.df$middle,]}
    #if has a middle name, must match
    if(!is.na(name.df$middle) & nchar(name.df$middle)>1){novel.names1<-novel.names1[is.na(novel.names1$middle) | novel.names1$m.i==name.df$m.i,]}
    #if has a country, it must match
    if(!is.na(name.df$country)){
      novel.names1<-novel.names1[is.na(novel.names1$country) | novel.names1$country==name.df$country,]
    }
    
    if(nrow(novel.names1)==0){novel.names1<-default.frame}
    if(!anyNA(novel.names1$ID)){
      #match full middle name
      match1<-!is.na(name.df$middle) & novel.names1$middle==name.df$middle
      #match addresses
      match2<-(!is.na(novel.names1$university)& !is.na(name.df$university)) & name.df$university==novel.names1$university
      #match middle initial
      match3<-!is.na(name.df$m.i) & novel.names1$m.i==name.df$m.i
      match4<-is.na(name.df$address) & novel.names1$address==name.df$address
      #match emails
      #if(nrow(novel.names1)==0){match1<-F;match2<-F;match3<-F}
      if(sum(ifelse(is.na(c(match1,match2,match3,match4)),F,c(match1,match2,match3,match4)))>0){
        matched<-T
        choice<-c(which(match1),which(match2),which(match3),which(match4))
        if(sum(!is.na(novel.names1$groupID[choice]))>0){groupid<-min(novel.names1$groupID[choice],na.rm=T)} else {groupid<-min(novel.names1$ID[choice],na.rm=T)}
        novel.names$groupID[novel.names$ID==p]<-groupid
      }
    }
    
    #Jaro_winkler
    if(!matched & nrow(novel.names1)>0 & !anyNA(novel.names1$ID)) {
      
      jw_m<-jarowinkler(paste0(novel.names1$last,novel.names1$first,novel.names1$middle),paste0(name.df$last,name.df$first,name.df$middle))
      choice<-which(max(jw_m)==jw_m)[1]
      
      if(sum(!is.na(novel.names1$groupID[choice]))>0){groupid<-min(novel.names1$groupID[choice],na.rm=T)
      groupname<-novel.names1$unique.name[novel.names1$groupID==groupid][1]
      } else {
        groupid<-min(novel.names1$ID[choice],na.rm=T)
        groupname<-novel.names1$unique.name[novel.names1$ID==groupid]}
      
      novel.names$groupID[novel.names$ID==p]<-groupid
      novel.names$match_name[novel.names$ID==p]<-as.character(groupname)
      #changeID<-final$authorID[novelids][choice][1]
      if(sum(choice)>0){novel.names$similarity[novel.names$ID==p]<-jw_m[choice][1]}
    }
    ###############################Clock#############################################
    total <- length(unique.groupid)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    setTxtProgressBar(pb, which(p==unique.groupid))
    flush.console()
    #################################################################################
    
  }
  # Fixes a small issue where sometimes matched names using Jaro_winkler will get a groupID distinction but that the author
  # it matched with will get a different groupID if it has a more perfect match criteria.
  novel.names$groupID[is.na(novel.names$groupID)]<-novel.names$ID[is.na(novel.names$groupID)]
  quick.check<-novel.names$ID[!is.na(novel.names$similarity)]
  for(m in quick.check){
    #m<-quick.check[1]
    novel.names$groupID[novel.names$ID==m]<-novel.names$groupID[novel.names$ID==novel.names$groupID[novel.names$ID==m]]
  }
  
  length(unique(novel.names$groupID))
  
  proc.time() - ptm
  
  final<-merge(final, novel.names[,c('ID','groupID','match_name','similarity')], by.x='authorID',by.y='ID',all.x=T)
  final<-final[,c('authorID','AU','AF','groupID','match_name','similarity',colnames(final)[!colnames(final)%in%c('authorID','AU','AF','groupID','match_name','similarity')])]
  
  #This only brings in the author to be matched and its actual match
  #sub<-final[!is.na(final$similarity) | (final$authorID %in% (final$groupID[!is.na(final$similarity)])),c('authorID','AU','AF','groupID','match_name','similarity','author_order','address','university','department','short_address','postal_code',"country",'RP_address','RI','OI','EM','UT','refID',"PT","PY","PU")]
  #This brings in the author to be matched and the whole groupID associated so you can have more information
  sub.authors<-final[final$groupID%in%final$groupID[!is.na(final$similarity)],c('authorID','AU','AF','groupID','match_name','similarity','author_order','university','department','postal_code',"country",'address','RP_address','RI','OI','EM','UT','refID',"PT","PY","PU")]
  sub.authors<-sub.authors[order(sub.authors$groupID, sub.authors$similarity,sub.authors$authorID),]
  
  #write it out
  write.csv(subset(final,select=-c(match_name,similarity)), 
            file=paste0(filename_root, "_authors_master.csv"), 
            row.names=FALSE)
  
  write.csv(sub.authors, 
            file=paste0(filename_root, "_authors.csv"), 
            row.names=FALSE)
  #
  return(list(master=final,authors=sub.authors))
} #end function
