#' Refines the authors code output from read_authors()
#' 
#' \code{refine_authors} This function takes the author list output after the output has been synthesized for incorrect author matches. It contains a similarity score cutoff like read_authors. This however is to further constrain the list. New values ARE NOT created, instead it filters by the sim_score column in the output file. An output file is created using the 'root' argument that specifies the folder/file prefix for the output. The final file will be appended with '_final.csv'. 
#' 
#' @param authors output from the read_authors() function
#' @parm  sim_score similarity score cut off point. Number between 0-1. Default is 88 (liberal)
#' @param filename_root the filename root, can include relative or absolute
#'   path, to which "_authors.csv" and "_authors__references.csv" will be appended and the
## so what you need to do is take the authors file and change the names (no au and af), with addresses, title, and authors
refine_authors<-function(authors, sim_score=NULL,filename_root=""){
  
  if(is.null(sim_score)){sim_score<-min(authors$similarity,na.rm=T)}
  
authors$groupID[!is.na(authors$similarity) & authors$similarity<sim_score]<-authors$authorID[!is.na(authors$similarity) & authors$similarity<sim_score]

for(i in unique(authors$groupID)){
  #i<-unique(authors$groupID)[2]
  authors$AF[authors$groupID==i]<-authors$AF[sort(authors$authorID[authors$groupID==i])[1]]
}

colnames(authors)
data1<-authors[,c('authorID','AF','groupID','author_order','address','RP_address','RI','OI','UT','refID')]
colnames(data1)[colnames(data1)=='AF']<-'author_name'
#write it out
#write it out
if(filename_root != "") {
  write.csv(final, 
            file=paste0(filename_root, "_authors_final.csv"), 
            row.names=FALSE)
}            
#
return(data1)
}

