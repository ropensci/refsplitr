context("authors refine")

test_that("Output of authors_refine makes sense", {
  

df.prelim<-data.frame("authorID"=c(1,2,3),
               "AU"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
               "AF"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
               "groupID"= c(3,2,3),      
               "match_name"=c('Smith,J',NA,'Smith, Jon J'),
               "similarity"= c(0.8833333,NA,0.8833333),   
               "author_order"= c(1,1,1),
               "address"=c("Univ Florida, Gainesville, FL USA",
                           "University of Texas, Austin, TX, USA",NA),      
               "RP_address"=c("Univ Florida, Gainesville, FL USA",
                              "University of Texas, Austin, TX, USA",NA),   
               "RI"=NA,          
               "OI"=NA,      
               "EM"=c("j.smith@ufl.edu",NA,'jsmith@usgs.gov'),          
               "refID"=c(1,2,3),
               "TA"=NA,
               "SO"=NA,          
               "UT"=NA,
               "PT"=NA,
               "PU"=NA,
               "PY"=NA,
               "university"=c('Univ Florida','University of Texas',NA),  
               "country"=c('USA','USA',NA),
               "state"=c('FL','TX',NA),
               "postal_code"=NA,
               "city"=c('Gainesville','Austin',NA),
               "department"=NA , stringsAsFactors=F )
  
df.review<-data.frame("authorID"=c(1,3),
               "AU"= c('Smith, Jon J.','Smith,J'),          
               "AF"= c('Smith, Jon J.','Smith,J'),          
               "groupID"= c(3,3),      
               "match_name"=c('Smith,J','Smith, Jon J'),
               "similarity"= c(0.8833333,0.8833333),   
               "author_order"= c(1,1),
               "address"=c("Univ Florida, Gainesville, FL USA",NA),      
               "RP_address"=c("Univ Florida, Gainesville, FL USA",NA),   
               "RI"=NA,          
               "OI"=NA,      
               "EM"=c("j.smith@ufl.edu",'jsmith@usgs.gov'),          
               "refID"=c(1,3),
               "UT"=NA,
               "PT"=NA,
               "PU"=NA,
               "PY"=NA,
               "university"=c('Univ Florida',NA),  
               "country"=c('USA',NA),
               "postal_code"=NA,
               "department"=NA , stringsAsFactors=F )

  actual <-authors_refine(df.review, df.prelim, sim_score=0.88)
  expect_equal(actual$author_name, c("Smith, Jon J.", "Thompson, Bob B.", 
                                     "Smith, Jon J."))
  expect_equal(nrow(df.prelim),nrow(actual))
  expect_equal(unique(actual[df.review$groupID[1]==actual$groupID,
                             'author_name']),'Smith, Jon J.')
  expect_equal(df.prelim[is.na(df.prelim$similarity) | 
                        df.prelim$similarity>0.88,'authorID'],actual$authorID)
  
  
  df.prelim<-data.frame("authorID"=c(1,2,3),
                        "AU"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
                        "AF"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
                        "groupID"= c(3,2,3),      
                        "match_name"=c('Smith,J',NA,'Smith, Jon J'),
                        "similarity"= c(0.8833333,NA,0.8833333),   
                        "author_order"= c(1,1,1),
                        "address"=c("Univ Florida, Gainesville, FL USA",
                                    "University of Texas, Austin, TX, USA",NA),      
                        "RP_address"=c("Univ Florida, Gainesville, FL USA",
                                       "University of Texas, Austin, TX, USA",NA),   
                        "RI"=NA,          
                        "OI"=NA,      
                        "EM"=c("j.smith@ufl.edu",NA,'jsmith@usgs.gov'),          
                        "refID"=c(1,2,3),
                        "TA"=NA,
                        "SO"=NA,          
                        "UT"=NA,
                        "PT"=NA,
                        "PU"=NA,
                        "PY"=NA,
                        "university"=c('Univ Florida','University of Texas',NA),  
                        "country"=c('USA','USA',NA),
                        "state"=c('FL','TX',NA),
                        "postal_code"=NA,
                        "city"=c('Gainesville','Austin',NA),
                        "department"=NA , stringsAsFactors=F )
  
  df.review2<-as.data.frame(matrix(ncol=ncol(df.review), nrow=0))

  colnames(df.review2) <- colnames(df.review)
  expect_warning(authors_refine(df.review2, df.prelim, sim_score=0.88),
    "Authors data.frame is empty.
      This likely means there are no authors that need to be handchecked.
      Outputting the prelim file.")

})

