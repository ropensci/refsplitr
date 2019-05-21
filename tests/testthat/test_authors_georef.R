context("test geo ref works")

test_that("Geo ref is gathered", {
  df<-data.frame("authorID"=c(1,2,3),
                 "AU"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
                 "AF"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
                 "groupID"= c(3,2,3),      
                 "match_name"=c('Smith,J',NA,'Smith, Jon J'),
                 "similarity"= c(0.8833333,NA,0.8833333),   
                 "author_order"= c(1,2,1),
                 "address"=c("Univ Florida, Gainesville, FL USA",
                             "University of Texas, Austin, TX, USA",NA),      
                 "RP_address"=c("Univ Florida, Gainesville, FL USA",
                                "University of Oxford, Oxfordshire, UK",
                                "University of California Berkley, Berkley, CA, USA"),   
                 "RI"=NA,          
                 "OI"=NA,      
                 "EM"=c("j.smith@ufl.edu",NA,'jsmith@usgs.gov'),          
                 "refID"=c(1,1,3),
                 "TA"=NA,
                 "SO"=NA,          
                 "UT"=c('test1','test1','test2'),
                 "PT"=NA,
                 "PU"=NA,
                 "PY"=NA,
                 "university"=c('Univ Florida','University of Oxford',
                                "University of California"),  
                 "country"=c('USA','United Kingdom',"USA"),
                 "state"=c('FL','Oxfordshire',"CA"),
                 "postal_code"=NA,
                 "city"=c('Gainesville','Oxford',"Berkley"),
                 "department"=NA ,
                 "lat"=c(35,51.7520,39),
                 "lon"=c(-100,1.2577,-80),
                 stringsAsFactors=FALSE )

  expect_message(result<-authors_georef(data=df), regexp = "Working... *")
  expect_equal(length(result),3)
  expect_false(any(is.na(result$addresses$lat)))
  expect_false(any(is.na(result$addresses$lon)))
})
