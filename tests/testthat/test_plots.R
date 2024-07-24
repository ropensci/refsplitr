context("test plot addresses")

test_that("Map plots work", {
  
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
  
  a<-plot_addresses_points(data=df)
 expect_equal( a$layers[[2]]$data$lat, c(35.00,51.752,39.00))
 expect_equal( a$layers[[2]]$data$lon, c(-100.00,1.2577,-80.000))
 
 expect_output(plot_addresses_country(data=df), regexp = "2 codes from your data successfully matched countries in the map
0 codes from your data failed to match with a country code in the map
     failedCodes failedCountries
241 codes from the map weren't represented in your data")
})

 test_that("Net plots work", {
   
  # skip_on_cran()
   require(ggplot2)
   df<-data.frame("authorID"=c(1,2,3),
                  "AU"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
                  "AF"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J'),          
                  "groupID"= c(3,2,3),      
                  "match_name"=c('Smith,J',NA,'Smith, Jon J'),
                  "similarity"= c(0.8833333,NA,0.8833333),   
                  "author_order"= c(1,2,1),
                  "address"=c("univ Florida, Gainesville, FL USA",
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
                  "country"=c('usa','united kingdom',"usa"),
                  "state"=c('FL','Oxfordshire',"CA"),
                  "postal_code"=NA,
                  "city"=c('Gainesville','Oxford',"Berkley"),
                  "department"=NA ,
                  "lat"=c(35,51.7520,39),
                  "lon"=c(-100,1.2577,-80),
                  stringsAsFactors=FALSE )
 c<-plot_net_address(data=df)
 c_lat_lon<-c$data_points$latlon
 expect_equal(factor(c_lat_lon), factor(c("35,-100","51.752,1.2577" ,"39,-80")))
 expect_equal(length(c$data_polygon$long),12334)

 d<-plot_net_coauthor(data=df)
 expect_equal(d[1],c("united kingdom"=0,"usa"=1))
 expect_equal(d[2],c("united kingdom"=1,"usa"=0))
 
 e<-plot_net_country(data=df)
 expect_equal(signif(e$data_points$LAT,4),c(53.88, 39.50))
 expect_equal(length(e$data_polygon$long),12334)
  
})
  

  