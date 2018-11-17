context("Test Plot Addresses")

test_that("Plotting Addresses works", {
  

  require(ggplot2)
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
  a<-ggplot(df, aes(x=lon,y=lat)) +geom_point()
  a
  ver <- gdtools::version_freetype()
  print(ver)
  vdiffr::expect_doppelganger('testplot',ggplot(df, aes(x=lon,y=lat)) +geom_point(),path='tests/figs/',verbose=T)
  vdiffr::expect_doppelganger('testplot',a)
  
 #  a<-plot_addresses_points(data=df)
 # vdiffr::expect_doppelganger("test_plot_addresses",a)
 #  
 #  vdiffr::expect_doppelganger("test_plot_country",
 #                              plot_addresses_country(data=df))
 #  
 #  c<-plot_net_address(data=df)
 #  vdiffr::expect_doppelganger("test_plot_net_address",c$plot)
  # At this moment we'cant create the test_plot_net_coauthors in a
  # way that always shows
  # the same plot. 
  # df1<-data.frame("authorID"=c(1,2,3,4,5),
  # "AU"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J','Dummy,P','Dummy,L'),      
  # "AF"= c('Smith, Jon J.','Thompson, Bob B.','Smith,J','Dummy, P','Dummy L'),     
  #                     "groupID"= c(3,2,3,5,6),      
  # 
  #                     "author_order"= c(1,2,1,2,3),
  #                             
  #                     "refID"=c(1,1,3,1,3),
  #                     "UT"=c('test1','test1','test2','test2','test2'),
  #                    
  #              "country"=c('USA','United Kingdom',"USA",'Brazil','France'),
  #                     stringsAsFactors=FALSE )
  # plot_net_coauthor(data=df1)
  # vdiffr::expect_doppelganger("test_plot_net_coauthor",
  # plot_net_coauthor(data=df1))
})
  

  