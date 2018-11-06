context("authors match")

test_that("Authors match correctly", {
  AF=c('Smith, Jon J.','Thompson, Bob B.','Smith,J'), AU=c('Smith, Jon J.','Thompson, Bob','Smith, J'), BP=NA , C1=c("Univ Florida, Gainesville, FL USA","University of Texas, Austin, TX, USA","Louisiana State Univeristy, Baton Rouge, LA, USA"),CR=NA,DE=NA, DI=NA, EM=c("j.smith@ufl.edu",NA,'jsmith@lsu.edu'), EN=NA, FN=NA, FU=NA, PD=NA, PG=NA, PT=NA, PU=NA, PY=NA, RI=NA, OI=NA,PM=NA, RP=c("Univ Florida, Gainesville, FL USA","University of Texas, Austin, TX, USA",NA)
  
df<-data.frame(
  authorID=1:4,
  AF=c('Smith, Jon J.','Thompson, Bob B.','Smith,J','Smith, Jon James'), 
  AU=c('Smith, Jon J.','Thompson, Bob','Smith, J','Smith, Jon James'),
  EM=c("j.smith@ufl.edu",NA,'jsmith@lsu.edu',NA),
  RI=c('B-5571-2011',NA,NA,NA),
  OI=c(NA,"1234-5678-9012-3456",NA,NA),
  university=c('Univ Florida','University of Texas',"Louisiana State Univeristy",'University of Florida'),
  country=c('USA','USA','USA','USA'),
  state=c('FL','TX','LA','FL'),
  postal_code=c(NA,NA,NA,NA),
  city=c('Gainesville','Austin','Baton Rouge','Gainesville'),
  department=c(NA,NA,NA,NA),
  address=c("Univ Florida, Gainesville, FL USA","University of Texas, Austin, TX, USA","Louisiana State Univeristy, Baton Rouge, LA, USA","University Florida, Gainesville, FL USA"),
stringsAsFactors=F
)
df[]<-lapply(df, as.character)
df$authorID<-as.numeric(df$authorID)
actual<-.authors_match(df,sim_score=0.88)


expect_equal(actual$groupID, c(4,2,4,4))
expect_equal(which(!is.na(actual$similarity)),3)
expect_gte(min(actual$similarity[!is.na(actual$similarity)]),0.88)
})
