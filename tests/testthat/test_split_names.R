context("Split names")
test_that("Split names correctly splits names", {
  x<-'Boone, Matthew, E'
  actual<-split_names(x)
  expect_equivalent(actual[2],'E')
  
  x<-'Boone, Matthew E'
  actual<-split_names(x)
  expect_equivalent(actual[2],'E')
  
  
  x<-'Boone-Gonzalez, Matthew, E'
  actual<-split_names(x)
  expect_equivalent(actual[3],'Boone-Gonzalez')

  x<-'Boone, Matthew, E, jr'
  actual<-split_names(x)
  expect_equivalent(actual[2],'Ejr')  

  x<-'Boone, Matthew, Earl, Ramos, jr,'
  actual<-split_names(x)
  expect_equivalent(actual[2],'EarlRamosjr')  
})  
