context("authors match")

test_that("Authors match correctly", {


  df <- data.frame(
    authorID = 1:6,
    AF = c("Smith, Jon J.", "Thompson, Bob B.", "Smith,J", "Smith, Jon James", "Aide, TM", "Aide, T.Mitchel"),
    AU = c("Smith, Jon J.", "Thompson, Bob", "Smith, J", "Smith, Jon James",'Aide, TM',"Aide, TM"),
    EM = c("j.smith@ufl.edu", NA, "jsmith@lsu.edu", NA,NA,NA),
    RI = c("B-5571-2011", NA, NA, NA,NA,NA),
    OI = c(NA, "1234-5678-9012-3456", NA, NA,NA,NA),
    university = c("Univ Florida", "University of Texas",
                   "Louisiana State Univeristy", "University of Florida", "Univ of Puerto Rico", "Univ of Maryland"),
    country = c("USA", "USA", "USA", "USA","USA",'USA'),
    state = c("FL", "TX", "LA", "FL","PR",'MD'),
    postal_code = c(NA, NA, NA, NA,NA,NA),
    city = c("Gainesville", "Austin", "Baton Rouge", "Gainesville",NA,NA),
    department = c(NA, NA, NA, NA,NA,NA),
    address = c("Univ Florida, Gainesville, FL USA", 
                "University of Texas, Austin, TX, USA", 
                "Louisiana State Univeristy, Baton Rouge, LA, USA", 
                "University Florida, Gainesville, FL USA",
                "Univ of Peurto Rico, PR, USA",
                "Univ of Maryland, MD, USA"),
    stringsAsFactors = FALSE
  )
  df[] <- lapply(df, as.character)
  df$authorID <- as.numeric(df$authorID)
  actual <- authors_match(df)


  expect_equal(actual$groupID, c(4, 2, 4, 4,6,6))
  expect_equal(which(!is.na(actual$similarity)), c(3,5))
  expect_gte(min(actual$similarity[!is.na(actual$similarity)]), 0.4)
  
  # testing if loop at line 34
  
  df <- data.frame(
    authorID = 1:4,
    AF = c("Smith, Jon J.", "Thompson, Bob B.", "Smith,J", "Smith, Jon James"),
    AU = c("Smith, Jon J.", "Thompson, Bob", "Smith, J", "Smith, Jon James"),
    EM = c("j.smith@ufl.edu", NA, "jsmith@lsu.edu", NA),
    RI = c("B-5571-2011", NA, NA, "B-5571-2011"),
    OI = c("1234-5678-9012-9999", "1234-5678-9012-3456", NA,
           "1234-5678-9012-9999"),
    university = c("Univ Florida", "University of Texas",
                   "Louisiana State Univeristy", "University of Florida"),
    country = c("USA", "USA", "USA", "USA"),
    state = c("FL", "TX", "LA", "FL"),
    postal_code = c(NA, NA, NA, NA),
    city = c("Gainesville", "Austin", "Baton Rouge", "Gainesville"),
    department = c(NA, NA, NA, NA),
    address = c("Univ Florida, Gainesville, FL USA", 
                "University of Texas, Austin, TX, USA", 
                "Louisiana State Univeristy, Baton Rouge, LA, USA", 
                "University Florida, Gainesville, FL USA"),
    stringsAsFactors = FALSE
  )
  df[] <- lapply(df, as.character)
  df$authorID <- as.numeric(df$authorID)
  actual <- authors_match(df)
  
  
  expect_equal(actual$groupID, c(1, 2, 1, 1))
  expect_equal(which(!is.na(actual$similarity)), 3)
  expect_gte(min(actual$similarity[!is.na(actual$similarity)]), 0.4)
  
# for loop on line 44
  
  df <- data.frame(
    authorID = 1:4,
    AF = c("Smith, Jon J.", "Thompson, Bob B.", "Thompson, B", 
           "Smith, Jon James"),
    AU = c("Smith, Jon J.", "Thompson, Bob", "Thompson, B", "Smith, Jon James"),
    EM = c("j.smith@ufl.edu", NA, NA, NA),
    RI = c("B-5571-2011", NA, NA, "B-5571-2011"),
    OI = c(NA,"2001-2001-2001-2001","2001-2001-2001-2001",NA),
    university = c("Univ Florida", "University of Texas",
                   "Louisiana State Univeristy", "University of Florida"),
    country = c("USA", "USA", "USA", "USA"),
    state = c("FL", "TX", "LA", "FL"),
    postal_code = c(NA, NA, NA, NA),
    city = c("Gainesville", "Austin", "Baton Rouge", "Gainesville"),
    department = c(NA, NA, NA, NA),
    address = c("Univ Florida, Gainesville, FL USA", 
                "University of Texas, Austin, TX, USA", 
                "Louisiana State Univeristy, Baton Rouge, LA, USA", 
                "University Florida, Gainesville, FL USA"),
    stringsAsFactors = FALSE
  )
  df[] <- lapply(df, as.character)
  df$authorID <- as.numeric(df$authorID)
  actual <- authors_match(df)
  
  
  expect_equal(actual$groupID, c(1,2,2,1))
  expect_equal(which(is.na(actual$similarity)), c(1,2,3,4))

# for loop line 62
  
  df <- data.frame(
    authorID = 1:4,
    AF = c("Smith, Jon J.", "Thompson, Bob B.", "Thompson, B", 
           "Smith, Jon James"),
    AU = c("Smith, Jon J.", "Thompson, Bob", "Thompson, B", "Smith, Jon James"),
    EM = c("j.smith@ufl.edu", NA, NA, "j.smith@ufl.edu"),
    RI = c(NA, NA, NA, NA),
    OI = c(NA,"2001-2001-2001-2001","2001-2001-2001-2001",NA),
    university = c("Univ Florida", "University of Texas",
                   "Louisiana State Univeristy", "University of Florida"),
    country = c("USA", "USA", "USA", "USA"),
    state = c("FL", "TX", "LA", "FL"),
    postal_code = c(NA, NA, NA, NA),
    city = c("Gainesville", "Austin", "Baton Rouge", "Gainesville"),
    department = c(NA, NA, NA, NA),
    address = c("Univ Florida, Gainesville, FL USA", 
                "University of Texas, Austin, TX, USA", 
                "Louisiana State Univeristy, Baton Rouge, LA, USA", 
                "University Florida, Gainesville, FL USA"),
    stringsAsFactors = FALSE
  )
  df[] <- lapply(df, as.character)
  df$authorID <- as.numeric(df$authorID)
  actual <- authors_match(df)
  
  
  expect_equal(actual$groupID, c(1,2,2,1))
  expect_equal(which(is.na(actual$similarity)), c(1,2,3,4))

  # if statement line 90
  
  df <- data.frame(
    authorID = 1:7,
    AF = c("Smith, Jon J.", "Thompson, Bob B.", "Thompson, B. B.", 
           "Smith, Jon James", "Thompson, Bob B.", "Thompson, B. B.", 
           "Smith, Jon James"),
    AU = c("Smith, Jon J.", "Thompson, Bob", "Thompson, B", "Smith, Jon James", 
           "Thompson, B.", "Thompson, Bob", "Smith, J. J."),
    EM = c("j.smith@ufl.edu", NA, NA, "j.smith@ufl.edu", NA, NA, 
           "j.smith@ufl.edu"),
    RI = NA,
    OI = NA,
    university = c("Univ Florida", "University of Texas",
                   "Louisiana State Univeristy", "University of Florida"
                   , "University of Texas",
                   "Louisiana State Univeristy", "University of Florida"),
    country = "USA",
    state = c("FL", "TX", "LA", "FL", "TX", "LA", "FL"),
    postal_code = NA,
    city = c("Gainesville", "Austin", "Baton Rouge", "Gainesville", 
             "Austin", "Baton Rouge", "Gainesville"),
    department = NA,
    address = c("Univ Florida, Gainesville, FL USA", 
                "University of Texas, Austin, TX, USA", 
                "Louisiana State Univeristy, Baton Rouge, LA, USA", 
                "University Florida, Gainesville, FL USA", 
                "University of Texas, Austin, TX, USA", 
                "Louisiana State Univeristy, Baton Rouge, LA, USA", 
                "University Florida, Gainesville, FL USA"),
    stringsAsFactors = FALSE
  )
  df[] <- lapply(df, as.character)
  df$authorID <- as.numeric(df$authorID)
  actual <- authors_match(df)
  
  
  expect_equal(actual$groupID, c(1,2,2,1,2,2,1))
  expect_equal(which(is.na(actual$similarity)), c(1,2,3,4,5,7))
  
  
})
