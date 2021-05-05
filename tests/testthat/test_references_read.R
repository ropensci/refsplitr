context("Reading References")

test_that("Read references, reads correctly", {
d<-references_read(data='../extdata/PubExample.txt' ,dir=F) 
expect_equal(nrow(d),10)
expect_equal(ncol(d),33)
expect_false(any(is.na(d$AF)))
expect_false(any(is.na(d$TI)))
expect_equal(d$refID,1:10)
expect_true(any(vapply(d,class,character(1))!='factor'))

expect_error(references_read(data='../extdata/BadHeader.txt' ,dir=F),
"ERROR:  The file ../extdata/BadHeader.txt doesn't appear to be a valid ISI or\n          Thomson Reuters reference library file!")
d <- references_read(data='../extdata/ISItop.txt' ,dir=F,
                     include_all=TRUE)
expect_equal(nrow(d),2)
expect_equal(ncol(d),60)
expect_false(any(is.na(d$AF)))
expect_false(any(is.na(d$TI)))
expect_equal(d$refID,1:2)
expect_true(any(vapply(d,class,character(1))!='factor'))

})
