context("Reading References")

test_that("Read references, reads correctly", {
d<-references_read(data='../extdata/PubExample.txt' ,dir=F)
expect_equal(nrow(d),10)
expect_equal(ncol(d),32)
expect_false(any(is.na(d$AF)))
expect_false(any(is.na(d$TI)))
expect_equal(d$refID,1:10)
expect_true(any(sapply(d,class)!='factor'))

})