context("parse out addresses, step 2 of authors clean")
test_that("addresses parse correctly, atleast logically", {
df<-data.frame(
address=c("Univ Sydney, Fac Vet Sci, Sch Life & Environm Sci,
          Sydney, NSW, Australia.",        
  "Univ Toronto, Dept Ecol & Evolutionary Biol, Toronto, ON M5S 3B2, Canada.",
  "Univ New South Wales, Evolut & Ecol Res Ctr, Sydney, NSW, Australia.",
  "Univ New South Wales, Evolut & Ecol Res Ctr, Sydney, NSW, Australia.",
  "Ryerson Univ, Dept Chem & Biol, Toronto, ON M5B 2K3, Canada.",
  "Univ Sao Paulo, Inst Biociencias, Dept Zool, BECO, Rua Matao,Trav 14 321,
  BR-05508090 Sao Paulo, Brazil.",
  "Univ Sydney, Charles Perkins Ctr, Sydney, NSW, Australia.",
  "niv Florida, IFAS, Ft Lauderdale, FL 33312 USA."),
  authorID=1:8,stringsAsFactors=FALSE)

actual<-authors_address(df$address, df$authorID)
expect_false(any(is.na(actual$country)))
expect_false(grepl('BR',actual$postal_code[actual$country=='brazil']))
expect_equal(sum(grepl("[a-z]{1}[0-9]{1}[a-z]{1}[0-9]{1}[a-z]{1}[0-9]{1}",
                       actual$postal_code)),2)
expect_equal(unique(actual$country[grepl(
  "[a-z]{1}[0-9]{1}[a-z]{1}[0-9]{1}[a-z]{1}[0-9]{1}", 
  actual$postal_code)]),'canada')
expect_equal(c(actual$country[8], actual$state[8], actual$postal_code[8]),
             c('usa','fl','33312'))
})
