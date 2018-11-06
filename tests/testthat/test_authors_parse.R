context("parse out authors, step 1 of authors clean")

test_that("Output of clean_authors makes sense", {
  df<-data.frame(filename=NA, AB=NA ,
  AF= "Senior, Alistair M.\nGrueber, Catherine E.\nKamiya, Tsukushi\nLagisz, Malgorzata\nO'Dwyer, Katie\nSantos, Eduardo S. A.\nNakagawa, Shinichi\n",
  AU= "Senior, Alistair M.\nGrueber, Catherine E.\nKamiya, Tsukushi\nLagisz, Malgorzata\nO'Dwyer, Katie\nSantos, Eduardo S. A.\nNakagawa, Shinichi\n",
  BP=NA , 
  C1="[Senior, Alistair M.] Univ Sydney, Charles Perkins Ctr, Sydney, NSW, Australia./[Senior, Alistair M.] Univ Sydney, Sch Math & Stat, Sydney, NSW, Australia./[Grueber, Catherine E.] Univ Sydney, Fac Vet Sci, Sch Life & Environm Sci, Sydney, NSW, Australia./[Grueber, Catherine E.] San Diego Zoo Global, San Diego, CA 92112 USA./[Kamiya, Tsukushi] Univ Toronto, Dept Ecol & Evolutionary Biol, Toronto, ON M5S 3B2, Canada./[Lagisz, Malgorzata; Nakagawa, Shinichi] Univ New South Wales, Evolut & Ecol Res Ctr, Sydney, NSW, Australia./[Lagisz, Malgorzata; Nakagawa, Shinichi] Univ New South Wales, Sch Biol Earth & Environm Sci, Sydney, NSW, Australia./[O'Dwyer, Katie] Ryerson Univ, Dept Chem & Biol, Toronto, ON M5B 2K3, Canada./[Santos, Eduardo S. A.] Univ Sao Paulo, Inst Biociencias, Dept Zool, BECO, Rua Matao,Trav 14 321, BR-05508090 Sao Paulo, Brazil./", 
  CR=NA,DE=NA, DI=NA,
  EM="alistair.senior@sydney.edu.au\n",
  EN=NA, FN=NA, FU=NA, PD=NA, PG=NA, PT=NA, PU=NA, PY=NA, 
  OI= "Nakagawa,Shinichi/0000-0002-7765-5182;Santos,Eduardo/0000-0002-0434-3655",
  PM=NA,
  RI= "Nakagawa,Shinichi/B-5571-2011",
  RP= "Senior, AM (reprint author), Univ Sydney, Charles Perkins Ctr, Sydney, NSW, Australia.; Senior, AM (reprint author), Univ Sydney, Sch Math & Stat, Sydney, NSW, Australia.\n",
  SC=NA, SN=NA, SO=NA, TC=NA, TI=NA, UT=NA, VL=NA, WC=NA,Z9=NA,refID=1 ,stringsAsFactors=F)
  df[]<-lapply(df, as.character)
  actual <- .authors_parse(df)
  expect_equal(nrow(actual),7)
  expect_equal(length(na.omit(unique(actual$OI))),2) 
  expect_equal(length(na.omit(unique(actual$RI))),1)
  expect_equal(actual$AF[!is.na(actual$RI)],"Nakagawa, Shinichi")  
  expect_false(any(is.na(actual$address)))
})  
