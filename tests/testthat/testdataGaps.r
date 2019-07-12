context("Test dataGaps")

testyears<-c(2,3,4,5,6,9)
tminmod <- 2000
tmaxmod <- 2010
tmindat <- 2001
tmaxdat <- 2009

Eresults<-list(gap_start = 1, gap_end = 1, gap_middle = 2)

test_that("Test data_gaps", {
  
  result <- dataGaps(years = testyears, minmod = tminmod, maxmod = tmaxmod, mindat = tmindat, maxdat = tmaxdat)
  
  expect_identical(result, Eresults)
  
})
