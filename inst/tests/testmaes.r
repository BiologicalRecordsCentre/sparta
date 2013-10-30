context("Test Maes method")

library(sparta)
data(ex_dat)

test_that("Runs without error", {
  sink('~/null')
  LL_out <- try(Ma_out<-maes(Data=ex_dat,
                             time_periods=(data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009))),
                             min_sp=1,
                             site_col='hectad',
                             sp_col='CONCEPT',
                             start_col='TO_STARTDATE',
                             end_col='Date'),
                silent = TRUE)
  sink()
  expect_equal(class(Ma_out), "data.frame")
    expect_equal(dim(Ma_out), c(62,19))
  unlink('~/null')
})
