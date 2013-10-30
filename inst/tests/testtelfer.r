context("Test Telfer")

library(sparta)
data(ex_dat)

test_that("Runs without error", {
  sink('~/null')  
  telfer_out <- try(telfer(Data=ex_dat,
                             time_periods=data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009)),
                             min_sq=2,
                             useIterations=T,
                             iterations=20,
                             site_col='hectad',
                             sp_col='CONCEPT',
                             start_col='TO_STARTDATE',
                             end_col='Date'),
                      silent = TRUE)
  sink()
  expect_equal(class(telfer_out), "data.frame")
  expect_equal(dim(telfer_out), c(62,4))

  unlink('~/null')
})
