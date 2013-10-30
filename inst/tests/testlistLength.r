context("Test List-Length model")

library(sparta)
data(ex_dat)

test_that("Runs without error", {
  sink('~/null')  
  LL_out <- try(listLength(Data = ex_dat,
                           year_range = c(1970,2010),
                           site_col = 'kmsq',
                           sp_col = 'CONCEPT',
                           start_col='TO_STARTDATE',
                           end_col='Date'),
                silent = TRUE)
  sink()
  expect_equal(class(LL_out), "data.frame")
  expect_equal(dim(LL_out), c(57,17))

  unlink('~/null')
})
