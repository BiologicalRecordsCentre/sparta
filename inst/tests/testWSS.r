# context("Test Well-Sampled Site model")
# 
# library(sparta)
# data(ex_dat)
# 
# test_that("Runs without error", {
#   sink('~/null')  
#   MM_out <- try(WSS(Data=ex_dat,
#                     year_range=c(1970,2000),
#                     min_list=1,
#                     min_years=2,
#                     site_col='kmsq',
#                     sp_col='CONCEPT',
#                     start_col='TO_STARTDATE',
#                     end_col='Date'),
#                  silent = TRUE)
#   sink()
#   expect_equal(class(MM_out), "data.frame")
#   expect_equal(dim(MM_out), c(52,15))
# 
#   unlink('~/null')
# })
