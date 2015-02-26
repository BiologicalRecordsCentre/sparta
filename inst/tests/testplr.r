# context("Test PLR")
# 
# library(sparta)
# data(ex_dat)
# 
# test_that("Runs without error", {
#   sink('~/null')  
#   plr_out <- try(plr(Data=ex_dat,
#                                time_periods=data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009)),
#                                min_sq=5,
#                                site_col='hectad',
#                                sp_col='CONCEPT',
#                                start_col='TO_STARTDATE',
#                                end_col='Date'),
#                 silent = TRUE)
#   sink()
#   expect_equal(class(plr_out), "data.frame")
#   expect_equal(dim(plr_out), c(62,4))
# 
#   unlink('~/null')
# })
