context("Test simOccData")

test_that("Test simOccData", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  set.seed(125)
  
  results <- simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20,
                        beta3=100, JD_range = c(100,300))
  
  head_spp_vis<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), mysp = c(1L, 1L, 1L, 1L, 0L, 1L)),
                               .Names = c("visit", "mysp"), row.names = c(NA, 6L), class = "data.frame")
  
  head_occDetdata<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), site = c(1L, 10L, 7L, 4L, 1L, 7L), 
                                  L = c(4, 1, 4, 2, 1, 2), TP = c(2L, 2L, 5L, 2L, 5L, 1L),
                                  Jul_date = c(171L, 283L, 299L, 133L, 280L, 298L)),
                             .Names = c("visit", "site", "L", "TP", "Jul_date"), row.names = c(NA, 6L), class = "data.frame")
  
  head_Z <-matrix(c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L), ncol = 5, nrow = 6)
  
  head_p <-c(0.9999603, 0.1705370, 0.9998911, 0.8201487, 0.2942406, 0.8923152)
  
  expect_identical(names(results), c('spp_vis','occDetdata','Z','p'))
  expect_equal(head(results$spp_vis), head_spp_vis)
  expect_equal(head(results$occDetdata), head_occDetdata)
  expect_equal(head(results$Z), head_Z)
  expect_equal(head(results$p), head_p, tolerance = 1e-7)
  
  # Test positive trend with unrestricted Julian date
  set.seed(125)
  resultsB<-simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20,
                       beta3=100,trend = +0.2)
  
  head_spp_visB<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), mysp = c(1L, 1L, 1L, 1L, 0L, 1L)),
                          .Names = c("visit", "mysp"), row.names = c(NA, 6L), class = "data.frame")
  
  head_occDetdataB<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), site = c(1L, 10L, 7L, 4L, 1L, 7L), 
                                  L = c(4, 1, 4, 2, 1, 2), TP = c(2L, 2L, 5L, 2L, 5L, 1L),
                                  Jul_date = c(72L, 184L, 200L, 290L, 199L, 91L)),
                             .Names = c("visit", "site", "L", "TP", "Jul_date"), row.names = c(NA, 6L), class = "data.frame")
  
  head_ZB <-matrix(c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L), ncol = 5, nrow = 6)
  
  head_pB <-c(0.9997792, 0.5993916, 0.9999712, 0.8050512, 0.6259862, 0.8923213)
  
  expect_equal(head(resultsB$spp_vis), head_spp_visB)
  expect_equal(head(resultsB$occDetdata), head_occDetdataB)
  expect_equal(head(resultsB$Z), head_ZB)
  expect_equal(head(resultsB$p), head_pB, tolerance = 1e-7)
  
  expect_error(results <- simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20, beta3=100, JD_range = c(100,400)),
               'Invalid Julian date range')
  
  sink()
})