context("Test simOccData")

test_that("Test simOccData", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  set.seed(125)
  
  results <- simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20,
                        beta3=100, JD_range = c(100,300))
  
  head_spp_vis<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), mysp = c(0L, 1L, 1L, 1L, 0L, 0L)),
                               .Names = c("visit", "mysp"), row.names = c(NA, 6L), class = "data.frame")
  
  head_occDetdata<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), site = c(6L, 9L, 10L, 7L, 2L, 2L), 
                                  L = c(1, 4, 4, 4, 1, 1), TP = c(3L, 5L, 4L, 3L, 2L, 4L),
                                  Jul_date = c(244L, 299L, 174L, 125L, 133L, 135L)),
                             .Names = c("visit", "site", "L", "TP", "Jul_date"), row.names = c(NA, 6L), class = "data.frame")
  
  head_Z <-matrix(c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L), ncol = 5, nrow = 6)
  
  head_p <-c(0.3087322, 0.9998911, 0.9999852, 0.9999002, 0.1850282, 0.3561440)
  
  expect_identical(names(results), c('spp_vis','occDetdata','Z','p'))
  expect_identical(head(results$spp_vis), head_spp_vis)
  expect_identical(head(results$occDetdata), head_occDetdata)
  expect_identical(head(results$Z), head_Z)
  expect_equal(head(results$p), head_p, tolerance = 1e-7)
  
  # Test positive trend with unrestricted Julian date
  set.seed(125)
  resultsB<-simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20,
                       beta3=100,trend = +0.2)
  
  head_spp_visB<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), mysp = c(0L, 1L, 1L, 1L, 0L, 0L)),
                          .Names = c("visit", "mysp"), row.names = c(NA, 6L), class = "data.frame")
  
  head_occDetdataB<-structure(list(visit = c(1L, 2L, 3L, 4L, 5L, 6L), site = c(6L, 9L, 10L, 7L, 2L, 2L), 
                                  L = c(1, 4, 4, 4, 1, 1), TP = c(3L, 5L, 4L, 3L, 2L, 4L),
                                  Jul_date = c(262L, 363L, 136L, 47L, 62L, 65L)),
                             .Names = c("visit", "site", "L", "TP", "Jul_date"), row.names = c(NA, 6L), class = "data.frame")
  
  head_ZB <-matrix(c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L), ncol = 5, nrow = 6)
  
  head_pB <-c(0.3053991, 0.9998911, 0.9999192, 0.9998967, 0.1705362, 0.3277827)
  
  expect_identical(head(resultsB$spp_vis), head_spp_visB)
  expect_identical(head(resultsB$occDetdata), head_occDetdataB)
  expect_identical(head(resultsB$Z), head_ZB)
  expect_equal(head(resultsB$p), head_pB, tolerance = 1e-7)
  
  expect_error(results <- simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20, beta3=100, JD_range = c(100,400)),
               'Invalid Julian date range')
  
  sink()
})