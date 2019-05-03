context("Test telfer")

# Create data
set.seed(seed = 128)
SS <- 1000 # number of observations
taxa <- sample(letters, SS, replace = TRUE)
site <- sample(paste('A', 1:20, sep = ''), SS, replace = TRUE)
time_period <- sample(1:3, SS, replace = TRUE)

df <- unique(data.frame(taxa, site, time_period))

# This is what the results should look like
results <- structure(list(taxa = structure(1:26, .Label = c("a", "b", "c", 
                                                            "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", 
                                                            "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"), class = "factor"), 
                          Nsite_1.x = c(7, 5, 11, 6, 8, 12, 13, 10, 11, 7, 9, 10, 10, 
                                        8, 6, 10, 10, 9, 7, 9, 7, 12, 7, 6, 11, 6), 
                          Nsite_2.x = c(8, 8, 10, 11, 14, 12, 8, 8,  8, 10, 10, 10,  
                                        9, 11, 7, 13, 9, 10, 12, 9, 14, 11, 5, 11, 13, 8), 
                          Telfer_1_2 = c(-0.73732519, -0.77133792, -0.09783170,  0.70566506,  1.84449356,  
                                         0.79320432, -1.39542120, -0.88966350, -0.98871825,  0.14526451,  0.02084756, -0.03627006, 
                                         -0.45903684,  0.49957587, -1.23170170,  1.26438351, -0.45903684,  0.02084756,  1.02785421, 
                                         -0.39539529,  1.98143673,  0.30605205, -2.22583487,  0.70566506, 1.25996498, 
                                         -0.72891844), Nsite_1.y = c(7, 5, 11, 6, 8, 12, 13, 10, 11, 7, 9, 10, 10, 8, 
                                                                     6, 10, 10, 9, 7, 9, 7, 12, 7, 6, 11, 6), 
                          Nsite_3.x = c(10, 8, 11, 11, 7, 11, 12, 9, 7, 11, 10, 9, 9, 7, 11, 5, 10, 12, 9, 8, 7, 10, 10, 11, 12, 8), 
                          Telfer_1_3 = c(0.4282569, -0.9490873, 0.6579022,  1.2282920, -1.2888609,  0.6135323,  
                                         1.0747447, -0.2624847, -1.2994921,  1.0052868,  0.2779805, -0.2624847, 
                                         -0.2624847, -1.2888609,  1.2282920, -2.4181066,  
                                         0.2248821,  1.2932700, -0.1487729, -0.7373091, -1.3469912,  0.1331646,  
                                         0.4282569,  1.2282920,  1.1469913, -0.7816810), 
                          Nsite_2.y = c(8, 8, 10, 11, 14, 12, 8, 8, 8, 10, 10, 10, 9, 11, 7, 13, 9, 10, 12, 9, 14, 11, 5, 11, 13, 8), 
                          Nsite_3.y = c(10, 8, 11, 11, 7, 11, 12, 9, 7, 11, 10, 9, 9, 7, 11, 5, 10, 12, 9, 8, 7, 10, 10, 11, 12, 8), 
                          Telfer_2_3 = c(0.241879867, -1.128220065,  0.905222087,  0.882140381, -0.835189914,  0.855157996, 
                                         1.611979800, -0.436860690, -1.846284033, 0.905222087, 0.325816517,
                                         -0.253589053, -0.340138161, -1.278488893,  0.896127341, -2.002866510,  
                                         0.290092553,  1.495399688, -0.112055477, -0.982085822, -0.835189914,  0.352123266,
                                         0.006522637,  0.882140381,  1.275183707, -1.128220065)), 
                     .Names = c("taxa", 
                                                                                                                                                                                                                                        "Nsite_1.x", "Nsite_2.x", "Telfer_1_2", "Nsite_1.y", "Nsite_3.x", 
                                                                                                                                                                                                                                        "Telfer_1_3", "Nsite_2.y", "Nsite_3.y", "Telfer_2_3"), row.names = c(NA, 
                                                                                                                                                                                                                                                                                                             -26L), class = "data.frame")

######################
test_that("Test errors and warnings", {
  
  expect_error(temp <- telfer(taxa = 'tom', site = df$site, time_period = df$time_period),
               'The following arguements are not of equal length: taxa, site, time_period')
  expect_error(temp <- telfer(taxa = df$taxa, site = head(df$site,-1), time_period = df$time_period),
               'The following arguements are not of equal length: taxa, site, time_period')
  expect_error(temp <- telfer(taxa = df$taxa, site = df$site, time_period = tail(df$time_period, -1)),
               'The following arguements are not of equal length: taxa, site, time_period')
  expect_error(temp <- telfer(taxa = df$taxa, site = df$site, time_period = df$time_period, minSite = 100),
               'No taxa satisfy the minSite criteria when comparing time period 1 and 2')
  expect_error(temp <- telfer(taxa = df$taxa, site = df$site, time_period = df$time_period, minSite = TRUE),
               'minSite must be numeric or integer')
  expect_warning(temp <- telfer(taxa, site, time_period),
                 '269 out of 1000 observations will be removed as duplicates')
  
})
test_that("The function works", {
  
  TelferResult <- telfer(df$taxa, df$site, df$time_period)
  expect_true(all.equal(TelferResult, results, tolerance = .0000001))
  expect_is(TelferResult, class = 'data.frame')  
  TelferResult <- telfer(df$taxa, df$site, df$time_period, useIterations = FALSE)
  expect_is(TelferResult, class = 'data.frame')  
  
})
