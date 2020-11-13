context("Test telfer")

# Create data
set.seed(seed = 128)
SS <- 1000 # number of observations
taxa <- sample(letters, SS, replace = TRUE)
site <- sample(paste('A', 1:20, sep = ''), SS, replace = TRUE)
time_period <- sample(1:3, SS, replace = TRUE)

df <- unique(data.frame(taxa, site, time_period))

# This is what the results should look like
results <- structure(list(taxa = c("a", "b", "c", "d", "e", "f", "g", "h", 
                                   "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", 
                                   "v", "w", "x", "y", "z"),
                          Nsite_1.x = c(7, 5, 11, 6, 8, 12, 13,
                                        10, 11, 7, 9, 10, 10, 8, 6, 10, 10, 9,
                                        7, 9, 7, 12, 7, 6, 11, 6),
                          Nsite_2.x = c(8, 8, 10, 11, 14, 12, 8, 8, 8, 10, 10, 10, 
                                        9, 11, 7, 13, 9, 10, 12, 9, 14, 11, 5, 11, 13, 8),
                          Telfer_1_2 = c(-0.737325194162432, -0.771337920914978,
                                         -0.0978316993182738, 0.705665061130678,
                                         1.84449355838129, 0.793204324994675,
                                         -1.39542120066517, -0.889663497755277,
                                         -0.988718249517388, 0.145264509176636,
                                         0.0208475603716532, -0.0362700616533217,
                                         -0.459036841151605, 0.49957587189965, -1.23170170347477,
                                         1.26438350620118, -0.459036841151605, 0.0208475603716532,
                                         1.0278542125157, -0.395395287450087, 1.98143673331934, 
                                         0.306052052307134, -2.22583487465281, 0.705665061130678,
                                         1.25996497575673, -0.728918435468554),
                          Nsite_1.y = c(7, 5, 11, 6, 8, 12, 13, 10, 11, 7, 9, 10, 10,
                                        8, 6, 10, 10, 9, 7, 9, 7, 12, 7, 6, 11, 6),
                          Nsite_3.x = c(10, 8, 11, 11, 7, 11, 12, 9, 7, 11, 10, 9, 
                                        9, 7, 11, 5, 10, 12, 9, 8, 7, 10, 10, 11, 12, 8),
                          Telfer_1_3 = c(0.428256932967209, -0.949087297190003, 0.65790219617248, 1.22829202628365,
                                         -1.28886094429482, 0.613532330883972, 1.07474465397477, -0.262484704574745, 
                                         -1.29949214495499, 1.00528677409354, 0.277980477724887, -0.262484704574745,
                                         -0.262484704574745, -1.28886094429482, 1.22829202628365, 
                                         -2.41810658165437, 0.224882116972965, 1.29327001776924, -0.148772908159123,
                                         -0.737309062319468, -1.3469911660205, 0.133164595928159, 
                                         0.42825693296721, 1.22829202628365, 1.14699133920679, -0.781680952309443),
                          Nsite_2.y = c(8, 8, 10, 11, 14, 12, 8, 8, 8, 10, 10, 10, 
                                        9, 11, 7, 13, 9, 10, 12, 9, 14, 11, 5, 11, 13, 8),
                          Nsite_3.y = c(10, 8, 11, 11, 7, 11, 12, 9, 7, 11, 10, 9, 9, 7, 11,
                                        5, 10, 12, 9, 8, 7, 10, 10, 11, 12, 8),
                          Telfer_2_3 = c(0.241879867181403, -1.1282200653054, 0.905222087260817, 0.882140380723752,
                                         -0.835189914093116, 0.855157996286309, 1.61197979966821, -0.436860689687682, 
                                         -1.84628403251525, 0.905222087260817, 0.325816517346919,
                                         -0.253589052566972, -0.340138161126636, -1.27848889281263, 
                                         0.896127341439063, -2.00286651006585, 0.290092552577541, 
                                         1.49539968751045, -0.112055476633155, -0.982085821852136, 
                                         -0.835189914093116, 0.352123266172587, 0.00652263716418258,
                                         0.882140380723752, 1.27518370742708, -1.12822006530541)),
                     row.names = c(NA, -26L),
                     class = "data.frame")

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
