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
               Telfer_1_2 = c(-1.76219570634173, 0.623430105052677, 1.11725755188291, 
                              0.496194137084219, 2.69638351997486, 0.0423754484307402, 
                              -1.16447389123526, 0.647001259012368, -1.35381016780119, 
                              NA, 0.496194137084219, -0.279692730579802, -0.144857476115618, 
                              1.33733125414767, 1.43856106466267, 0.183654264216283, 0.0423754484307402, 
                              -1.50729629957349, 0.496194137084219, -0.411443240222739, 
                              -0.691938259741924, -0.547760853197409, -0.144857476115618, 
                              -0.443223119604865, -0.279692730579802, -0.728756554621733),
               Telfer_1_3 = c(-0.538033469234501, -0.000578495523797077, 
                                 -0.294947711906226, 0.72134757793512, -1.28225409112311, 
                                 -0.661220601603885, -0.417968688623987, 0.150781020924082, 
                                 0.372522119773067, NA, -1.02351986474447, -0.190454183562896, 
                                 0.275836380943916, -0.0678711312682607, 0.0300634881656174, 
                                 2.00129843236545, 1.08364684107571, -1.16136393969837, -1.40810947014601, 
                                 -0.661220601603885, 1.55554833095462, -0.665772497203108, 
                                 -0.781589187327301, -0.373089801440806, 0.839595511082665, 
                                 2.78232044488968), 
               Telfer_2_3 = c(-0.830708595219291, 0.703384578934107, 
                             -0.442846878502637, 0.943050326028467, -1.43179458039776, 
                             -0.501859746727325, -0.722526257055875, 0.250101723762915, 
                             0.280387970250718, -0.854162761324971, -0.806018506073029, 
                             -0.231393604216318, 0.0966108200715454, -0.0181832022522676, 
                             0.460104851435195, 1.97626214549243, 1.19486481959003, -1.07402095494588, 
                             -1.19153414783721, -0.56549612937196, 0.685433592299333, 
                             -0.983885561850573, -0.912503768421813, 0.170351029132528, 
                             0.75871776951505, 2.85970051517717)),
          .Names = c("taxa", "Telfer_1_2", "Telfer_1_3", "Telfer_2_3"),
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
               'minSite must be numeric or interger')
  expect_warning(temp <- telfer(taxa, site, time_period),
                 '260 out of 1000 observations have been removed as duplicates')
  
})
test_that("The function works", {
  
  TelferResult <- telfer(df$taxa, df$site, df$time_period)
  expect_true(all.equal(TelferResult, results))
  expect_is(TelferResult, class = 'data.frame')  
  TelferResult <- telfer(df$taxa, df$site, df$time_period, useIterations = FALSE)
  expect_is(TelferResult, class = 'data.frame')  
  
})
