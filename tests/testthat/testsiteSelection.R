context("Test siteSelection, siteSelectionMinL and siteSelectionMinTP")

# Create data
n <- 150 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 20 # set number of dates
set.seed(seed = 125)

# Create somes dates
first <- as.POSIXct(strptime("2003/01/01", "%Y/%m/%d")) 
last <- as.POSIXct(strptime(paste(2003+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(c('one', 'two', 'three'), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

# combine this to a dataframe
df <- unique(data.frame(taxa, site, time_period) )

######################
test_that("Test errors and warnings - siteSelectionMinL", {
  
  expect_error(dfSEL  <- siteSelectionMinL('tom', df$site, df$time_period, minL = 4),
               'The following arguements are not of equal length')
  expect_error(dfSEL  <- siteSelectionMinL(df$taxa, df$site, df$time_period, minL = 'four'),
               'minL must be numeric')
  expect_warning(dfSEL  <- siteSelectionMinL(df$taxa, df$site, df$time_period, minL = 100),
                 'Filtering in siteSelectionMinL resulted in no data returned')
})
test_that("Sub-selection works - siteSelectionMinL", {
  
  dfSEL  <- siteSelectionMinL(df$taxa, df$site, df$time_period, minL = 4)
  
  expect_lt(nrow(dfSEL), nrow(df))
  
  dfSEL$temp <- paste(dfSEL$site, dfSEL$time_period)
  
  # Check list length and min years are no smaller than those specified
  expect_true(min(table(dfSEL$temp)) >= 4)
})

################
test_that("Test errors and warnings - siteSelectionMinTP", {
  
  expect_error(dfSEL  <- siteSelectionMinTP('tom', df$site, df$time_period, minTP = 4),
               'The following arguements are not of equal length')
  expect_error(dfSEL  <- siteSelectionMinTP(df$taxa, df$site, df$time_period, minTP = 'four'),
               'minTP must be numeric')
  expect_warning(dfSEL  <- siteSelectionMinTP(df$taxa, df$site, df$time_period, minTP = 100),
                 'Filtering in siteSelectionMinTP resulted in no data returned')
})
test_that("Sub-selection works - siteSelectionMinTP", {
  
  dfSEL  <- siteSelectionMinTP(df$taxa, df$site, df$time_period, minTP = 14)
  
  expect_equal(nrow(dfSEL), nrow(df))
  
  expect_true(min(table(unique(cbind(dfSEL$site, as.numeric(format(dfSEL$time_period, '%Y'))))[,1])) >= 14)
  
})

###############
test_that("Test errors and warnings - siteSelection", {
  
  expect_error(dfSEL  <- siteSelection(c('a', df$taxa, 'z'),
                                       c(NA, df$site, 'one'),
                                       c(df$time_period[1], df$time_period, df$time_period[1]),
                                       minL = 4, minTP = 3),
               'site must not contain NAs')
  expect_error(dfSEL  <- siteSelection(c('a', df$taxa, NA),
                                       c('one', df$site, 'one'),
                                       c(df$time_period[1], df$time_period, df$time_period[1]),
                                       minL = 4, minTP = 3),
               'taxa must not contain NAs')
  expect_error(dfSEL  <- siteSelection(c('a', df$taxa, 'a'),
                                       c('one', df$site, 'one'),
                                       c(NA, df$time_period, df$time_period[1]),
                                       minL = 4, minTP = 3),
               'time_period must not contain NAs')
  expect_error(dfSEL  <- siteSelection('tom', df$site, df$time_period, minL = 4, minTP = 3),
               'The following arguements are not of equal length')
  expect_error(dfSEL  <- siteSelection(df$taxa, df$site, df$time_period, minL = 'four', minTP = 3),
               'minL must be numeric')
  expect_error(dfSEL  <- siteSelection(df$taxa, df$site, df$time_period, minL = 4, minTP = 'three'),
               'minTP must be numeric')
  expect_warning(dfSEL  <- siteSelection(df$taxa, df$site, df$time_period, minL = 100, minTP = 100),
                 'Filtering in siteSelection resulted in no data returned')
})
test_that("Sub-selection works - siteSelection", {
  
  dfSEL  <- siteSelection(df$taxa, df$site, df$time_period, minL = 4, minTP = 5)
  
  expect_lt(nrow(dfSEL), nrow(df))
  
  temp <- paste(dfSEL$site, dfSEL$time_period)
  
  # Check list length and min years are no smaller than those specified
  expect_true(min(table(temp)) >= 4)
  expect_true(min(table(unique(cbind(dfSEL$site, as.numeric(format(dfSEL$time_period, '%Y'))))[,1])) >= 3)
  
  # Test that doing the selection the other way around has a different effect
  dfSEL2  <- siteSelection(df$taxa, df$site, df$time_period, minL = 4, minTP = 5, LFirst = FALSE)
  
  # Check list length and min years are no smaller than those specified
  temp <- paste(dfSEL2$site, dfSEL2$time_period)
  expect_true(min(table(temp)) >= 4)
  expect_true(max(table(unique(cbind(dfSEL2$site, as.numeric(format(dfSEL2$time_period, '%Y'))))[,1])) >= 5)
  
  expect_true(!identical(x = dfSEL, y = dfSEL2))

})
