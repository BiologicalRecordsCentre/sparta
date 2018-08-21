context("Test formatOccData")

# Create data
n <- 15000 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites
set.seed(125)

# Create somes dates
first <- as.Date(strptime("2010/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
survey <- sample(rDates, size = n, TRUE)

# set the closure period to be in 2 year bins
closure_period <- ceiling((as.numeric(format(rDates,'%Y')) - 2009)/2)

test_that("Test formatOccData", {

  expect_warning(visitData <- formatOccData(taxa = taxa, site = site, survey = survey),
                 '854 out of 15000 observations will be removed as duplicates')
  
  head_spp_vis <- structure(list(visit = c("A102010-02-17", "A102010-04-14", "A102010-04-22", 
                            "A102010-08-29", "A102010-11-04", "A102011-02-09"), a = c(TRUE, 
                            FALSE, FALSE, FALSE, FALSE, FALSE), b = c(FALSE, FALSE, FALSE, 
                            FALSE, FALSE, FALSE), c = c(FALSE, FALSE, FALSE, FALSE, TRUE, 
                            FALSE), d = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), e = c(FALSE, 
                            FALSE, FALSE, FALSE, TRUE, FALSE), f = c(TRUE, FALSE, FALSE, 
                            FALSE, FALSE, FALSE), g = c(FALSE, FALSE, FALSE, FALSE, FALSE, 
                            FALSE), h = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), i = c(FALSE, 
                            TRUE, FALSE, FALSE, TRUE, FALSE), j = c(FALSE, FALSE, FALSE, 
                            TRUE, FALSE, FALSE), k = c(FALSE, FALSE, FALSE, FALSE, FALSE, 
                            FALSE), l = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), m = c(FALSE, 
                            FALSE, FALSE, FALSE, FALSE, FALSE), n = c(FALSE, FALSE, FALSE, 
                            FALSE, FALSE, FALSE), o = c(FALSE, FALSE, FALSE, FALSE, FALSE, 
                            FALSE), p = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE), q = c(FALSE, 
                            FALSE, FALSE, FALSE, FALSE, TRUE), r = c(FALSE, FALSE, FALSE, 
                            TRUE, FALSE, FALSE), s = c(FALSE, TRUE, FALSE, FALSE, FALSE, 
                            FALSE), t = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), u = c(FALSE, 
                            FALSE, FALSE, FALSE, FALSE, FALSE), v = c(FALSE, FALSE, FALSE, 
                            TRUE, FALSE, FALSE), w = c(FALSE, FALSE, FALSE, FALSE, TRUE, 
                            FALSE), x = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), y = c(FALSE, 
                            FALSE, FALSE, FALSE, FALSE, FALSE), z = c(TRUE, FALSE, FALSE, 
                            FALSE, FALSE, FALSE)), .Names = c("visit", "a", "b", "c", "d", 
                            "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", 
                            "r", "s", "t", "u", "v", "w", "x", "y", "z"), row.names = c(NA, 
                            6L), class = "data.frame")
  

 head_occDetdata <- structure(list(visit = c("A102010-02-17", "A102010-04-14", "A102010-04-22", 
                                             "A102010-08-29", "A102010-11-04", "A102011-02-09"), 
                                   site = structure(c(2L, 2L, 2L, 2L, 2L, 2L), 
                                  .Label = c("A1", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A2", "A20", "A21",
                                           "A22", "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A3",
                                           "A30", "A31", "A32", "A33", "A34", "A35", "A36", "A37", "A38", 
                                            "A39", "A4", "A40", "A41", "A42", "A43", "A44", "A45", "A46", 
                                            "A47", "A48", "A49", "A5", "A50", "A6", "A7", "A8", "A9"), class = "factor"), 
                                   L = c(5L, 2L, 1L, 5L, 5L, 1L), 
                                  year = c(2010, 2010, 2010, 2010, 2010, 2011)), 
                              .Names = c("visit", "site", "L", "TP"), 
                              row.names = c(1L, 6L, 8L, 9L, 14L, 19L), class = "data.frame")
 
 
  expect_identical(head(visitData$spp_vis), head_spp_vis)
  expect_identical(head(visitData$occDetdata), head_occDetdata)
    
})

test_that("Test formatOccData errors", {
  
  expect_error(visitData <- formatOccData(taxa = head(taxa), site = site, survey = survey),
               'The following arguements are not of equal length: taxa, site, survey')
  expect_error(visitData <- formatOccData(taxa = taxa, site = head(site), survey = survey, closure_period = closure_period),
               'The following arguements are not of equal length: taxa, site, survey, closure_period')
  expect_error(visitData <- formatOccData(taxa = taxa, site = site, survey = head(survey), closure_period = closure_period),
               'The following arguements are not of equal length: taxa, site, survey, closure_period')
  expect_error(visitData <- formatOccData(taxa = taxa, site = site, survey = survey, closure_period=head(closure_period)),
               'The following arguements are not of equal length: taxa, site, survey, closure_period')
  
})



