context("Test visitsSummary")

set.seed(123)

# Create data
n <- 15000 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites

# Create somes dates
first <- as.Date(strptime("2010/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# sites are visited randomly
site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
survey <- sample(rDates, size = n, TRUE)


test_that("Test formatOccData errors", {
  expect_warning(visitData <- formatOccData(taxa = taxa, site = site, survey = survey),
                 '828 out of 15000 observations will be removed as duplicates')
  names(visitData) <- c("spp_vis", "NotoccDetdata")
  expect_error(visitsSum <- visitsSummary(visitData),
                 'formatOccData object does not contain occDetdata dataframe component')
})


test_that("Test all columns present", {
  expect_warning(visitData <- formatOccData(taxa = taxa, site = site, survey = survey),
                 '828 out of 15000 observations will be removed as duplicates')
  visitsSum <- visitsSummary(visitData)
  expect_identical(colnames(visitsSum), c("TP", "PercRevisited", "meanRevisits"))
})


# Removing the below test for now as I set.seed generates different values on my machine/R version

#test_that("Test formatOccData outputs", {
#  expect_warning(visitData <- formatOccData(taxa = taxa, site = site, survey = survey),
#                 '828 out of 15000 observations will be removed as duplicates')
#  visitsSum <- visitsSummary(visitData)
#  head_visitSum <- data.frame(TP=as.numeric(2010:2015),
#                              PercRevisited=as.numeric(c("100", "92", "100", "100", "100", "100")),
#                              meanRevisits=as.numeric(c("4.70", "2.00", "8.64", "2.90", "6.68", "3.72")))
#  
#  expect_identical(head(visitsSum), head_visitSum)
#})


