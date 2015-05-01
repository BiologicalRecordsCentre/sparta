context("Test occDetModel")

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
time_period <- sample(rDates, size = n, TRUE)

df <- data.frame(taxa, site, time_period)

test_that("Test occDetModel", {
  
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  results <- occDetModel(taxa = df$taxa,
                         write_results = FALSE,
                         site = df$site,
                         time_period = df$time_period,
                         species_list = c('a','m','g'),
                         n_iterations = 100,
                         burnin = 10,
                         thinning = 2,
                         seed = 111)
  sink()
  unlink(temp, recursive = TRUE)
  
  testComp <- structure(c(0.980148148148148, 0.888296296296296, 0.9, 0.938074074074074, 
              0.597925925925926, 0.967851851851852, 0.956888888888889, 0.878518518518518, 
              0.986814814814815, 0.884888888888889, 0.954962962962963, 0.981185185185185, 
              0.905333333333333, 0.977481481481482, 0.743555555555556, 0.969333333333333, 
              0.92562962962963, 0.75837037037037, 0.787703703703704, 0.799555555555556
              ), .Dim = 20L)
    
  expect_identical(names(results), c('a','m','g'))
  expect_equal(results[['a']][['BUGSoutput']][['mean']][['psi.fs']], testComp)
  
})