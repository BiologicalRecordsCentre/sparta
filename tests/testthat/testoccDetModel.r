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
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
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

  expect_identical(names(results), c('a','m','g'))
  expect_identical(names(results[[1]]), c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                         "n.iter", "DIC", "SPP_NAME", "min_year", "max_year"))
  
})