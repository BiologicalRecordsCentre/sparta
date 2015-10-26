context("Test occDetFunc")

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

# format data
visitData <- formatOccData(taxa = taxa, site = site, time_period = time_period)

test_that("Test occDetFunc errors", {
  
 expect_error(results <- occDetFunc(taxa_name = 'a',
                       n_iterations = 50,
                       burnin = 500, 
                       occDetdata = visitData$occDetdata,
                       spp_vis = visitData$spp_vis,
                       write_results = FALSE,
                       seed = 111),
              'must not be larger that the number of iteration')
  
 expect_error(results <- occDetFunc(taxa_name = 'apple',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData$occDetdata,
                                    spp_vis = visitData$spp_vis,
                                    write_results = FALSE,
                                    seed = 111),
              'taxa_name is not the name of a taxa in spp_vis')    
})

test_that("Test occDetFunc", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111)
  sink()

  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year"))
  
})