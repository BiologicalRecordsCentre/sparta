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
  
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111)
  sink()
  unlink(temp, recursive = TRUE)
  
  testComp <- structure(c(0.941666666666667, 0.857777777777778, 0.903333333333333, 
                          0.75, 0.51, 0.986111111111111, 0.925555555555556, 0.871111111111111, 
                          0.965, 0.796111111111111, 0.985555555555556, 0.963888888888889, 
                          0.790555555555556, 0.975, 0.689444444444444, 0.969444444444444, 
                          0.904444444444444, 0.680555555555556, 0.752222222222222, 0.713888888888889
                          )
                        , .Dim = 20L)
  
  expect_equal(results[['BUGSoutput']][['mean']][['psi.fs']], testComp)
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  
})