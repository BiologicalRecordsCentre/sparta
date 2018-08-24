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
survey <- sample(rDates, size = n, TRUE)

# set up regions for some testing
regions <- data.frame(site = unique(site),
                      region1 = c(rep(1, 20), rep(0, 30)),
                      region2 = c(rep(0, 20), rep(1, 15), rep(0, 15)),
                      region3 = c(rep(0, 20), rep(0, 15), rep(1, 15)))

# format data
suppressWarnings({visitData <- formatOccData(taxa = taxa, site = site,
                                             survey = survey)})

## additional data for testing missing years
# remove one year 
time_period_missing <- sub("2018-", "2019-", survey)
time_period_missing <- as.Date(time_period_missing)

# format data
suppressWarnings({visitData_missing <- formatOccData(taxa = taxa, site = site,
                                                     survey = time_period_missing)})


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

 expect_error(results <- occDetFunc(taxa_name = 'a',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData_missing$occDetdata,
                                    spp_vis = visitData_missing$spp_vis,
                                    write_results = FALSE,
                                    seed = 111),
              'It looks like you have years with no data. This will crash BUGS')
 
 expect_error(results <- occDetFunc(taxa_name = 'a',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData$occDetdata,
                                    spp_vis = visitData$spp_vis,
                                    max_year = "2028",
                                    write_results = FALSE,
                                    seed = 111),
              'max_year should be a numeric value')
 
 expect_error(results <- occDetFunc(taxa_name = 'a',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData$occDetdata,
                                    spp_vis = visitData$spp_vis,
                                    max_year = 2028,
                                    write_results = FALSE,
                                    seed = 111),
              'max_year should be greater than the final year of available data')
 
})

test_that("Test occDetFunc with defaults", {
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
})

test_that("Test occDetFunc with model types", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'centering',
                                                  'halfcauchy'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))

  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'centering',
                                                  'inversegamma'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'intercept'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'intercept',
                                                  'halfcauchy'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'intercept',
                                                  'inversegamma'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'centering',
                                                  'halfcauchy'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'centering',
                                                  'inversegamma'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'intercept'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'intercept',
                                                  'halfcauchy'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'intercept',
                                                  'inversegamma'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk',
                                                  'halfcauchy'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  
  sink()
  
})

test_that("Test occDetFunc with julian date", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  
  suppressWarnings({visitData <- formatOccData(taxa = taxa, site = site,
                                               survey = survey,
                                               includeJDay = TRUE)})
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'intercept',
                                                  'inversegamma', 'jul_date'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  expect_true('beta1' %in% row.names(results$BUGSoutput$summary))
  expect_true('beta2' %in% row.names(results$BUGSoutput$summary))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'centering',
                                                  'halfcauchy', 'jul_date'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  expect_true('beta1' %in% row.names(results$BUGSoutput$summary))
  expect_true('beta2' %in% row.names(results$BUGSoutput$summary))
  
  sink()
  
})  

test_that("Test occDetFunc with catagorical list length", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  
  suppressWarnings({visitData <- formatOccData(taxa = taxa, site = site,
                                               survey = survey,
                                               includeJDay = TRUE)})
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('ranwalk', 'intercept',
                                                  'inversegamma', 'catlistlength'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  expect_true('dtype2.p' %in% row.names(results$BUGSoutput$summary))
  expect_true('dtype3.p' %in% row.names(results$BUGSoutput$summary))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111, modeltype = c('indran', 'centering',
                                                  'halfcauchy', 'catlistlength'))
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations"))
  expect_true('dtype2.p' %in% row.names(results$BUGSoutput$summary))
  expect_true('dtype3.p' %in% row.names(results$BUGSoutput$summary))
  
  sink()
  
})  

test_that("Test occDetFunc using regions and region aggregates", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  
  results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111,
                        modeltype = c("ranwalk", "halfcauchy"),
                        regional_codes = regions,
                        region_aggs = list(agg1 = c('region1', 'region2')))
  
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year",
                     "nsites", "nvisits", "species_sites", "species_observations",
                     "regions", "region_aggs"))
  expect_identical(results$regions,
                   c("region1", "region2", "region3"))
  expect_identical(names(results$region_aggs), "agg1")
  RNs <- row.names(results$BUGSoutput$summary)
  expect_true("a_region1[1]" %in% RNs)
  expect_true("a_region2[1]" %in% RNs)
  expect_true("a_region3[1]" %in% RNs)
  expect_true("psi.fs.r_region1[1]" %in% RNs)
  expect_true("psi.fs.r_region2[1]" %in% RNs)
  expect_true("psi.fs.r_region3[1]" %in% RNs)
  expect_true("psi.fs.r_agg1[1]" %in% RNs)
  
  regions$region4 <- 0
  
  # test with a region with no data
  expect_warning(results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111,
                        modeltype = c("ranwalk", "halfcauchy"),
                        regional_codes = regions,
                        region_aggs = list(agg1 = c('region1', 'region2'))),
                 'The following regions have no data and')

  sink()
  
}) 