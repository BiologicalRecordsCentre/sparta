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
regionsDF <- data.frame(site = unique(site),
                      region1 = c(rep(1, 20), rep(0, 30)),
                      region2 = c(rep(0, 20), rep(1, 15), rep(0, 15)),
                      region3 = c(rep(0, 20), rep(0, 15), rep(1, 15)))

# format data
suppressWarnings({visitData <- formatOccData(taxa = taxa, site = site,
                                             survey = survey)})

## additional data for testing missing years
# remove one years 
time_period_missing <- sub("2018-", "2019-", survey)
time_period_missing <- as.Date(time_period_missing)

# remove a second year
time_period_missing2 <- sub("2017-", "2019-", time_period_missing)
time_period_missing2 <- as.Date(time_period_missing2)

# format data
suppressWarnings({visitData_missing <- formatOccData(taxa = taxa, site = site,
                                                     survey = time_period_missing)})
suppressWarnings({visitData_missing2 <- formatOccData(taxa = taxa, site = site,
                                                     survey = time_period_missing2)})

# additional data for testing species lost after nyr filter
taxa_filterError <- c(taxa,"aa")
site_filterError <- c(site,"B1")
survey_filterError <- c(survey,sample(rDates, size = 1, TRUE)) 

suppressWarnings({visitData_filterError <- formatOccData(taxa = taxa_filterError, site = site_filterError,
                                                      survey = survey_filterError)})

test_that("Test occDetFunc errors", {
  
 expect_error(results <- occDetFunc(taxa_name = 'a',
                       n_iterations = 50,
                       burnin = 500, 
                       occDetdata = visitData$occDetdata,
                       spp_vis = visitData$spp_vis,
                       write_results = FALSE,
                       seed = 111),
              'must not be larger that the number of iteration')
  
  expect_error(results <- occDetFunc(taxa_name = 'a',
                                     n_iterations = 50,
                                     burnin = 15, 
                                     occDetdata = visitData$occDetdata,
                                     spp_vis = visitData$spp_vis,
                                     write_results = FALSE,
                                     criterion = "OnlyGoodData",
                                     seed = 111),
               "Criterion must be either an integer, `EqualWt` or `HighSpec`")
  
  
  expect_error(results <- occDetFunc(taxa_name = 'apple',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData$occDetdata,
                                    spp_vis = visitData$spp_vis,
                                    write_results = FALSE,
                                    criterion = "EqualWt",
                                    seed = 111),
              'taxa_name is not the name of a taxa in spp_vis')

 expect_error(results <- occDetFunc(taxa_name = 'a',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData_missing$occDetdata,
                                    spp_vis = visitData_missing$spp_vis,
                                    write_results = FALSE,
                                    criterion = "HighSpec",
                                    seed = 111),
              'There are no visits in year 2018. This means there is no data, for any species in this year. BUGS cannot run if there is a year with no data. Quitting...')

  expect_error(results <- occDetFunc(taxa_name = 'a',
                                    n_iterations = 50,
                                    burnin = 15, 
                                    occDetdata = visitData_missing2$occDetdata,
                                    spp_vis = visitData_missing2$spp_vis,
                                    write_results = FALSE,
                                    seed = 111),
              'There are 2 years with no visits, including 2017. This means there is no data, for any species in these years. BUGS cannot run if any year has no data. Quitting...')
 
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits","species_observations","sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations","sparta_version"))

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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  expect_true('beta1' %in% row.names(results$BUGSoutput$summary))
  expect_true('beta2' %in% row.names(results$BUGSoutput$summary))
  expect_true('beta3' %in% row.names(results$BUGSoutput$summary))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
  expect_true('beta1' %in% row.names(results$BUGSoutput$summary))
  expect_true('beta2' %in% row.names(results$BUGSoutput$summary))
  expect_true('beta3' %in% row.names(results$BUGSoutput$summary))
  
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
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
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations", "sparta_version"))
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
                        regional_codes = regionsDF,
                        region_aggs = list(agg1 = c('region1', 'region2')))
  
  expect_identical(results$SPP_NAME, 'a')
  expect_identical(results$n.iter, 50)
  expect_identical(names(results),
                   c("model", "BUGSoutput", "parameters.to.save", "model.file", 
                     "n.iter", "DIC", "SPP_NAME", "min_year", "max_year", "sites_included",
                     "nsites", "nvisits", "species_observations","sparta_version",
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
  
  # test with a region with no data
  regionsempty<-regionsDF
  regionsempty$region4 <- 0
  
  expect_warning(results <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111,
                        modeltype = c("ranwalk", "halfcauchy"),
                        regional_codes = regionsempty,
                        region_aggs = list(agg1 = c('region1', 'region2'))),
                 'The following regions have no data and should not be modelled: region4 - These regions will not be included in the model')
  
  # test with regional_codes not as a dataframe
  expect_error(results <- occDetFunc(taxa_name = 'a',
                                       n_iterations = 50,
                                       burnin = 15, 
                                       occDetdata = visitData$occDetdata,
                                       spp_vis = visitData$spp_vis,
                                       write_results = FALSE,
                                       seed = 111,
                                       modeltype = c("ranwalk", "halfcauchy"),
                                       regional_codes = as.matrix(regionsDF),
                                       region_aggs = list(agg1 = c('region1', 'region2'))),
                 'regional_codes should be a data.frame')
  
  # test with NAs in regional_codes
  regionsNA <- regionsDF
  regionsNA[1,3] <- NA
  
  expect_warning(results <- occDetFunc(taxa_name = 'a',
                                     n_iterations = 50,
                                     burnin = 15, 
                                     occDetdata = visitData$occDetdata,
                                     spp_vis = visitData$spp_vis,
                                     write_results = FALSE,
                                     seed = 111,
                                     modeltype = c("ranwalk", "halfcauchy"),
                                     regional_codes = regionsNA,
                                     region_aggs = list(agg1 = c('region1', 'region2'))),
               "NAs are present in regional_codes, these will be replaced with 0s")
  
  # test with sites in multiple regions
  regionsmulti <- regionsDF
  regionsmulti[1,3] <- 1
  
  expect_warning(results <- occDetFunc(taxa_name = 'a',
                                       n_iterations = 50,
                                       burnin = 15, 
                                       occDetdata = visitData$occDetdata,
                                       spp_vis = visitData$spp_vis,
                                       write_results = FALSE,
                                       seed = 111,
                                       modeltype = c("ranwalk", "halfcauchy"),
                                       regional_codes = regionsmulti,
                                       region_aggs = list(agg1 = c('region1', 'region2'))),
                  '1 sites are assigned to more than one region in regional_codes and will be removed')
  
  # test with sites in no regions
  regionsmulti <- regionsDF
  regionsmulti[1,2] <- 0
  
  expect_warning(results <- occDetFunc(taxa_name = 'a',
                                     n_iterations = 50,
                                     burnin = 15, 
                                     occDetdata = visitData$occDetdata,
                                     spp_vis = visitData$spp_vis,
                                     write_results = FALSE,
                                     seed = 111,
                                     modeltype = c("ranwalk", "halfcauchy"),
                                     regional_codes = regionsmulti,
                                     region_aggs = list(agg1 = c('region1', 'region2'))),
               'sites are not assigned to a region in regional_codes and will be removed')
  
  # test for sites in occurence data but not regions
  regionsmissing <- regionsDF[2:50,]
  
  expect_warning(results <- occDetFunc(taxa_name = 'a',
                                     n_iterations = 50,
                                     burnin = 15, 
                                     occDetdata = visitData$occDetdata,
                                     spp_vis = visitData$spp_vis,
                                     write_results = FALSE,
                                     seed = 111,
                                     modeltype = c("ranwalk", "halfcauchy"),
                                     regional_codes = regionsmissing,
                                     region_aggs = list(agg1 = c('region1', 'region2'))),
               '1 sites are in occurrence data but not in regional data and will be removed')
  
  # test for regional aggregate containing unknown region
  expect_error(results <- occDetFunc(taxa_name = 'a',
                                       n_iterations = 50,
                                       burnin = 15, 
                                       occDetdata = visitData$occDetdata,
                                       spp_vis = visitData$spp_vis,
                                       write_results = FALSE,
                                       seed = 111,
                                       modeltype = c("ranwalk", "halfcauchy"),
                                       regional_codes = regionsDF,
                                       region_aggs = list(agg1 = c('region1', 'region2','region4'))),
               'Aggregate members [region4] not in regional_codes column names [region1, region2, region3]',fixed=TRUE)
  
  # test for regional aggregate without regional_codes
  expect_error(results <- occDetFunc(taxa_name = 'a',
                                     n_iterations = 50,
                                     burnin = 15, 
                                     occDetdata = visitData$occDetdata,
                                     spp_vis = visitData$spp_vis,
                                     write_results = FALSE,
                                     seed = 111,
                                     modeltype = c("ranwalk", "halfcauchy"),
                                     regional_codes = NULL,
                                     region_aggs = list(agg1 = c('region1', 'region2'))),
               'Cannot use regional aggregates if regional_codes is not supplied')
  sink()
  
}) 

test_that("Test occDetFunc with empty species post nyr filter", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  
  expect_warning(results <- occDetFunc(taxa_name = 'aa',
                                     n_iterations = 50,
                                     burnin = 15, 
                                     occDetdata = visitData_filterError$occDetdata,
                                     spp_vis = visitData_filterError$spp_vis,
                                     write_results = FALSE,
                                     seed = 111,
                                     modeltype = c("ranwalk", "halfcauchy")),
                 'aa has insufficient data after site filtering. Either decrease nyr or change the criterion')
  sink()
  
})
