context("Test detection_phenology")

test_that("Test detection_phenology", {
  
  sink(file=ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))
  # Create data
  n <- 15000 #size of dataset
  nyr <- 20 # number of years in data
  nSamples <- 100 # set number of dates
  nSites <- 50 # set number of sites
  set.seed(125)
  
  # Create somes dates
  first <- as.Date(strptime("2010/01/01", "%Y/%m/%d")) 
  last <- as.Date(strptime(paste(2010+(nyr-1),"/12/31", sep=''),
                           "%Y/%m/%d")) 
  dt <- last-first 
  rDates <- first + (runif(nSamples)*dt)
  
  # taxa are set as random letters
  taxa <- sample(letters, size = n, TRUE)
  
  # three sites are visited randomly
  site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
  
  # the date of visit is selected at random from those created earlier
  survey <- sample(rDates, size = n, TRUE)
  
  # format data
  suppressWarnings({visitData <- formatOccData(taxa = taxa,
                                               site = site,
                                               survey = survey,
                                               includeJDay = TRUE)})
  # create some model results
  modelresults <- occDetFunc(taxa_name = 'a',
                        n_iterations = 50,
                        burnin = 15, 
                        occDetdata = visitData$occDetdata,
                        spp_vis = visitData$spp_vis,
                        write_results = FALSE,
                        seed = 111,
                        modeltype = c('ranwalk','halfcauchy','jul_date'))
  
  # create some model results without Julian date
  modelresults2 <- occDetFunc(taxa_name = 'a',
                             n_iterations = 50,
                             burnin = 15, 
                             occDetdata = visitData$occDetdata,
                             spp_vis = visitData$spp_vis,
                             write_results = FALSE,
                             seed = 111,
                             modeltype = c('ranwalk','halfcauchy'))
  
  # run the function
  results <- detection_phenology(modelresults,
                                 spname ='a',
                                 bins = 12,
                                 density_function = TRUE)
  
  # create expected data
  head_data_bin <- c(1L, 2L, 3L, 4L, 5L, 6L)
  head_data_mean_pDet <- c(0.07952482, 0.07952476, 0.07952399, 0.07951970, 0.07951016, 0.07955624)
  head_data_lower95CI <- c(0.03930188, 0.03930189, 0.03930365, 0.03936658, 0.04011354, 0.04298810)
  head_data_upper95CI <- c(0.1448798, 0.1448798, 0.1448798, 0.1448798, 0.1448798, 0.1448754)
  head_data_JulianDay <- c(1.00000, 34.09091, 67.18182, 100.27273, 133.36364, 166.45455)
  
  
  expect_identical(names(results), c("data", "layers", "scales", "mapping", "theme", "coordinates", "facet", "plot_env", "labels"))
  expect_identical(names(results$data), c("bin", "mean_pDet", "lower95CI", "upper95CI", "JulianDay"))
  expect_equal(head(results$data$bin), head_data_bin, tolerance = 1e-7)
  expect_equal(head(results$data$mean_pDet), head_data_mean_pDet, tolerance = 1e-7)
  expect_equal(head(results$data$lower95CI), head_data_lower95CI, tolerance = 1e-7)
  expect_equal(head(results$data$upper95CI), head_data_upper95CI, tolerance = 1e-7)
  expect_equal(head(results$data$JulianDay), head_data_JulianDay, tolerance = 1e-5)
  
  expect_error(results <- detection_phenology(modelresults2, spname='a', bins=12, density_function = TRUE),
               'no phenological effect was modelled!')
  
  sink()
})