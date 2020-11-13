context("Test plot_DetectionPhenology")

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
  
test_that("Test plot_DetectionPhenology", {
  
  sink(file = ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))

  
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
  
  # quick check this output is as expected
  expect_equal(head(modelresults$BUGSoutput$sims.list$alpha.p[,1]),
               c(-3.0072164156934, -2.17484348943153, -2.04228261009605, -2.53916362332146, 
                 -3.18523796966506, -3.02335381838008),
               tolerance = 1e-7)
  
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
  results <- plot_DetectionPhenology(modelresults,
                                 spname ='a',
                                 bins = 12,
                                 density_function = TRUE)
  
  # create expected data
  head_data_bin <- c(1L, 2L, 3L, 4L, 5L, 6L)
  head_data_mean_pDet <- c(0.0795248229875861, 0.0795247569031214, 0.0795239877368355, 
                           0.0795197030738163, 0.0795101592056275, 0.0795562377193303)
  head_data_lower95CI <- c(0.0393018751339037, 0.0393018916184026, 0.0393036478896574, 
                           0.0393665776000181, 0.040113536889726, 0.0429881045873861)
  head_data_upper95CI <- c(0.144879763944464, 0.144879763944464, 0.144879763944464, 0.144879763944464, 
                           0.144879763944464, 0.144875423401869)
  head_data_JulianDay <- c(1, 34.0909090909091, 67.1818181818182, 100.272727272727, 133.363636363636, 
                           166.454545454545)
  
  
  expect_identical(names(results), c("data", "layers", "scales", "mapping", "theme", "coordinates", "facet", "plot_env", "labels"))
  expect_identical(names(results$data), c("bin", "mean_pDet", "lower95CI", "upper95CI", "JulianDay"))
  expect_equal(head(results$data$bin), head_data_bin, tolerance = 1e-7)
  expect_equal(head(results$data$mean_pDet), head_data_mean_pDet, tolerance = 1e-7)
  expect_equal(head(results$data$lower95CI), head_data_lower95CI, tolerance = 1e-7)
  expect_equal(head(results$data$upper95CI), head_data_upper95CI, tolerance = 1e-7)
  expect_equal(head(results$data$JulianDay), head_data_JulianDay, tolerance = 1e-5)
  
  expect_error(results <- plot_DetectionPhenology(modelresults2, spname='a', bins=12, density_function = TRUE),
               'no phenological effect was modelled!')
  
  sink()
})
