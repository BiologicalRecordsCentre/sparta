test_that("Test plot_DetectionPhenology", {

  # skip the test if jagsUI is not installed
  if ((!requireNamespace("jagsUI", quietly = TRUE))) {
    skip("jagsUI software not available")
  }
  
  sink(file = ifelse(Sys.info()["sysname"] == "Windows",
                   "NUL",
                   "/dev/null"))

  # format data
  suppressWarnings({visitData <- formatOccData(taxa = taxa,
                                               site = site,
                                               survey = survey,
                                               includeJDay = TRUE)})
  set.seed(125)
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
               c(-2.4679040815341, -2.43913399694479, -2.29288533656873, -2.57561472634767, 
                 -2.55650969777029, -2.61713328855139),
               tolerance = 1e-3)
  
  set.seed(125)
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
  head_data_mean_pDet <- c(0.074623695755272, 0.0746058357077367, 0.0745315069872329, 
                           0.0743754132847238, 0.0742907200187843, 0.0742643919310321)
  head_data_lower95CI <- c(0.0503723337285162, 0.0503723337285162, 0.0503723337285162, 
                           0.0503723337285157, 0.0503723102183517, 0.0498456552997817)
  head_data_upper95CI <- c(0.108862569584464, 0.108862569584464, 0.108862569584464, 0.108862569584464, 
                           0.108862569584464, 0.108862569584464)
  head_data_JulianDay <- c(1, 34.0909090909091, 67.1818181818182, 100.272727272727, 133.363636363636, 
                           166.454545454545)
  
  expect_identical(names(results), c("data", "layers", "scales", "guides", "mapping", "theme", "coordinates", "facet", "plot_env", "layout", "labels"))
  expect_identical(names(results$data), c("bin", "mean_pDet", "lower95CI", "upper95CI", "JulianDay"))
  expect_equal(head(results$data$bin), head_data_bin, tolerance = 1e-3)
  expect_equal(head(results$data$mean_pDet), head_data_mean_pDet, tolerance = 1e-3)
  expect_equal(head(results$data$lower95CI), head_data_lower95CI, tolerance = 1e-3)
  expect_equal(head(results$data$upper95CI), head_data_upper95CI, tolerance = 1e-3)
  expect_equal(head(results$data$JulianDay), head_data_JulianDay, tolerance = 1e-3)
  
  expect_error(results <- plot_DetectionPhenology(modelresults2, spname='a', bins=12, density_function = TRUE),
               'no phenological effect was modelled!')
  
  sink()
})
