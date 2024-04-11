test_that("Test errors and warnings", {
  
  # skip the test if jagsUI is not installed
  if ((!requireNamespace("jagsUI", quietly = TRUE))) {
    skip("jagsUI software not available")
  }

  # Create a small dataset
  n <- 15000 # size of dataset
  nyr <- 20 # number of years in data
  nSamples <- 100 # set number of dates
  nSites <- 50 # set number of sites
  set.seed(seed = 648)

  # Create somes dates
  first <- as.Date(strptime("1980/01/01", "%Y/%m/%d"))
  last <- as.Date(strptime(paste(1980 + (nyr - 1), "/12/31", sep = ""), "%Y/%m/%d"))
  dt <- last - first
  rDates <- first + (runif(nSamples) * dt)

  # taxa are set as random letters
  taxa <- sample(letters, size = n, TRUE)

  # three sites are visited randomly
  site <- sample(paste("A", 1:nSites, sep = ""), size = n, TRUE)

  # the date of visit is selected at random from those created earlier
  survey <- sample(rDates, size = n, TRUE)

  # run with regions
  # Create region definitions
  regions <- data.frame(
    site = unique(site),
    region1 = c(rep(1, 20), rep(0, 30)),
    region2 = c(rep(0, 20), rep(1, 15), rep(0, 15)),
    region3 = c(rep(0, 20), rep(0, 15), rep(1, 15))
  )

  # run the model with these data for one species
  suppressWarnings(results <- occDetModel(
    taxa = taxa,
    site = site,
    survey = survey,
    species_list = c("a"),
    write_results = FALSE,
    n_iterations = 200,
    burnin = 10,
    thinning = 2,
    regional_codes = regions
  ))

  expect_error(
    occurrenceChange(firstYear = 1900, lastYear = 1990, bayesOut = results$a),
    "firstYear must be in the year range of the data"
  )
  expect_error(
    occurrenceChange(firstYear = 1990, lastYear = 2050, bayesOut = results$a),
    "lastYear must be in the year range of the data"
  )
  expect_error(
    occurrenceChange(firstYear = 1990, lastYear = 1990, bayesOut = results$a, change = results$a),
    "Change must be a character string identifying the change metric.  Either: difference, percentdif, growthrate or lineargrowth"
  )
  expect_error(
    occurrenceChange(firstYear = 1990, lastYear = 1990, bayesOut = results$a, change = as.character("Galina")),
    "The change metric must be one of the following: difference, percentdif, growthrate or lineargrowth"
  )
  expect_error(
    occurrenceChange(firstYear = 1990, lastYear = 1990, bayesOut = results$a, region = results$a),
    "region must be a character string identifying the regional estimates that change is to be calculated for."
  )
  expect_error(
    occurrenceChange(firstYear = 1990, lastYear = 1990, bayesOut = results$a, region = as.character("Galina")),
    "region must match that used in the model output file, check spelling."
  )
})

test_that("Test occurrenceChange functionality", {
  # skip the test if jagsUI is not installed
  if ((!requireNamespace("jagsUI", quietly = TRUE))) {
    skip("jagsUI software not available")
  }

  # Create a small dataset
  n <- 15000 # size of dataset
  nyr <- 20 # number of years in data
  nSamples <- 100 # set number of dates
  nSites <- 50 # set number of sites
  set.seed(seed = 648)

  # Create somes dates
  first <- as.Date(strptime("1980/01/01", "%Y/%m/%d"))
  last <- as.Date(strptime(paste(1980 + (nyr - 1), "/12/31", sep = ""), "%Y/%m/%d"))
  dt <- last - first
  rDates <- first + (runif(nSamples) * dt)

  # taxa are set as random letters
  taxa <- sample(letters, size = n, TRUE)

  # three sites are visited randomly
  site <- sample(paste("A", 1:nSites, sep = ""), size = n, TRUE)

  # the date of visit is selected at random from those created earlier
  survey <- sample(rDates, size = n, TRUE)

  # run with regions
  # Create region definitions
  regions <- data.frame(
    site = unique(site),
    region1 = c(rep(1, 20), rep(0, 30)),
    region2 = c(rep(0, 20), rep(1, 15), rep(0, 15)),
    region3 = c(rep(0, 20), rep(0, 15), rep(1, 15))
  )

  # run the model with these data for one species
  suppressWarnings(results <- occDetModel(
    taxa = taxa,
    site = site,
    survey = survey,
    species_list = c("a"),
    write_results = FALSE,
    n_iterations = 200,
    burnin = 10,
    thinning = 2,
    regional_codes = regions
  ))

  # estimate the growthrate (default change measure) for one species ('a')
  changeGrowthrate <- occurrenceChange(firstYear = 1990, lastYear = 1999, bayesOut = results$a, change = "growthrate")

  # estimate the lineargrowth for one species ('a')
  changeLineargrowth <- occurrenceChange(firstYear = 1990, lastYear = 1999, bayesOut = results$a, change = "lineargrowth")

  # estimate the difference for one species ('a')
  changeDifference <- occurrenceChange(firstYear = 1990, lastYear = 1999, bayesOut = results$a, change = "difference")

  # estimate the percentdif for one species ('a')
  changePercentdif <- occurrenceChange(firstYear = 1990, lastYear = 1999, bayesOut = results$a, change = "percentdif")

  # growthrate
  expect_equal(nrow(changeGrowthrate$data[, 1]), nrow(changeGrowthrate$data[, 2]), nrow(changeGrowthrate$data[, 3]))
  expect_true(all(changeGrowthrate$data[, 1:2] <= 1))
  expect_equal(length(changeGrowthrate), 4)
  expect_equal((((changeGrowthrate$data[, 2] / changeGrowthrate$data[, 1])^(1 / 10)) - 1) * 100, changeGrowthrate$data[, 3])
  # Lineargrowth
  expect_equal(nrow(changeLineargrowth$data[, 1]), nrow(changeLineargrowth$data[, 2]), nrow(changeLineargrowth$data[, 3]))
  expect_true(all(changeLineargrowth$data[, 1:2] <= 1))
  expect_equal(length(changeLineargrowth), 4)
  expect_equal(((changeLineargrowth$data[, 2] - changeLineargrowth$data[, 1]) / changeLineargrowth$data[, 1]), changeLineargrowth$data[, 3])
  # Difference
  expect_equal(nrow(changeDifference$data[, 1]), nrow(changeDifference$data[, 2]), nrow(changeDifference$data[, 3]))
  expect_true(all(changeDifference$data[, 1:2] <= 1))
  expect_equal(length(changeDifference), 4)
  expect_equal(changeDifference$data[, 2] - changeDifference$data[, 1], changeDifference$data[, 3])
  # percentdif
  expect_equal(nrow(changePercentdif$data[, 1]), nrow(changePercentdif$data[, 2]), nrow(changePercentdif$data[, 3]))
  expect_true(all(changePercentdif$data[, 1:2] <= 1))
  expect_equal(length(changePercentdif), 4)
  expect_equal((((changePercentdif$data[, 2] - changePercentdif$data[, 1]) / changePercentdif$data[, 1]) * 100), changePercentdif$data[, 3])
})
