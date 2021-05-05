context("Test reportingRateModel and associated functions")

# Create data
n <- 3000 #size of dataset
nyr <- 10 # number of years in data
nSamples <- 30 # set number of dates
nSites <- 15 # set number of sites
set.seed(1985)

# Create somes dates
first <- as.POSIXct(strptime("2010/01/01", "%Y/%m/%d")) 
last <- as.POSIXct(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

# combine this to a dataframe (adding a final row of 'bad' data)
df <- data.frame(taxa = c(taxa,'bad'),
                 site = c(site,'A1'),
                 time_period = c(time_period, as.POSIXct(strptime("1200/01/01", "%Y/%m/%d"))))

######################
test_that("Test errors and warnings", {
  
  # outside errorChecks this is the only catch present in this set of functions
  #expect_error(RR_out <- reportingRateModel(df$taxa, df$site, 1:length(df$time_period)),
  #             "non-dates not yet supported for time_period")
  # A couple in errorChecks for this function
  # warning when family is overridden by listlength
  expect_warning(RR_out <- reportingRateModel(df$taxa,
                               df$site,
                               df$time_period,
                               list_length = TRUE,
                               family = 'Binomial'),
                 "When list_length is TRUE family will default to Bernoulli")
  expect_error(suppressWarnings(RR_out <- reportingRateModel(df$taxa,
                                              df$site,
                                              df$time_period,
                                              list_length = 1)),
                 "list_length must be logical")
  expect_error(suppressWarnings(RR_out <- reportingRateModel(df$taxa,
                                              df$site,
                                              df$time_period,
                                              overdispersion = 1)),
                 "overdispersion must be logical")
  expect_error(suppressWarnings(RR_out <- reportingRateModel(df$taxa,
                                              df$site,
                                              df$time_period,
                                              verbose = 1)),
                 "verbose must be logical")
  expect_error(suppressWarnings(RR_out <- reportingRateModel(df$taxa,
                                              df$site,
                                              df$time_period,
                                              site_effect = 1)),
                 "site_effect must be logical")
  expect_warning(RR_out <- reportingRateModel(df$taxa,
                                            df$site,
                                            df$time_period,
                                            species_to_include = c('tom','a','b','c')),
               "The following species in species_to_include are not in your data: tom")
})

test_that("Test formulaBuilder", {
  
    
  family <- 'binomial'
  list_length <- FALSE
  site_effect <- FALSE
  overdispersion <- FALSE
  
  # Here we test thay the formula builder is prodicing the right responses under a range of conditions
  model_formula <- formulaBuilder(family,list_length,site_effect,overdispersion)
  expect_identical(model_formula, "cbind(successes, failures) ~ year")
  model_formula <- formulaBuilder('Bernoulli',list_length,site_effect,overdispersion)
  expect_identical(model_formula, "taxa ~ year")
  model_formula <- formulaBuilder(family,TRUE,site_effect,overdispersion)
  expect_identical(model_formula, "taxa ~ year + log(listLength)")
  model_formula <- formulaBuilder(family,TRUE,TRUE,overdispersion)
  expect_identical(model_formula, "taxa ~ year + log(listLength) + (1|site)")
  model_formula <- formulaBuilder(family,TRUE,TRUE,TRUE)
  expect_identical(model_formula, "taxa ~ year + log(listLength) + (1|site) + (1|obs)")
  model_formula <- formulaBuilder(family,list_length,TRUE,TRUE)
  expect_identical(model_formula, "cbind(successes, failures) ~ year + (1|site) + (1|obs)")
  
})

test_that("Check outputs are in the correct form", {
  
  expect_warning(RR_out <- reportingRateModel(df$taxa, df$site, df$time_period, species_to_include = c('a','b','c')),
                 "353 out of 3001 observations will be removed as duplicates")
  atts <- attributes(RR_out)  
  expect_equal(atts$intercept_year, 2014)
  expect_equal(atts$min_year, -814)
  expect_equal(atts$max_year, 5)
  expect_equal(atts$nVisits, 450)
  expect_equal(atts$model_formula, "cbind(successes, failures) ~ year")
  expect_is(RR_out, "data.frame")
  expect_identical(c('a','b','c'), sort(as.character(RR_out$species_name)))

})