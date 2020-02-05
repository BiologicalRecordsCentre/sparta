context("Test errorChecks")

set.seed(seed = 128)

# Create data
n <- 3000 #size of dataset
nyr <- 10 # number of years in data
nSamples <- 30 # set number of dates
nSites <- 15 # set number of sites

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
time_period_missing <- sample(c(rDates, NA), size = n, TRUE)

dist_sub <- rnorm(n, 10, 1)
sim_sub <- rnorm(n, 10, 1)

dist_sub_fac <- as.factor(rnorm(n, 10, 1))
sim_sub_fac <- as.factor(rnorm(n, 10, 1))


dist_sub_chr <- as.character(rnorm(n, 10, 1))
sim_sub_chr <- as.character(rnorm(n, 10, 1))

dist_sub_fac <- as.factor(rnorm(n, 10, 1))
sim_sub_fac <- as.factor(rnorm(n, 10, 1))


# combine this to a dataframe 
df <- data.frame(taxa = taxa,
                 site = site,
                 time_period = as.character(time_period),
                 time_period_missing = time_period_missing,
                 dist_sub = dist_sub,
                 sim_sub = sim_sub,
                 dist_sub_chr = dist_sub_chr,
                 sim_sub_chr = sim_sub_chr,
                 dist_sub_fac = dist_sub_fac,
                 sim_sub_fac = sim_sub_fac)


useIterations_num <- 1
useIterations_chr <- "TRUE"


temp <- tempfile()

dir.create(temp)

test_that("Test errors", {
  expect_error(errorChecks(startDate = df$time_period),
               'startDate is not in a date format. This should be of class "Date" or "POSIXct"')
  
  expect_error(errorChecks(startDate = df$time_period_missing),
               'startDate must not contain NAs')
  
  expect_error(errorChecks(Date = df$time_period),
               'Date must be a data.frame or date vector')
  
  expect_error(errorChecks(Date = df$time_period_missing),
               'Date must not contain NAs')
  
  expect_error(errorChecks(endDate = df$time_period),
               'endDate is not in a date format. This should be of class "Date" or "POSIXct"')
  
  expect_error(errorChecks(endDate = df$time_period_missing),
               'endDate must not contain NAs')
  
  expect_error(errorChecks(dist_sub = df$dist_sub_chr, sim_sub = df$sim_sub),
               'dist_sub must be integer or numeric')
  
  expect_error(errorChecks(dist_sub = df$dist_sub, sim_sub = df$sim_sub_chr),
               'sim_sub must be integer or numeric')
  
  expect_error(errorChecks(dist_sub = df$dist_sub_fac, sim_sub = df$sim_sub),
               'dist_sub must be integer or numeric')

  expect_error(errorChecks(dist_sub = df$dist_sub, sim_sub = df$sim_sub_fac),
               'sim_sub must be integer or numeric')
  
  expect_error(errorChecks(useIterations = useIterations_chr),
               'useIterations must be logical')
  
  expect_error(errorChecks(useIterations = useIterations_num),
               'useIterations must be logical')
  
  expect_error(errorChecks(iterations = "1000"),
               'iterations must be numeric or integer')
  
  expect_error(errorChecks(family = "Poisson"),
               'family must be either Binomial or Bernoulli')
  
  expect_error(errorChecks(n_iterations = 1000, burnin = 500, thinning = 1100, n_chains = 3),
               'thinning must not be larger that the number of iteration (n_iterations)',
               fixed = TRUE)
  
  expect_error(errorChecks(n_iterations = "1000", burnin = 500, thinning = 5, n_chains = 3),
               'n_iterations should be numeric')
  
  expect_error(errorChecks(n_iterations = 1000, burnin = "500", thinning = 5, n_chains = 3),
               'burnin should be numeric')
  
  expect_error(errorChecks(n_iterations = 1000, burnin = 500, thinning = "5", n_chains = 3),
               'thinning should be numeric')
  
  expect_error(errorChecks(n_iterations = 1000, burnin = 500, thinning = 5, n_chains = "3"),
               'n_chains should be numeric')
  
  expect_error(errorChecks(seed = "1"),
               'seed muct be numeric')
  
  expect_error(errorChecks(year_col = NA, start_col = NA, end_col = time_period[1]),
               'year_col or start_col and end_col must be given')
  
  expect_error(errorChecks(year_col = NA, start_col = df$time_period[1], end_col = NA),
               'year_col or start_col and end_col must be given')
  
  expect_error(errorChecks(year_col = NA, start_col = df$time_period[1], end_col = df$time_period[1]),
               'year_col cannot be used at the same time as start_col and end_col')
  
  expect_error(errorChecks(phi = 0.1),
               "phi is outside permitted range of 0.50 to 0.95")
  
  expect_error(errorChecks(phi = 0.99),
               "phi is outside permitted range of 0.50 to 0.95")
  
  expect_error(errorChecks(alpha = 0.05),
               "alpha is outside permitted range of 0.08 to 0.50")
  
  expect_error(errorChecks(alpha = 0.99),
               "alpha is outside permitted range of 0.08 to 0.50")
  
  expect_error(errorChecks(non_benchmark_sp = c(1,5,10,12)),
               'non_benchmark_sp must be a character vector')
  
  expect_error(errorChecks(non_benchmark_sp = 12),
               'non_benchmark_sp must be a character vector')
  
  expect_error(errorChecks(fres_site_filter = c(1,5,10,12)),
               'fres_site_filter must be a character vector')
  
  expect_error(errorChecks(fres_site_filter = 12),
               'fres_site_filter must be a character vector')
  
  expect_error(errorChecks(time_periods = as.matrix(df$time_period)),
               'time_periods should be a data.frame. e.g. "data.frame(start=c(1980,1990),end=c(1989,1999))"',
               fixed = TRUE)

  expect_error(errorChecks(frespath = temp),
               "filepath is not the path to a '.exe' file")

  expect_error(errorChecks(frespath = "file.exe"),
               'file.exe does not exist')
  
  expect_error(errorChecks(site = c('a', 'b', '')),
               "site must not contain empty values (i.e. '')",
               fixed = TRUE)
  
})
