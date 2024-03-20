# Test error for startDate not in date format
test_that("Does it detect when startDate is not in a date format?", {
  expect_error(
    errorChecks(startDate = df$time_period),
    'startDate is not in a date format. This should be of class "Date" or "POSIXct"'
  )
})

# Test error for startDate containing NAs
test_that("Does it detect when startDate contains NAs?", {
  expect_error(
    errorChecks(startDate = df$time_period_missing),
    "startDate must not contain NAs"
  )
})

# Test error for Date not being a data.frame or date vector
test_that("Does it detect when date is not a dataframe or date vector?", {
  expect_error(
    errorChecks(Date = df$time_period),
    "Date must be a data.frame or date vector"
  )
})

# Test error for Date containing NAs
test_that("Does it detect when time_period_missing contains NAs?", {
  expect_error(
    errorChecks(Date = df$time_period_missing),
    "Date must not contain NAs"
  )
})

# Test error for endDate not in date format
test_that("Does it detect when time_period is not in a date format?", {
  expect_error(
    errorChecks(endDate = df$time_period),
    'endDate is not in a date format. This should be of class "Date" or "POSIXct"'
  )
})

# Test error for endDate containing NAs
test_that("Does it detect when endDate contains NAs?", {
  expect_error(
    errorChecks(endDate = df$time_period_missing),
    "endDate must not contain NAs"
  )
})

# Test error for dist_sub must be integer or numeric
test_that("Does it detect when dist_sub is not an integer or numeric value?", {
  expect_error(
    errorChecks(dist_sub = df$dist_sub_chr, sim_sub = df$sim_sub),
    "dist_sub must be integer or numeric"
  )
  expect_error(
    errorChecks(dist_sub = df$dist_sub_fac, sim_sub = df$sim_sub),
    "dist_sub must be integer or numeric"
  )
})

test_that("Does it detect when sim_sub is not an integer or numeric value?", {
  expect_error(
    errorChecks(dist_sub = df$dist_sub, sim_sub = df$sim_sub_fac),
    "sim_sub must be integer or numeric"
  )

  expect_error(
    errorChecks(dist_sub = df$dist_sub, sim_sub = df$sim_sub_chr),
    "sim_sub must be integer or numeric"
  )
})

# Test error for useIterations must be logical
test_that("Does it detect when iterations are not logical?", {
  useIterations_num <- 1
  useIterations_chr <- "TRUE"


  expect_error(
    errorChecks(useIterations = useIterations_chr),
    "useIterations must be logical"
  )
  expect_error(
    errorChecks(useIterations = useIterations_num),
    "useIterations must be logical"
  )
})

# Test error for iterations, family, thinning, and numeric parameters
test_that("Does it detect when iterations are not an integer?", {
  expect_error(
    errorChecks(iterations = "1000"),
    "iterations must be numeric or integer"
  )
})

test_that("Does it detect when Family is not Binomial or Bernoulli?", {
  expect_error(
    errorChecks(family = "Poisson"),
    "family must be either Binomial or Bernoulli"
  )
})

test_that("Does it detect when thinning is larger than the number of iterations?", {
  expect_error(
    errorChecks(n_iterations = 1000, burnin = 500, thinning = 1100, n_chains = 3),
    "thinning must not be larger that the number of iteration (n_iterations)",
    fixed = TRUE
  )
})

test_that("Does it detect when n_iterations is not an integer", {
  expect_error(
    errorChecks(n_iterations = "1000", burnin = 500, thinning = 5, n_chains = 3),
    "n_iterations should be numeric"
  )
})

test_that("Does it detect when burnin is not an integer?", {
  expect_error(
    errorChecks(n_iterations = 1000, burnin = "500", thinning = 5, n_chains = 3),
    "burnin should be numeric"
  )
})

test_that("Does it detect when thinning is not an integer?", {
  expect_error(
    errorChecks(n_iterations = 1000, burnin = 500, thinning = "5", n_chains = 3),
    "thinning should be numeric"
  )
})

test_that("Does it detect when n_chains is not an integer?", {
  expect_error(
    errorChecks(n_iterations = 1000, burnin = 500, thinning = 5, n_chains = "3"),
    "n_chains should be numeric"
  )
})

test_that("Does it detect when The seed is not an integer?", {
  expect_error(
    errorChecks(seed = "1"),
    "seed muct be numeric"
  )
})

# Test for detecting missing year_col when start_col and end_col are NA
test_that("can it detect when year_col is missing with start_col and end_col as NA?", {
  expect_error(
    errorChecks(year_col = NA, start_col = NA, end_col = df$time_period[1]),
    "year_col or start_col and end_col must be given"
  )
})

# Test for detecting missing end_col when year_col is NA
test_that("can it detect when end_col is missing with year_col as NA?", {
  expect_error(
    errorChecks(year_col = NA, start_col = df$time_period[1], end_col = NA),
    "year_col or start_col and end_col must be given"
  )
})

# Test for detecting simultaneous use of year_col with start_col and end_col
test_that("can it detect the simultaneous use of year_col with start_col and end_col?", {
  expect_error(
    errorChecks(year_col = NA, start_col = df$time_period[1], end_col = df$time_period[1]),
    "year_col cannot be used at the same time as start_col and end_col"
  )
})

# Test for detecting phi value below the permitted range
test_that("can it detect when phi is below the permitted range?", {
  expect_error(
    errorChecks(phi = 0.1),
    "phi is outside permitted range of 0.50 to 0.95"
  )
})

# Test for detecting phi value above the permitted range
test_that("can it detect when phi is above the permitted range?", {
  expect_error(
    errorChecks(phi = 0.99),
    "phi is outside permitted range of 0.50 to 0.95"
  )
})

# Test for detecting alpha value below the permitted range
test_that("can it detect when alpha is below the permitted range?", {
  expect_error(
    errorChecks(alpha = 0.05),
    "alpha is outside permitted range of 0.08 to 0.50"
  )
})

# Test for detecting alpha value above the permitted range
test_that("can it detect when alpha is above the permitted range?", {
  expect_error(
    errorChecks(alpha = 0.99),
    "alpha is outside permitted range of 0.08 to 0.50"
  )
})

# Test for detecting fres_site_filter not being a character vector
test_that("can it detect when fres_site_filter is not a character vector?", {
  expect_error(
    errorChecks(fres_site_filter = c(1, 5, 10, 12)),
    "fres_site_filter must be a character vector"
  )
})

# Test for detecting non_benchmark_sp not being a character vector
test_that("can it detect when non_benchmark_sp is not a character vector?", {
  expect_error(
    errorChecks(non_benchmark_sp = c(1, 5, 10, 12)),
    "non_benchmark_sp must be a character vector"
  )
})

# Test for detecting fres_site_filter not being a character vector
test_that("can it detect when fres_site_filter is not a character vector and is a single number?", {
  expect_error(
    errorChecks(fres_site_filter = 1),
    "fres_site_filter must be a character vector"
  )
})

# Test for detecting fres_site_filter not being a character vector
test_that("can it detect when fres_site_filter is not a character vector and is a single number?", {
  expect_error(
    errorChecks(non_benchmark_sp = 1),
    "non_benchmark_sp must be a character vector"
  )
})

# Test for detecting incorrect time_periods format
test_that("can it detect when time_periods format is incorrect?", {
  expect_error(
    errorChecks(time_periods = as.matrix(df$time_period)),
    'time_periods should be a data.frame. e.g. "data.frame(start=c(1980,1990),end=c(1989,1999))"',
    fixed = TRUE
  )
})

# Test for detecting incorrect filepath for a .exe file
test_that("can it detect when the filepath is not to a '.exe' file?", {
  temp <- tempfile()

  dir.create(temp)


  expect_error(
    errorChecks(frespath = temp),
    "filepath is not the path to a '.exe' file"
  )
})

# Test for detecting non-existent .exe file
test_that("can it detect when a .exe file does not exist?", {
  expect_error(
    errorChecks(frespath = "file.exe"),
    "file.exe does not exist"
  )
})

# Test error for site must not contain empty values
test_that("Does it detect empty string values?", {
  expect_error(
    errorChecks(site = c("a", "b", "")),
    "site must not contain empty values (i.e. '')",
    fixed = TRUE
  )
})
