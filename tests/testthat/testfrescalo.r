### Frescalo testing is currently skipped if not on a ###
### windows machine, or if libcurl is not supported.  ###

# Create data
n <- 1500 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites
set.seed(125)

# Create somes dates
first <- as.Date(strptime("1980/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(1980+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste('a', 11:(nSites + 10), sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

df1 <- data.frame(taxa = taxa,
                  site = site,
                  year = as.numeric(format(time_period, '%Y')),
                  startdate = time_period,
                  enddate = time_period + 500)

allsites <- sort(unique(site))

weights <- merge(allsites, allsites)
weights$W <- runif(n = nrow(weights), min = 0, max = 1)

frespath <- file.path(tempdir(), 'fres.exe')

# Save the system info as an object
system_info <- Sys.info()

test_that("Test errors", {
  
  # skip_on_appveyor()
  # skip_on_travis()
  #if (!capabilities('libcurl')) skip('skipping as libcurl not supported')
  if(grepl("mac", .Platform$pkgType)) skip('Frescalo exe does not run on Mac OS')
  
  if (system_info['sysname'] == 'Windows') {
    download.file(url = 'https://github.com/BiologicalRecordsCentre/frescalo/raw/master/Frescalo_3a_windows.exe',
                  destfile = frespath,
                  method = "libcurl",
                  mode = 'wb', quiet = TRUE)
  } else if(system_info['sysname'] == 'Linux'){
    download.file(url = 'https://github.com/BiologicalRecordsCentre/frescalo/raw/master/Frescalo_3a_linux.exe',
                  destfile = frespath,
                  method = "libcurl",
                  quiet = TRUE)
    system(command = paste('chmod', '+x', normalizePath(frespath)))
  } else{
    stop(paste('frescalo is not supported on'))
  }
  
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  expect_error(suppressWarnings(frescalo(Data = df1,
                        frespath = frespath,
                        time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                        site_col = 'site',
                        sp_col = 'FOO',
                        year = 'year',
                        sinkdir = temp)),
               'FOO is not the name of a column in data')
  
  expect_error(suppressWarnings(frescalo(Data = df1,
                        frespath = frespath,
                        time_periods = c(1980,1990,1989,1999),
                        site_col = 'site',
                        sp_col = 'taxa',
                        year = 'year',
                        sinkdir = temp)),
               'time_periods should be a data.frame')
  
  expect_error(suppressWarnings(frescalo(Data = df1,
                        frespath = frespath,
                        time_periods = data.frame(start=c(1980,1850),end=c(1989,1999)),
                        site_col = 'site',
                        sp_col = 'taxa',
                        year = 'year',
                        sinkdir = temp)),
               'In time_periods year ranges should not overlap')
  

  expect_error(suppressWarnings(frescalo(Data = df1,
                        frespath = frespath,
                        time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                        site_col = 'site',
                        sp_col = 'taxa',
                        year = 'year',
                        sinkdir = temp)),
               'the sites in your data do not match those in your weights file')

})

test_that("Runs without error", {

  # skip_on_appveyor()
  # skip_on_travis()
  if (!capabilities('libcurl')) skip('skipping as libcurl not supported')
  if(grepl("mac", .Platform$pkgType)) skip('Frescalo exe does not run on Mac OS')
  
  # This first run is done using years
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  fres_try <- suppressWarnings(frescalo(Data = df1, 
                           Fres_weights = weights,
                           frespath = frespath,
                           time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                           site_col = 'site',
                           sp_col = 'taxa',
                           year = 'year',
                           sinkdir = temp))
  sink()
  unlink(temp, recursive = TRUE)
  
  expect_equal(class(fres_try), "frescalo")
  expect_true("paths" %in% names(fres_try) &
              "trend" %in% names(fres_try) &
              "stat" %in% names(fres_try) &
              "freq" %in% names(fres_try) &
              "log" %in% names(fres_try) &
              "lm_stats" %in% names(fres_try))
  
  dir.create(temp)
  sink(file.path(temp, 'null'))
  fres_try <- suppressWarnings(frescalo(Data = df1,
                           Fres_weights = weights,
                           start_col = 'startdate',
                           end_col = 'enddate',
                           frespath = frespath,
                           time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                           site_col = 'site',
                           sp_col = 'taxa',
                           year = 'year',
                           sinkdir = temp))
  sink()
  unlink(temp, recursive = TRUE)
  
  expect_equal(class(fres_try), "frescalo")
  expect_true("paths" %in% names(fres_try) &
                "trend" %in% names(fres_try) &
                "stat" %in% names(fres_try) &
                "freq" %in% names(fres_try) &
                "log" %in% names(fres_try) &
                "lm_stats" %in% names(fres_try))
  
  # test a very low value of phi
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  fres_try <- suppressWarnings(frescalo(Data = df1,
                           phi = 0.51,
                           Fres_weights = weights,
                           frespath = frespath,
                           time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                           site_col = 'site',
                           sp_col = 'taxa',
                           year = 'year',
                           sinkdir = temp))
  sink()
  unlink(temp, recursive = TRUE)
  
  expect_equal(class(fres_try), "frescalo")
  expect_true("paths" %in% names(fres_try) &
                "trend" %in% names(fres_try) &
                "stat" %in% names(fres_try) &
                "freq" %in% names(fres_try) &
                "log" %in% names(fres_try) &
                "lm_stats" %in% names(fres_try))

})

# Create data
n <- 1500 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites
set.seed(125)

# Create somes dates
first <- as.Date(strptime("1980/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(1980+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste('SK', 11:(nSites + 10), sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

df1 <- data.frame(taxa = taxa,
                  site = site,
                  year = as.numeric(format(time_period, '%Y')),
                  startdate = time_period,
                  enddate = time_period + 500)

allsites <- sort(unique(site))

weights <- merge(allsites, allsites)
weights$W <- runif(n = nrow(weights), min = 0, max = 1)

test_that("Test plotting", {
  
  skip_on_travis()
  skip_on_appveyor()
  if (!capabilities('libcurl')) skip('skipping as libcurl not supported')
  if(grepl("mac", .Platform$pkgType)) skip('Frescalo exe does not run on Mac OS')
  
  # test plotting
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  fres_try <- suppressWarnings(frescalo(Data = df1,
                           Fres_weights = weights,
                           frespath = frespath,
                           time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                           site_col = 'site',
                           sp_col = 'taxa',
                           year = 'year',
                           plot_fres = TRUE,
                           sinkdir = temp))
  sink()
  unlink(temp, recursive = TRUE)
  
  expect_equal(class(fres_try), "frescalo")
  expect_true("paths" %in% names(fres_try) &
                "trend" %in% names(fres_try) &
                "stat" %in% names(fres_try) &
                "freq" %in% names(fres_try) &
                "log" %in% names(fres_try) &
                "lm_stats" %in% names(fres_try))
})


# Create data
n <- 15000 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites
set.seed(125)

# Create somes dates
first <- as.Date(strptime("1980/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(1980+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

df1 <- data.frame(taxa = taxa,
                  site = site,
                  year = as.numeric(format(time_period, '%Y')),
                  startdate = time_period,
                  enddate = time_period + 500)

allsites <- sort(unique(site))

weights <- merge(allsites, allsites)
weights$W <- runif(n = nrow(weights), min = 0, max = 1)

test_that("Runs high value of phi", {
  
  # skip_on_appveyor()
  # skip_on_travis()
  if (!capabilities('libcurl')) skip('skipping as libcurl not supported')
  if(grepl("mac", .Platform$pkgType)) skip('Frescalo exe does not run on Mac OS')
  
  # test a very low value of phi
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  fres_try <- suppressWarnings(frescalo(Data = df1,
                           Fres_weights = weights,
                           phi = NULL,
                           frespath = frespath,
                           time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                           site_col = 'site',
                           sp_col = 'taxa',
                           year = 'year',
                           sinkdir = temp))
  sink()
  unlink(temp, recursive = TRUE)
  
  expect_equal(class(fres_try), "frescalo")
  expect_true("paths" %in% names(fres_try) &
                "trend" %in% names(fres_try) &
                "stat" %in% names(fres_try) &
                "freq" %in% names(fres_try) &
                "log" %in% names(fres_try) &
                "lm_stats" %in% names(fres_try))
  
})
