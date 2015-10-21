context("Test date2timeperiod")

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
Date <- first + (runif(nSamples)*dt)

# Create time periods dataframe
time_periods <- data.frame(start=c(1980,1990),end=c(1989,1999))

test_that("Test errors and warnings", {
  
  Date2 <- data.frame(start = Date, end = Date+100) 
  
  expect_error(tps <- date2timeperiod(Date, data.frame(start=c(1980,1990,1995),end=c(1989,1999,2008))),               
               'In time_periods year ranges cannot overlap')
  expect_error(tps <- date2timeperiod(Date, data.frame(start=c(1980,1990,2005),end=c(1989,1999,2000))),               
               'In time_periods end years must be greater than or equal to start years')
  
})

test_that("Test time period allocation", {
  
  tps <- date2timeperiod(head(Date), time_periods)
  
  expect_equal(tps, c(2,1,1,1,2,2))
  
  tps2 <- date2timeperiod(head(data.frame(start = Date, end = Date + 500)), time_periods)
  
  expect_equal(tps2, c(2,1,1,1,NA,NA))
    
})