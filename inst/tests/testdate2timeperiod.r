context("Test date2timeperiod")

# Load example dataset
data(ex_dat)
startDate = ex_dat$TO_STARTDATE
endDate = ex_dat$Date

# Create time periods dataframe
time_periods <- data.frame(start=c(1980,1990),end=c(1989,1999))

# Create time periods column
ex_dat$timeperiod <- date2timeperiod(startDate = ex_dat$TO_STARTDATE,
                                     endDate = ex_dat$Date,
                                     time_periods = time_periods) 

test_that("Test errors and warnings", {
  
  expect_error(ex_dat$timeperiod <- date2timeperiod(head(startDate, -1), endDate, time_periods),               
               'The following arguements are not of equal length')
  expect_error(ex_dat$timeperiod <- date2timeperiod(startDate, head(endDate, -1), time_periods),               
               'The following arguements are not of equal length')
  expect_error(ex_dat$timeperiod <- date2timeperiod(as.numeric(startDate), endDate, time_periods),               
               'startDate is not in a date format')
  expect_error(ex_dat$timeperiod <- date2timeperiod(startDate, as.character(endDate), time_periods),               
               'endDate is not in a date format')
  expect_error(ex_dat$timeperiod <- date2timeperiod(startDate, endDate, data.frame(start=c(1980,1990,1995),end=c(1989,1999,2008))),               
               'In time_periods year ranges cannot overlap')
  expect_error(ex_dat$timeperiod <- date2timeperiod(startDate, endDate, data.frame(start=c(1980,1990,2005),end=c(1989,1999,2000))),               
               'In time_periods end years must be greater than or equal to start years')
  
})

test_that("Test time period allocation", {
  
  ex_dat$timeperiod <- date2timeperiod(startDate, endDate, time_periods)
  
  years <- time_periods[1,1]:time_periods[1,2]
  startDate1 <- ex_dat$TO_STARTDATE[ex_dat$timeperiod == 1 & !is.na(ex_dat$timeperiod)]
  endDate1 <- ex_dat$Date[ex_dat$timeperiod == 1 & !is.na(ex_dat$timeperiod)]
  expect_true(all(as.numeric(format(startDate1, '%Y')) %in% years))
  expect_true(all(as.numeric(format(endDate1, '%Y')) %in% years))
  
  startDateNA <- ex_dat$TO_STARTDATE[is.na(ex_dat$timeperiod)]
  endDateNA <- ex_dat$Date[is.na(ex_dat$timeperiod)]
  
  expect_true(!any(as.numeric(format(startDateNA, '%Y')) %in% years &
                   as.numeric(format(endDateNA, '%Y')) %in% years))
  
})