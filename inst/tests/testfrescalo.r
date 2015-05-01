context("Test frescalo")

library(sparta)
data(ex_dat)

test_that("Runs without error", {
  #cat('Testing exectutable functionality\n')
  temp <- tempfile(pattern = 'dir')
  dir.create(temp)
  sink(file.path(temp, 'null'))
  fres_try <- try(frescalo(Data=ex_dat,
                     time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
                     site_col='hectad',
                     sp_col='CONCEPT',
                     start_col='TO_STARTDATE',
                     end_col='Date',
                     sinkdir=temp),
                  silent=TRUE
  )
  sink()
  unlink(temp, recursive = TRUE)
  
  expect_equal(class(fres_try), "frescalo")
  expect_true("paths" %in% names(fres_try) &
              "trend" %in% names(fres_try) &
              "stat" %in% names(fres_try) &
              "freq" %in% names(fres_try) &
              "log" %in% names(fres_try) &
              "lm_stats" %in% names(fres_try))
  unlink('~/null')
  if(class(fres_try)=='frescalo') unlink(gsub('/Output','',dirname(fres_try$paths[1])),recursive=T)
})
