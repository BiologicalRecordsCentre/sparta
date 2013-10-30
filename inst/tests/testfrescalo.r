context("Test frescalo")

library(sparta)
data(ex_dat)

test_that("Runs without error", {
  sink('~/null')
  cat('Testing exectutable functionality\n')
  fres_try <- try(frescalo(Data=ex_dat,
                     time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
                     site_col='hectad',
                     sp_col='CONCEPT',
                     start_col='TO_STARTDATE',
                     end_col='Date',
                     sinkdir='~'),
                  silent=TRUE
  )
  sink()
  expect_equal(class(fres_out), "frescalo")
  expect_true("paths" %in% names(fres_out) &
              "trend" %in% names(fres_out) &
              "stat" %in% names(fres_out) &
              "freq" %in% names(fres_out) &
              "log" %in% names(fres_out) &
              "lm_stats" %in% names(fres_out))
  unlink('~/null')
  if(class(fres_out)=='frescalo') unlink(gsub('/Output','',dirname(fres_out$paths[1])),recursive=T)
})
