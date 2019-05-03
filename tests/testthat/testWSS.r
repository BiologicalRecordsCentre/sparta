context("Test WSS")

# Create data
n <- 150 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 20 # set number of dates
set.seed(seed = 125)

# Create somes dates
first <- as.POSIXct(strptime("2003/01/01", "%Y/%m/%d")) 
last <- as.POSIXct(strptime(paste(2003+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(c('one', 'two', 'three'), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

# combine this to a dataframe
df <- unique(data.frame(taxa, site, time_period))

test_that("Test WSS2", {
  
  set.seed(seed = 125)
  results <- WSS(df$taxa,
                 df$site,
                 df$time_period,
                 minL = 4,
                 minTP = 3) 
  
  expect_is(results, 'data.frame')
  expect_equal(colnames(results), 
               c("species_name", "intercept.estimate", "year.estimate", "intercept.stderror", 

                 "year.stderror", "intercept.zvalue", "year.zvalue", "intercept.pvalue", 
                 "year.pvalue", "observations"))
  expect_equal(nrow(results), 25)
  
})