## ----eval=FALSE---------------------------------------------------------------
#  # Install the package from CRAN
#  # THIS WILL WORK ONLY AFTER THE PACKAGE IS PUBLISHED
#  # install.packages('sparta')
#  
#  # Or install the development version from GitHub
#  library(devtools)
#  # install_github('biologicalrecordscentre/sparta')

## -----------------------------------------------------------------------------
# Once installed, load the package
library(sparta)

## -----------------------------------------------------------------------------
# Create data
n <- 8000 # size of dataset
nyr <- 50 # number of years in data
nSamples <- 200 # set number of dates
nSites <- 100 # set number of sites
set.seed(125) # set a random seed

# Create somes dates
first <- as.Date(strptime("1950/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(1950+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set semi-randomly
taxa_probabilities <- seq(from = 0.1, to = 0.7, length.out = 26)
taxa <- sample(letters, size = n, TRUE, prob = taxa_probabilities)

# sites are visited semi-randomly
site_probabilities <- seq(from = 0.1, to = 0.7, length.out = nSites)
site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE, prob = site_probabilities)

# the date of visit is selected semi-randomly from those created earlier
time_probabilities <- seq(from = 0.1, to = 0.7, length.out = nSamples)
time_period <- sample(rDates, size = n, TRUE, prob = time_probabilities)

myData <- data.frame(taxa, site, time_period)

# Let's have a look at the my example data
head(myData)

## ----cache = TRUE-------------------------------------------------------------
# Run some data diagnostics on our data
results <- dataDiagnostics(taxa = myData$taxa,
                           site = myData$site,
                           time_period = myData$time_period,
                           progress_bar = FALSE)

## ----cache = TRUE-------------------------------------------------------------
# Run some data diagnostics on our data, now time_period
# is set to be a year
results <- dataDiagnostics(taxa = myData$taxa,
                           site = myData$site,
                           time_period = as.numeric(format(myData$time_period, '%Y')),
                           progress_bar = FALSE)

## -----------------------------------------------------------------------------
# See what is in results..
names(results)

# Let's have a look at the details
head(results$RecordsPerYear)
head(results$VisitListLength)
summary(results$modelRecs)
summary(results$modelList)

## ----cache = TRUE-------------------------------------------------------------
## Create a new column for the time period
# First define my time periods
time_periods <- data.frame(start = c(1950, 1960, 1970, 1980, 1990),
                           end = c(1959, 1969, 1979, 1989, 1999))

time_periods

# Now use these to assign my dates to time periods
myData$tp <- date2timeperiod(myData$time_period, time_periods)

head(myData)

## ----cache = TRUE-------------------------------------------------------------
## Create a dataset where we have date ranges
Date_range <- data.frame(startdate = myData$time_period,
                         enddate = (myData$time_period + 600))

head(Date_range)

# Now assign my date ranges to time periods
Date_range$time_period <- date2timeperiod(Date_range, time_periods)

head(Date_range)

## ----cache = TRUE-------------------------------------------------------------
# Here is our data
head(myData)

telfer_results <- telfer(taxa = myData$taxa,
                         site = myData$site,
                         time_period = myData$tp,
                         minSite = 2)

## -----------------------------------------------------------------------------
head(telfer_results)

## ----cache = TRUE-------------------------------------------------------------
# Select only records which occur on lists of length 2 or more
myDataL <- siteSelectionMinL(taxa = myData$taxa,
                             site = myData$site,
                             time_period = myData$time_period,
                             minL = 2) 

head(myDataL)

# We now have a much smaller dataset after subsetting
nrow(myData)
nrow(myDataL)

## ----cache = TRUE-------------------------------------------------------------
# Select only data from sites sampled in at least 10 years
myDataTP <- siteSelectionMinTP(taxa = myData$taxa,
                               site = myData$site,
                               time_period = myData$time_period,
                               minTP = 10) 

head(myDataTP)

# Here we have only lost a small number rows, this is because
# many sites in our data are visited in a lot of years. Those
# rows that have been removed are duplicates
nrow(myData)
nrow(myDataTP)

## ----cache = TRUE-------------------------------------------------------------
# We need to create a new column to represent unique months
# this could also be any unit of time you wanted (week, decade, etc.)

# This line returns a unique character for each month
unique_Months <- format(myData$time_period, "%B_%Y")
head(unique_Months)

# Week could be done like this, see ?strptime for more details
unique_Weeks <- format(myData$time_period, "%U_%Y")
head(unique_Weeks)

# Now lets subset to records found on 60 months or more
myData60Months <- siteSelectionMinTP(taxa = myData$taxa,
                                     site = myData$site,
                                     time_period = unique_Months,
                                     minTP = 60) 

head(myData60Months)

# We could merge this back with our original data if
# we need to retain the full dates
myData60Months <- merge(myData60Months, myData$time_period, 
                        all.x = TRUE, all.y = FALSE,
                        by = "row.names")
head(myData60Months)

nrow(myData)
nrow(myData60Months)

## ----cache = TRUE-------------------------------------------------------------
# Subset our data as above but in one go
myDataSubset  <- siteSelection(taxa = myData$taxa,
                               site = myData$site,
                               time_period = myData$time_period,
                               minL = 2,
                               minTP = 10,
                               LFirst = TRUE)

head(myDataSubset)
nrow(myDataSubset)

## ----cache = TRUE-------------------------------------------------------------
# Run the reporting rate model using list length as a fixed effect and 
# site as a random effect. Here we only model a few species.
system.time({
RR_out <- reportingRateModel(taxa = myData$taxa,
                             site = myData$site,
                             time_period = myData$time_period,
                             list_length = TRUE,
                             site_effect = TRUE,
                             species_to_include = c('e','u','r','o','t','a','s'),
                             overdispersion = FALSE,
                             family = 'Bernoulli',
                             print_progress = TRUE)
})

# Let's have a look at the data that is returned
str(RR_out)

# We could plot these to see the species trends
with(RR_out,
     # Plot graph
     {plot(x = 1:7, y = year.estimate,
           ylim = range(c(year.estimate - year.stderror,
                          year.estimate + year.stderror)),
           ylab = 'Year effect (+/- Std Dev)',
           xlab = 'Species',
           xaxt = "n")
     # Add x-axis with species names
     axis(1, at = 1:7, labels = species_name)
     # Add the error bars
     arrows(1:7, year.estimate - year.stderror,
            1:7, year.estimate + year.stderror,
            length = 0.05, angle = 90, code = 3)}
     )

## ----cache = TRUE-------------------------------------------------------------
# Load in snowfall
library(snowfall)

# I have 4 cpus on my PC so I set cpus to 4
# when I initialise the cluster
sfInit(parallel = TRUE, cpus = 4)

# Export my data to the cluster
sfExport('myData')

# I create a function that takes a species name and runs my models
RR_mod_function <- function(taxa_name){
  
  library(sparta)
  
  RR_out <- reportingRateModel(species_to_include = taxa_name,
                               taxa = myData$taxa,
                               site = myData$site,
                               time_period = myData$time_period,
                               list_length = TRUE,
                               site_effect = TRUE,
                               overdispersion = FALSE,
                               family = 'Bernoulli',
                               print_progress = FALSE)  
} 

# I then run this in parallel
system.time({
para_out <- sfClusterApplyLB(c('e','u','r','o','t','a','s'), RR_mod_function)
})

# Name each element of this output by the species
RR_out_combined <- do.call(rbind, para_out)

# Stop the cluster
sfStop()

# You'll see the output is the same as when we did it serially but the
# time taken is shorter. Using a cluster computer with many more than 
# 4 cores can greatly reduce run time.
str(RR_out_combined)

## ----cache = TRUE-------------------------------------------------------------
# Run our data through the well-sampled sites function
# This time we run all species
WSS_out <- WSS(taxa = myData$taxa,
               site = myData$site,
               time_period = myData$time_period,
               minL = 2,
               minTP = 10,
               print_progress = FALSE)

# The data is returned in the same format as from reportingRateModel
str(WSS_out)

# We can plot these and see that we get different results to our
# previous analysis since this time the method includes subsetting
with(WSS_out[1:10,],
     # Plot graph
     {plot(x = 1:10, y = year.estimate,
           ylim = range(c(year.estimate - year.stderror,
                          year.estimate + year.stderror)),
           ylab = 'Year effect (+/- Std Dev)',
           xlab = 'Species',
           xaxt="n")
     # Add x-axis with species names
     axis(1, at=1:10, labels = species_name[1:10])
     # Add the error bars
     arrows(1:10, year.estimate - year.stderror,
            1:10, year.estimate + year.stderror,
            length=0.05, angle=90, code=3)}
     )

