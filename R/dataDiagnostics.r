#' ### Diagnostics functions ###
#' rm(list = ls())
#' # Create data
#' n <- 2000 #size of dataset
#' nyr <- 20 # number of years in data
#' nSamples <- 200 # set number of dates
#' useDates <- TRUE
#' 
#' # Create somes dates
#' first <- as.POSIXct(strptime("2003/01/01", "%Y/%m/%d"))
#' last <- as.POSIXct(strptime(paste(2003+(nyr-1),"/12/31", sep=''), "%Y/%m/%d"))
#' dt <- last-first
#' rDates <- first + (runif(nSamples)*dt)
#' 
#' # taxa are set as random letters
#' taxa <- sample(letters, size = n, TRUE)
#' 
#' # three sites are visited randomly
#' site <- sample(c('one', 'two', 'three'), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' if(useDates){
#'   time_period <- sample(rDates, size = n, TRUE)
#' } else {
#'   time_period <- sample(1:nSamples, size = n, TRUE)
#' }
#' 
#' dataDiagnostics(taxa, site, time_period)
#' @import dplyr

## Change in List-length over time ##
dataDiagnostics <- function(taxa, site, time_period){
  
  cat('Calculating diagnostics\n')
  pb <- txtProgressBar(min = 0, max = 10, style = 3)
  
  # Do error checks
  errorChecks(taxa = taxa, site = site, time_period = time_period)
  
  # Create dataframe from vectors
  taxa_data <- distinct(data.frame(taxa, site, time_period))
  setTxtProgressBar(pb, 2)
  
  # Create records over time plot
  head(taxa_data)
  
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    recOverTime <- as.numeric(format(time_period,'%Y'))
  } else {
    recOverTime <- time_period
  }
  setTxtProgressBar(pb, 3)
  
  # Model the trend in records over time
  bars <- table(recOverTime)
  mData <- data.frame(time_period = as.numeric(names(bars)), count = as.numeric(bars))
  modelRecs <- glm(count ~ time_period, data = mData)
  modelRecsSummary <- summary(modelRecs)  
  setTxtProgressBar(pb, 5)
  
  # Reshape the data
  space_time <- dcast(taxa_data, time_period + site ~ ., value.var='taxa',
                      fun = function(x) length(unique(x)))
  names(space_time)[ncol(space_time)] <- 'listLength'  
  setTxtProgressBar(pb, 8)
  
  
  # If time_period is a date then we want to convert it to a year for plotting
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    
    space_time$time_period <- as.numeric(format(space_time$time_period,'%Y')) # take year from date year
    
  }
  setTxtProgressBar(pb, 9)
  
  # Model the trend in list length
  modelList <- glm(listLength ~ time_period, family = 'poisson', data = space_time)
  modelListSummary <- summary(modelList)  
  setTxtProgressBar(pb, 10)
  cat('\n\n')
  
  # Setup plot space
  par(mfrow = c(2,1))
  par(mar = c(0.1, 4.1, 4.1, 2.1))
  
  # Plot a simple histogram
  barplot(height = as.numeric(bars),
          ylab = 'Number of records',
          main = 'Change in records and list length over time')
  
  # Plot the change in list length over time
  par(mar = c(5.1, 4.1, 0.1, 2.1))

  boxplot(listLength ~ time_period,
          data = space_time,
          xlab = 'Time Period',
          ylab = 'List Length', frame.plot=FALSE, ylim = c(0, max(space_time$listLength)))
  
  # Print the result to console
  cat(ifelse(modelRecsSummary$coefficients[2,4] < 0.05,
             'There is a significant change in the number of records over time:\n\n',
             'There is no detectable change in the number of records over time:\n\n'))
  print(modelRecsSummary$coefficients)
  cat('\n\n')
  cat(ifelse(modelListSummary$coefficients[2,4] < 0.05,
             'There is a significant change in list lengths over time:\n\n',
             'There is no detectable change in list lengths over time:\n\n'))
  print(modelListSummary$coefficients)
 
  invisible(list(modelRecs = modelRecs, modelList = modelList))
  
}