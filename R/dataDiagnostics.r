#' Data Diagnostics
#' 
#' This function provides visualisations of how the number of records in the
#' dataset changes over time and how the number of species recorded on a
#' visit changes over time. For each of these an linear model is run to test
#' if there is a significant trend.
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param plot Logical, if \code{TRUE} plots and model results will be printed to
#'        the console
#' @param progress_bar If \code{TRUE} a progress bar is printed to console
#' 
#' @return A list of filepaths, one for each species run, giving the location of the
#'         output saved as a .rdata file, containing an object called 'out'
#'          
#' @examples
#' \dontrun{
#' 
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
#' }
#' @export
#' @importFrom dplyr distinct
#' @importFrom reshape2 dcast

## Change in List-length over time ##
dataDiagnostics <- function(taxa, site, time_period, plot = TRUE, progress_bar = TRUE){
  
  if(progress_bar) cat('Calculating diagnostics\n')
  if(progress_bar) pb <- txtProgressBar(min = 0, max = 10, style = 3)
  
  # Do error checks
  errorChecks(taxa = taxa, site = site, time_period = time_period)
  
  # Create dataframe from vectors
  taxa_data <- distinct(data.frame(taxa, site, time_period))
  if(progress_bar) setTxtProgressBar(pb, 2)
  
  # Create records over time plot
  head(taxa_data)
  
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    recOverTime <- as.numeric(format(time_period,'%Y'))
  } else {
    recOverTime <- time_period
  }
  if(progress_bar) setTxtProgressBar(pb, 3)
  
  # Model the trend in records over time
  bars <- table(recOverTime, dnn = 'RecordsPerYear')
  mData <- data.frame(time_period = as.numeric(names(bars)), count = as.numeric(bars))
  modelRecs <- glm(count ~ time_period, data = mData)
  modelRecsSummary <- summary(modelRecs)  
  if(progress_bar) setTxtProgressBar(pb, 5)
  
  # Reshape the data
  space_time <- dcast(taxa_data, time_period + site ~ ., value.var='taxa',
                      fun = function(x) length(unique(x)))
  names(space_time)[ncol(space_time)] <- 'listLength'  
  if(progress_bar) setTxtProgressBar(pb, 8)
  
  
  # If time_period is a date then we want to convert it to a year for plotting
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    
    space_time$time_period <- as.numeric(format(space_time$time_period,'%Y')) # take year from date year
    
  }
  
  if(progress_bar) setTxtProgressBar(pb, 9)
  
  # Model the trend in list length
  modelList <- glm(listLength ~ time_period, family = 'poisson', data = space_time)
  modelListSummary <- summary(modelList)  
  if(progress_bar) setTxtProgressBar(pb, 10)
  if(progress_bar) cat('\n\n')
  
  if(plot){
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
    
  }
  
  invisible(list(RecordsPerYear = bars, VisitListLength = space_time, modelRecs = modelRecs, modelList = modelList))
  
}