#' Histogram of records over time
#' 
#' This is a useful function for visualising your data and how the number of records
#' change over time. This is key for understanding biases that may be present in your
#' data (Isaac et al, 2014). 
#' 
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param Log Logical, should the y-axis be on a log scale?
#' @param col Passed to \link{barplot}, the colour of bars
#' @param xlab Passed to \link{barplot}, the x-axis label
#' @param ylab Passed to \link{barplot}, the y-axis label
#' @param ... other arguements to pass to \link{barplot}
#'        
#' @return A plot
#' @examples
#' \dontrun{
#' 
#' # Create data
#' n <- 3000 #size of dataset
#' nyr <- 10 # number of years in data
#' nSamples <- 30 # set number of dates
#' nSites <- 15 # set number of sites
#' 
#' # Create somes dates
#' first <- as.POSIXct(strptime("2010/01/01", "%Y/%m/%d")) 
#' last <- as.POSIXct(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
#' dt <- last-first 
#' rDates <- first + (runif(nSamples)*dt)
#' 
#' # taxa are set as random letters
#' taxa <- sample(letters, size = n, TRUE)
#' 
#' # three sites are visited randomly
#' site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' time_period <- sample(rDates, size = n, TRUE)
#' 
#' # combine this to a dataframe (adding a final row of 'bad' data)
#' df <- data.frame(taxa = c(taxa,'bad'),
#'                  site = c(site,'A1'),
#'                  time_period = c(time_period, as.POSIXct(strptime("1200/01/01", "%Y/%m/%d"))))
#'                  
#' # This reveals the 'bad data'                  
#' recsOverTime(df$time_period)
#' 
#' # remove and replot
#' df <- df[format(df$time_period, '%Y') > 2000, ]
#' recsOverTime(df$time_period)
#' 
#' # plot with style
#' recsOverTime(df$time_period, col = 'blue', main = 'Records of Species A',
#'              ylab = 'log(number of records)', Log = TRUE) 
#' 
#' }
#' @export

recsOverTime <- function(time_period, Log = FALSE, col = 'black', xlab = 'Year',
                         ylab = ifelse(Log, 'log(Frequency)', 'Frequency'), ...){
  
  # Do some error checks
  errorChecks(time_period = time_period)
  
  # Make if we have dates convert to year for plotting
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){    
    time_period <- format(time_period, format='%Y')  
  }
  
  # This may not include all values needed since a year in the middle might be missing
  year_range <- range(as.numeric((time_period)))
  # nil counts are filled in as 0.1 if using log to avoid errors
  frequencies_full <- data.frame(all_years = year_range[1]:year_range[2],
                                 freq = NA)
  
  # Get our frequencies
  frequencies <- as.data.frame(table(time_period))
  frequencies_full$freq <- frequencies$Freq[match(x = frequencies_full$all_years,
                                                  table = frequencies$time_period)]
   
  # Plot
  if(Log){
    barplot(height = log(frequencies_full$freq), names.arg = frequencies_full$all_years,
            col = col, xlab = xlab, ylab = ylab, ...)   
  } else {
    barplot(height = frequencies_full$freq, names.arg = frequencies_full$all_years,
            col = col, xlab = xlab, ylab = ylab, ...)      
  }  
}