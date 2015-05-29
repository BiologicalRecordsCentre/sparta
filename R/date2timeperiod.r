#' Assign dates to time periods
#' 
#' This function assigns dates to timeperiods a nessecary step for methods that use time periods 
#' rather than dates, such as Frescalo and Telfer
#' 
#' @param Date Either a vector of dates or a 2 column data.frame where the first column is the start
#' date and the second column is the end date.
#' @param time_periods A data.frame with two columns, the first column gives the start year of each
#' time period and the second column gives the end year. These year ranges are inclusive.
#' @return A vector, the same length as date giving the time periods. Time period 1
#' is the time period with the earliest start year, 2 with the second earliest start
#' year and so on. If the dates don't fit in a time period or overlap a time period
#' they are given an NA value 
#' @export
#' @examples
#' 
#' # Create data
#' n <- 15000 #size of dataset
#' nyr <- 20 # number of years in data
#' nSamples <- 100 # set number of dates
#' nSites <- 50 # set number of sites
#' 
#' # Create somes dates
#' first <- as.Date(strptime("1980/01/01", "%Y/%m/%d")) 
#' last <- as.Date(strptime(paste(1980+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
#' dt <- last-first 
#' Date <- first + (runif(nSamples)*dt)
#' 
#' # Create time periods dataframe
#' time_periods <- data.frame(start=c(1980,1990),end=c(1989,1999))
#' 
#' # Create time periods column using a vector
#' tps <- date2timeperiod(Date = Date, time_periods = time_periods) 
#'
#' # Create time periods column using a data.frame
#' tps <- date2timeperiod(Date = data.frame(start = Date, end = (Date+100)),
#'                        time_periods = time_periods) 


date2timeperiod <- function(Date, time_periods){
  
    
  # Make sure the time periods are correctly formatted
  # Sort it by start year
  time_periods <- time_periods[with(time_periods, order(time_periods[,1])),]
  
  # Error checks
  errorChecks(Date = Date, time_periodsDF = time_periods)
  
  tps <- NULL
  
  if(is.data.frame(Date)){
  
    # Loop through each time period and get a T/F if each date range is in that time period
    # The time peiods are saved in the object tps
    for(i in 1:nrow(time_periods)){
    
      TF_tp <- as.numeric(format(Date[,1],'%Y')) >= time_periods[i,1] &
               as.numeric(format(Date[,2],'%Y')) <= time_periods[i,2]
      
      tps[TF_tp] <- i
      
    }  
    
    return(tps)
  
  } else {
    
    for(i in 1:nrow(time_periods)){
      
      TF_tp <- as.numeric(format(Date,'%Y')) >= time_periods[i,1] &
        as.numeric(format(Date,'%Y')) <= time_periods[i,2]
      
      tps[TF_tp] <- i
      
    }
    
    return(tps)
    
  }

}