#' Assign dates to time periods
#' 
#' This function assigns dates to timeperiods a nessecary step for methods that use time periods 
#' rather than dates, such as Frescalo and Telfer
#' 
#' @param startDate A vector of dates indicating the start date of a record
#' @param endDate A vector of dates indicating the end date of a record
#' @param time_periods A data.frame with two columns, the first column gives the start year of each
#' time period and the second column gives the end year. These year ranges are inclusive.
#' @return A vector, the same length as startDate and endDate giving the time period of each pair
#' of dates. Time period 1 is the time period with the earliest start year, 2 with the second
#' earliest start year and so on. If the dates don't fit in a time period or overlap a time period
#' they are given an NA value 
#' @export
#' @examples
#' # Load example dataset
#' data(ex_dat)
#'
#' # Create time periods dataframe
#' time_periods <- data.frame(start=c(1980,1990),end=c(1989,1999))
#' 
#' # Create time periods column
#' ex_dat$timeperiod <- date2timeperiod(startDate = ex_dat$TO_STARTDATE,
#'                                      endDate = ex_dat$Date,
#'                                      time_periods = time_periods) 

date2timeperiod <- function(startDate, endDate, time_periods){
  
    
  # Make sure the time periods are correctly formatted
  # Sort it by start year
  time_periods <- time_periods[with(time_periods, order(time_periods[,1])),]
  
  # Error checks
  errorChecks(startDate = startDate, endDate = endDate, time_periodsDF = time_periods)
  
  tps <- NULL
  
  # Loop through each time period and get a T/F if each date range is in that time period
  # The time peiods are saved in the object tps
  for(i in 1:nrow(time_periods)){
  
    TF_tp <- as.numeric(format(startDate,'%Y')) >= time_periods[i,1] &
             as.numeric(format(endDate,'%Y')) <= time_periods[i,2]
    
    tps[TF_tp] <- i
    
  }  
  
  return(tps)
  
}