#' Minimum time-period site selection
#' 
#' This function uses part of the method outlined in Roy et al (2012) and Isaac et al (2014) for selecting
#' well-sampled sites from a dataset using the number of time periods only. \code{\link{siteSelection}} is a wrapper
#' for this function that performs the complete site selection process as outlined in these papers.
#' 
#' @param taxa A character vector of taxon names
#' @param site A character vector of site names
#' @param time_period A numeric vector of user defined time periods, or a date vector
#' @param minTP numeric, The minimum number of time periods, or if time_period is a date the minimum
#' number of years, a site must be sampled in for it be be considered well sampled.
#' @return A data.frame of data that forefills the selection criteria. This data has two attributes:
#' \code{sites} gives the total number of sites in the dataset, and \code{sucess} gives the number
#' of sites that satify the selection criteria
#' @export
#' @import plyr 
#' @references needed

siteSelectionMinTP <- function(taxa, site, time_period, minTP){
  
  # Error checks
  errorChecks(taxa, site, time_period)
  if(!is.numeric(minTP)) stop('minTP must be numeric')
  
  # Load library
  library(plyr)
  
  # If tp is a date get the year out
  # This will be used in TP selection step
  year <- NULL
  if(any(class(time_period) %in% c("Date", "POSIXct", "POSIXt"))){
    year <- as.numeric(format(time_period, '%Y'))  
  }
  
  # Create a data.frame from the vectors
  Data <- data.frame(taxa, site, time_period)
  if(!is.null(year)) Data$year <- year
    
  # Get a list of sites with visits in >=minTP time_periods
  # If we have year use that, else time_period
  if(!is.null(year)){
    minTP_site_counts <- tapply(Data$year, Data$site, FUN = function(x) length(unique(x)))
  } else {
    minTP_site_counts <- tapply(Data$time_period, Data$site, FUN = function(x) length(unique(x)))   
  }
  
  minTP_sites <- names(minTP_site_counts[minTP_site_counts >= minTP])
  
  # Subset the data to these sites
  minTP_Data <- Data[Data$site %in% minTP_sites,]
  
  # Add some helpful attributes
  attr(minTP_Data, which = 'sites') <- length(minTP_site_counts)
  attr(minTP_Data, which = 'success') <- length(minTP_sites)
  
  if(nrow(minTP_Data) == 0) warning('Filtering in siteSelectionMinTP resulted in no data returned')
    
  # Return
  return(minTP_Data[,c('taxa', 'site', 'time_period')])
 
}