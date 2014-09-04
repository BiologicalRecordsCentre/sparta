#' List-length site selection
#' 
#' This function uses part of the method outlined in Roy et al (2012) and Isaac et al (2014) for selecting
#' well-sampled sites from a dataset using list length only. \code{\link{siteSelection}} is a wrapper
#' for this function that performs the complete site selection process as outlined in these papers.
#' 
#' @param taxa A character vector of taxon names
#' @param site A character vector of site names
#' @param time_period Anumeric vector of user defined time periods, or a date vector
#' @param minL numeric, The minimum number of taxa recorded at a site at a given time period 
#' (list-length) for the visit to be considered well sampled.
#' @return A data.frame of data that forefills the selection criteria. This data has two attributes:
#' \code{visits} gives the total number of visits in the dataset (unique combinations of \code{site}
#'  and \code{time_period}), \code{success} gives the number of visits that satify the selection criteria
#' @export
#' @import plyr 
#' @references needed

siteSelectionMinL <- function(taxa, site, time_period, minL){
  
  # Run error checks 
  errorChecks(taxa, site, time_period)
  if(!is.numeric(minL)) stop('minL must be numeric')
  
  # Create dataframe
  Data <- data.frame(taxa, site, time_period)
    
  # Using plyr to create list lengths
  dfLL <- ddply(Data, .(site, time_period), nrow)

  # Visits that mean minL criteria
  minL_dfLL <- dfLL[dfLL$V1 >= minL, 1:2]
  
  # Subset Data to these visits (and keep columns in same order)
  minL_Data <- merge(x = Data, y = minL_dfLL, all = F)[c('taxa', 'site', 'time_period')]
  
  # Add attributes
  attr(minL_Data, which = 'visits') <- nrow(dfLL)
  attr(minL_Data, which = 'success') <- nrow(minL_dfLL)
  
  if(nrow(minL_Data) == 0) warning('Filtering in siteSelectionMinL resulted in no data returned')
    
  return(minL_Data)
  
}