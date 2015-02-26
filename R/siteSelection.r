#' Site selection method
#' 
#' This function uses the method outlined in Roy et al (2012) and Isaac et al (2014) for selecting
#' well-sampled sites from a dataset using list length and number of years as selection criteria.
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#' as long as the number of observations. 
#' @param minL numeric, The minimum number of taxa recorded at a site at a given time period 
#' (list-length) for the visit to be considered well sampled.
#' @param minTP numeric, The minimum number of time periods, or if time_period is a date the minimum
#' number of years, a site must be sampled in for it be be considered well sampled.
#' @param LFirst Logical, if \code{TRUE} data is first filtered by list-length then time periods,
#' else time period then list-length
#' @return A data.frame of data that forefills the selection criteria
#' @export
#' @references needed
#' @examples
#' # Create data
#' n <- 150 #size of dataset
#' nyr <- 8 # number of years in data
#' nSamples <- 20 # set number of dates
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
#' time_period <- sample(rDates, size = n, TRUE)
#' 
#' # combine this to a dataframe
#' df <- data.frame(taxa, site, time_period)
#' head(df)
#' 
#' # Use the site selection function on this simulated data
#' dfSEL  <- siteSelection(df$taxa, df$site, df$time_period, minL = 4, minTP = 3)

siteSelection <- function(taxa, site, time_period, minL, minTP, LFirst = TRUE){
  
  # Run error checks 
  errorChecks(taxa, site, time_period)
   
  # If list length first
  if(LFirst){
    
    f_sub <- suppressWarnings(siteSelectionMinL(taxa, site, time_period, minL))
    s_sub <- suppressWarnings(siteSelectionMinTP(f_sub$taxa, f_sub$site, f_sub$time_period, minTP))
    
  } else {
    
    f_sub <- suppressWarnings(siteSelectionMinTP(taxa, site, time_period, minTP))
    s_sub <- suppressWarnings(siteSelectionMinL(f_sub$taxa, f_sub$site, f_sub$time_period, minL))
    
  }
  
  ## ADD IN CAPTURE AND OUTPUT OF THE ATTRIBUTES
  
  if(nrow(s_sub) == 0) warning('Filtering in siteSelection resulted in no data returned')
  return(s_sub)
    
}