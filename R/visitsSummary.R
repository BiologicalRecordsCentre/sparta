#' Summarises visits
#' 
#' This function takes a formatOccData object and returns a dataframe summarising the percentage of total sites that have been visited twice or more (i.e. revisited) within each closure period, as well as the mean number of visits to sites that have been visited two times or more for each closure period. 
#' 
#' @param x A formatOccData object (an object of class List, of 2 dataframe components (spp_vis and occDetdata).
#' @return A dataframe that forefills the selection criteria.
#' @examples
#' \dontrun{
#' 
#' set.seed(123)
#' 
#' # Create data
#' n <- 15000 #size of dataset
#' nyr <- 20 # number of years in data
#' nSamples <- 100 # set number of dates
#' nSites <- 50 # set number of sites
#' 
#' # Create somes dates
#' first <- as.Date(strptime("2010/01/01", "%Y/%m/%d")) 
#' last <- as.Date(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
#' dt <- last-first 
#' rDates <- first + (runif(nSamples)*dt)
#' 
#' # taxa are set as random letters
#' taxa <- sample(letters, size = n, TRUE)
#' 
#' # sites are visited randomly
#' site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' survey <- sample(rDates, size = n, TRUE)
#'
#' # Format the data
#' visitData <- formatOccData(taxa = taxa, site = site, survey = survey)
#' 
#' # Summarise visits
#' visitsSummary(visitData)
#'
#' }
#' @export
#' @importFrom dplyr count

#' @importFrom dplyr arrange

visitsSummary <- function(x) {
  
  # Test that the formatOccData object contains an occDetdata dataframe component
  if(!(is.data.frame(x$occDetdata))) stop('formatOccData object does not contain occDetdata dataframe component')
  
  # Specify the occDetdata dataframe
  data <- x$occDetdata
  # make a vector with unique closure periods in the data
  TPs <- unique(data$TP)
  # Create empty dataframe to be populated
  output = data.frame(TP = as.numeric(TPs), 
                      PercRevisited = as.numeric(NA),
                      meanRevisits = as.numeric(NA))
  # Fill df with % of sites that have >1 visit within all TPs
  for (i in TPs){
    output[TPs == i,2] <- 100*((nrow(subset(((data[data$TP == i, ]) %>% 
                                               dplyr::count(site)), n>1)))/(length(unique(data$site))))
  }
  # Fill df with mean number of revisits per site per TP
  for (i in TPs){
    # For closure periods without sites that have >1 visit, enter 0
    if (output[TPs == i,2] == 0)  (output[TPs == i,3] <- 0)
    # Otherwise find the mean number of revisits per site per TP
    else output[TPs == i,3] <- mean((subset(((data[data$TP == i, ]) %>%
                                               dplyr::count(site)), n>1))$n)
  }
  output <- output %>% arrange(TP)
  return(output)
  }