#' Calculate percentage change between two years using Bayesian output
#' 
#' Using the data returned from occDetModel this function models a linear
#' trend between two years for each iteration of the models. The predicted
#' values for the two years are then used to calculates a percentage
#' change. The results is a percentage change estimate for each of the
#' interations of the model. This distribution of the results is used to
#' calculate the mean estimate and the 95% credibale intervals. 
#'
#' @param firstYear numeric, the first year over which the change is to be estimated
#' @param lastYear numeric, the last year over which the change is to be estimated
#' @param bayesOut occDet object as returned from occDetModel
#' @return A list giving the mean, credible intervals and raw data from the
#' estimations.
#' @examples
#' \dontrun{
#' 
#' #' # Create data
#' n <- 15000 #size of dataset
#' nyr <- 20 # number of years in data
#' nSamples <- 100 # set number of dates
#' nSites <- 50 # set number of sites
#' 
#' # Create somes dates
#' first <- as.Date(strptime("1980/01/01", "%Y/%m/%d")) 
#' last <- as.Date(strptime(paste(1980+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
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
#' survey <- sample(rDates, size = n, TRUE)
#'
#' # run the model with these data for one species
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        survey = survey,
#'                        species_list = c('a','m','g'),
#'                        write_results = FALSE,
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2)
#'
#'  # estimate the change for one species      
#'  change <- occurrenceChange(firstYear = 1990,
#'                             lastYear = 1999,
#'                             bayesOut = results$a)   
#' }                   
#' @export

occurrenceChange <- function(firstYear, lastYear, bayesOut){
  
  if(!firstYear %in% bayesOut$min_year:bayesOut$max_year) stop('firstYear must be in the year range of the data')
  if(!lastYear %in% bayesOut$min_year:bayesOut$max_year) stop('lastYear must be in the year range of the data')
  
  occ_it <- bayesOut$BUGSoutput$sims.list$psi.fs
  colnames(occ_it) <- bayesOut$min_year:bayesOut$max_year
  years <- firstYear:lastYear
  
  prediction <- function(years, series){
    
    # cut data
    data_table <- data.frame(occ = series[as.character(years)], year = (years - min(years) + 1))
    
    # run model
    model <- glm(occ ~ year, data = data_table, family = 'binomial')
    
    # create predicted values
    predicted <- plogis(predict(model))
    names(predicted) <- years
    
    # build results
    results <- data.frame(predicted[1], predicted[length(predicted)], row.names = NULL)
    colnames(results) <- as.character(c(min(years), max(years)))
    results$change = (results[,2] - results[,1]) / results[,1]
    
    return(results)
    
  }
  
  predictions <- do.call(rbind, apply(X = occ_it, MARGIN = 1, years = years, FUN = prediction))
  
  return(list(mean = mean(predictions$change),
              CIs = quantile(predictions$change, probs = c(0.025, 0.975)),
              data = predictions))
}