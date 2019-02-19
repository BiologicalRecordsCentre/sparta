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
#' @param change A character string that specifies the type of change to be calculated, the default
#' is annual growth rate.  See details for options.
#' @param region A character string specifying the region name if change is to be determined regional estimates of occupancy.
#' Region names must match those in the model output.
#' 
#' 
#' @details \code{change} is used to specify which change measure to be calculated.
#' There are four options to choose from: difference, percentdif, growthrate and
#' lineargrowth.
#' 
#' \code{difference} calculates the simple difference between the first and last year.
#' 
#' \code{percentdif} calculates the percentage difference between the first and last year.
#' 
#' \code{growthrate} calculates the annual growth rate across years.
#' 
#' \code{lineargrowth} calculates the linear growth rate from a linear model.
#' 
#' @return A list giving the mean, median, credible intervals and raw data from the
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

occurrenceChange <- function(firstYear, lastYear, bayesOut, change = 'growthrate', region = NULL){
  
  # error checks for years
  if(!firstYear %in% bayesOut$min_year:bayesOut$max_year) stop('firstYear must be in the year range of the data')
  if(!lastYear %in% bayesOut$min_year:bayesOut$max_year) stop('lastYear must be in the year range of the data')
  
  # error checks for change
  if(!class(change) == 'character') stop('Change must be a character string identifying the change metric.  Either: difference, percentdif, growthrate or lineargrowth')
  if(!change %in% c('difference', 'percentdif', 'growthrate', 'lineargrowth')) stop('The change metric must be one of the following: difference, percentdif, growthrate or lineargrowth')
  
  # error check for region
  if(!is.null(region)){
    if(!class(region) == 'character') stop('region must be a character string identifying the regional estimates that change is to be calculated for.')
    if(!region %in% bayesOut$regions) stop('region must match that used in the model output file, check spelling.')
  }
  
  
  # extract the sims list, if there is a region code, use the psi.fs for that region
  if(!is.null(region)){
    reg_code <- paste("psi.fs.r_", region, sep = "")

    occ_it <- bayesOut$BUGSoutput$sims.list
    occ_it <- occ_it[[grep(reg_code, names(occ_it))]]

  }else{
    occ_it <- bayesOut$BUGSoutput$sims.list$psi.fs
    
  }
  
  
  colnames(occ_it) <- bayesOut$min_year:bayesOut$max_year
  years <- firstYear:lastYear
  
  ## edit values that are 0 or 1 to prevent estimates of inf later on
  occ_it[occ_it == 0] <- 0.0001
  occ_it[occ_it == 1] <- 0.9999
  
  
  ### loops depend on which change metric has been specified
  
  if(change == 'lineargrowth'){
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

    res_tab <- do.call(rbind, apply(X = occ_it, MARGIN = 1, years = years, FUN = prediction))
    
  } # end of loop for linear growth rate
  
  
  if(change == 'difference'){
    first <- years[1]
    last <- years[length(years)]
    res_tab <- data.frame(occ_it[, colnames(occ_it) == first],
                          occ_it[, colnames(occ_it) == last],
                          row.names = NULL)
    colnames(res_tab) <- as.character(c(min(years), max(years)))
    res_tab$change = res_tab[,2] - res_tab[,1]
  } # end of loop for simple difference
  
  
  if(change == 'percentdif'){
    first <- years[1]
    last <- years[length(years)]
    res_tab <- data.frame(occ_it[, colnames(occ_it) == first],
                          occ_it[, colnames(occ_it) == last],
                          row.names = NULL)
    colnames(res_tab) <- as.character(c(min(years), max(years)))
    res_tab$change = ((res_tab[,2] - res_tab[,1])/res_tab[,1])*100
  } # end of loop for percentage difference
  
  
  if(change == 'growthrate'){
    nyr <- length(years)
    first <- years[1]
    last <- years[length(years)]
    res_tab <- data.frame(occ_it[, colnames(occ_it) == first],
                          occ_it[, colnames(occ_it) == last],
                          row.names = NULL)
    colnames(res_tab) <- as.character(c(min(years), max(years)))
    res_tab$change = (((res_tab[,2]/res_tab[,1])^(1/nyr))-1)*100
  } # end of loop for growth rate

  # return the mean, quantiles, and the data
  return(list(mean = mean(res_tab$change),
              median = median(res_tab$change),
              CIs = quantile(res_tab$change, probs = c(0.025, 0.975)),
              data = res_tab))

}
