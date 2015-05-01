#' Run Reporting Rate Models
#' 
#' Run reporting rate models to assess the change in species occurrence over time. 
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param list_length Logical, if \code{TRUE} then list length is added to the models as a fixed
#'        effect. Note that since list_length is a property of each visit the model will run as 
#'        a binomial model rather that as a bernoulli model.
#' @param site_effect Logical, if \code{TRUE} then site is added to the models as a random
#'        effect.
#' @param overdispersion This option allows modelling overdispersion (\code{TRUE}) in models.
#'        Default is \code{FALSE}.
#' @param verbose This option, if \code{TRUE}, sets models to verbose, allowing the 
#'        interations of each model to be viewed.
#' @param family The type of model to be use. Can be \code{"Binomial"} or \code{"Bernoulli"}.
#'        Note the if list_length is used family defaults to Bernoulli.
#' @param print_progress Logical, if \code{TRUE} progress is printed to console when
#'        running models. Default is \code{TRUE}   
#' 
#' @return A dataframe of results are returned to R. Each row gives the results for a
#'         single species, with the species name given in the first column, \code{species_name}.
#'         For each of the following columns the prefix (before ".") gives the covariate and the
#'         sufix (after the ".") gives the parameter of that covariate.
#'         \code{number_observations} gives the number of visits where the species of interest
#'         was observed. If any of the models encountered an error this will be given in the
#'         column \code{error_message}.\cr
#'         
#'         The data.frame has a number of attributes:
#'         \itemize{
#'          \item{\code{intercept_year}}{ - The year used for the intercept (i.e. the
#'           year whose value is set to 0). Setting the intercept to the median year helps
#'           to increase model stability}
#'          \item{\code{min_year} and \code{max_year}}{ - The earliest and latest year
#'           in the dataset (after years have been centered on \code{intercept_year}}
#'          \item{\code{nVisits}}{ - The total number of visits that were in the dataset}
#'          \item{\code{model_formula}}{ - The model used, this will vary depending on the
#'           combination of arguements used}
#'          }
#'          
#' @keywords trends, species, distribution
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
#' # Run the model
#' RR_out <- reportingRateModel(df$taxa, df$site, df$time_period, print_progress = TRUE)
#' head(RR_out)
#' 
#' }
#' @export
#' @importFrom reshape2 dcast
#' @importFrom plyr rbind.fill
#' @importFrom dplyr distinct
#' @references Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012) Invasive alien predator
#'             causes rapid declines of native European ladybirds. Diversity & Distributions,
#'             18, 717-725.

reportingRateModel <- function(taxa, site, time_period, list_length = FALSE, site_effect = FALSE,
                               overdispersion = FALSE, verbose = FALSE, family = 'Binomial',
                               print_progress = FALSE){
  
  # Do error checks
  errorChecks(taxa = taxa, site = site, time_period = time_period, list_length = list_length,
              site_effect = site_effect, overdispersion = overdispersion, verbose = verbose,
              family = family)
  
  # Create dataframe from vectors
  taxa_data <- distinct(data.frame(taxa, site, time_period))
  
  # Reshape the data so that it is suitable for model
  space_time <- dcast(taxa_data, time_period + site ~ ., value.var='taxa', fun = function(x) length(unique(x)))
  names(space_time)[ncol(space_time)] <- 'listLength'
  
  # time_period could be a numeric or a date. If it is a date extract the year
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    
    space_time$year <- as.numeric(format(space_time$time_period,'%Y')) # take year from date year
    # centre the Year on the median value (for numerical stability)
    intercept_year <- median(unique(as.numeric(space_time$year)))
    space_time$year <- space_time$year - intercept_year    
    
  } else{
    
    stop('non-dates not yet supported for time_period')
    
  }
  
  model_formula <- formulaBuilder(family,
                                  list_length,
                                  site_effect,
                                  overdispersion)
  
  counter <- data.frame(species = sort(unique(taxa_data$taxa)),
                        count = 1:length(sort(unique(taxa_data$taxa))))
    
  # Run an apply across all species which undertakes the modelling
  all_coefs <- lapply(sort(unique(taxa_data$taxa)), function(species_name){ # the sort ensures species are done in order
    
    if(print_progress) cat('Modelling', species_name, '- Species', 
                           counter$count[counter$species == species_name],
                           'of',length(unique(taxa_data$taxa)), '\n')
    
    ii_coefs <- RR_model_func(taxa_data,
                              species_name,
                              space_time,
                              overdispersion,
                              verbose,
                              site_effect,
                              list_length,
                              model_formula,
                              family)
  
    return(as.data.frame(ii_coefs))
    
  })
      
  # Bind the results of the apply into a data.frame
  coef_df <- do.call(rbind.fill, all_coefs)
   
  # Get some attributes of the dataset
  attributes(coef_df) <- c(attributes(coef_df), list(intercept_year = intercept_year,
                                                     min_year =  min(unique(as.numeric(space_time$year))),
                                                     max_year =  max(unique(as.numeric(space_time$year))),
                                                     nVisits = nrow(space_time),
                                                     model_formula = model_formula))
  
  return(coef_df)
}