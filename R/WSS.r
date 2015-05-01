#' Well sampled sites model
#' 
#' This function is a wrapper for \code{siteSelection} and \code{reportingRateModel} that
#' allows users the run a well sampled sites analysis as in Roy et al (2012).
#'  
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param minL numeric, The minimum number of taxa recorded at a site at a given time period 
#'        (list-length) for the visit to be considered well sampled.
#' @param minTP numeric, The minimum number of time periods, or if time_period is a date the minimum
#'        number of years, a site must be sampled in for it be be considered well sampled.
#' @param overdispersion This option allows modelling overdispersion (\code{TRUE}) in models.
#'        Default is \code{FALSE}.
#' @param verbose This option, if \code{TRUE}, sets models to verbose, allowing the 
#'        interations of each model to be viewed.
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
#'          \item{\code{minL}}{ - The setting of minL used in site selection}
#'          \item{\code{minTP}}{ - The setting of minTP used in site selection}
#'          } 
#' @references Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012) Invasive alien predator
#'             causes rapid declines of native European ladybirds. Diversity & Distributions,
#'             18, 717-725.
#' @export
#' @examples
#' \dontrun{
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
#' 
#' results <- WSS2(df$taxa,
#'                 df$site,
#'                 df$time_period,
#'                 minL = 4,
#'                 minTP = 3)
#' 
#' # Look at the results for the first few species
#' head(results)
#' # Look at the attributes of the object returned
#' attributes(results)
#' }

WSS <- function(taxa, site, time_period, minL = 2, minTP = 3, overdispersion = FALSE, verbose = FALSE, print_progress = FALSE){
  
  selected_data  <- siteSelection(taxa,
                          site,
                          time_period,
                          minL,
                          minTP,
                          LFirst = TRUE)
  
  WSS_result <- reportingRateModel(selected_data$taxa, 
                                   selected_data$site,
                                   selected_data$time_period,
                                   list_length = FALSE,
                                   site_effect = TRUE,
                                   verbose = verbose,
                                   print_progress = print_progress,
                                   overdispersion = overdispersion)  
  
  attr(WSS_result, 'minL') <- minL
  attr(WSS_result, 'minTP') <- minTP
  
  return(WSS_result)
  
}