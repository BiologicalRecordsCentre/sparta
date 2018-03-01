#' Format data for Occupancy detection models
#' 
#' This takes occurrene data in the form of a vector of taxa names, locations
#' and survey (usually a date) and converts them into the form needed for
#' occupancy models (see value section)
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param survey A vector as long as the number of observations. 
#'        This must be a Date if either closure_period is not supplied or if includeJDay = \code{TRUE}
#' @param closure_period An optional vector of integers specifying the closure period. 
#'        If \code{FALSE} then closure_period will be extracted as the year from the survey.
#' @param includeJDay Logical. If \code{TRUE} a Julian day column is returned in the
#'        occDetData object, centered on 1 July.
#' 
#' @return A list of length 2 the first element 'spp_vis' is a data.frame with visit
#'  (unique combination of site and time period) in the first column and taxa for all
#'  the following columns. Values in taxa columns are either \code{TRUE} or
#'  \code{FALSE} depending on whether they were observed on that visit. The second
#'  element ('occDetData') is a dataframe giving the site, list length (the number of
#'  species observed on a visit) and year for each visit. Optionally this also includes
#'  a Julian Day column, centered on 1 July.
#'           
#' @keywords trends, species, distribution, occupancy, bayesian, modeling
#' @references Isaac, N.J.B., van Strien, A.J., August, T.A., de Zeeuw, M.P. and Roy, D.B. (2014).
#'             Statistics for citizen science: extracting signals of change from noisy ecological data.
#'             Methods in Ecology and Evolution, 5 (10), 1052-1060.
#' @references van Strien, A.J., Termaat, T., Groenendijk, D., Mensing, V. & Kéry, M. (2010).
#'             Site-occupancy models may offer new opportunities for dragonfly monitoring based on daily species lists.
#'             Basic and Applied Ecology, 11, 495-503.
#' @examples
#' \dontrun{
#' 
#' # Create data
#' n <- 15000 #size of dataset
#' nyr <- 20 # number of years in data
#' nSurveys <- 100 # set number of dates
#' nSites <- 50 # set number of sites
#' 
#' # Create somes dates
#' first <- as.Date(strptime("2010/01/01", "%Y/%m/%d")) 
#' last <- as.Date(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
#' dt <- last-first 
#' rDates <- first + (runif(nSurveys)*dt)
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
#' formatted_data <- formatOccData(taxa = taxa,
#'                                 site = site,
#'                                 survey = survey,
#'                                 includeJDay = TRUE)
#' }
#' @examples
#' \dontrun{
#' 
#' # Create data with coarser survey information
#' n <- 1500 #number of species observation in dataset
#' np <- 10 # number of closure periods in data
#' nSurveys <- 100 # set number of surveys
#' nSites <- 20 # set number of sites
#' 
#' # taxa are set as random letters
#' taxa <- sample(letters, size = n, TRUE)
#' 
#' # three sites are visited randomly
#' site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' survey <- sample(nSurveys, size = n, TRUE)
#'
#' # allocate the surveys randomly to closure periods 
#' cp <- sample(1:np, nSurveys, TRUE)
#' closure_period <- cp[survey]
#'
#' # run the model with these data for one species
#' formatted_data <- formatOccData(taxa = taxa,
#'                                 site = site,
#'                                 survey = survey,
#'                                 closure_period = closure_period)
#' }
#' @export
#' @importFrom dplyr distinct
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom reshape2 dcast

formatOccData <- function(taxa, site, survey, closure_period = NULL, includeJDay = FALSE){

  # Do error checks
  errorChecks(taxa = taxa, site = site, survey = survey, closure_period = closure_period)

  # Additional error check whether survey is a date
  if(!'POSIXct' %in% class(survey) & !'Date' %in% class(survey)){
    # This is necessary if either closure_period is not supplied or if includeJDay = TRUE
    if(is.null(closure_period)){
      stop('survey must be a date if closure_period not supplied')
    } else {
      if(includeJDay == TRUE){
        stop('survey must be a date if Julian Date is to be included')        
    }
   }
  } # otherwise survey can be anything (including a character vector)
  
  # Create dataframe from vectors
  taxa_data <- data.frame(taxa, site, survey)
  
  # now add the time period (TP). It's defined by closure_period if supplied
  # TP was formerly referred to as year
  if(!is.null(closure_period)){
    # replace with sort order in case they go in as decades
    lookup <- data.frame(cp = sort(unique(closure_period)),
                        TP = 1:length(unique(closure_period)))
    
    taxa_data$TP <- lookup$TP[match(closure_period, lookup$cp)]

    #check that surveys are nested within closure_periods
    temp <- taxa_data %>% 
            group_by(survey) %>% 
            summarise(ns = length(unique(TP)))
    if(any(temp$ns) > 1) {
      # return a warning but assume they know what they're doing
      warning(paste(as.numeric(table(temp$ns >1)[2]), 'survey identities appear in multiple closure periods'))
    }
  } else {
    # we need to create the TP from the survey date.
    # survey should be a date if we've got this far, so extract the year
    taxa_data$TP <- as.numeric(format(taxa_data$survey,'%Y')) # take year from date year 
  }
  
  # remove duplicates
  taxa_data <- distinct(taxa_data)
  
  # add list length column
  taxa_data$visit <- paste(taxa_data$site, taxa_data$survey, sep="") # create a factor which combines date and site
  visit_Ls <- as.data.frame(table(taxa_data$visit))
  names(visit_Ls) <- c("visit", "L")
  taxa_data <- merge(taxa_data, visit_Ls)
  
  # create species_visit dataframe/matrix
  temp <- taxa_data[,c('taxa','visit')]
  names(temp)[1] <- "species_name"
  temp$pres <- TRUE # add TRUE column which will populate the spp with visit matrix/dataframe
  spp_vis <- dcast(temp, formula = visit ~ species_name, value.var = "pres", fill = FALSE) # This is the dataframe that contains a row per visit and a column for each species present or not.  USed to create the focal column in the next step
  
  
  # Add Julian Day if needed
  if(includeJDay){
    taxa_data$Jul_date <-as.numeric(
            format(as.POSIXlt(taxa_data[,"survey"],
                        format = "%Y-%m-%d"), "%j")
      )
    #center on the 1st of July by subtracting 182
    taxa_data$Jul_date <- taxa_data$Jul_date - 182
    occDetdata <- taxa_data[,c("visit", "site", "L", "TP", "Jul_date")]
  } else {
    # create occDetdata which is the main file sent to bugs (1 row per visist) - this will have "focal" added to it within the species loop
    occDetdata <- taxa_data[,c("visit", "site", "L", "TP")]
  }
  
  return(list(spp_vis = spp_vis, occDetdata = occDetdata))
  
}