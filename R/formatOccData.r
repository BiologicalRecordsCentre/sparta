#' Format data for Occupancy detection models
#' 
#' This takes occurrene data in the form of a vector of taxa names, locations
#' and time_period (usually a date) and converts them into the form needed for
#' occupancy models (see value section)
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param includeJDay Logical. If \code{TRUE} a Julian day column is returned in the
#'        occDetData object, centered on 1 July.
#' 
#' @return A list of length 2 the first element 'spp_vis' is a data.frame with visit
#'  (unique combination of site and time period) in the first column and taxa for all
#'  the following columns. Values in taxa columns are either \code{TRUE} or
#'  \code{FALSE} depending on whether they were observed on that visit. The second
#'  element ('occDetData') is a dataframe giving the site, list length (the number of
#'  species observed on a visit) and year for each visit. Optionally this also includes
#'  a Julian Day column
#'           
#' @keywords trends, species, distribution, occupancy, bayesian, modeling
#' @references Isaac, N.J.B., van Strien, A.J., August, T.A., de Zeeuw, M.P. and Roy, D.B. (2014).
#'             Statistics for citizen science: extracting signals of change from noisy ecological data.
#'             Methods in Ecology and Evolution, 5 (10), 1052-1060.
#' @references van Strien, A.J., Termaat, T., Groenendijk, D., Mensing, V. & Kéry, M. (2010).
#'             Site-occupancy models may offer new opportunities for dragonfly monitoring based on daily species lists.
#'             Basic and Applied Ecology, 11, 495–503.
#' @examples
#' \dontrun{
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
#' # three sites are visited randomly
#' site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' time_period <- sample(rDates, size = n, TRUE)
#'
#' # run the model with these data for one species
#' formatted_data <- formatOccData(taxa = taxa,
#'                                 site = site,
#'                                 time_period = time_period)
#' }
#' @export
#' @importFrom dplyr distinct
#' @importFrom reshape2 dcast

formatOccData <- function(taxa, site, time_period, includeJDay = FALSE){

  # Do error checks
  errorChecks(taxa = taxa, site = site, time_period = time_period)
  
  # Create dataframe from vectors
  taxa_data <- distinct(data.frame(taxa, site, time_period))
  
  # time_period could be a numeric or a date. If it is a date extract the year
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    
    taxa_data$year <- as.numeric(format(taxa_data$time_period,'%Y')) # take year from date year 
  
  } else{
      
    stop('non-dates not yet supported for time_period')
    
  }
  
  # add list length column
  taxa_data$visit <- paste(taxa_data$site, taxa_data$time_period, sep="") # create a factor which combines date and site
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
            format(as.POSIXlt(taxa_data[,"time_period"],
                        format = "%Y-%m-%d"), "%j")
      )
    #center on the 1st of July by subtracting 182
    taxa_data$Jul_date <- taxa_data$Jul_date - 182
    occDetdata <- unique(taxa_data[,c("visit", "site", "L", "year", "Jul_date")])
  } else {
    # create occDetdata which is the main file sent to bugs (1 row per visist) - this will have "focal" added to it within the species loop
    occDetdata <- unique(taxa_data[,c("visit", "site", "L", "year")])
  }
  
  return(list(spp_vis = spp_vis, occDetdata = occDetdata))
  
}