#' Occupancy detection models
#' 
#' Run occupancy detection models as described in Isaac et al, 2014 
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param species_list A list of taxa names for which models should be run. This is
#'        optional and by default models will be run for all taxa 
#' @param output_dir The output directory were the output for each taxa will be saved
#'        as .rdata files. This will defualt to the working directory
#' @param nyr The minimum number of years on which a site must have records for it
#'        to be included in the models
#' @param n_iterations An MCMC parameter - The number of interations
#' @param burnin An MCMC parameter - The length of the burn in
#' @param thinning An MCMC parameter - The thinning factor
#' @param n_chains An MCMC parameter - The number of chains to be run
#' @param model.file optionally a user defined BUGS model coded as a function (see ?jags,
#'        including the example there, for how this is done)
#' 
#' @return A list of filepaths, one for each species run, giving the location of the
#'         output saved as a .rdata file, containing an object called 'out'
#'          
#' @keywords trends, species, distribution, occupancy, bayesian, modeling
#' @references Isaac, N.J.B., van Strien, A.J., August, T.A., de Zeeuw, M.P. and Roy, D.B. (2014).
#'             Statistics for citizen science: extracting signals of change from noisy ecological data.
#'             Methods in Ecology and Evolution, 5 (10), 1052-1060.
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
#' # run the model with these data for one species
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        time_period = time_period,
#'                        species_list = c('a','m','g'),
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2,
#'                        output_dir = "W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/R Packages/Trend analyses/occ_test_out")
#' }
#' @export
#' @import reshape2
#' @import R2jags
#' @import dplyr
#' @references Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012) Invasive alien predator
#'             causes rapid declines of native European ladybirds. Diversity & Distributions,
#'             18, 717-725.

occDetModel <- function(taxa, site, time_period, print_progress = FALSE,
                        species_list = unique(taxa), output_dir = getwd(),
                        nyr = 3, n_iterations = 5000, burnin = 1500,
                        thinning = 3, n_chains = 3, model.file = occDetBUGScode){
 
  # Do error checks
  # ADD IN BUGS PARAMETERS
  errorChecks(taxa = taxa, site = site, time_period = time_period,
              n_iterations = n_iterations, burnin = burnin,
              thinning = thinning, n_chains = n_chains)
  
  # Do we have JAGS installed - this works only on windows
  if(.Platform$OS.type == "windows"){
    JAGS_test <- Sys.which(names = 'jags-terminal.exe')
    if(JAGS_test[[1]] == '') stop('R cannot find jags-terminal.exe, check that you have installed JAGS')
  }
  
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
  
  # create "simdata" which is the main file sent to bugs (1 row per visist) - this will have "focal" added to it within the species loop
  occDetdata <- unique(taxa_data[,c("visit", "site", "L", "year")])
  
  ### loop through the species list running the Bayesian occupancy model function ###
  filepaths <- list()
  for (taxa_name in species_list){
    cat('\n###\nModeling', taxa_name, '-', grep(taxa_name, species_list),
        'of', length(species_list), 'taxa\n' )
    filepaths[taxa_name] <- occDetFunc(taxa_name = taxa_name,
                                       occDetdata = occDetdata,
                                       spp_vis = spp_vis,
                                       n_iterations = n_iterations,
                                       burnin = burnin,
                                       thinning = thinning,
                                       n_chains = n_chains,
                                       output_dir = output_dir,
                                       nyr = nyr)
  }
  
  return(filepaths)
  
}
  