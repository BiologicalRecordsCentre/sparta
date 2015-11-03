#' Occupancy detection models
#' 
#' Run occupancy detection models as described in Isaac et al, 2014 
#' 
#' This function requires both the R package R2jags and the program JAGS.
#' These are not installed by default when sparta is loaded and so should be
#' installed by the user. More details can be found in teh vignette.
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param species_list A character vector of taxa names for which models should be run. This is
#'        optional and by default models will be run for all taxa
#' @param write_results logical, should results be saved to \code{output_dir}. This is
#'        recommended since these models can take a long time to run. If \code{TRUE} (default)
#'        the results from each species will be saved as an .rdata file once the species
#'        has run. This prevents loss of data should anything go wrong.
#' @param output_dir character, the output directory were the output for each taxa will be saved
#'        as .rdata files. This will defualt to the working directory
#' @param nyr numeric, the minimum number of years on which a site must have records for it
#'        to be included in the models
#' @param n_iterations numeric, An MCMC parameter - The number of interations
#' @param burnin numeric, An MCMC parameter - The length of the burn in
#' @param thinning numeric, An MCMC parameter - The thinning factor
#' @param n_chains numeric, an MCMC parameter - The number of chains to be run
#' @param model.file optionally a user defined BUGS model coded as a function (see \code{?jags},
#'        including the example there, for how this is done)
#' @param seed numeric, uses \code{set.seed} to set the randon number seed. Setting
#'        this number ensures repeatabl analyses
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
#' time_period <- sample(rDates, size = n, TRUE)
#'
#' # run the model with these data for one species
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        time_period = time_period,
#'                        species_list = c('a','m','g'),
#'                        write_results = FALSE,
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2)
#' }
#' @export
#' @references Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012) Invasive alien predator
#'             causes rapid declines of native European ladybirds. Diversity & Distributions,
#'             18, 717-725.

occDetModel <- function(taxa, site, time_period,
                        species_list = unique(taxa), write_results = TRUE,
                        output_dir = getwd(), nyr = 3, n_iterations = 5000,
                        burnin = 1500, thinning = 3, n_chains = 3, 
                        model.file = occDetBUGScode, seed = NULL){
 
  # Error checking done in lower functions
    
  # Do we have JAGS installed - this works only on windows
  if(.Platform$OS.type == "windows"){
    JAGS_test <- Sys.which(names = 'jags-terminal.exe')
    if(JAGS_test[[1]] == '') stop('R cannot find jags-terminal.exe, check that you have installed and loaded r-package R2jags and you have JAGS installed')
  }
  
  # reformat the data into visits
  visitData <- formatOccData(taxa = taxa, site = site, time_period = time_period)
  
  ### loop through the species list running the Bayesian occupancy model function ###
  output <- list()
  for (taxa_name in species_list){
    cat('\n###\nModeling', taxa_name, '-', grep(taxa_name, species_list),
        'of', length(species_list), 'taxa\n' )
    output[[taxa_name]] <- occDetFunc(taxa_name = taxa_name,
                                    occDetdata = visitData$occDetdata,
                                    spp_vis = visitData$spp_vis,
                                    n_iterations = n_iterations,
                                    burnin = burnin,
                                    thinning = thinning,
                                    n_chains = n_chains,
                                    write_results = write_results,
                                    output_dir = output_dir,
                                    nyr = nyr,
                                    seed = seed)
  }
  
  return(output)
  
}
  