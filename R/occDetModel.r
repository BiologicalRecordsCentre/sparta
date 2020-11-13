#' Occupancy detection models
#' 
#' Run occupancy detection models as described in Isaac et al, 2014 
#' 
#' This function requires both the R package R2jags and the program JAGS.
#' These are not installed by default when sparta is loaded and so should be
#' installed by the user. More details can be found in the vignette.
#' 
#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param survey A  vector as long as the number of observations. 
#'        This must be a Date if includeJDay = \code{TRUE}
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
#' @param model.function optionally a user defined BUGS model coded as a function (see \code{?jags},
#'        including the example there, for how this is done)
#' @param modeltype A character string or vector that specifies the model to use. See details. If
#' used then model.function is ignored.
#' @param regional_codes A data.frame object detailing which site is associated with which region.
#' each row desginates a site and each column represents a region. The first column represents the 
#' site name (as in \code{site}). Subsequent columns are named for each regions with 1 representing
#' the site is in that region and 0 that it is not. NOTE a site should only be in one region
#' @param region_aggs A named list giving aggregations of regions that you want trend
#' estimates for. For example \code{region_aggs = list(GB = c('england', 'scotland', 'wales'))}
#' will produced a trend for GB (Great Britain) as well as its constituent nations. Note that
#' 'england', scotland' and 'wales' must appear as names of columns in \code{regional_codes}. 
#' More than one aggregate can be given, eg \code{region_aggs = list(GB = c('england', 'scotland',
#'  'wales'), UK = c('england', 'scotland', 'wales', 'northern_ireland'))}.
#' @param max_year numeric, final year to which analysis will be run, this can be set if it is beyond
#' the limit of the dataset.  Defaults to final year of the dataset.
#' @param seed numeric, uses \code{set.seed} to set the randon number seed. Setting
#'        this number ensures repeatabl analyses
#' @param additional.parameters A character vector of additional parameters to monitor
#' @param additional.BUGS.elements A named list giving additioanl bugs elements passed 
#' to \code{R2jags::jags} 'data' argument
#' @param additional.init.values A named list giving user specified initial values to 
#' be added to the defaults.
#' @param return_data Logical, if \code{TRUE} (default) the bugs data object is returned with the data
#' 
#' @details \code{modeltype} is used to choose the model as well as the associated initial values,
#' and parameters to monitor. Elements to choose from can be separated into the following components:
#' 
#' A. Prior type: this has 3 options, each of which was tested in Outhwaite et al (in review):
#'   1. sparta - This uses the same model as in Isaac et al (2014).
#'   2. indran - This is the adaptive stationary model.
#'   3. ranwalk - This is the random walk model.
#' 
#' B. Hyperprior type: This has 3 options, each of these are discussed in Outhwaite et al (in review):
#'   1. halfuniform - the original formulation in Isaac et al (2014).
#'   2. halfcauchy - preferred form, tested in Outhwaite et al (in review).
#'   3. inversegamma - alternative form presented in the literature.
#' 
#' C. List length specification:  This has 3 options:
#'   1. catlistlength - list length as a categorical variable.
#'   2. contlistlength - list length as a continuous variable.
#'   3. nolistlength - no list length variable.
#' 
#' D. Julian date: this is an additional option for including Julian date within the detection model:
#'   1. jul_date.
#' 
#' Not all combinations are available in sparta. You will get an error if you try and use
#' a combination that is not supported. There is usually a good reason why that
#' combination is not a good idea. Here are the model elements available:
#' 
#' \itemize{
#'  \item{\code{"sparta"}}{ - This uses the same model as in Isaac et al (2014)}
#'  \item{\code{"indran"}}{ - Here the prior for the year effect of the state model is modelled as a random effect.  This allows the model to adapt to interannual variability.}
#'  \item{\code{"ranwalk"}}{ - Here the prior for the year effect of the state model is modelled as a random walk.  Each estimate for the year effect is dependent on that of the previous year.}
#'  \item{\code{"halfcauchy"}}{ - Includes half-Cauchy hyperpriors for all random effects within the model.  The half-Cauchy is a special case of the Student’s t distribution with 1 degree of freedom. }
#'  \item{\code{"inversegamma"}}{ - Includes inverse-gamma hyperpriors for random effects within the model}
#'  \item{\code{"catlistlength"}}{ - This specifies that list length should be considered as a catagorical variable. There are 3 classes, lists of length 1, 2-3, and 4 and over. If none of the list length options are specifed 'contlistlength' is used}
#'  \item{\code{"contlistlength"}}{ - This specifies that list length should be considered as a continious variable. If none of the list length options are specifed 'contlistlength' is used}
#'  \item{\code{"nolistlength"}}{ - This specifies that no list length should be used. If none of the list length options are specifed 'contlistlength' is used}
#'  \item{\code{"jul_date"}}{ - This adds Julian date to the model as a normal distribution with its mean and standard deviation as monitered parameters.}
#'  \item{\code{"intercept"}}{ - No longer available.  Includes an intercept term in the state and observation model.  By including intercept terms, the occupancy and detection probabilities in each year are centred on an overall mean level.}
#'  \item{\code{"centering"}}{ - No longer available.  Includes hierarchical centering of the model parameters.   Centring does not change the model explicitly but writes it in a way that allows parameter estimates to be updated simultaneously.}
#' }
#' These options are provided as a vector of characters, e.g. \code{modeltype = c('indran', 'halfcauchy', 'catlistlength')}
#'  
#' @return A list of occDet objects (see occDetFunc), as an occDetList class of object
#'          
#' @keywords trends, species, distribution, occupancy, bayesian, modeling
#' @references Isaac, N.J.B., van Strien, A.J., August, T.A., de Zeeuw, M.P. and Roy, D.B. (2014).
#'             Statistics for citizen science: extracting signals of change from noisy ecological data.
#'             \emph{Methods in Ecology and Evolution}, 5: 1052-1060.
#' @references Outhwaite, C.L., Chandler, R.E., Powney, G.D., Collen, B., Gregory, R.D. & Isaac, N.J.B. (2018).
#'             Prior specification in Bayesian occupancy modelling improves analysis of species occurrence data. 
#'             \emph{Ecological Indicators}, 93: 333-343.
#' @examples
#' \dontrun{
#' 
#' # Create data
#' set.seed(125)
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
#' # using defaults
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        survey = survey,
#'                        species_list = 'a',
#'                        write_results = FALSE,
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2)
#'                        
#' # run with a different model type
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        survey = survey,
#'                        species_list = 'a',
#'                        write_results = FALSE,
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2,
#'                        seed = 125, 
#'                        modeltype = c("indran", "intercept"))
#'                        
#' # run with regions
#' 
#' # Create region definitions
#' regions <- data.frame(site = unique(site),
#'                       region1 = c(rep(1, 20), rep(0, 30)),
#'                       region2 = c(rep(0, 20), rep(1, 15), rep(0, 15)),
#'                       region3 = c(rep(0, 20), rep(0, 15), rep(1, 15)))
#'  
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        survey = survey,
#'                        species_list = 'a',
#'                        write_results = FALSE,
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2,
#'                        seed = 125, 
#'                        modeltype = c("indran", "intercept"),
#'                        regional_codes = regions,
#'                        region_aggs = list(agg1 = c('region1', 'region2')))       
#' }
#' @export
#' @references Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012) Invasive alien predator
#'             causes rapid declines of native European ladybirds. Diversity & Distributions,
#'             18, 717-725.

occDetModel <- function(taxa, site, survey,
                        species_list = unique(taxa), write_results = TRUE,
                        output_dir = getwd(), nyr = 2, n_iterations = 5000,
                        burnin = 1500, thinning = 3, n_chains = 3, 
                        modeltype = 'sparta', regional_codes = NULL,
                        region_aggs = NULL, model.function = NULL, max_year = NULL,
                        seed = NULL, additional.parameters = NULL,
                        additional.BUGS.elements = NULL,
                        additional.init.values = NULL,
                        return_data = FALSE){
 
  # Error checking done in lower functions
  # Check if R2jags is installed
  if (!requireNamespace("R2jags", quietly = TRUE)) {
    stop("Package 'R2jags' is needed for the 'occDetModel' function to work. Please insatll this from CRAN. You will also be required to install JAGS, which you can download from https://sourceforge.net/projects/mcmc-jags/files/JAGS/",
         call. = FALSE)
  }
    
  # Do we have JAGS installed - this works only on windows
  if(.Platform$OS.type == "windows"){
    JAGS_test <- Sys.which(names = 'jags-terminal.exe')
    if(JAGS_test[[1]] == '') stop('R cannot find jags-terminal.exe, check that you have installed and loaded r-package R2jags and you have JAGS installed')
  }
  
  includeJDay <- ifelse('jul_date' %in% tolower(modeltype), TRUE, FALSE)
  
  # reformat the data into visits
  visitData <- formatOccData(taxa = taxa,
                             site = site,
                             survey = survey,
                             includeJDay = includeJDay)
  
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
                                      modeltype = modeltype,
                                      regional_codes = regional_codes,
                                      region_aggs = region_aggs,
                                      model.function = model.function,
                                      seed = seed, 
                                      max_year = max_year,
                                      additional.parameters = additional.parameters,
                                      additional.BUGS.elements = additional.BUGS.elements,
                                      additional.init.values = additional.init.values,
                                      return_data = return_data)
  }
  
  class(output) <- 'occDetList'
  return(output)
  
}
  