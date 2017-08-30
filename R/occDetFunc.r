#' Occupancy detection Function
#' 
#' Run occupancy detection models using the output from \code{formatOccData}
#' 
#' This function requires both the R package R2jags and the program JAGS.
#' These are not installed by default when sparta is loaded and so should be
#' installed by the user. More details can be found in teh vignette.
#' 
#' @param taxa_name A character giving the name of the species to be modelled.
#' @param occDetdata The 2nd element of the object returned by formatOccData.
#' @param spp_vis The 1st element of the object returned by formatOccData.
#' @param n_iterations numeric, An MCMC parameter - The number of interations
#' @param nyr numeric, the minimum number of years on which a site must have records for it
#'        to be included in the models
#' @param burnin numeric, An MCMC parameter - The length of the burn in
#' @param thinning numeric, An MCMC parameter - The thinning factor
#' @param n_chains numeric, an MCMC parameter - The number of chains to be run
#' @param write_results logical, should results be saved to \code{output_dir}. This is
#'        recommended since these models can take a long time to run. If \code{TRUE} (default)
#'        the results from each species will be saved as an .rdata file once the species
#'        has run. This prevents loss of data should anything go wrong.
#' @param output_dir character, the output directory were the output for each taxa will be saved
#'        as .rdata files. This will defualt to the working directory
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
#' # Format the data
#' visitData <- formatOccData(taxa = taxa, site = site, time_period = time_period)
#'
#' # run the model with these data for one species (very small number of iterations)
#' results <- occDetFunc(taxa_name = taxa[1],
#'                       n_iterations = 50,
#'                       burnin = 15, 
#'                       occDetdata = visitData$occDetdata,
#'                       spp_vis = visitData$spp_vis,
#'                       write_results = FALSE)
#' }
#' @export
#' @importFrom reshape2 acast

occDetFunc <- function (taxa_name, occDetdata, spp_vis, n_iterations = 5000, nyr = 3,
                        burnin = 1500, thinning = 3, n_chains = 3, write_results = TRUE,
                        output_dir = getwd(), model.file = occDetBUGScode, seed = NULL) {
  
  J_success <- requireNamespace("R2jags", quietly = TRUE)
  
  # test is R2jags installed if not error
  if(!J_success) stop('Please install the R2jags R package. This also requires you to in stall JAGS from http://sourceforge.net/projects/mcmc-jags/files/JAGS/')
  
  errorChecks(n_iterations = n_iterations, burnin = burnin,
              thinning = thinning, n_chains = n_chains, seed = seed)
  
  # Do we have JAGS installed - this works only on windows
  if(.Platform$OS.type == "windows"){
    JAGS_test <- Sys.which(names = 'jags-terminal.exe')
    if(JAGS_test[[1]] == '') stop('R cannot find jags-terminal.exe, check that you have installed JAGS')
  }
  
  # Set seed for repeatability
  if(!is.null(seed)) set.seed(seed)
  
  # Check the taxa_name is one of my species
  if(!taxa_name %in% colnames(spp_vis)) stop('taxa_name is not the name of a taxa in spp_vis')
    
  # Add the focal column (was the species recorded on the visit?). Use the spp_vis dataframe to extract this info
  occDetdata <- merge(occDetdata, spp_vis[,c("visit", taxa_name)])
  names(occDetdata)[names(occDetdata) == taxa_name] <- "focal"
  
  # Record the max and min year
  min_year <- min(occDetdata$year)
  max_year <- max(occDetdata$year)
  
  # year and site need to be numeric starting from 1 to length of them.  This is due to the way the bugs code is written
  occDetdata$year <- occDetdata$year - min(occDetdata$year) + 1
  occDetdata$site <- as.numeric(as.factor(occDetdata$site))
  
  # need to get a measure of whether the species was on that site in that year, unequivocally, in zst
  zst <- acast(occDetdata, site ~ factor(year), value.var = 'focal', max, fill = 0) # initial values for the latent state = observed state
  nyear <- max_year - min_year + 1
  
  # look for missing years
  if(length(unique(occDetdata$year)) != nyear) stop('It looks like you have years with no data. This will crash BUGS')
  
  parameters <- c("fit", "fit.new", "psi.fs", "regres.psi","regres.pdet", "sigma2",
                  "sd.lp", "mu.lp", "tau.lp", "pdet.alpha", "LL.p")
  
  yps <- rowSums(acast(occDetdata, site ~ year, length, value.var = 'L') > 0)
  sites_to_include <- names(yps[yps >= nyr])
  zst <- zst[row.names(zst) %in% sites_to_include,]
  i <- occDetdata$site %in% sites_to_include
  
  # now assemble the bugs_data and related objects
  #need to convert Site identities into row numbers
  site_to_row_lookup <- data.frame(site = as.integer(row.names(zst)),
                                   rownum = 1:nrow(zst)) 
  
  # HERE IS THE BUGS DATA
  bugs_data <- with(merge(occDetdata[i,], site_to_row_lookup), # adds rownum to occDetdata (used below)
                    list(y = as.numeric(focal), Year = year, Site = rownum, 
                         nyear = nyear, nsite = nrow(zst), nvisit = nrow(occDetdata[i,]),
                         sumX = sum(unique(year)), sumX2 = sum(unique(year)^2)))
  
  ### Specifiy additional parameters and parts of the bugs data
  bugs_data$logL = log(occDetdata[i,]$L)
  bugs_data$dtype2p_min = -10
  bugs_data$dtype2p_max = 10 #constraints on the priors
  init = 2 # for defining which initial values are required
  
  initiate <- function(z, i = 1, nyear) {
    init <- list (z = z, alpha.p = rep(runif(1, -2, 2), nyear))
    if(i >= 2) init$LL.p = runif(1, -2, 2)
    init
  }
  
  # set the initial values... 
  init.vals <- replicate(n_chains, initiate(z = zst, i = init, nyear = nyear),
                         simplify = F)
  
  if(model.file == occDetBUGScode){
    warning('The current formulation of the priors on the state model are strongly informative (at 0 or 1) on the occupancy scale, this is not ideal, as it can cause issues when modelling species with sparse data.  We are currently investigating solutions to this issue as part of overall development work on the occupancy model.  A quick fix is to logit transform the prior for the year and site effects as shown on page 573 of Kery and Royle (2015) Applied hierarchical modelling in ecology')
  }
            
  error_status <- try(    
    out <- R2jags::jags(bugs_data, init.vals, parameters, model.file = model.file,
                        n.chains = n_chains, n.iter = n_iterations, n.thin = thinning,
                        n.burnin = burnin, DIC = TRUE)
  )
  
  dir.create(path = output_dir, showWarnings = FALSE) # create the top results folder
  
  if (class(error_status) == "try-error" ){
    warning('JAGS returned an error when modelling', taxa_name, 'error:', error_status[1])
    return(NULL)
  } else {
    out$SPP_NAME <- taxa_name
    out$min_year <- min_year
    out$max_year <- max_year
    class(out) <- 'occDet'
    if(write_results) save(out, file = file.path(output_dir, paste(taxa_name, ".rdata", sep = "")))  
    return(out)
  }  	
}
