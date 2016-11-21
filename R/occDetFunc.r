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
#'        to be included in the models. Defaults to 2
#' @param burnin numeric, An MCMC parameter - The length of the burn in
#' @param thinning numeric, An MCMC parameter - The thinning factor
#' @param n_chains numeric, an MCMC parameter - The number of chains to be run
#' @param write_results logical, should results be saved to \code{output_dir}. This is
#'        recommended since these models can take a long time to run. If \code{TRUE} (default)
#'        the results from each species will be saved as an .rdata file once the species
#'        has run. This prevents loss of data should anything go wrong.
#' @param output_dir character, the output directory were the output for each taxa will be saved
#'        as .rdata files. This will defualt to the working directory
#' @param modeltype A character string that specifies the model to use. See details. If
#' used then model.function is ignored.
#' @param seed numeric, uses \code{set.seed} to set the randon number seed. Setting
#'        this number ensures repeatabl analyses
#' @param model.function optionally a user defined BUGS model coded as a function (see \code{?jags},
#'        including the example there, for how this is done)
#' @param additional.parameters A character vector of additional parameters to monitor
#' @param additional.BUGS.elements A named list giving additioanl bugs elements passed 
#' to \code{R2jags::jags} 'data' argument
#' @param additional.init.values A named list giving user specified initial values to 
#' be added to teh defaults.
#'
#' @details \code{modeltype} is used to choose the model as well as the initial values, and
#' the parameter to monitor. At present this can take the following values.
#' \itemize{
#'  \item{\code{halfcauchy}}{Stuff}
#'  \item{\code{inversegamma}}{Stuff}
#'  \item{\code{intercept}}{Stuff}
#'  \item{\code{centering}}{Stuff}
#'  \item{\code{indran}}{Stuff}
#'  \item{\code{ranwalk}}{Stuff}
#'  \item{\code{indran_halfcauchy}}{Stuff}
#'  \item{\code{indran_inversegamma}}{Stuff}
#'  \item{\code{indran_intercept}}{Stuff}
#'  \item{\code{indran_centering}}{Stuff}
#'  \item{\code{ranwalk_halfcauchy}}{Stuff}
#'  \item{\code{ranwalk_inversegamma}}{Stuff}
#'  \item{\code{ranwalk_intercept}}{Stuff}
#'  \item{\code{ranwalk_centering}}{Stuff}
#'  \item{\code{indran_true}}{edited to make it a "true" random effect - possibly}
#'  \item{\code{ranwalk_edit}}{attempted to fix first year always 0.5 issue.}
#' }
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
#' @import LearnBayes
#' @import R2jags

occDetFunc <- function (taxa_name, occDetdata, spp_vis, n_iterations = 5000, nyr = 2,
                        burnin = 1500, thinning = 3, n_chains = 3, write_results = TRUE,
                        output_dir = getwd(),  modeltype = 'sparta', seed = NULL,
                        model.function = NULL, additional.parameters = NULL,
                        additional.BUGS.elements = NULL,
                        additional.init.values = NULL){
  
  J_success <- requireNamespace("R2jags", quietly = TRUE)
  
  if(!'catlistlength' %in% modeltype) modeltype <- c(modeltype, 'contlistlength')
  
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
  
  # Parameter you wish to monitor, shown in the output
  parameters <- c("psi.fs", "regres.psi","regres.pdet","pdet.alpha", "tau2", "tau.lp")
  
  if(!is.null(additional.parameters)) parameters <- c(parameters, additional.parameters)
  
  for(ptype in modeltype){
    parameters <- getParameters(parameters, modeltype = ptype)
  }
  
  # only include sites which have more than nyr of records
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
  
  # added extra elements to bugs data if needed
  for(btype in modeltype){
    bugs_data <- getBugsData(bugs_data, modeltype = btype,
                             occDetData = merge(occDetdata[i,], site_to_row_lookup))
  }
  
  # Add additional elements if specified
  if(!is.null(additional.BUGS.elements)){
    if(!is.list(additional.BUGS.elements)){
      stop("additional.BUGS.elements must be a list")
    } else {
      bugs_data <- c(bugs_data, additional.BUGS.elements)
    }
  }
  
  initiate <- function(z, nyear) {
    init <- list (z = z, alpha.p = rep(runif(1, -2, 2), nyear))

    # add extra init values if needed
    for(itype in modeltype){
      init <- getInitValues(init, modeltype = itype)
    }
    
    # add user specified values
    if(!is.null(additional.init.values)){
      if(!is.list(additional.init.values)){
        stop("additional.BUGS.elements must be a list")
      } else {
        init <- c(init, additional.init.values)
      }
    }
    return(init)
  }
  
  # set the initial values... 
  init.vals <- replicate(n_chains, initiate(z = zst, nyear = nyear),
                         simplify = F)
  
  # Select the correct model file
  if(is.null(model.function)){
    model.file <- getModelFile(modeltype)
  } else {
    cat('Using user model.function')
    model.file <- model.function
  }
  
  ### REVIEW CODE
  cat('#### PLEASE REVIEW THE BELOW ####\n\n')
  cat('Your model settings:', paste(modeltype, collapse = ', '))
  cat('Model File:\n\n')
  cat(paste(readLines(model.file), collapse = '\n'))
  cat('\n\nbugs_data:\n\n')
  cat(str(bugs_data))
  cat('\n\ninit.vals:\n\n')
  cat(str(init.vals))
  cat('\n\nparameters:\n\n')
  cat(parameters)
  ###
  
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