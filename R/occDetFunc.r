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
#' @param nyr numeric, the minimum number of periods on which a site must have records for it
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
#' @param modeltype A character string or vector of strings that specifies the model to use. See details. If
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
#' @param model.function optionally a user defined BUGS model coded as a function (see \code{?jags},
#'        including the example there, for how this is done)
#' @param additional.parameters A character vector of additional parameters to monitor
#' @param additional.BUGS.elements A named list giving additioanl bugs elements passed 
#' to \code{R2jags::jags} 'data' argument
#' @param additional.init.values A named list giving user specified initial values to 
#' be added to the defaults.
#' @param return_data Logical, if \code{TRUE} (default) the BUGS data object is returned with the data
#' @param saveMatrix Logical, if \code{FALSE} (default) the sims.matrix element of the jags object is omitted, in order to reduce the filesize.
#' @param criterion Determines whether the model should be run. If an integer then this defines the threshold number of records (50 in Outhwaite et al 2019).
#' Other options are `EqualWt` or `HighSpec`, which define the application of "rules of thumb" defined in Pocock et al 2019. 
#' Defaults to 1, in which case the model is applied for so long there is a single record of the focal species.
#' @param provenance An optional text string allowing the user to identify the dataset.
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
#'   2. halfcauchy - preferred form, tested in Outhwaite et al (2018).
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
#'  \item{\code{"intercept"}}{ - No longer available. Includes an intercept term in the state and observation model.  By including intercept terms, the occupancy and detection probabilities in each year are centred on an overall mean level.}
#'  \item{\code{"centering"}}{ - No longer available. Includes hierarchical centering of the model parameters.   Centring does not change the model explicitly but writes it in a way that allows parameter estimates to be updated simultaneously.}
#' }
#' These options are provided as a vector of characters, e.g. \code{modeltype = c('indran', 'halfcauchy', 'catlistlength')}
#' 
#' @return A list including the model, JAGS model output, the path of the model file used and information on the number of iterations, first year, last year, etc.
#' Key aspects of the model output include:
#' \itemize{
#'  \item{\code{"out$model"}}{ - The model used as provided to JAGS. Also contained is a list of fully observed variables. These are those listed in the BUGS data.}
#'  \item{\code{"out$BUGSoutput$n.chains"}}{ - The number of Markov chains ran in the MCMC simulations.}
#'  \item{\code{"out$BUGSoutput$n.iter"}}{ - The total number of iterations per chain.}
#'  \item{\code{"out$BUGSoutput$n.burnin"}}{ - The number of interations discarded from the start as a burn-in period.}
#'  \item{\code{"out$BUGSoutput$n.thin"}}{ - The thinning rate used. For example a thinning rate of 3 retains only every third iteration. This is used to reduce autocorrelation.}
#'  \item{\code{"out$BUGSoutput$n.keep"}}{ - The number of iterations kept per chain. This is the total number of iterations minus the burn-in then divided by the thinning rate.}
#'  \item{\code{"out$BUGSoutput$n.sims"}}{ - The total number of iterations kept.}
#'  \item{\code{"out$BUGSoutput$summary"}}{ - A summary table of the monitored parameter. The posterior distribution for each parameter is summaried with the mean, standard deviation, various credible intervals, a formal convergence metric (Rhat), and a measure of effective sample size (n.eff).}
#'  \item{\code{"out$BUGSoutput$mean"}}{ - the mean values for all monitored parameters}
#'  \item{\code{"out$BUGSoutput$sd"}}{ - the standard deviation values for all monitored parameters}
#'  \item{\code{"out$BUGSoutput$median"}}{ - the median values for all monitored parameters}
#'  \item{\code{"out$parameters.to.save"}}{ - The names of all monitored parameters.}
#'  \item{\code{"out$BUGSoutput$model.file"}}{ - The user provided or temporary generated model file detailing the occupancy model.}
#'  \item{\code{"out$n.iter"}}{ - The total number of interations per chain.}
#'  \item{\code{"out$DIC"}}{ - Whether the Deviance Information Criterion (DIC) is calculated.}
#'  \item{\code{"out$BUGSoutput$sims.list"}}{ - A list of the posterior distribution for each monitored parameter. Use sims.array and sims.matrix if a different format of the posteriors is desired.}
#'  \item{\code{"out$SPP_NAME"}}{ - The name of the study species.}
#'  \item{\code{"out$min_year"}}{ - First year of data included in the occupancy model run.}
#'  \item{\code{"out$max_year"}}{ - Final year of data included in the occupancy model run or final year specified by the user.}
#'  \item{\code{"out$nsite"}}{ - The number of unique sites included in the occupancy model run.}
#'  \item{\code{"out$nvisits"}}{ - The number of unique visits included int he occupancy model run.}
#'  \item{\code{"out$species_sites"}}{ - The number of unique sites the species of interest was recorded in.}
#'  \item{\code{"out$species_observations"}}{ - The number of unique records for the species of interest.}
#'  \item{\code{"out$regions"}}{ - The names of the regions included in the model run.}
#'  \item{\code{"out$region_aggs"}}{ - The names of the region aggregates included in the model run.}
#'  \item{\code{"out$nsites_region"}}{ - Named vector containing the number of sites in each region included in the occupancy model run.}
#' }
#'
#' @keywords trends, species, distribution, occupancy, bayesian, modeling
#' @references Isaac, N.J.B., van Strien, A.J., August, T.A., de Zeeuw, M.P. and Roy, D.B. (2014).
#'             Statistics for citizen science: extracting signals of change from noisy ecological data.
#'             \emph{Methods in Ecology and Evolution}, 5: 1052-1060.
#' @references Outhwaite, C.L., Chandler, R.E., Powney, G.D., Collen, B., Gregory, R.D. & Isaac, N.J.B. (2018).
#'             Prior specification in Bayesian occupancy modelling improves analysis of species occurrence data. 
#'             \emph{Ecological Indicators}, 93: 333-343.
#' @references Pocock, Logie, Isaac, Outhwaite & August. Rapid assessment of the suitability of multi-species citizen science datasets for occupancy trend analysis. \emph{bioRxiv} 813626 (2019) doi:10.1101/813626.
#'             
#' @examples
#' \dontrun{
#' 
#' set.seed(123)
#' 
#' # Create data
#' n <- 15000 #size of dataset
#' nyear <- 20 # number of years in data
#' nSamples <- 100 # set number of dates
#' nSites <- 50 # set number of sites
#' 
#' # Create somes dates
#' first <- as.Date(strptime("2010/01/01", format="%Y/%m/%d")) 
#' last <- as.Date(strptime(paste(2010+(nyear-1),"/12/31", sep=''), format="%Y/%m/%d")) 
#' dt <- last-first 
#' rDates <- first + (runif(nSamples)*dt)
#' 
#' # taxa are set as random letters
#' taxa <- sample(letters, size = n, TRUE)
#' 
#' # sites are visited randomly
#' site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' survey <- sample(rDates, size = n, TRUE)
#'
#' # Format the data
#' visitData <- formatOccData(taxa = taxa, site = site, survey = survey)
#'
#' # run the model with these data for one species (very small number of iterations)
#' results <- occDetFunc(taxa_name = taxa[1],
#'                       n_iterations = 50,
#'                       burnin = 15, 
#'                       occDetdata = visitData$occDetdata,
#'                       spp_vis = visitData$spp_vis,
#'                       write_results = FALSE,
#'                       provenance  = "sparta test dataset")
#' }
#' @export
#' @importFrom reshape2 acast
#' @import LearnBayes

occDetFunc <- function (taxa_name, occDetdata, spp_vis, n_iterations = 5000, nyr = 2,
                        burnin = 1500, thinning = 3, n_chains = 3, write_results = TRUE,
                        output_dir = getwd(),  modeltype = 'sparta', max_year = NULL, 
                        seed = NULL, model.function = NULL, regional_codes = NULL,
                        region_aggs = NULL, additional.parameters = NULL,
                        additional.BUGS.elements = NULL, additional.init.values = NULL,
                        return_data = FALSE, criterion = 1, provenance = NULL, saveMatrix = FALSE){
  
  ################## BASIC CHECKS
  # first run the error checks
  errorChecks(n_iterations = n_iterations, burnin = burnin,
              thinning = thinning, n_chains = n_chains, seed = seed)
  
  # Set seed for repeatability
  if(!is.null(seed)) set.seed(seed)
  
  # Check the taxa_name is one of my species
  if(!taxa_name %in% colnames(spp_vis)) stop('taxa_name is not the name of a taxa in spp_vis')

  # only include sites which have more than nyr of records
  yps <- rowSums(acast(occDetdata, site ~ TP, length, value.var = 'L') > 0)
  sites_to_include <- names(yps[yps >= nyr])
  
  # strip out the visits to sites that were visited in just one year
  i <- occDetdata$site %in% sites_to_include
  
  if(sum(i) > 0){
    occDetdata <- occDetdata[i,]
    spp_vis <- spp_vis[i,]
  } else stop(paste0("There are no sites visited in at least ", nyr, " years."))

  # calcluate a set of data metrics for this species
  data_Metrics <- dataMetrics(sp = taxa_name, 
                                formattedData = list(occDetdata=occDetdata, spp_vis=spp_vis))

  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  {
      if(is.numeric(x)) abs(x - round(x)) < tol
      else FALSE
      }

  # check there is enough data to run a model. If so, proceed with the main event
  if(is.wholenumber(criterion)) {
      # the criterion is a whole number. this defines the number of records
      # check whether the number of records meets this value
      proceed <- sum(spp_vis[,taxa_name]) >= criterion
  } else if(criterion == "EqualWt") {
      proceed <- applyRuleOfThumb(data_Metrics, "EqualWt")
  } else if(criterion == "HighSpec") {
      proceed <- applyRuleOfThumb(data_Metrics, "HighSpec")
  } else
      stop("Criterion must be either an integer, `EqualWt` or `HighSpec`")

  
  if(!proceed){
    # there is not enough data: set the outputs accordingly
    bugs_data <- list(y=0,nsite=0,nvisit=0)
    BD_MD <- error_status <- site_match <- modelcode <- NA
    warning(paste(taxa_name,
                  "has insufficient data after site filtering. Either decrease nyr or change the criterion"))
    out <- list(message = "No model run: insufficient data")    
  } else {
    # There is enough data: we can proceed with the main event
    
    # Check if R2jags is installed
    if (!requireNamespace("R2jags", quietly = TRUE)) {
      stop("Package 'R2jags' is needed for the 'occDetModel' function to work. Please insatll this from CRAN. You will also be required to install JAGS, which you can download from https://sourceforge.net/projects/mcmc-jags/files/JAGS/",
           call. = FALSE)
    }
    
    # If doing regional we take control of model specification
    if(!is.null(regional_codes)){
      oldmodeltype <- modeltype
      modeltype <-c('ranwalk', 'halfcauchy',
                    c('jul_date', 'catlistlength')[c('jul_date', 'catlistlength') %in% modeltype])
      if(!all(oldmodeltype %in% modeltype))
        message('When using regional data the model specification will be set to ranwalk, halfcauchy. jul_date and catlistlength can still be specified by the user')
    }  
    
    if(!'catlistlength' %in% modeltype & !'nolistlength' %in% modeltype){
      modeltype <- c(modeltype, 'contlistlength')
    } 
    if(!any(c('catlistlength', 'nolistlength', 'contlistlength') %in% modeltype)){
      stop('modeltype should contain one of "catlistlength", "nolistlength", "contlistlength",
           which specify the list-length effect to be included')
    }
    
    # Do we have JAGS installed - this works only on windows
    if(.Platform$OS.type == "windows"){
      JAGS_test <- Sys.which(names = 'jags-terminal.exe')
      if(JAGS_test[[1]] == '') stop('R cannot find jags-terminal.exe, check that you have installed JAGS')
    }
    
    # Add the focal column (was the species recorded on the visit?). Use the spp_vis dataframe to extract this info
    nrow1 <- nrow(occDetdata)
    occDetdata <- merge(occDetdata, spp_vis[,c("visit", taxa_name)])
    
    if(nrow1 != nrow(occDetdata)) stop('some visits have been lost')
    
    names(occDetdata)[names(occDetdata) == taxa_name] <- "focal"
    
    # If we are using regional codes do some checks
    if(!is.null(regional_codes)){
      if(!inherits(regional_codes, 'data.frame')) stop("regional_codes should be a data.frame")
      
      # check whether there is a column called "site". 
      #If not, let's assume that the site column is the first in the dataframe
      #NB previous behaviour was to assume *both* that it was column 1 and named 'site'
      if(!"site" %in% names(regional_codes)) {
        warning(paste0("renaming ", names(regional_codes)[1], " as 'site'"))
        names(regional_codes)[1] <- "site" 
      }
      
      # remove locations that are not in the data
      abs_sites <- as.character(regional_codes$site)[!as.character(regional_codes$site) %in% as.character(occDetdata$site)]
      if(length(abs_sites) > 0){
        warning(paste(length(abs_sites), 'sites are in regional_codes but not in occurrence data'))
      }
      if(any(is.na(regional_codes))){
        warning(paste(sum(is.na(regional_codes)),
                      "NAs are present in regional_codes, these will be replaced with 0s"))
        regional_codes[is.na(regional_codes)] <- 0
      }
      
      sites_no_region <- as.character(regional_codes$site[rowSums(regional_codes[,2:ncol(regional_codes)]) == 0])
      sites_multi_region <- as.character(regional_codes$site[rowSums(regional_codes[,2:ncol(regional_codes)]) > 1])
      site_counts <- table(regional_codes[,1])
      sites_multi_row <- names(site_counts[site_counts > 1])
      
      if(length(sites_no_region) > 0) 
        warning(paste(length(sites_no_region), 'sites are not assigned to a region in regional_codes and will be removed'))
      if(length(sites_multi_region) > 0) 
        warning(paste(length(sites_multi_region), 'sites are assigned to more than one region in regional_codes and will be removed'))
      if(length(sites_multi_row) > 0)
        warning(paste(length(bad_sites), 'site(s) are present in more than one region and will be removed'))
      
      # finally check that every site with species data also has a region
      sites_no_region2 <- setdiff(sites_to_include, as.character(regional_codes$site))
      if(length(sites_no_region2) >= 1) 
        warning(paste(length(sites_no_region2), 'sites are in occurrence data but not in regional data and will be removed'))
      
      # strip these same sites out of the occDetdata & the regional codes
      bad_sites <- unique(c(abs_sites, sites_multi_row, sites_multi_region, sites_no_region, sites_no_region2))
      regional_codes <- subset(regional_codes, !site %in% bad_sites)
      occDetdata <- subset(occDetdata, !site %in% bad_sites)
    }
        
    # If we are using regional aggregates do some checks
    if(!is.null(region_aggs)){
      if(is.null(regional_codes)) stop('Cannot use regional aggregates if regional_codes is not supplied')
      stopifnot(inherits(region_aggs, 'list'))
      if(!all(unique(unlist(region_aggs)) %in% tail(colnames(regional_codes), -1))){
        stop(paste0('Aggregate members [',
                    paste(unique(unlist(region_aggs))[!unique(unlist(region_aggs)) %in% tail(colnames(regional_codes), -1)],
                          collapse = ', '),
                    '] not in regional_codes column names [',
                    paste(tail(colnames(regional_codes), -1),
                          collapse = ', '),
                    ']')) 
      }
    }
    
    # look for missing years before time frame can be extended using max_year parameter
    years <- (max(occDetdata$TP) - min(occDetdata$TP))+1
    if(length(unique(occDetdata$TP)) != years) {
      # find out which years have no data
      missing_yrs <-  with(occDetdata, setdiff(min(TP):max(TP), unique(TP)))
      if(length(missing_yrs) ==1)
        error_msg <- paste0('There are no visits in year ', missing_yrs,'. This means there is no data, for any species in this year. BUGS cannot run if there is a year with no data. Quitting...')
      else 
        error_msg <- paste0('There are ', length(missing_yrs),' years with no visits, including ', missing_yrs[1],'. This means there is no data, for any species in these years. BUGS cannot run if any year has no data. Quitting...')
      stop(error_msg)
    }

    # year and site need to be numeric starting from 1 to length of them.  This is due to the way the bugs code is written
    nsite <- length(unique(occDetdata$site))
    site_match <- data.frame(name = unique(occDetdata$site), id = 1:nsite)
    occDetdata <- merge(occDetdata, site_match, by.x='site', by.y="name")
    
    # need to get a measure of whether the species was on that site in that year, unequivocally, in zst
    zst <- acast(occDetdata, id ~ factor(TP), value.var = 'focal', max, fill = 0) # initial values for the latent state = observed state

    # Calculate min year. We're doing it now as it's fine if we've dropped the first year(s) of data and nothing in the middle
    min_year <- min(occDetdata$TP)
    
    # if the max_year is not null, edit the zst table to add the additional years required
    if(!is.null(max_year)){
      
      # check that max_year is a numeric value
      if(!is.numeric(max_year)) stop('max_year should be a numeric value')
      
      # check that max_year is greater than the final year of the dataset
      if(max_year <= max(occDetdata$TP)) stop('max_year should be greater than the final year of available data')
      
      nTP <- max_year - min_year + 1
      
      # if nyear is greater than the number of years due to different specification of max_year, 
      # add on additional columns to zst so that inital values can be create for these years.
      
      if(nTP > ncol(zst)){
        # work out how many columns need to be added
        to_add <- nTP - ncol(zst)
        zst <- cbind(zst, matrix(0, ncol = to_add, nrow = nrow(zst)))
        # add column names
        colnames(zst) <- 1:nTP 
      }
      
      # if a value has not been selected for max_year then continue as before
    }else{
      # record the max year
      max_year <- max(occDetdata$TP)
      nTP <- max_year - min_year + 1
    }
    
    # look for time periods with no data
    if(length(unique(occDetdata$TP)) != nTP) stop('It looks like you have time periods with no data. This will crash BUGS')
    
    # TP and site need to be numeric starting from 1 to length of them.  This is due to the way the bugs code is written
    occDetdata$TP <- occDetdata$TP - min(occDetdata$TP) + 1
  
    # Parameter you wish to monitor, shown in the output
    parameters <- c("psi.fs", "tau2", "tau.lp", "alpha.p", "a")
    
    # If ranwalk + halfcauchy monitor mu.lp 
    if(all(c('ranwalk', 'halfcauchy') %in% modeltype)){
      if(!'centering' %in% tolower(modeltype) & !'intercept' %in% tolower(modeltype)){
        parameters <- c(parameters, "mu.lp")
      }
    }
    
    # If sparta monitor mu.lp 
    if('sparta' %in% tolower(modeltype)) {
      parameters <- c(parameters, "mu.lp")
    }
    
    # Add user specified paramters if given
    if(!is.null(additional.parameters)) parameters <- c(parameters, additional.parameters)
    
    # Add parameters for each of the model types
    for(ptype in modeltype){
      parameters <- getParameters(parameters, modeltype = ptype)
    }
    
    # add parameters for regions
    if(!is.null(regional_codes)){
      # remove spaces from region names, then extract them
      colnames(regional_codes)[-1] <- gsub(' ', '_', colnames(regional_codes)[-1])
      region_names <- colnames(regional_codes)[-1]      
      
      parameters <- c(parameters,
                      paste0("psi.fs.r_", region_names),
                      paste0("a_", region_names))
      # ignore some parameters not used in regions model
      parameters <- parameters[!parameters %in% c('a')]
    }
    
    # add parameters for regional aggregates
    if(!is.null(region_aggs)){
      parameters <- c(parameters, paste0('psi.fs.r_', names(region_aggs)))
    }
    
    # now assemble the bugs_data and related objects
    # HERE IS THE BUGS DATA
    bugs_data <- with(occDetdata, 
                      list(y = as.numeric(focal), Year = TP, Site = id, 
                           nyear = nTP, nsite = nsite, nvisit = nrow(occDetdata)))

    # temporary test
    if(max(occDetdata$id) != bugs_data$nsite) stop(paste0("Site idenitifier exceeds nsite (",
                                                          max(occDetdata$id), nsite,")"))
    
    
    for(btype in modeltype){ # one call per element of modeltype: each adds a section
      bugs_data <- getBugsData(bugs_data, modeltype = btype,
                               occDetData = occDetdata)
    }

    # Add additional elements if specified
    if(!is.null(additional.BUGS.elements)){
      if(!is.list(additional.BUGS.elements)){
        stop("additional.BUGS.elements must be a list")
      } else {
        bugs_data <- c(bugs_data, additional.BUGS.elements)
      }
    }
    
    # make a copy of the bugs_data to calculate metadata from
    bugs_data_copy <- with(occDetdata, data.frame(y = as.numeric(focal), year = TP, site = site))
    BD_MD <- list()
    
    # Add regional elements to bugs data
    if(!is.null(regional_codes)){

      # removed unwanted bugs elements
      bugs_data <- bugs_data[!names(bugs_data) %in% c('psi0.a', 'psi0.b')]
      
      # expand the lookup table to include regions
      regional_lookup <- merge(regional_codes, site_match, by.y="name", by.x="site")
      
      zero_sites <- NULL
      for(region in region_names){
        if(sum(regional_codes[ , region]) != 0){
          bugs_data[paste0('r_', region)] <- list(regional_lookup[order(regional_lookup$id),region])
          bugs_data[paste0('nsite_r_', region)] <- sum(regional_codes[, region])
        } else {
          zero_sites <- c(zero_sites, region)
        }
      }
      
      if(!is.null(zero_sites)){
        warning(paste('The following regions have no data and should not be modelled:',
                      paste(zero_sites, collapse = ', '),
                      '- These regions will not be included in the model'))
        # remove parameters
        parameters <- parameters[!parameters %in% c(paste0("psi.fs.r_", zero_sites),
                                                    paste0("a_", zero_sites))]
        # remove regions for regions_codes
        regional_codes <- regional_codes[ ,!colnames(regional_codes) %in% zero_sites]
        region_names <- setdiff(region_names, zero_sites)

        # remove region aggregates
        rem_aggs <- unlist(lapply(region_aggs, FUN = function(x) any(zero_sites %in% x)))
        rem_aggs_names <- names(region_aggs)[rem_aggs]
        
        # remove aggs if you need to
        if(length(rem_aggs_names) > 0){
          warning(paste('The following region aggregates have to be removed as they contain a region with no data:',
                        paste(rem_aggs_names, collapse = ', '),
                        '- These region aggregates will not be included in the model'))
          region_aggs <- region_aggs[!names(region_aggs) %in% rem_aggs_names]
          parameters <- parameters[!parameters %in% paste0('psi.fs.r_', rem_aggs_names)]
        }
      } 

      regions_years <- list()
      regions_nobs <- list()
      regions_sites <-list()
  
      bugs_data_copy <- merge(bugs_data_copy, regional_codes, all.x = TRUE)
      
      # add regional codes to this copy and get n_obs, max and min years and year gaps for each region
      for(region_name in region_names){
        
        regions_nobs[paste0('n_obs_','r_', region_name)] <- sum(bugs_data_copy$y * bugs_data_copy[,region_name])
        regions_sites[paste0('n_sites_','r_', region_name)] <- sum(bugs_data_copy[,region_name])
        current_r <- bugs_data_copy$y * bugs_data_copy[,region_name] * bugs_data_copy$year
        current_r <- subset(current_r,current_r !=0)
        
        if(length(current_r) > 2){
          
          current_rmin <- (min_year-1) + min(current_r)
          current_rmax <- (min_year-1) + max(current_r)
          regions_years[paste0('min_year_data_','r_', region_name)] <- current_rmin
          regions_years[paste0('max_year_data_','r_', region_name)] <- current_rmax
          current_datagaps <- dataGaps(current_r, min_year, max_year, current_rmin, current_rmax)
          regions_years[paste0('gap_start_','r_', region_name)] <- current_datagaps$gap_start
          regions_years[paste0('gap_end_','r_', region_name)] <- current_datagaps$gap_end
          regions_years[paste0('gap_middle_','r_', region_name)] <- current_datagaps$gap_middle
        
        } else if(length(current_r) == 1) {
          
          current_rmin <- (min_year-1) + min(current_r)
          current_rmax <- (min_year-1) + max(current_r)
          regions_years[paste0('min_year_data_','r_', region_name)] <- current_rmin
          regions_years[paste0('max_year_data_','r_', region_name)] <- current_rmax
          current_datagaps <- dataGaps(current_r, min_year, max_year, current_rmin, current_rmax)
          regions_years[paste0('gap_start_','r_', region_name)] <- current_datagaps$gap_start
          regions_years[paste0('gap_end_','r_', region_name)] <- current_datagaps$gap_end
          regions_years[paste0('gap_middle_','r_', region_name)] <- NA
          
        } else if(length(current_r) < 1){
          
          regions_years[paste0('min_year_data_','r_', region_name)] <- NA
          regions_years[paste0('max_year_data_','r_', region_name)] <- NA
          regions_years[paste0('gap_start_','r_', region_name)] <- NA
          regions_years[paste0('gap_end_','r_', region_name)] <- NA
          regions_years[paste0('gap_middle_','r_', region_name)] <- NA
          
        }
      }
    }
      
    # add max and min data years for the whole dataset
    all_years_data <- bugs_data_copy$y * bugs_data_copy$year
    all_years_data <- subset(all_years_data, all_years_data !=0)
    BD_MD$min_year_data <- (min_year-1) + min(all_years_data)
    BD_MD$max_year_data <- (min_year-1) + max(all_years_data)
    
    # use these to find year gap data
    BD_MD$yeargaps<-dataGaps(all_years_data, min_year, max_year, BD_MD$min_year_data, BD_MD$max_year_data)
    
    
    initiate <- function(z, nTP, bugs_data) {
      init <- list (z = z,
                    alpha.p = rep(runif(1, -2, 2),
                                  nTP),
                    a = rep(runif(1, -2, 2), nTP),
                    eta = rep(runif(1, -2, 2), bugs_data$nsite))
      
      # add extra init values if needed
      for(itype in modeltype){
        init <- getInitValues(init, modeltype = itype)
      }
      
      # if ranwalk + centreing a -> aa
      if(all(c('ranwalk', 'centering') %in% modeltype)){
        names(init)[names(init) == 'a'] <- 'aa'
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
    init.vals <- replicate(n_chains, initiate(z = zst,
                                              nTP = nTP,
                                              bugs_data = bugs_data),
                           simplify = F)
    
    # modify initial values for regional model
    if(!is.null(regional_codes)){
      # remove initial values for a and psi
      init.vals <- lapply(init.vals, FUN = function(x){
        x[!names(x) %in% c('psi0', 'a')]
      })
    }
    
    # Select the correct model file
    if(is.null(model.function)){
      model.file <- getModelFile(modeltype,
                                 regional_codes = regional_codes,
                                 region_aggs = region_aggs)
    } else {
      cat('Using user model.function')
      model.file <- model.function
    }
    
    modelcode <- paste(readLines(model.file), collapse = '\n')
    
    ### REVIEW CODE
    cat('#### PLEASE REVIEW THE BELOW ####\n\n')
    cat('Your model settings:', paste(modeltype, collapse = ', '))
    cat('\n\nModel File:\n\n')
    cat(modelcode)
    cat('\n\nbugs_data:\n\n')
    cat(str(bugs_data))
    cat('\n\ninit.vals:\n\n')
    cat(str(init.vals))
    cat('\n\nparameters:\n\n')
    cat(parameters)
    ###
    
    if(identical(model.file, occDetBUGScode)){
      warning("You have selected a formulation with potentially informative priors that are subject to boundary effects (See Outhwaite et al 2018 for details). 
      This option is retained within sparta for backwards compatibility only: we strongly recommend that you do 
              not use this option for inferring changes in species' distributions")
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
    
    }
  }  # end of "if(proceed)"
  
    ########################################## Add metadata
    
    # calcluate number of site:year combinations with repeat visits (across the whole dataset)
    temp <- as.data.frame(with(occDetdata, table(site, TP)))$Freq
    prop_visits_repeated <- mean(temp[temp>0] > 1)

    if(is.null(attributes(occDetdata))){
      metadata <- list()
    } else if('metadata' %in% names(attributes(occDetdata))){
      metadata <- attr(occDetdata, 'metadata')
    } else {
      metadata <- list()
    }
    
    # get the sessionInfo and coerce into a useable format
    session.info <- sessionInfo()
    packages <- c(sapply(session.info[7][[1]], function(x) x$Version),
                  sapply(session.info[8][[1]], function(x) x$Version))
    
    MD <- list(method = 'sparta::occDetFunc',
               call =   call <- match.call(),
               date = Sys.Date(),
               user = Sys.info()['user'],
               summary = list(species = taxa_name,
                              n_sites = length(unique(occDetdata$site)),
                              n_years = length(unique(occDetdata$TP)),
                              n_visits = nrow(occDetdata),
                              n_obs = sum(occDetdata$focal),
                              n_species_sites <- length(unique(subset(occDetdata, focal=TRUE)$site)),
                              min_year_model = min_year,
                              max_year_model = max_year),
                gaps = ifelse(is.na(BD_MD), NA, list( 
                              min_year_data = BD_MD$min_year_data,
                              max_year_data = BD_MD$max_year_data,
                              gap_start = BD_MD$yeargaps$gap_start,
                              gap_end = BD_MD$yeargaps$gap_end,
                              gap_middle = BD_MD$yeargaps$gap_middle)),
               spp_Metrics = as.list(data_Metrics),
               dataset_Metrics = list(# dataset properties
                              totalObservations = sum(occDetdata$L),
                              propRepeats = prop_visits_repeated),
               provenance = provenance,
               output_path = ifelse(test = write_results,
                                    file.path(getwd(), output_dir, paste(taxa_name, ".rdata", sep = "")),
                                    NA),
               session_info = list(session.info[-c(7:8)],
                                   packages)
               )
    
    # add regional elements if applicable
    if(!is.null(regional_codes) & proceed){
      MD$summary$region_nobs <- regions_nobs
      MD$summary$region_years <- regions_years
      MD$summary$region_nsite <- regions_sites
    }else{
      MD$summary$region_nobs <- NA
      MD$summary$region_years <- NA
      MD$summary$region_nsite <- NA
    }
    
    # If the data coming in is the result of analysis we want to
    # append this data
    name <- 'analysis'
    i = 1
    while(name %in% names(metadata)){
      name <- paste0(name, i)
      i = i + 1 
    }
    
    metadata[name] <- list(MD)
    attr(out, 'metadata') <- metadata

    if(!saveMatrix) out$BUGSoutput$sims.matrix <- NULL
    
    out$SPP_NAME <- taxa_name
    out$min_year <- min_year
    out$max_year <- max_year
    out$sites_included <- ifelse(test = proceed, yes = site_match, no = NA)
    out$nsites <- bugs_data$nsite
    out$nvisits <- bugs_data$nvisit
    out$species_observations <- sum(bugs_data$y)
    out$sparta_version <- packages["sparta"]
    if(!is.null(regional_codes)) out$regions <- region_names
    if(!is.null(region_aggs)) out$region_aggs <- region_aggs
    if(return_data) out$bugs_data <- bugs_data
    attr(out, 'modeltype') <- modeltype
    attr(out, 'modelcode') <- modelcode
    class(out) <- 'occDet'
    if(write_results) save(out, file = file.path(output_dir, paste(taxa_name, ".rdata", sep = "")))  
    return(out)
  }  	
  