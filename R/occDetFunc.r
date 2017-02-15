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
#' @param seed numeric, uses \code{set.seed} to set the randon number seed. Setting
#'        this number ensures repeatabl analyses
#' @param model.function optionally a user defined BUGS model coded as a function (see \code{?jags},
#'        including the example there, for how this is done)
#' @param additional.parameters A character vector of additional parameters to monitor
#' @param additional.BUGS.elements A named list giving additioanl bugs elements passed 
#' to \code{R2jags::jags} 'data' argument
#' @param additional.init.values A named list giving user specified initial values to 
#' be added to the defaults.
#' @param return_data Logical, if \code{TRUE} (default) the bugs data object is returned with the data
#'
#' @details \code{modeltype} is used to choose the model as well as the initial values,
#' and the parameter to monitor. There are 9 elements that define models, however not
#' all combinations are available in sparta. You will get an error if you try and use
#' a combination that is not supported. There is usually a good reason why that
#' combination is not a good idea. Here are the model elements available.
#' \itemize{
#'  \item{\code{"sparta"}}{ - This uses the same model as in Isaac et al (2014)}
#'  \item{\code{"indran"}}{ - Here the prior for the year effect of the state model is modelled as a random effect.  This allows the model to adapt to interannual variability.}
#'  \item{\code{"inversegamma"}}{ - Includes inverse-gamma hyperpriors for random effects within the model}
#'  \item{\code{"intercept"}}{ - Includes an intercept term in the state and observation model.  By including intercept terms, the occupancy and detection probabilities in each year are centred on an overall mean level.}
#'  \item{\code{"centering"}}{ - Includes hierarchical centering of the model parameters.   Centring does not change the model explicitly but writes it in a way that allows parameter estimates to be updated simultaneously.}
#'  \item{\code{"ranwalk"}}{ - Here the prior for the year effect of the state model is modelled as a random walk.  Each estimate for the year effect is dependent on that of the previous year.}
#'  \item{\code{"halfcauchy"}}{ - Includes half-Cauchy hyperpriors for all random effects within the model.  The half-Cauchy is a special case of the Student’s t distribution with 1 degree of freedom. }
#'  \item{\code{"catlistlength"}}{ - This specifies that list length should be considered as a catagorical variable. There are 3 classes, lists of length 1, 2-3, and 4 and over. If not set list length is treated as continious.}
#'  \item{\code{"jul_date"}}{ - This adds Julian date to the model as a polynomial centered on the middle of the year. Note your data must include Julian day (use formatOccData(..., includeJDay = TRUE))}
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
#' # sites are visited randomly
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
                        model.function = NULL, regional_codes = NULL,
                        region_aggs = NULL, additional.parameters = NULL,
                        additional.BUGS.elements = NULL, additional.init.values = NULL,
                        return_data = FALSE){
  
  J_success <- requireNamespace("R2jags", quietly = TRUE)
  
  # test is R2jags installed if not error
  if(!J_success) stop('Please install the R2jags R package. This also requires you to in stall JAGS from http://sourceforge.net/projects/mcmc-jags/files/JAGS/')
  
  # If doing regional we take control of model specification
  if(!is.null(regional_codes)){
    message('When using regional data the model specification will be set to ranwalk, halfcauchy. jul_date and catlistlength can still be specified by the user')
    modeltype <-c('ranwalk', 'halfcauchy',
                  c('jul_date', 'catlistlength')[c('jul_date', 'catlistlength') %in% modeltype])
  }  
  
  if(!'catlistlength' %in% modeltype) modeltype <- c(modeltype, 'contlistlength')
  
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
  
  # If we are using regional codes do some checks
  if(!is.null(regional_codes)){
    if(!inherits(regional_codes, 'data.frame')) stop("regional_codes should be a data.frame")
    # remove locations that are not in the data
    abs_sites <- as.character(regional_codes[,1])[!as.character(regional_codes[,1]) %in% as.character(occDetdata$site)]
    if(length(abs_sites) > 0){
      warning(paste(length(abs_sites), 'sites are in regional_codes but not in occurrence data'))
      regional_codes <- regional_codes[as.character(regional_codes[,1]) %in% as.character(occDetdata$site),]
    }
    if(any(is.na(regional_codes))){
      warning(paste(sum(is.na(regional_codes)),
                    "NAs are present in regional_codes, these will be replaced with 0's"))
      regional_codes[is.na(regional_codes)] <- 0
    }
    
    sites_no_region <- regional_codes$site[rowSums(regional_codes[,2:ncol(regional_codes)]) == 0]
    sites_multi_region <- regional_codes$site[rowSums(regional_codes[,2:ncol(regional_codes)]) > 1]
    
    if(length(sites_no_region) > 0) stop(paste(length(sites_no_region), 'sites are not assigned to a region in regional_codes'))
    if(length(sites_multi_region) > 0) stop(paste(length(sites_multi_region), 'sites are assigned to more than one region in regional_codes'))
    
    site_counts <- table(regional_codes[,1])
    sites_multi_row <- names(site_counts[site_counts > 1])
    bad_sites <- unique(c(sites_multi_row, sites_multi_region))
    
    if(length(bad_sites) > 0){
      warning(paste(length(bad_sites), 'site(s) are present in more than one region and will be removed'))
      regional_codes <- regional_codes[!regional_codes[,1] %in% bad_sites, ]
    }
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
  
  # Record the max and min year
  min_year <- min(occDetdata$year)
  max_year <- max(occDetdata$year)
  
  # year and site need to be numeric starting from 1 to length of them.  This is due to the way the bugs code is written
  occDetdata$year <- occDetdata$year - min(occDetdata$year) + 1
  site_match <- data.frame(original_site = occDetdata$site, new_site_name = as.numeric(as.factor(occDetdata$site)))
  site_match <- unique(site_match)
  occDetdata$site <- as.numeric(as.factor(occDetdata$site))
  
  # Convert the regional table to these numeric versions of site names
  if(!is.null(regional_codes)){
    regional_codes$numeric_site_name <- site_match$new_site_name[match(x = as.character(regional_codes[,1]),
                                                             table = as.character(site_match$original_site))]
  }
  
  # need to get a measure of whether the species was on that site in that year, unequivocally, in zst
  zst <- acast(occDetdata, site ~ factor(year), value.var = 'focal', max, fill = 0) # initial values for the latent state = observed state
  nyear <- max_year - min_year + 1
  
  # look for missing years
  if(length(unique(occDetdata$year)) != nyear) stop('It looks like you have years with no data. This will crash BUGS')
  
  # Parameter you wish to monitor, shown in the output
  parameters <- c("psi.fs", "tau2", "tau.lp", "alpha.p", "a", "mu.lp")
  
  # If not sparta add monitoring for eta.psi0
  if(!'sparta' %in% tolower(modeltype)) parameters <- c(parameters, "eta.p0", "eta.psi0")
  
  # Add user specified paramters if given
  if(!is.null(additional.parameters)) parameters <- c(parameters, additional.parameters)
  
  # Add parameters for each of the model types
  for(ptype in modeltype){
    parameters <- getParameters(parameters, modeltype = ptype)
  }
  
  # add parameters for regions
  if(!is.null(regional_codes)){
    regions <- colnames(regional_codes)[2:(length(colnames(regional_codes))-1)]
    # remove spaces
    regions <- gsub(' ', '_', regions)
    parameters <- c(parameters,
                    paste0("psi.fs.r_", regions),
                    paste0("a_", regions))
    # ignore some parameters not used in regions model
    parameters <- parameters[!parameters %in% c('eta.psi0', 'a')]
  }
  
  # add parameters for regional aggregates
  if(!is.null(region_aggs)){
    parameters <- c(parameters, paste0('psi.fs.r_', names(region_aggs)))
  }
  
  # only include sites which have more than nyr of records
  # and are in the regional data if used
  yps <- rowSums(acast(occDetdata, site ~ year, length, value.var = 'L') > 0)
  sites_to_include <- names(yps[yps >= nyr])
  
  # If we are using regional data makes sure all 'good' sites
  # are in the regional data and and visa versa. Ensures datasets
  # line up.
  if(!is.null(regional_codes)){
    bad_sites_to_include <- sites_to_include[!sites_to_include %in% regional_codes$numeric_site_name]
    if(length(bad_sites_to_include) >= 1) warning(paste(length(bad_sites_to_include), 'sites are in occurrence data but not in regional data and will be removed'))
    sites_to_include <- sites_to_include[sites_to_include %in% regional_codes$numeric_site_name]
    regional_codes <- regional_codes[regional_codes$numeric_site_name %in% sites_to_include, ]
  }
  
  zst <- zst[row.names(zst) %in% sites_to_include,]
  i <- occDetdata$site %in% sites_to_include
  
  # now assemble the bugs_data and related objects
  #need to convert Site identities into row numbers
  site_to_row_lookup <- data.frame(site = as.integer(row.names(zst)),
                                   rownum = 1:nrow(zst)) 
  
  # HERE IS THE BUGS DATA
  bugs_data <- with(merge(occDetdata[i,], site_to_row_lookup), # adds rownum to occDetdata (used below)
                    list(y = as.numeric(focal), Year = year, Site = rownum, 
                         nyear = nyear, nsite = nrow(zst), nvisit = nrow(occDetdata[i,])))
  
  # added extra elements to bugs data if needed
  occDetData_temp <- merge(occDetdata[i,], site_to_row_lookup)

  for(btype in modeltype){
    bugs_data <- getBugsData(bugs_data, modeltype = btype,
                             occDetData = occDetData_temp)
  }
  
  rm(list = 'occDetData_temp')
  
  # Add additional elements if specified
  if(!is.null(additional.BUGS.elements)){
    if(!is.list(additional.BUGS.elements)){
      stop("additional.BUGS.elements must be a list")
    } else {
      bugs_data <- c(bugs_data, additional.BUGS.elements)
    }
  }
  
  # Add regional elements to bugs data
  if(!is.null(regional_codes)){
    # use site_to_row_lookup to get the correct site names
    regional_codes$rownum <- site_to_row_lookup$rownum[match(x = regional_codes$numeric_site_name, 
                                                             table = site_to_row_lookup$site)]
    
    # removed unwanted bugs elements
    bugs_data <- bugs_data[!names(bugs_data) %in% c('psi0.a', 'psi0.b')]
    
    zero_sites <- NULL
    for(region_name in colnames(regional_codes)[2:(ncol(regional_codes)-2)]){
      
      if(sum(regional_codes[ , region_name]) != 0){
      
        bugs_data[paste0('r_', region_name)] <- list(regional_codes[order(regional_codes$rownum), region_name])
        bugs_data[paste0('nsite_r_', region_name)] <- list(sum(regional_codes[order(regional_codes$rownum), region_name]))
        
      } else {
        
        zero_sites <- c(zero_sites, region_name)
        
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
  }
  
  initiate <- function(z, nyear, bugs_data) {
    init <- list (z = z,
                  alpha.p = rep(runif(1, -2, 2),
                                nyear),
                  a = rep(runif(1, -2, 2), nyear),
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
                                            nyear = nyear,
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
  
  ### REVIEW CODE
  cat('#### PLEASE REVIEW THE BELOW ####\n\n')
  cat('Your model settings:', paste(modeltype, collapse = ', '))
  cat('\n\nModel File:\n\n')
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
    out$nsites <- bugs_data$nsite
    out$nvisits <- bugs_data$nvisit
    out$species_sites <- length(unique(bugs_data$Site[bugs_data$y == 1]))
    out$species_observations <- sum(bugs_data$y)
    if(!is.null(regional_codes)) out$regions <- head(tail(colnames(regional_codes), -1), -2)
    if(!is.null(region_aggs)) out$region_aggs <- region_aggs
    if(return_data) out$bugs_data <- bugs_data
    class(out) <- 'occDet'
    if(write_results) save(out, file = file.path(output_dir, paste(taxa_name, ".rdata", sep = "")))  
    return(out)
  }  	
}