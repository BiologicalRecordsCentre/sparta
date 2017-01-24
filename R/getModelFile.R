getModelFile <- function(modeltype, regional_codes = NULL, region_aggs = NULL, verbose = FALSE){
  
  # first get all the available model files
  modelFiles <- list.files(system.file("models", package = "sparta"),
                           full.names = TRUE)
  # fileLocation <- '//nercwlctdb.ad.nerc.ac.uk/shared/SHARED1/PYWELL_SHARED/Pywell Projects/BRC/Charlie/0. Model Testing/2. Final models and function scripts'
  # modelFiles  <- list.files(path = fileLocation, full.names = TRUE, pattern = '^SPARTA_')
  
  # Now break them down
  mods <- gsub('^SPARTA_', '', basename(modelFiles))
  mods <- gsub('.txt', '', tolower(mods))
  mods <- strsplit(mods, split = '_')
  
  # match only attibutes that effect model file selection
  modeltype_file <- modeltype[!tolower(modeltype) %in% c('catlistlength', 'jul_date',
                                                         'contlistlength')]
  
  matches <- lapply(mods, FUN = function(x) all(modeltype_file %in% x) & all(x %in% modeltype_file))
  
  # No match
  if(all(!unlist(matches))){
    stop('There are no model files that match your combination: ',
         paste(modeltype_file, collapse = ', '))
  } else if(sum(unlist(matches) > 1)){
    stop('There is more than one model file that matches:',
         paste(mods[unlist(matches)]))
  } else if(sum(unlist(matches) == 1)){
    modelfile <- modelFiles[unlist(matches)]
    if(verbose) cat('Selected model file:', basename(modelfile), '\n')
  }
  
  ## read in and adapt model before parsing and returning as an expression ##
  modelScript <- readLines(modelfile)
  
  # If we are using regions we need to edit this
  if(!is.null(regional_codes)){
    
    regions <- colnames(regional_codes)[2:(length(colnames(regional_codes))-2)]
    # remove spaces
    regions <- gsub(' ', '_', regions)
    
    ## Priors ##
    # year 1 for each region 
    yr_1 <- paste(paste0('a_', regions, '[1] ~ dnorm(mu.a_', regions, ', 0.001)'))
    
    # hyper-priors for the first year for each region
    hp_yr1 <- paste(paste0('mu.a_', regions, ' ~ dnorm(0, 0.01)'))
    
    # Random walk prior for remaining years, 1 for each region
    rw <- c('for(t in 2:nyear){',
            paste0('  a_', regions, '[t] ', '~ dnorm(a_', regions, '[t-1], tau.a_', regions, ')'),
            '}')
    
    # tau halfcauchy
    tau <- paste(paste0('tau.a_', regions, ' ~ dt(0, 1, 1)T(0,)'))
    
    # State model
    state <- c('for (i in 1:nsite){',
               '  for (t in 1:nyear){',
               '    z[i,t] ~ dbern(muZ[i,t])',
               paste('    logit(muZ[i,t]) <-',
                     paste0('(a_', regions, '[t]*r_', regions, '[i])', 
                            collapse = ' + '),
                            '+ eta[i]'),
               '  }',
               '}')  
    
    # Derived parameters - regions
    derived_regions <- unlist(lapply(regions, FUN = function(region){
      c('for (t in 1:nyear) {',
        paste0('  psi.fs.r_', region, '[t] <- sum(z[1:nsite,t]*r_', region, '[1:nsite])/nsite_r_', region),
        '}')
    }))
    
    # Derived parameters - aggregates
    derived_aggs <- unlist(lapply(names(region_aggs), FUN = function(name, region_aggs){
      c('for (t in 1:nyear) {',
        paste0('  psi.fs.r_', name,
               '[t] <- sum(',
               paste0('(z[1:nsite,t]*r_', region_aggs[[name]], '[1:nsite])', collapse = ', '),
               ') / sum(',
               paste0('nsite_r_', region_aggs[[name]], collapse = ', '),
               ')'),
        '}')
    }, region_aggs))
      
      
    ## Bring them together ##
    modelScript <- c('# State model',
                     state,
                     '',
                     '# State Priors',
                     yr_1,
                     '',
                     hp_yr1,
                     '',
                     rw,
                     '',
                     tau,
                     '',
                     modelScript[18:length(modelScript)],
                     '',
                     derived_regions,
                     '',
                     derived_aggs,
                     ''
                     )
    
  }
  
  # Construct the observation model on the fly
  obsModel <- getObsModel(modeltype, verbose)

  tFile <- tempfile(fileext = '.txt')
  
  fullModel <- paste('model{\n',
                    paste(modelScript, collapse = '\n'),
                    obsModel,
                   '}')
  
  write.table(x = fullModel, file = tFile,
              row.names = FALSE, col.names = FALSE,
              quote = FALSE)
  
  return(tFile)
  
}