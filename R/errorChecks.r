#' @importFrom dplyr distinct

errorChecks <- function(taxa = NULL, site = NULL, survey = NULL, closure_period = NULL, time_period = NULL, 
                        startDate = NULL, endDate = NULL, Date = NULL, 
                        time_periodsDF = NULL, dist = NULL, sim = NULL,
                        dist_sub = NULL, sim_sub = NULL, minSite = NULL, useIterations = NULL,
                        iterations = NULL, overdispersion = NULL, verbose = NULL,
                        list_length = NULL, site_effect = NULL, family = NULL,
                        n_iterations = NULL, burnin = NULL, thinning = NULL,
                        n_chains = NULL, seed = NULL, year_col = NULL, site_col = NULL,
                        sp_col = NULL, start_col = NULL, end_col = NULL, phi = NULL,
                        alpha = NULL, non_benchmark_sp = NULL, fres_site_filter = NULL,
                        time_periods = NULL, frespath = NULL, species_to_include = NULL){
  
  # Create a list of all non-null arguements that should be of equal length
  valid_argumentsTEMP <- list(taxa=taxa,
                          site=site,
                          survey=survey,
                          closure_period=closure_period,
                          time_period=time_period,
                          startDate=startDate,
                          endDate=endDate)
  valid_arguments <- valid_argumentsTEMP[!unlist(lapply(valid_argumentsTEMP, FUN = is.null))]
  
  # Check these are all the same length
  if(length(valid_arguments) > 0){
    lengths <- sapply(valid_arguments, length)
    # This tests if all are the same
    if(abs(max(lengths) - min(lengths)) > .Machine$double.eps ^ 0.5){
      stop(paste('The following arguements are not of equal length:', paste(names(valid_arguments), collapse = ', ')))
    }
  }
  
  if(!is.null(taxa) & !is.null(site) & !is.null(survey)){
    
    df <- data.frame(taxa, site, survey)
    NR1 <- nrow(df)
    NR2 <- nrow(distinct(df))
    
    if(NR1 != NR2) warning(paste(NR1 - NR2, 'out of', NR1, 'observations will be removed as duplicates'))
    
  }
  
  if(!is.null(taxa) & !is.null(site) & !is.null(time_period)){
    
    df <- data.frame(taxa, site, time_period)
    NR1 <- nrow(df)
    NR2 <- nrow(distinct(df))
    
    if(NR1 != NR2) warning(paste(NR1 - NR2, 'out of', NR1, 'observations will be removed as duplicates'))
    
  }
  
  ### Checks for taxa ###
  if(!is.null(taxa)){    
    # Make sure there are no NAs
    if(!all(!is.na(taxa))) stop('taxa must not contain NAs')    
  }
  
  ### Checks for site ###
  if(!is.null(site)){    
    # Make sure there are no NAs
    if(!all(!is.na(site))) stop('site must not contain NAs')    
  }
  
  ### Checks for time_period ###
  if(!is.null(time_period)){    
    # Make sure there are no NAs
    if(!all(!is.na(time_period))) stop('time_period must not contain NAs')    
  }
  
  ### Checks for startDate ###
  if(!is.null(startDate)){
    if(!'POSIXct' %in% class(startDate) & !'Date' %in% class(startDate)){
      stop(paste('startDate is not in a date format. This should be of class "Date" or "POSIXct"'))
    }
    # Make sure there are no NAs
    if(!all(!is.na(startDate))) stop('startDate must not contain NAs')
  }
  
  ### Checks for Date ###
  if(!is.null(Date)){
    if(!'POSIXct' %in% class(Date) & !'Date' %in% class(Date) & !'data.frame' %in% class(Date)){
      stop(paste('Date must be a data.frame or date vector'))
    }
    # Make sure there are no NAs
    if(!all(!is.na(Date))) stop('Date must not contain NAs')
  }
  
  ### Checks for endDate ###
  if(!is.null(endDate)){
    if(!'POSIXct' %in% class(endDate) & !'Date' %in% class(endDate)){
      stop(paste('endDate is not in a date format. This should be of class "Date" or "POSIXct"'))
    }
    # Make sure there are no NAs
    if(!all(!is.na(endDate))) stop('endDate must not contain NAs')
  }
  
  ### Checks for time_periodsDF ###
  if(!is.null(time_periodsDF)){
    # Ensure end year is after start year
    if(any(time_periodsDF[,2] < time_periodsDF[,1])) stop('In time_periods end years must be greater than or equal to start years')
    
    # Ensure year ranges don't overlap
    starts <- tail(time_periodsDF$start, -1)
    ends <- head(time_periodsDF$end, -1)
    if(any(ends > starts)) stop('In time_periods year ranges cannot overlap')  
  }
  
  ### Checks for dist ###
  if(!is.null(dist)){
    
    if(class(dist) != 'data.frame') stop('dist must be a data.frame')
    if(ncol(dist) != 3) stop('dist must have three columns') 
    if(!class(dist[,3]) %in% c('numeric', 'integer')) stop('the value column in dist must be an integer or numeric')
    
    # Check distance table contains all combinations of sites
    sites <- unique(c(as.character(dist[,1]), as.character(dist[,2])))
    combinations_temp <- merge(sites, sites)
    all_combinations <- paste(combinations_temp[,1],combinations_temp[,2])
    data_combinations <- paste(dist[,1],dist[,2])
    if(!all(all_combinations %in% data_combinations)){
      stop('dist table does not include all possible combinations of sites')
    }    
  }
  
  ### Checks for sim ###
  if(!is.null(sim)){
    
    if(class(sim) != 'data.frame') stop('sim must be a data.frame')
    if(!all(lapply(sim[,2:ncol(sim)], class) %in% c('numeric', 'integer'))) stop('the values in sim must be integers or numeric')
        
  }
  
  ### Checks for sim_sub and dist_sub ###
  if(!is.null(sim_sub) & !is.null(dist_sub)){
    
    if(!class(dist_sub) %in% c('numeric', 'integer')) stop('dist_sub must be integer or numeric')
    if(!class(sim_sub) %in% c('numeric', 'integer')) stop('sim_sub must be integer or numeric')
    if(dist_sub <= sim_sub) stop("'dist_sub' cannot be smaller than or equal to 'sim_sub'")
    
  }
  
  ### checks for minSite ###
  if(!is.null(minSite)){
  
    if(!class(minSite) %in% c('numeric', 'integer')) stop('minSite must be numeric or integer')
  
  }
  
  ### checks for useIterations ###
  if(!is.null(useIterations)){
    
    if(class(useIterations) != 'logical') stop('useIterations must be logical')
  
  }
  
  ### checks for iterations ###
  if(!is.null(iterations)){
   
    if(!class(iterations) %in% c('numeric', 'integer')) stop('iterations must be numeric or integer')
        
  }
  
  ### checks for overdispersion ###
  if(!is.null(overdispersion)){
    
    if(class(overdispersion) != 'logical') stop('overdispersion must be logical')
    
  }
  
  ### checks for verbose ###
  if(!is.null(verbose)){
    
    if(class(verbose) != 'logical') stop('verbose must be logical')
    
  }
  
  ### checks for list_length ###
  if(!is.null(list_length)){
    
    if(class(list_length) != 'logical') stop('list_length must be logical')
    
  }
  
  ### checks for site_effect ###
  if(!is.null(site_effect)){
    
    if(class(site_effect) != 'logical') stop('site_effect must be logical')
    
  }  
  
  ### checks for family ###
  if(!is.null(family)){
    
    if(!family %in% c('Binomial', 'Bernoulli')){
      
      stop('family must be either Binomial or Bernoulli')
      
    }
    
    if(!is.null(list_length)){
      
      if(list_length & family == 'Binomial'){
        warning('When list_length is TRUE family will default to Bernoulli')
      }      
    }
  }
  
  ### checks for species_to_include ###
  
  if(!is.null(species_to_include)){
    
    missing_species <- species_to_include[!species_to_include %in% unique(taxa)]
    
    if(length(missing_species) > 0){
      
      warning('The following species in species_to_include are not in your data: ',
           paste(missing_species, collapse = ', '))
      
    }
  }
  
  ### check BUGS parameters ###
  if(!is.null(c(n_iterations, burnin, thinning, n_chains))){
    
    if(burnin > n_iterations) stop('Burn in (burnin) must not be larger that the number of iteration (n_iterations)')
    if(thinning > n_iterations) stop('thinning must not be larger that the number of iteration (n_iterations)')
   
    if(!is.numeric(n_iterations)) stop('n_iterations should be numeric')
    if(!is.numeric(burnin)) stop('burnin should be numeric')
    if(!is.numeric(thinning)) stop('thinning should be numeric')
    if(!is.numeric(n_chains)) stop('n_chains should be numeric')
    
  }
  
  if(!is.null(seed)){
    
    if(!is.numeric(seed)) stop('seed muct be numeric')
    
  }  
  
  ## Checks for frescalo
  if(!is.null(year_col)){
    if(is.na(year_col)){
      if(!is.null(start_col) & !is.null(end_col)){
        if(is.na(start_col)|is.na(end_col)){
          stop('year_col or start_col and end_col must be given')
        } else {  
          if(!is.na(start_col)|!is.na(end_col)){
            stop('year_col cannot be used at the same time as start_col and end_col')
          }
        }
      }
    }
  }
  
  if(!is.null(phi)){
    if(phi>0.95|phi<0.5){
      stop("phi is outside permitted range of 0.50 to 0.95")
    } 
  }
  
  if(!is.null(alpha)){
    if(alpha>0.5|alpha<0.08){
      stop("alpha is outside permitted range of 0.08 to 0.50")
    } 
  }
  
  if(!is.null(non_benchmark_sp)){    
    if(any(!is.vector(non_benchmark_sp), !is.character(non_benchmark_sp))){
        stop('non_benchmark_sp must be a character vector')
    }
  }
  
  if(!is.null(fres_site_filter)){
    if(any(!is.vector(fres_site_filter), !is.character(fres_site_filter))){
      stop('fres_site_filter must be a character vector')
    }  
  }
  
  if(!is.null(time_periods)){
    if(!is.data.frame(time_periods)) stop('time_periods should be a data.frame. e.g. "data.frame(start=c(1980,1990),end=c(1989,1999))"')
  }
  
  if(!is.null(frespath)){
    if(!grepl('.exe$', tolower(frespath))) stop("filepath is not the path to a '.exe' file") 
    if(!file.exists(frespath)) stop(paste(frespath, 'does not exist'))
  }
}
