errorChecks <- function(taxa = NULL, site = NULL, time_period = NULL, startDate = NULL,
                        endDate = NULL, time_periodsDF = NULL, dist = NULL, sim = NULL,
                        dist_sub = NULL, sim_sub = NULL){
  
  # Create a list of all non-null arguements that should be of equal length
  valid_argumentsTEMP <- list(taxa=taxa,
                          site=site,
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
  
  ### Checks for startDate ###
  if(!is.null(startDate)){
    if(!'POSIXct' %in% class(startDate) & !'Date' %in% class(startDate)){
      stop(paste('startDate is not in a date format. This should be of class "Date" or "POSIXct"'))
    }
  }
  
  ### Checks for endDate ###
  if(!is.null(endDate)){
    if(!'POSIXct' %in% class(endDate) & !'Date' %in% class(endDate)){
      stop(paste('endDate is not in a date format. This should be of class "Date" or "POSIXct"'))
    }
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
    if(!class(dist[,3]) %in% c('numeric', 'interger')) stop('the value column in dist must be an integer or numeric')
    
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
    if(!all(lapply(sim[,2:ncol(sim)], class) %in% c('numeric', 'interger'))) stop('the values in sim must be integers or numeric')
        
  }
  
  ### Checks for sim_sub and dist_sub ###
  if(!is.null(sim_sub) & !is.null(dist_sub)){
    
    if(!class(dist_sub) %in% c('numeric', 'interger')) stop('dist_sub must be integer or numeric')
    if(!class(sim_sub) %in% c('numeric', 'interger')) stop('sim_sub must be integer or numeric')
    if(dist_sub <= sim_sub) stop("'dist_sub' cannot be smaller than or equal to 'sim_sub'")
    
  }
  
  # check all are the same length
  # check that there are no duplicate 'rows'
  # check all are valid formats
  # tp must be date or numeric
  
}