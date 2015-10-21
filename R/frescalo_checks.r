# Run some checks and basic formatting on the data
#' @importFrom dplyr distinct

frescalo_checks <- function(site_col, sp_col, year_col, start_col, end_col,
                            Data, time_periods, ignore.ireland, ignore.channelislands,
                            species_to_include){
  
  # Check column names are in the data
  new.colnames <- c(site_col,sp_col,year_col,start_col,end_col)
  missingColNames <- new.colnames[!new.colnames %in% names(Data)]
  if(length(missingColNames) > 0) stop(paste(unlist(missingColNames),'is not the name of a column in data'))
  
  # Remove columns that are not needed
  Data <- Data[,names(Data) %in% new.colnames]
  
  # Ensure date columns are dates
  if(!is.null(start_col)){
    Data <- colToDate(Data, start_col)  
  } 
  if(!is.null(end_col)){
    Data <- colToDate(Data, end_col)  
  }
  if(!is.null(year_col)){
    if(!is.numeric(Data[year_col][,1])){
      stop('column specified by year_col must be numeric') 
    }
  } 
  
  if(!is.null(start_col) & !is.null(end_col)){      
    for( i in c(start_col,end_col)){
      if(!'POSIXct' %in% class(Data[[i]]) & !'Date' %in% class(Data[[i]])){
        warning(paste('column',i,'Date is not in a date format. This should be of class "Date" or "POSIXct", conversion attempted'))
        Data[[i]]<-as.Date(Data[[i]])
      }
    }
  }    
  
  #If the data has a startdate and enddate ensure the dates are within one 
  #of the time periods, else if it just has a year, ensure this is in the
  #right time period
  if(!is.null(start_col) & !is.null(end_col)){
    for(ii in 1:length(time_periods[,1])){
      Data$yearnew[as.numeric(format(Data[start_col][[1]],'%Y'))>=time_periods[ii,1][[1]] &
                     as.numeric(format(Data[end_col][[1]],'%Y'))<=time_periods[ii,2][[1]]]<-rowMeans(time_periods[ii,])[[1]]
    }
  } else {
    for(ii in 1:length(time_periods[,1])){
      Data$yearnew[Data[year_col]>=time_periods[ii,1][[1]] &
                     Data[year_col]<=time_periods[ii,2][[1]]]<-rowMeans(time_periods[ii,])[[1]]
    }
  }
  
  # Those that are not in these time periods are removed
  Data <- Data[!is.na(Data$yearnew),]
  
  #rename columns
  newnames <- c('site','CONCEPT')
  oldnames <- c(site_col,sp_col)
  Data <- change_colnames(Data,newnames,oldnames)
  
  # Ensure CONCEPT is a factor
  if(!is.na(sp_col))Data$CONCEPT <- as.factor(Data$CONCEPT)
  
  # Include only desired species (note this removes data for unwanted species, this data is not used
  # in the analysis)
  if(!is.null(species_to_include)) Data <- Data[Data$CONCEPT %in% species_to_include,]
  
  # remove irish and/or channel islands data if desired
  if(ignore.ireland) Data <- subset(Data, regexpr('^[A-Z]{2}', Data$site)==1)
  if(ignore.channelislands) Data <- subset(Data, grepl('^[Ww][[:alpha:]]{1}', Data$site)==FALSE)
  # make sure there are no empty site names
  if('' %in% unique(Data$site)) stop('Site names cannot be empty (i.e. "")')
  
  # retain only the columns we need (site, concept, timeperiod)
  # at the same time keep only unique rows
  Data <- na.omit(distinct(Data[c('site','CONCEPT','yearnew')]))
  
  return(Data)
  
}
