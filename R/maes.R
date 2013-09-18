#' Maes trend analysis
#' 
#' This function uses the method presented in Maes et al, (2012) to estimate a percentage
#' change in distribution area between two time periods. The formula attempts to account
#' for uneven recording effort between time periods. This method is specifically designed
#' to feed into IUCN red list analyses (criterion A2c). This function can take multiple
#' time periods and will complete all pairwise comparisons
#' 
#' @param Data A dataframe object or string giving the file path to the
#'        location of data (either .rdata or .csv). This should consist of rows of
#'        observations and columns indicating the species and location as well as
#'        either the date/year of the observation or columns specifying the start and end
#'        dates/years of the observation. If \code{NULL} (default) the user is prompted to
#'        select a .csv or .rdata file.
#' @param time_periods This parameter gives the time periods to be compared. A dataframe
#'        object with two columns. The first column contains the start year of each time
#'        period and the second column contains the end year of each time period. Time
#'        periods should not overlap.
#' @param ignore.ireland If \code{TRUE} data from hectads in Ireland are removed.
#' @param ignore.channelislands If \code{TRUE} data from hectads in the Channel Islands
#' @param sinkdir An optional argument specifying  the file path where output should be saved.
#'        If the folder does not exist it will be created. Files are titled as a concatenation
#'        of 'Maes_' and the date in yymmdd format (i.e. 'Maes_130702.csv'). If a file of
#'        this name already exists in the directory specified the file name will be
#'        appended with an index number (i.e. 'Maes_130702(1).csv').
#' @param min_sp A numeric argument giving the minimum number of species that must be
#'        observed in both time periods for a site to be included. For example, if set to
#'        5, then at least 5 species much be observed in both time period 1 and time
#'        period 2 for that site to be included in the analysis. Default is 5, as in 
#'        Maes et al, (2012).
#' @param year_col The name of the year column in \code{Data}
#' @param site_col The name of the site column in \code{Data}
#' @param sp_col The name of the species column in \code{Data}
#' @param start_col The name of the start date column in \code{Data}
#' @param end_col The name of the end date column in \code{Data}
#' @return A dataframe of results are returned to R. Each row gives the results for a
#'         single species, with the species name given in the first column. Each column
#'         name is prefixed with the time periods it addresses (this makes the results
#'         clear when many timeperiods are being compared). \code{1_2} indicates a
#'         comparison of time period 1 and 2. \code{gridcells1} and \code{gridecells2}
#'         give the number of cells occupied in each time period (1st and 2nd respectively)
#'         after removing sites that do not meet the selection criteria. \code{relDist1}
#'         and \code{relDist2} give the relative distribution, and \code{change} gives the
#'         percentage change.
#' 
#' @keywords trends, species, distribution, IUCN, red list, Maes
#' @examples
#' \dontrun{
#' 
#'  # Load the library
#'  library(sparta)
#' 
#'  # Load example dataset
#'  data(ex_dat)
#'  
#'  # Run the analysis
#'  Ma_out<-maes(Data=ex_dat,
#'               time_periods=(data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009))),
#'               min_sp=1,
#'               site_col='hectad',
#'               sp_col='CONCEPT',
#'               start_col='TO_STARTDATE',
#'               end_col='Date')
#'
#' }
#' @references Maes, D., Vanreusel, T., Jacobs, I., Berwaerts, K., Van Dyck, H. (2012)
#' Applying IUCN Red List criteria at a small regional level: a test case with butterflies
#' in Flanders (north Belgium). Biological Conservation, 145, 258-266.


maes <-function(Data=NULL,#your data (.rdata files) as a file path (or list of file paths), or r object
                time_periods=NULL,
                ignore.ireland=F,#do you want to remove Irish hectads?
                ignore.channelislands=F, ##do you want to remove Channel Islands (they are not UK)?
                sinkdir=NULL,#where is the data going to be saved
                min_sp=5,
                year_col=NA,
                site_col=NA,
                sp_col=NA,
                start_col=NA,
                end_col=NA          
                ){
    
  # Clear warnings
  assign("last.warning", NULL, envir = baseenv())
  warn=FALSE
  if(is.na(site_col)){
    warning('Site column not specified')
    warn=TRUE
  }
  if(is.na(sp_col)){
    warning('Species column not specified')
    warn=TRUE
  }
  if(is.na(year_col)){
    if(is.na(start_col)|is.na(end_col)){
      warning('year_col or start_col and end_col must be given')
      warn=TRUE
    } 
  } else {
    if(!is.na(start_col)|!is.na(end_col)){
      warning('year_col cannot be used at the same time as start_col and end_col')
      warn=TRUE
    }
  }
  if(!is.numeric(min_sp)){
    warning('min_sp must be numeric')
    warn=TRUE
  }
  if(is.null(time_periods)){warning('time_periods must be given');warn=TRUE}
  if(!is.data.frame(Data)&length(Data)>1){warning('Data cannot have length > 1');warn=TRUE}
  if(warn) stop("Oops, you need to address these warnings")

  # ensure time_periods is ordered chronologically (this orders by the first column - start year)
  time_periods<-time_periods[with(time_periods, order(time_periods[,1])),]
  # ensure the end years are all greater than the start years
  if(TRUE %in% (time_periods[,2]<=time_periods[,1])) stop('In time_periods end years must be greater than start years')
    
  required.packages <- c('reshape2')
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # If datafile is not given give dialog to locate file
  if(is.null(Data)){
    cat("Choose .csv or .rdata file. Else assign data.frame of observations to 'data' argument")
    Data<-choose.files()
    if(length(Data)==0) stop('User failed to select data')
    if(!grepl('.csv',Data) | grepl('.rdata',Data)) stop('Data file must be .csv or .rdata')
  }
  
  analType<-'Maes'
  
  print(paste('Starting',analType))
  datecode <- format(Sys.Date(),'%y%m%d')
  
  #set up object to return
  return_object=list()
 
  ## Load data
  if(is.data.frame(Data)){
    taxa_data<-Data
    rm(Data)
  } else if(is.character(Data)&grepl('.rdata',Data,ignore.case=TRUE)){
    print('loading raw data')
    loaded<-load(Data)
    if(is.character(Data)&sum(grepl('taxa_data',loaded))==0){
      stop('The .rdata file used does not contain an object called "taxa_data"')
    }
  }else if(grepl('.csv',Data,ignore.case=TRUE)){
    print('loading raw data')
    taxa_data<-read.table(Data,header=TRUE,stringsAsFactors=FALSE,sep=',',check.names=FALSE)
    # If reading in the table then invalid column names will have been changed.
    # We could account for this by also making valid the column names that have been 
    # suppied by the user but the way in which they have been made valid may not be
    # predictable so I have chosen to add check.names=FALSE
  }
  
  # Check column names
  new.colnames<-na.omit(c(site_col,sp_col,start_col,end_col,year_col))
  missingColNames<-new.colnames[!new.colnames %in% names(taxa_data)]
  if(length(missingColNames)>0) stop(paste(paste(unlist(missingColNames),collapse = ' and '),'is/are not the name of a column(s) in data'))
  
  # Remove columns that are not needed
  taxa_data <- taxa_data[,names(taxa_data) %in% new.colnames]
  
  # remove NAs
  # It is important NAs are not included in the analysis. These may arise where
  # the original dataset has more columns. For example it may have a 10km column
  # but here is using 1km
  taxa_data<-na.omit(taxa_data)
  
  # Make sure date columns are dates
  if(!is.na(start_col)){
    if(!is.numeric(taxa_data[start_col][,1])){
      taxa_data<-colToDate(taxa_data,start_col)  
    }
  } 
  if(!is.na(end_col)){
    if(!is.numeric(taxa_data[end_col][,1])){
      taxa_data<-colToDate(taxa_data,end_col)  
    }
  }
  if(!is.na(year_col)){
    if(!is.numeric(taxa_data[year_col][,1])){
      stop('column specified by year_col must be numeric') 
    }
  }
    
  if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$Site)==1)
  if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$Site)==FALSE)
  
  # ensure time_periods is ordered chronologically (this orders by the first column - start year)
  time_periods<-time_periods[with(time_periods, order(time_periods[,1])),]
    
  # We need to put each record into its time period
  if(!is.na(start_col) & !is.na(end_col)){
    for(ii in 1:length(time_periods[,1])){
      taxa_data$tpnew[as.numeric(format(taxa_data[start_col][[1]],'%Y'))>=time_periods[ii,1][[1]] &
                          as.numeric(format(taxa_data[end_col][[1]],'%Y'))<=time_periods[ii,2][[1]]]<-floor(rowMeans(time_periods[ii,])[[1]])
    }
  }else{
    for(ii in 1:length(time_periods[,1])){
      taxa_data$tpnew[taxa_data[year_col]>=time_periods[ii,1][[1]] &
                          taxa_data[year_col]<=time_periods[ii,2][[1]]]<-floor(rowMeans(time_periods[ii,])[[1]])
    }
  }
  
  #rename columns
  newnames<-c('Site','Species','Start','End','Year')
  oldnames<-c(site_col,sp_col,start_col,end_col,year_col)
  taxa_data<-change_colnames(taxa_data,newnames,oldnames)
  
  # For each pair of time periods go through and compare them
  # Compare the time periods
  for(ii in 1:(length(time_periods[,1])-1)){
    # to all other time periods
    for(j in (ii+1):length(time_periods[,1])){
      time_periods_temp<-time_periods[c(ii,j),] 
      taxon_temp<-paste(analType,'_',ii,'_',j,sep='')
      maes_out<-fit_maes_trend(records=taxa_data,time_periods=time_periods_temp,min_sp=min_sp)
      print(paste('Maes analysis for tp',ii,'vs tp',j,'done',sep=' '))
      names(maes_out)<-paste(ii,'_',j,'_',names(maes_out),sep='')
      maes_out[sp_col]<-row.names(maes_out)
      #print(head(maes_out))
      if(exists('master')){
        master<-merge(master,maes_out,by=sp_col,all=TRUE)
        #print(head(master))
      }else{
        master<-maes_out
      }
    }
  }
  
  # If sinkdir is given, write data there. If not just return it to console
  if(!is.null(sinkdir)){
    dir.create(sinkdir,showWarnings = FALSE) # creates the directory if it does not exist
    org_wd<-getwd()
    setwd(sinkdir)
    file_name<-paste(sinkdir,'/Maes_',datecode,'.csv', sep='')
    # If the filename we want already exists create a new filename with a number
    # after it
    if(file.exists(file_name)){
      files <- dir(sinkdir)
      files <- files[grepl(paste('Maes_',datecode,sep=''),files)]
      if(sum(grepl('\\(',files))>0){ # if we have indexed files already index the new file as max+1
        files <- gsub(".csv",'',gsub(paste('Maes_',datecode,sep=''),'',files)) #remove text from file name
        files <- gsub("\\)",'',gsub("\\(",'',files)) # remove brackets
        max_index <- max(as.numeric(files),na.rm=TRUE) # find the highest index number
        new_index <- max_index + 1
      } else { # if we dont have any indexed files it is numbered as 1
        new_index <- 1
      }
      file_name <- paste(sinkdir,'/Maes_',datecode,'(',new_index,').csv', sep='')
      warning('sinkdir already contains Maes data from today. New data saved as ', paste('Maes_',datecode,'(',new_index,').csv', sep=''))
    }
    write.table(master,file=file_name,sep=',',row.names=FALSE,)
    setwd(org_wd) # Set our directory back to where it was
  }  
  return(master)
}
