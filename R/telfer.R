#' Telfer's change index
#' 
#' Telfers change index is designed to assess the relative change in range size of species 
#' between two time periods (Telfer et al 2002). This function can take multiple time periods
#' and will complete all pairwise comparisons.
#'
#' @param Data The data to be analysed. This should consist of rows of observations
#'        and columns indicating the species and location as well as either the year
#'        of the observation or columns specifying the start and end dates of the
#'        observation. This can be a dataframe object or a string giving the file path to
#'        a .csv or .rdata file. If left blank you will be prompted for a .csv or
#'        .rdata file.
#' @param time_periods This specifies the time periods to be analysed. A dataframe
#'        object with two columns. The first column contains the start year of each
#'        time period and the second column contains the end year of each time period.
#'        Time periods should not overlap. 
#' @param ignore.ireland If \code{TRUE} data from hectads in Ireland are removed.
#' @param ignore.channelislands If \code{TRUE} data from hectads in the Channel Islands
#'        are removed.
#' @param sinkdir An optional argument giving a file path where results should be written.
#'        This is useful if running the function in a loop over a number of datasets. Results
#'        are still returned to R when using \code{sinkdir}.
#' @param min_sq The minimum number of squares occupied in the first time period in
#'        order for a trend to be calculated for a species.
#' @param useIterations A logical variable indicating whether iterations should be used.
#'        Iterations are used to account for increased variation in the logit proportions
#'        close to zero and one (see Telfer et al 2002). Default is \code{TRUE}
#' @param iterations If \code{useIterations} is \code{TRUE}, then this parameter indicates
#'        the number of iterations to be used. In Telfer et al 2002 the number of iterations
#'        used were 7 and 8 for the two datasets for which it was applied. The defualt here
#'        is 10. 
#' @param year_col The name of the year column in \code{Data}
#' @param site_col The name of the site column in \code{Data}
#' @param sp_col The name of the species column in \code{Data}
#' @param start_col The name of the start date column in \code{Data}
#' @param end_col The name of the end date column in \code{Data}
#' @return A dataframe of results are returned to R. The first column gives the names of 
#'         each species. Each subsequent column gives the result of a pairwise comparison
#'         between two time periods. The numbers in column headings indicate the time periods
#'         compared i.e. '1_2' indicates a comparison of the 1st and 2nd time periods '3_5'
#'         indicates a comparison of the 3rd and 5th time period. Time periods are ordered
#'         by their start year.

#' @keywords trends, telfer, species
#' @import reshape2
#' @examples
#' \dontrun{
#'  # Load the library
#'  library(sparta)
#' 
#'  # Load example dataset
#'  data(ex_dat)
#'  
#'  # Run the telfer analysis
#'  telfer_out <- telfer(Data=ex_dat,
#'                       time_periods=data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009)),
#'                       min_sq=2,
#'                       useIterations=T,
#'                       iterations=20,
#'                       site_col='hectad',
#'                       sp_col='CONCEPT',
#'                       start_col='TO_STARTDATE',
#'                       end_col='Date')
#' 
#' }
#' @references Telfer, M.G., Preston, C.D., & Rothery, P. (2002) A general method for
#'             measuring relative change in range size from biological atlas data.
#'             Biological Conservation, 107, 99-109.

telfer <- 
  function(Data=NULL,#your data (path to .csv or .rdata, or an R object)
           time_periods=NULL,
           ignore.ireland=F,#do you want to remove Irish hectads?
           ignore.channelislands=F, ##do you want to remove Channel Islands (they are not UK)?
           sinkdir=NULL,#where is the data going to be saved
           min_sq=5,
           useIterations=TRUE,
           iterations=10,
           year_col=NA,
           site_col=NA,
           sp_col=NA,
           start_col=NA,
           end_col=NA){
    
    useIterations
    
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
    if(is.null(time_periods)){warning('time_periods must be given');warn=TRUE}
    if(!is.data.frame(Data)&length(Data)>1){warning('Data cannot have length > 1');warn=TRUE}
    if(warn) stop("Oops, you need to address these warnings")
    
    # ensure time_periods is ordered chronologically (this orders by the first column - start year)
    time_periods<-time_periods[with(time_periods, order(time_periods[,1])),]
    # ensure the end years are all greater than the start years
    if(TRUE %in% (time_periods[,2]<=time_periods[,1])) stop('In time_periods end years must be greater than start years')
    
    # Ensure reshape2 is installed
    required.packages <- c('reshape2')
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    # If datafile is not given give dialog to locate file
    if(is.null(Data)){
      cat("Choose .csv or .rdata file. Else assign data.frame of data to 'data'")
      Data<-choose.files()
      if(length(Data)==0) stop('User failed to select data')
      if(!grepl('.csv',Data) | grepl('.rdata',Data)) stop('Data file must be .csv or .rdata')
    }
    
    analType<-'telfer'
    
    if(!is.null(sinkdir)) dir.create(sinkdir,showWarnings = FALSE)
    
    print(paste('Starting',analType))
    datecode <- format(Sys.Date(),'%y%m%d')
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
    }
    
    # Check column names
    new.colnames<-na.omit(c(site_col,sp_col,year_col,start_col,end_col))
    missingColNames<-new.colnames[!new.colnames %in% names(taxa_data)]
    if(length(missingColNames)>0) stop(paste(unlist(missingColNames),'is not the name of a column in data'))
    
    # Remove columns that are not needed
    taxa_data <- taxa_data[,names(taxa_data) %in% new.colnames]
    
    # Ensure date columns are dates
    if(!is.na(start_col)){
      taxa_data<-colToDate(taxa_data,start_col)  
    } 
    if(!is.na(end_col)){
      taxa_data<-colToDate(taxa_data,end_col)  
    }
    if(!is.na(year_col)){
      if(!is.numeric(taxa_data[year_col][,1])){
        stop('column specified by year_col must be numeric') 
      }
    } 
    
    # We need to put each record into its time period
    if(!is.na(start_col) & !is.na(end_col)){
      for(ii in 1:length(time_periods[,1])){
        taxa_data$yearnew[as.numeric(format(taxa_data[start_col][[1]],'%Y'))>=time_periods[ii,1][[1]] &
                            as.numeric(format(taxa_data[end_col][[1]],'%Y'))<=time_periods[ii,2][[1]]]<-floor(rowMeans(time_periods[ii,])[[1]])
      }
    }else{
      for(ii in 1:length(time_periods[,1])){
        taxa_data$yearnew[taxa_data[year_col]>=time_periods[ii,1][[1]] &
                            taxa_data[year_col]<=time_periods[ii,2][[1]]]<-floor(rowMeans(time_periods[ii,])[[1]])
      }
    }
    
    taxa_data$year<-taxa_data$yearnew
    # Those that are not inthese time periods are removed
    taxa_data<-taxa_data[!is.na(taxa_data$year),]
    
    #rename columns
    newnames<-c('site','CONCEPT')
    oldnames<-c(site_col,sp_col)
    taxa_data<-change_colnames(taxa_data,newnames,oldnames)
    
    # Ensure CONCEPT is a factor
    if(!is.na(sp_col))taxa_data$CONCEPT<-as.factor(taxa_data$CONCEPT)
    
    # Remove Ireland and Channel Islands if desired
    if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data[site_col])==1)
    if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data[site_col])==FALSE)
    
    # For each pair of time periods go through and compare them
    # Compare the time periods
    for(ii in 1:(length(time_periods[,1])-1)){
      # to all other time periods
      for(j in (ii+1):length(time_periods[,1])){
        time_periods_temp<-time_periods[c(ii,j),] 
        taxon_temp<-paste(analType,'_',ii,'_',j,sep='')
        #save(taxa_data,file = 'testGPtelfer.rda')
        #save(time_periods_temp, file = 'timeperiodstemp.rda')
        basic_temp<-GP_telfer(taxa_data,time_periods_temp,iterations=iterations,useIterations=useIterations,min_sq=min_sq)
        colnames(basic_temp)[2]<-paste(analType,'_',ii,'_',j,sep='')
        print(paste('Telfer analysis for tp',ii,'vs tp',j,'done',sep=' '))
        if(exists('basic_master')){
          basic_master<-merge(basic_master,basic_temp,by='CONCEPT',all=TRUE)
        }else{
          basic_master<-basic_temp
        }
      }
    }
    #Add in NAs
    basic_master<-merge(basic_master,unique(taxa_data[c('CONCEPT')]),by='CONCEPT',all=TRUE)
    
    basic_master<-change_colnames(basic_master,sp_col,'CONCEPT')
    
    # If a sink directory is given write the output to file
    if(!is.null(sinkdir)){  
      file_name<-paste(analType,'_',datecode,sep='')
      orgwd<-getwd()
      setwd(sinkdir)      
      if (file.exists(paste(file_name,'.csv',sep=''))){
        files <- dir(sinkdir)
        files <- files[grepl(paste(analType,'_',datecode,sep=''),files)]
        if(sum(grepl('\\(',files))>0){ # if we have indexed files already index the new file as max+1
          files <- gsub(".csv",'',gsub(paste(analType,'_',datecode,sep=''),'',files)) #remove text from file name
          files <- gsub("\\)",'',gsub("\\(",'',files)) # remove brackets
          max_index <- max(as.numeric(files),na.rm=TRUE) # find the highest index number
          new_index <- max_index + 1
        } else {
          new_index <- 1
        }
        file_name <- paste(analType,'_',datecode,'(',new_index,')',sep='')
        warning(paste(analType,'_',datecode,' already exists.',
                      ' The new data is indexed as (',new_index,')',sep=''))
      }
      write.csv(basic_master,paste(file_name,'.csv',sep=''),row.names=FALSE)
      setwd(orgwd)
    }
    
    return(basic_master)
    
  }
