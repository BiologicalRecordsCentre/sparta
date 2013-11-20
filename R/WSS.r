#' Well sampled sites (WSS) trend analysis
#' 
#' This function undertakes a 'well sampled sites' analysis as laid out by Roy et al (2012).
#' This method accounts for variation in recording intensity between sites and excludes
#' data that may introduce error into trend estimates.
#' 
#' @param Data A dataframe object or string giving the file path to the
#'        location of data (either .rdata or .csv). This should consist of rows of
#'        observations and columns indicating the species and location as well as
#'        either the date/year of the observation or columns specifying the start and end
#'        dates/years of the observation. If \code{NULL} (default) the user is prompted to
#'        select a .csv or .rdata file.
#' @param year_range A vector of two numbers, the start and end year of the time period
#'        you wish to analyse. This simply subsets your data to the given year range. 
#' @param ignore.ireland If \code{TRUE} data from hectads in Ireland are removed.
#' @param ignore.channelislands If \code{TRUE} data from hectads in the Channel Islands
#'        are removed.
#' @param min_list The minimum list length (number of species) required from a visit for it 
#'        to be included in the WSS analysis. Default is 2 (as in Roy et al, 2010)
#'        but should be changed dependent on the distribution of list lengths in the data.
#'        By setting this value to 'median' you can chose to set min_list to the median list
#'        length (or 2 if the median list length is less than 2). If using this method the min_list
#'        used can be extracted from the object returned by retrieving the attribute min_list.
#' @param min_years This variable defines the minimum number of years in which a site must have
#'        well sampled visits (defined by \code{min_list}) to be included in te analysis. The 
#'        default is set to 3 as in Roy et al (2010).
#' @param od This option allows modelling overdispersion (\code{TRUE}) in models.
#'        Default is \code{FALSE}.
#' @param verbose This option, if \code{TRUE}, sets models to verbose, allowing the 
#'        interations of each model to be veiwed.
#' @param sinkdir An optional argument specifying  the file path where output should be saved.
#'        If the folder does not exist it will be created. Files are titled as a concatenation
#'        of 'WSS_' and the date in yymmdd format (i.e. 'WSS_130702.csv').
#'        If a file of this name already exists in the directory specified the file name will be
#'        appended with an index number (i.e. 'WSS_130702(1).csv').
#' @param trend_option Set the method by which you wish to calculate percentage change. This can currently
#'        be set to either \code{'arithmetic'} (default) or \code{'geometric'}. Arimthmetic calculates
#'        percentage change in a linear fashion such that a decline of 50\% over 50 years is
#'        equal to 10\% in 10 years. Using the same example a Geometric trend would be 8.44\%
#'        every 10 years as this work on a compound rate.
#' @param NYears The number of years over which you want the percentage change to be calculated (i.e.
#'        10 gives a decadal change). Default = 10        
#' @param date_col The name of the date column in \code{Data}. This column must be a date and
#'        should be in date format. If it isn't, a conversion is attempted. This column defines the
#'        temporal period of a visit.
#' @param site_col The name of the site column in \code{Data}. This column defines the spatial scale of a
#'        visit and so could be a unique site name (e.g. \code{'West Farm'}), or larger scale spatial unit
#'        (e.g. a 1km square). 
#' @param sp_col The name of the species column in \code{Data}
#' @param start_col The name of the start date column in \code{Data}. This must be a date and should be
#'        in date format. If it isn't a conversion is attempted. Note that rows where start_col and
#'        end_col are different will be removed from the analysis. Where rows are removed due to differences
#'        in values between start_col and end_col a warning is given.
#' @param end_col The name of the end date column in \code{Data}. See \code{start_col}.
#' @param print_progress Logical, if \code{TRUE} progress is printed to console when
#'        running models. Default is \code{TRUE}   
#' @return A dataframe of results are returned to R. Each row gives the results for a
#'         single species, with the species name given in the first column. The columns
#'         \code{year} and \code{intercept} give the estimates of these coefficients.
#'         The suffix \code{SE} indicates the standard error, \code{zscore} the Z-score
#'         and \code{pvalue} the P-value. \code{yearZero} gives the year used for the
#'         intercept (i.e. that year whose value is set to 0). This helps to increase
#'         model stability. \code{pVisitsUsed} gives the proportion of all visits
#'         (unique combinations of date and site) used in the modelling after removing
#'         short lists (\code{min_list}), and sites sampled in few years (\code{min_years}).
#'         \code{nVisitsUsed} is similar, giving the absolute number of visits used, and
#'         \code{nSpeciesObs} gives the number of these visits when the species of interest
#'         was observed.  \code{Ymin} and \code{Ymax} give the minimum and maximum years
#'         used in the model. Note these values are centered on \code{yearZero}.
#'         \code{change_} gives the percentage change dependent on the values given to
#'         \code{trend_option} and \code{NYears}. \code{errorMsg} captures error messages
#'         returne by lme4 during the running of models.
#'        
#' @keywords trends, species, distribution
#' @examples
#' \dontrun{
#' 
#' library(sparta)
#'
#' #load example dataset
#' data(ex_dat)
#' 
#' WSS_out<-WSS(Data=ex_dat,
#'             year_range=c(1970,2000),
#'             min_list=1,
#'             min_years=2,
#'             site_col='kmsq',
#'             sp_col='CONCEPT',
#'             start_col='TO_STARTDATE',
#'             end_col='Date')
#'
#' }
#' @references Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012) Invasive alien predator
#'             causes rapid declines of native European ladybirds. Diversity & Distributions,
#'             18, 717-725.


WSS <-
  function(Data=NULL,#your data (path to .csv or .rdata, or an R object)
           year_range = NULL, #for subsetting data
           ignore.ireland=F,#do you want to remove Irish hectads?
           ignore.channelislands=F, ##do you want to remove Channel Islands (they are not UK)?
           min_list = 2,
           min_years = 3,
           od = FALSE,
           verbose = FALSE,
           sinkdir = NULL,#where is the data going to be saved
           trend_option = 'arithmetic',
           NYears = 10,
           date_col = NA,
           site_col = NA,
           sp_col = NA,
           start_col = NA,
           end_col = NA,
           print_progress = TRUE){
    
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
    if(is.na(date_col)){
      if(is.na(start_col)|is.na(end_col)){
        warning('date_col or start_col and end_col must be given')
        warn=TRUE
      } 
    } else {
      if(!is.na(start_col)|!is.na(end_col)){
        warning('date_col cannot be used at the same time as start_col and end_col')
        warn=TRUE
      }
    }
    if(!is.null(year_range)){
      if(!is.numeric(year_range)){
        warning('year_range must be numeric')
        warn = TRUE
      } 
      if(length(year_range)!=2){
        warning('year_range should be of length 2')
        warn = TRUE
      }
      if (year_range[1]==year_range[2]){
        warning('year_range start and end years cannot be the same')
        warn = TRUE
      }
    }
    if(!is.data.frame(Data)&length(Data)>1){warning('Data cannot have length > 1');warn=TRUE}
    if(warn) stop("Oops, you need to address these warnings")
    
    # Ensure packages are installed
    required.packages <- c('reshape2','lme4')
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    # If datafile is not given give dialog to locate file
    if(is.null(Data)){
      cat("Choose .csv or .rdata file. Else assign data.frame of observations to 'data' argument")
      Data<-choose.files()
      if(length(Data)==0) stop('User failed to select data')
      if(!grepl('.csv',Data) | grepl('.rdata',Data)) stop('Data file must be .csv or .rdata')
    }
    
    analType<-'Mixed model'
    
    print(paste('Starting',analType))
    datecode <- format(Sys.Date(),'%y%m%d')
    
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
    new.colnames<-na.omit(c(site_col,sp_col,start_col,end_col,date_col))
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
    if(!is.na(date_col)){ # This can also be a year eg 1990
      taxa_data<-colToDate(taxa_data,date_col)  
    } 
    if(!is.na(start_col)){
      taxa_data<-colToDate(taxa_data,start_col)  
    } 
    if(!is.na(end_col)){
      taxa_data<-colToDate(taxa_data,end_col)  
    }   
    
    #rename columns
    newnames<-c('site','CONCEPT','TO_STARTDATE','Date','visit')
    oldnames<-c(site_col,sp_col,start_col,end_col,date_col)
    taxa_data<-change_colnames(taxa_data,newnames,oldnames)
    
    # Our visit scale is always date since we are using a biomial model
    # and therefore need to have multiple visits per year
    if(!is.na(date_col)){ # and we are using date column
      if(is.numeric(taxa_data$visit)) stop('date column must be of class date')
      taxa_data$time_period <- taxa_data$visit # just use it as it is
    } else if(!is.na(start_col) & !is.na(end_col)){ # if we are using start and end dates
      if(is.numeric(taxa_data$TO_STARTDATE)|is.numeric(taxa_data$Date)) stop('date columns must be of class date')
      nrow_data <- nrow(taxa_data) # how many rows in the data?
      taxa_data <- taxa_data[taxa_data$TO_STARTDATE == taxa_data$Date,] # only keep obs where start and end on the same date
      nrow_data_post <- nrow(taxa_data) # now how many rows?
      diff_rows <- nrow_data-nrow_data_post # what is the differnce in row count?
      if(diff_rows != 0) warning(paste(diff_rows,'rows of data were removed as start date and end date were from different dates'))
      taxa_data$time_period <- taxa_data$Date # just use it as it is (as start and end are the same it doesn't matter which we use)
    }    
    
    # Subset to year_range if given
    if(!is.null(year_range)){
      year_range <- sort(year_range)
      years<-year_range[1]:year_range[2]
      if(is.numeric(taxa_data$time_period)){
        taxa_data <- taxa_data[taxa_data$time_period %in% years,]
      } else {
        taxa_data <- taxa_data[as.numeric(format(taxa_data$time_period,'%Y')) %in% years,]    
      }    
    }
    
    # ignore irish and channel island sites if desired  
    if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$site)==1)
    if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$site)==FALSE)
    
    print('Recasting data...')
    
    # Work out the list lengths
    require(reshape2)
    space_time <- dcast(taxa_data[c('CONCEPT','time_period','site')], time_period + site ~ ., value.var='CONCEPT', fun=LenUniq)
    names(space_time)[ncol(space_time)] <- 'L' # this is the column with the list length in it
    
    # add year column here to space_time, this saves on computation
    # set the year column
    # when using year scale time_period could be a numeric or a date
    space_time$year <- as.numeric(format(space_time$time_period,'%Y')) # take year from date year
    
    
    ## FILTER TO WELL SAMPLED SITES
    space_time <- unique(space_time)
    total_vis <- length(space_time$L)
    
    #subset the data: remove the short lists (defined by nsp)
    space_time <- subset(space_time, L >= min_list)
    
    # of these visits, which are on well-sampled sites?
    space_time <- subset(space_time, is.gridcell.wellsampled2(space_time, n = min_years))
    total_WSS_vis <- length(space_time$L)
    prop_vis_used <- total_WSS_vis/total_vis
    
    
    # If calculating min_list from data do it now
    if(!is.numeric(min_list) & min_list != 'median') stop('min_list must be numeric or "median"')
    median_list_used<-FALSE
    if(min_list == 'median'){
      median_list_used<-TRUE
      min_list<-median(space_time$L)
      if(min_list<2) min_list <- 2
    }
    
    # If sinkdir is given, write data there. If not just return it to console
    if(!is.null(sinkdir)){
      dir.create(sinkdir,showWarnings = FALSE) # creates the directory if it does not exist
      org_wd<-getwd()
      setwd(sinkdir)
      file_name<-paste(sinkdir,'/WSS_',datecode,'.csv', sep='')
      # If the filename we want already exists create a new filename with a number
      # after it
      if(file.exists(file_name)){
        files <- dir(sinkdir)
        files <- files[grepl(paste('WSS_',datecode,sep=''),files)]
        if(sum(grepl('\\(',files))>0){ # if we have indexed files already index the new file as max+1
          files <- gsub(".csv",'',gsub(paste('WSS_',datecode,sep=''),'',files)) #remove text from file name
          files <- gsub("\\)",'',gsub("\\(",'',files)) # remove brackets
          max_index <- max(as.numeric(files),na.rm=TRUE) # find the highest index number
          new_index <- max_index + 1
        } else { # if we dont have any indexed files it is numbered as 1
          new_index <- 1
        }
        file_name <- paste(sinkdir,'/WSS_',datecode,'(',new_index,').csv', sep='')
        warning('sinkdir already contains WSS data from today. New data saved as ', paste('WSS_',datecode,'(',new_index,').csv', sep=''))
      }   
      setwd(org_wd) # Set our directory back to where it was
    }
    
    counter=1
    
    # Run the model species by species
    for (ii in sort(unique(taxa_data$CONCEPT))){ # the sort ensures species are done in order
      if(print_progress) print(paste('Modelling',ii,'- Species',counter,'of',length(unique(taxa_data$CONCEPT))))
      y<-unique(taxa_data[taxa_data$CONCEPT==ii&!is.na(taxa_data$site)&!is.na(taxa_data$time_period),][c('CONCEPT','time_period','site')])
      species_space_time <- merge(x = space_time, y = y, all.x = T, all.y = F)
      species_space_time$CONCEPT <- as.character(species_space_time$CONCEPT)
      species_space_time$CONCEPT[is.na(species_space_time$CONCEPT)] <- 0
      species_space_time$CONCEPT[species_space_time$CONCEPT == ii] <- 1
      
      #### Remove this code
#       print(head(y))
#       print(head(space_time))
#       print(head(species_space_time))
#       space_time <<- space_time
      ####
      
      Mod_out<-t(as.data.frame(WSS_func(MMdata=species_space_time,
                                        od=od,
                                        V=verbose,
                                        pvis = prop_vis_used)
                               ,stringsAsFactors = FALSE)) # run the model
      row.names(Mod_out)<-ii
      Mod_out<-as.data.frame(Mod_out)
      Mod_out[paste('change_',NYears,'yr',sep='')]<-percentageChange(intercept=Mod_out$intercept,
                                                                 slope=Mod_out$year,
                                                                 Ymin=Mod_out$Ymin,
                                                                 Ymax=Mod_out$Ymax,
                                                                 NYears=NYears,
                                                                 option=trend_option)
      Mod_out<-cbind(row.names(Mod_out),Mod_out)
      names(Mod_out)[1] <- sp_col
      
      #Mod_out$CONCEPT<-row.names(Mod_out)
      
      # writing the data out as we go preserves it in case of a crash
      # but is only done if the a sink directory is set
      if(!is.null(sinkdir)){
        if(file.exists(file_name)){
          write.table(Mod_out, file=file_name, append=T,col.names=F,sep=',',row.names=FALSE)
        }else{
          write.table(Mod_out, file=file_name,col.names=T,sep=',',row.names=FALSE)
        }  
      }
      
      # Data is aggregated in an R object
      if(exists('Mod_out_master')){
        Mod_out_master <- rbind(Mod_out_master,Mod_out)
      } else {
        Mod_out_master <- Mod_out
      }
      counter=counter+1  
    }
    
    # Because of the way the dataframe has been constructed some columns are chr
    # but should be num, I correct that here
    for(i in 2:13){
      Mod_out_master[,i]<-as.numeric(Mod_out_master[,i])
    }    
    
    if(median_list_used){
      print(paste('min_list set to',min_list,'using median method'))
      attr(Mod_out_master,'min_list') <- min_list
    } 
    return(Mod_out_master)
    
  }
