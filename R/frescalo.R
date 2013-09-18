#' Frescalo trend analysis
#' 
#' A function for using Frescalo (Hill, 2011), a tool for analysing occurrence data when
#' recording effort is not known. This function returns the output from Frescalo to the 
#' R session and saves it to the path specified by \code{sinkdir}. By setting 
#' \code{plot_fres} to \code{TRUE} maps of the results will also be saved. Plotting the 
#' returned object gives a useful summary.
#'
#' @param Data A dataframe object or string giving the file path to the
#'        location of data (either .rdata or .csv). This should consist of rows of
#'        observations and columns indicating the species and location as well as
#'        either the year of the observation or columns specifying the start and end
#'        dates of the observation. If \code{NULL} (default) the user is prompted to select
#'        a .csv or .rdata file. If using a dataframe it is important that date columns are in
#'        a date format. If using a .csv dates are assumed to be in the format dd/mm/yyyy.
#' @param species_to_include Optionally a character vector listing the names of species to be used.
#'        Species not in your list are ignored. This is useful if you are only interested in a
#'        subset of species.
#' @param ignore.ireland Logical, if \code{TRUE} Irish hectads are removed. Default
#'        is \code{FALSE}
#' @param ignore.channelislands Logical, if \code{TRUE} channel island hectads are 
#'        removed. Default is \code{FALSE}
#' @param sinkdir String giving the output directory for results
#' @param time_periods A dataframe object with two columns. The first column contains the
#'        start year of each time period and the second column contains the end year of 
#'        each time period. Time periods should not overlap.
#' @param plot_fres Logical, if \code{TRUE} maps are produced by Frescalo. Default is 
#'        \code{FALSE}. CURRENTLY ONLY WORKS FOR UK GRID-REFERENCE DATA
#' @param Fres_weights 'LC' specifies a weights file based on landcover data
#'        for the UK and 'VP' uses a weights file based on vascular plant data for the UK
#'        , both are included in the package. Alternativly a custom weights file can be
#'        given as a data.frame. This must have three columns: target cell, neighbour cell,
#'        weight. 
#' @param non_benchmark_sp a character vector or data.frame with one column, giving the 
#'        names of species not to be used as benchmarks in Frescalo. Default is 
#'        \code{NULL} and all species are used. See Hill, 2011 for reasons why some
#'        species may not be suitable benchmarks.
#' @param fres_site_filter Optionally a character vector or data.frame with one column, giving
#'        the names of sites to be used for in the trend analysis. Sites not include in this
#'        list are not used for estimating TFactors. Default is \code{NULL} and all sites are
#'        used.
#' @param phi Target frequency of frequency-weighted mean frequency. Default is 0.74 as in
#'        Hill (2011). If this value is smaller than the 98.5 percentile of input phi it is
#'        automatically increased and a warning message is generated. This is limited to
#'        0.50 to 0.95.
#' @param alpha the proportion of the expected number of species in a cell to be treated as
#'        benchmarks. Default is 0.27 as in Hill (2011). This is limited to 0.08 to 0.50.
#' @param trend_option Set the method by which you wish to calculate percentage change. This can currently
#'        be set to either \code{'arithmetic'} (default) or \code{'geometric'}. Arimthmetic calculates
#'        percentage change in a linear fashion such that a decline of 50\% over 50 years is
#'        equal to 10\% in 10 years. Using the same example a Geometric trend would be 8.44\%
#'        every 10 years as this work on a compound rate.
#' @param NYears The number of years over which you want the percentage change to be calculated (i.e.
#'        10 gives a decadal change). Default = 10
#' @param year_col The name of the year column in \code{Data}
#' @param site_col The name of the site column in \code{Data}
#' @param sp_col The name of the species column in \code{Data}
#' @param start_col The name of the start date column in \code{Data}
#' @param end_col The name of the end date column in \code{Data}
#' @return Results are saved to file and most are returned in a list to R.
#' 
#'         The list object returned is comprised of the following:
#'         
#'         \item{\bold{$paths}}{This list of file paths provides the locations of the raw data files
#'         for $log, $stat, $freq and $trend, in that order}         
#'         
#'         \item{\bold{$trend}}{This dataframe provides the list of time factors for each species}
#'          
#'          \tabular{rll}{
#'          - \tab \code{Species} \tab Name of species\cr
#'          - \tab \code{Time} \tab Time period, specified as a class (e.g. 1970); times need not be numeric and are indexed as character strings\cr
#'          - \tab \code{TFactor} \tab Time factor, the estimated relative frequency of species at the time\cr
#'          - \tab \code{St_Dev} \tab Standard deviation of the time factor, given that spt (defined below) is a weighted sum of binomial variates\cr
#'          - \tab \code{Count} \tab Number of occurrences of species at the time period\cr
#'          - \tab \code{spt} \tab Number of occurrences, given reduced weight of locations having very low sampling effort\cr
#'          - \tab \code{est} \tab Estimated number of occurrences; this should be equal to spt if the algorithm has converged\cr
#'          - \tab \code{N>0.00} \tab Number of locations with non-zero probability of the species occurring\cr
#'          - \tab \code{N>0.98} \tab Number of locations for which the probability of occurrence was estimated as greater than 0.98\cr
#'          }
#'        
#'        \item{\bold{$stat}}{Location report}
#'         
#'          \tabular{rll}{
#'          - \tab \code{Location} \tab Name of location; in this case locations are hectads of the GB National Grid \cr
#'          - \tab \code{Loc_no} \tab Numbering (added) of locations in alphanumeric order \cr
#'          - \tab \code{No_spp} \tab Number of species at that location; the actual number which may be zero \cr
#'          - \tab \code{Phi_in} \tab Initial value of phi, the frequency-weighted mean frequency \cr
#'          - \tab \code{Alpha} \tab Sampling effort multiplier (to achieve standard value of phi) \cr
#'          - \tab \code{Wgt_n2} \tab effective number N2 for the neighbourhood weights; this is small if there are few floristically similar hectads close to the target hectad.  It is (sum weights)^2 / (sum weights^2) \cr
#'          - \tab \code{Phi_out} \tab Value of phi after rescaling; constant, if the algorithm has converged\cr
#'          - \tab \code{Spnum_in} \tab Sum of neighbourhood frequencies before rescaling\cr
#'          - \tab \code{Spnum_out} \tab Estimated species richness, i.e. sum of neighbourhood frequencies after rescaling\cr
#'          - \tab \code{Iter} \tab Number of iterations for algorithm to converge\cr
#'          }
#'              
#'         \item{\bold{$freq}}{Listing of rescaled species frequencies}
#'         
#'          \tabular{rll}{
#'          - \tab \code{Location} \tab Name of location\cr
#'          - \tab \code{Species} \tab Name of species\cr
#'          - \tab \code{Pres} \tab Record of species in location (1 = recorded, 0 = not recorded)\cr
#'          - \tab \code{Freq} \tab Frequency of species in neighbourhood of location\cr
#'          - \tab \code{Freq_1} \tab Estimated probabilty of occurrence, i.e. frequency of species after rescaling\cr
#'          - \tab \code{SD_Frq1} \tab Standard error of Freq_1, calculated on the assumption that Freq is a binomial variate with standard error sqrt(Freq*(1-Freq)/ Wgt_n2), where Wgt_n2 is as defined for samples.txt in section (b)\cr
#'          - \tab \code{Rank} \tab Rank of frequency in neighbourhood of location\cr
#'          - \tab \code{Rank_1} \tab Rescaled rank, defined as Rank/Estimated species richness\cr
#'          }
#'               
#'         \item{\bold{$log}}{This records all the output sent to the console when running frescalo}
#'           
#'         \item{\bold{$lm_stats}}{The results of linear modelling of TFactors}
#'         
#'          \tabular{rll}{
#'          - \tab \code{SPECIES} \tab Name of species used internally by frescalo\cr
#'          - \tab \code{NAME} \tab Name of species as appears in raw data\cr
#'          - \tab \code{b} \tab The slope of the model\cr
#'          - \tab \code{a} \tab The intercept\cr
#'          - \tab \code{b_std_err} \tab Standard error of the slope\cr
#'          - \tab \code{b_tval} \tab t-value for a test of significance of the slope\cr
#'          - \tab \code{b_pval} \tab p-value for a test of significance of the slope\cr
#'          - \tab \code{a_std_err} \tab Standard error of the intercept\cr
#'          - \tab \code{a_tval} \tab t-value for a test of significance of the intercept\cr
#'          - \tab \code{a_pval} \tab p-value for a test of significance of the intercept\cr
#'          - \tab \code{adj_r2} \tab Rescaled rank, defined as Rank/Estimated species richness\cr
#'          - \tab \code{r2} \tab t-value for a test of significance of the intercept\cr
#'          - \tab \code{F_val} \tab F-value of the model\cr
#'          - \tab \code{F_num_df} \tab Degrees of freedom of the model\cr
#'          - \tab \code{F_den_df} \tab Denominator degrees of freedom from the F-statistic\cr
#'          - \tab \code{Ymin} \tab The earliest year in the dataset\cr
#'          - \tab \code{Ymax} \tab The latest year in the dataset\cr
#'          - \tab \code{change_...} \tab The percentage change dependent on the values given to \code{trend_option} and \code{NYears}.\cr
#'          }
#'          \bold{The following columns are only produced when there are only two time periods}
#'          \tabular{rll}{
#'          - \tab \code{Z_VAL} \tab Z-value for the significance test of the trend\cr
#'          - \tab \code{SIG_95} \tab A logical statement indicating if the trend is significant (TRUE) or non-significant (FALSE)\cr
#'          }
#' @keywords trends, frescalo
#' @references Hill, Mark. Local frequency as a key to interpreting species occurrence data when
#' recording effort is not known. 2011. \emph{Methods in Ecology and Evolution}, 3 (1), 195-205.
#' @import lme4 reshape2 sp RODBC
#' @examples
#' \dontrun{
#' # Load the library
#' library(sparta)
#' 
#' # Load data
#' data(ex_dat)
#'
#' # Run frescalo (data is save to the working directory as sinkdir is not given)
#' fres_out<-frescalo(Data=ex_dat,
#'                    time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
#'                    site_col='hectad',
#'                    sp_col='CONCEPT',
#'                    start_col='TO_STARTDATE',
#'                    end_col='Date')
#'}

frescalo <-
  function(Data=NULL,#your Data (.rdata files) as a file path (or list of file paths)
           species_to_include=NULL, #A species list with which to subset your data
           ignore.ireland=F,#do you want to remove Irish hectads?
           ignore.channelislands=F, ##do you want to remove Channel Islands (they are not UK)?
           sinkdir=NULL,#where is the data going to be saved
           time_periods=NULL, #a list of vector pairs used in frescalo (ie 'c((1990,1995),(1996,2000))')
           plot_fres=FALSE,#do you want to plot the maps and do linear regression for frescalo?
           Fres_weights='LC',#the name of the weights file in the frescalo directory to be used
           non_benchmark_sp=NULL,#species not to be used as benchmarks 
           fres_site_filter=NULL, #optional list of sites not to be included in analysis
           phi = NULL, #phi value for frescalo
           alpha = NULL, #alpha value for frescalo
           trend_option = 'arithmetic',
           NYears = 10,
           year_col=NA, # the name of your year column
           site_col=NA, # name of site column
           sp_col=NA, # name od species column
           start_col=NA, # name of start date column
           end_col=NA # name of end date column
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
    if(is.null(time_periods)){warning('time_periods must be given');warn=TRUE}
    if(!is.null(Data)&!is.data.frame(Data)&length(Data)>1){warning('Data cannot have length > 1');warn=TRUE}
    # Frescalo specific warnings    
    if(!is.null(phi)){
      if(phi>0.95|phi<0.5){
        warning("phi is outside permitted range of 0.50 to 0.95")
        warn=TRUE
      } 
    }
    if(!is.null(alpha)){
      if(alpha>0.5|alpha<0.08){
        warning("alpha is outside permitted range of 0.08 to 0.50")
        warn=TRUE
      } 
    }
    if(is.data.frame(non_benchmark_sp)){
      if(length(non_benchmark_sp)!=1){
        warning('data.frame "non_benchmark_sp" should only have one column')
        warn=TRUE
      } else {
        non_benchmark_sp<-as.character(non_benchmark_sp[[1]])
      }
    }
    if(is.data.frame(fres_site_filter)){
      if(length(fres_site_filter)!=1){
        warning('data.frame "fres_site_filter" should only have one column')
        warn=TRUE
      } else {
        fres_site_filter<-as.character(fres_site_filter[[1]])
      }
    }
    # This method collects all warnings together and then reports them to be fixed
    # this seems better than reporting them one at a time.
    if(warn) stop("Oops, you need to address these warnings")
    
    # ensure time_periods is ordered chronologically (this orders by the first column - start year)
    time_periods<-time_periods[with(time_periods, order(time_periods[,1])),]
    # ensure the end years are all greater than the start years
    if(TRUE %in% (time_periods[,2]<time_periods[,1])) stop('In time_periods end years must be greater than or equal to start years')
       
    # load required packages
    required.packages <- c('lme4','reshape2','sp','gdata')
    if(plot_fres) required.packages <- c(required.packages,'ggplot')
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    # if data not supplied ask for it
    if(is.null(Data)){
      cat("Choose .csv or .rdata file. Else assign data.frame of data to 'data'")
      Data<-choose.files()
      if(length(Data)==0) stop('User failed to select data')
      if(!grepl('.csv',Data) | grepl('.rdata',Data)) stop('Data file must be .csv or .rdata')
    }
   
    analType<-'frescalo'
    
    # Create the sink directory if given
    if(!is.null(sinkdir)){
      dir.create(sinkdir,showWarnings = FALSE)
    } else {
      warning(paste('sinkdir not given. Defaulted to working directory',getwd()))
      sinkdir<-getwd()
      dir.create(sinkdir,showWarnings = FALSE)
    }
    
    # Load in the map of the UK if we are plotting
    if(plot_fres) data(UK)
    
    # set up frescalo path
    frespath<-paste(normalizePath(find.package('sparta')),'\\exec\\Frescalo_2c.exe',sep='')
    
    # unpack weights file if needed
    if(class(Fres_weights)=='character'){
      if(Fres_weights=='LC'){
        Fres_weights_name<-'GB_LC_Wts.txt'
        if(!file.exists(paste(dirname(frespath),'/GB_LC_Wts.txt',sep=''))){
          data(GB_LC_Wts)
          write.table(GB_LC_Wts,file=paste(dirname(frespath),'/GB_LC_Wts.txt',sep=''),row.names=FALSE,col.names=FALSE,quote=FALSE) 
        }  
      }
      if(Fres_weights=='VP'){
        Fres_weights_name<-'Wts.txt' 
        if(!file.exists(paste(dirname(frespath),'/Wts.txt',sep=''))){
          data(Wts)
          write.table(Wts,file=paste(dirname(frespath),'/Wts.txt',sep=''),row.names=FALSE,col.names=FALSE,quote=FALSE)             
        }  
      }
    }
    if(is.data.frame(Fres_weights)){
      if(length(Fres_weights)!=3) stop('Fres_weights data.frame must have three columns: target, neighbour, weight')
      if(!is.numeric(Fres_weights[,3])){
        warning('Weights column in Fres_weights should be numeric, conversion attempted')
        if(is.factor(Fres_weights[,3])){
          Fres_weights[,3]<-as.numeric(as.character(Fres_weights[,3]))
        } else if(is.character(Fres_weights[,3])){
          Fres_weights[,3]<-as.numeric(Fres_weights[,3])
        } else{
          stop('Conversion failed')
        }
      } 
      if(max(Fres_weights[,3])>1|min(Fres_weights[,3])<0) stop('Weight in Fres_weights cannot be greater than 1 or less than 0')
      
      # Weights file rounded to 4 decimal places as frescalo takes a fixed width format
      round(Fres_weights[,3],4)
      # Write the weights file
      write.fwf(Fres_weights,colnames=FALSE,rownames=FALSE,width=c(9,9,7),file=paste(dirname(frespath),'/Custom_Wts.txt',sep=''))
      Fres_weights_name<-'Custom_Wts.txt'         
    }
    
    print('loading raw data')
    if(class(Data)=='data.frame'){
      taxa_data<-Data
      rm(Data)
    } else if(is.character(Data)&grepl('.rdata',Data,ignore.case=TRUE)){
      loaded<-load(Data)
      if(is.character(Data)&sum(grepl('taxa_data',loaded))==0){
        stop('The .rdata file used does not contain an object called "taxa_data"')
      }
    }else if(grepl('.csv',Data,ignore.case=TRUE)){
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
    
    if(!is.na(start_col)&!is.na(end_col)){      
      for( i in c(start_col,end_col)){
        if(!'POSIXct' %in% class(taxa_data[[i]]) & !'Date' %in% class(taxa_data[[i]])){
          warning(paste('column',i,'Date is not in a date format. This should be of class "Date" or "POSIXct", conversion attempted'))
          taxa_data[[i]]<-as.Date(taxa_data[[i]])
        }
      }
    }    
   
    #If the data has a startdate and enddate ensure the dates are within one 
    #of the time periods, else if it just has a year, ensure this is in the
    #right time period
    if(!is.na(start_col) & !is.na(end_col)){
      for(ii in 1:length(time_periods[,1])){
        taxa_data$yearnew[as.numeric(format(taxa_data[start_col][[1]],'%Y'))>=time_periods[ii,1][[1]] &
                            as.numeric(format(taxa_data[end_col][[1]],'%Y'))<=time_periods[ii,2][[1]]]<-rowMeans(time_periods[ii,])[[1]]
      }
    }else{
      for(ii in 1:length(time_periods[,1])){
        taxa_data$yearnew[taxa_data[year_col]>=time_periods[ii,1][[1]] &
                            taxa_data[year_col]<=time_periods[ii,2][[1]]]<-rowMeans(time_periods[ii,])[[1]]
      }
    }
    
    # Those that are not in these time periods are removed
    taxa_data<-taxa_data[!is.na(taxa_data$yearnew),]
    
    #rename columns
    newnames<-c('hectad','CONCEPT')
    oldnames<-c(site_col,sp_col)
    taxa_data<-change_colnames(taxa_data,newnames,oldnames)
    
    # Ensure CONCEPT is a factor
    if(!is.na(sp_col))taxa_data$CONCEPT<-as.factor(taxa_data$CONCEPT)
    
    # Include only desired species (note this removes data for unwanted species, this data is not used
    # in the analysis)
    if(!is.null(species_to_include)) taxa_data<-taxa_data[taxa_data$CONCEPT %in% species_to_include,]
    
    # remove irish and/or channel islands data if desired
    if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$hectad)==1)
    if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$hectad)==FALSE)
        
    # retain only the columns we need (hectad, concept, timeperiod)
    # at the same time keep only unique rows
    taxa_data<-na.omit(unique(taxa_data[c('hectad','CONCEPT','yearnew')]))
    
    # Setup output
    datecode <- format(Sys.Date(),'%y%m%d')
    fresoutput <- paste(sinkdir,'/','frescalo_',datecode,sep='')
    # if the output directory already has data in from a frescalo run with the same date
    # then create a new directory adding on an index number
    if(file.exists(fresoutput)){
      files <- dir(sinkdir)
      files <- files[grepl(paste('frescalo_',datecode,sep=''),files)]
      if(sum(grepl('\\(',files))>0){ # if we have indexed files already index the new file as max+1
        files <- gsub(".csv",'',gsub(paste('frescalo_',datecode,sep=''),'',files)) #remove text from file name
        files <- gsub("\\)",'',gsub("\\(",'',files)) # remove brackets
        max_index <- max(as.numeric(files),na.rm=TRUE) # find the highest index number
        new_index <- max_index + 1
      } else {
        new_index <- 1
      }
      fresoutput <- paste(fresoutput,'(',new_index,')',sep='')
      dir.create(fresoutput,showWarnings = FALSE)
      warning('sinkdir already contains frescalo output. New data saved in ', paste('frescalo_',datecode,'(',new_index,')',sep=''))
    } else {
      dir.create(fresoutput,showWarnings = FALSE)
    }
    
    # Set up species names
    spp_names<-NULL
    # Create a lookup table of old and new names
    new_names <- data.frame(SPECIES=paste('S',1:length(unique(taxa_data$CONCEPT)),sep=''),NAME=sort(unique(taxa_data$CONCEPT)))
    spp_names <- paste(fresoutput,'/species_names.csv',sep='')
    write.table(new_names,spp_names,sep=',',row.names=FALSE)
    # Merge in the new species names and use these going into frescalo
    taxa_data<-merge(taxa_data,new_names,by.x='CONCEPT',by.y='NAME',all=T)
    taxa_data<-taxa_data[c('hectad','SPECIES','yearnew')]
       
    # Create non benchmark list if needed
    non_bench_txt<-NULL
    if(!is.null(non_benchmark_sp)){
      # Set up path to text file
      NonBenchPath<-paste(dirname(frespath),'/NonBench.txt',sep='')  
      # Get the new names of these species      
      non_benchmark_sp <- new_names$SPECIES[new_names$NAME %in% non_benchmark_sp]
      # This write.table works for both data.frames (with one column) and vectors
      write.table(non_benchmark_sp,sep='\n',file=NonBenchPath,row.names=F,col.names=F,quote=F)
      non_bench_txt<-'NonBench.txt'      
    }   
    
    # If needed create a site filter
    fres_site_txt <- NULL
    if(!is.null(fres_site_filter)){
      # Setup path for this file
      fres_site_path <- paste(dirname(frespath), '/fres_site_filter.txt',sep='')         
      write.table(fres_site_filter, sep='\n', file=fres_site_path, row.names=F, col.names=F, quote=F)
      fres_site_txt <- 'fres_site_filter.txt'          
    }         
    
    if(nrow(taxa_data)==0) stop("By Zeus' beard! The data heading into frescalo has 0 rows. Make sure your time periods match your years, and your not subsetting out all your data")
    fres_return<-run_fresc_file(channel=channel,fres_data=taxa_data,output_dir=fresoutput,frescalo_path=frespath,fres_f_wts=Fres_weights_name,
                                Plot=plot_fres,spp_names_file=spp_names,fres_f_nobench=non_bench_txt,fres_f_filter=fres_site_txt,
                                fres_phi_val=phi,fres_bench_val=alpha)
    class(fres_return)<-'frescalo'
    
    fres_lm_path<-paste(fresoutput,'/Maps_Results/Frescalo Tfactor lm stats.csv',sep='')
    
    # Calculate z-values if only two time periods & lm_stats exists
    if(length(time_periods[,1])==2 & 'lm_stats' %in% names(fres_return)){
      fres_lm <- read.csv(fres_lm_path)
      trendpath <- paste(fresoutput, '/Output/Trend.txt', sep='')
      zvalues <- fres_zvalues(trendpath)
      lm_z <- merge(x=fres_lm, y=zvalues, by='SPECIES', all=TRUE)
      lm_z <- lm_z[with(lm_z,order(NAME)),]
      write.csv(lm_z, fres_lm_path, row.names=FALSE)
      fres_return$lm_stats <- lm_z
    }
   
    if('lm_stats' %in% names(fres_return)){
      fres_return$lm_stats[paste('change_',NYears,'yr',sep='')]<-percentageChange(intercept=fres_return$lm_stats['a'],
                                                          slope=fres_return$lm_stats['b'],
                                                          Ymin=fres_return$lm_stats['Ymin'],
                                                          Ymax=fres_return$lm_stats['Ymax'],
                                                          NYears=NYears,
                                                          option=trend_option)
      write.csv(fres_return$lm_stats, fres_lm_path, row.names=FALSE)
    }
    
    # Remove .txt files created by frescalo. These have been converted to .csv
    unlink(fres_return$paths[2:4])
    
    # Update paths
    fres_return$paths[2:4]<-gsub('.txt','.csv',fres_return$paths[2:4])
    
    print('frescalo complete')

    return(fres_return)
  }
