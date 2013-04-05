#' Frescalo
#' 
#' A function for using Frescalo (Hill, 2011), a tool for analysing occurrence data when
#' recording effort is not known. This function returns the output from Frescalo to the 
#' R session and saves it to the path specified by \code{sinkdir}. By setting 
#' \code{Plot_Fres} to \code{TRUE} maps of the results will also be saved. Plotting the 
#' returned object gives a useful summary.
#'
#' @param data A dataframe object or string giving the file path to the
#'        location of data (either .rdata or .csv). Required columns are: 'CONCEPT',
#'        'hectad' and 'year'. Optionally 'TO_STARTDATE' can be included, if it is this
#'        is assumed to be the start date (from which start year is extracted) and year
#'        is assumed to be the end date. If \code{NULL} the user is prompted to select
#'        a .csv or .rdata file.
#' @param taxon_name string giving the name of data. This is used to name
#'        output files when written
#' @param species_to_include A vector of strings (that match your CONCEPT column in
#'        data) which are to be used. Species not in your list are ignored.
#'        This is useful if you are only interested in a subset of species, i.e. in red listing
#' @param ignore.ireland Logical, if \code{TRUE} Irish hectads are removed. Default
#'        is \code{FALSE}
#' @param ignore.channelislands Logical, if \code{TRUE} channel island hectads are 
#'        removed. Default is \code{FALSE}
#' @param sinkdir String giving the output directory for result
#' @param get_names_from_BRC Default is \code{FALSE}. if \code{TRUE} it assumes the CONCEPT 
#'        in your data relates to a BRC concept code and the names of species will be retireve
#'        from the database using \code{channel} for use in output.
#' @param time_periods A dataframe object with two columns. The first column contains the
#'        start year of each time period and the second column contains the end year of 
#'        each time period. This is required if running Frescalo or basic trends.
#' @param channel An ODBC channel, creaded using odbcConnect(), this can be used to get spp
#'        names if get_names_from_BRC is \code{TRUE}
#' @param Plot_Fres Logical, if \code{TRUE} maps are produced by Frescalo
#' @param Fres_weights 'LC' specifies a weights file based on landcover data
#'        for the UK and 'VP' uses a weights file based on vascular plant data for the UK
#'        , both are included in the package. Alternativly a custom weights file can be
#'        given as a data.frame. This must have three columns: target cell, neighbour cell,
#'        weight. 
#' @param non_benchmark_sp a character vector giving the concepts of species not to be
#'        used as benchmarks in Frescalo
#' @param fres_site_filter Optionally a character vector of the names of sites to be used for
#'        in the trend analysis. Sites not include in this list are not used for estimating
#'        TFactors. Default is \code{NULL} and all sites are used
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
#'          - \tab \code{F_den_df} \tab Denominator degrees of freedom from the F-statistic \cr
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
#' #script for testing frescalo
#' #data will be written to your working directory
#'
#' data(ex_dat)
#'
#' x<-frescalo(data=ex_dat,
#'          taxon_name='EXAMPLE',
#'          time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
#'          sinkdir=paste(getwd(),'/example_sparta_output',sep=''))
#' }

frescalo <-
  function(data=NULL,#your data (.rdata files) as a file path (or list of file paths)
           taxon_name=NULL,#the name of your data (string) or list of names,
           #used to name your output files
           species_to_include=NULL, #A species list with which to subset your data
           ignore.ireland=F,#do you want to remove Irish hectads?
           ignore.channelislands=F, ##do you want to remove Channel Islands (they are not UK)?
           sinkdir=NULL,#where is the data going to be saved
           taxon_reg=NULL, #additional info about each species which is merged to results
           time_periods=NULL, #a list of vector pairs used in frescalo (ie 'c((1990,1995),(1996,2000))')
           channel=NULL, #channel is needed to get a taxon_reg for frescalo if not given
           Plot_Fres=TRUE,#do you want to plot the maps and do linear regression for frescalo?
           Fres_weights='LC',#the name of the weights file in the frescalo directory to be used
           non_benchmark_sp=NULL,#species not to be used as benchmarks 
           get_names_from_BRC=FALSE, #change to TRUE is using concepts and you want frescalo to output with names
           fres_site_filter=NULL #optional list of sites not to be included in analysis
  ){
    
    required.packages <- c('lme4','reshape2','sp','RODBC','gdata','ggplot2')
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    if(is.null(data)){
      cat("Choose .csv or .rdata file. Else assign data.frame of data to 'data'")
      data<-choose.files()
    } 
    if(is.null(taxon_name)){
      warning("'taxon_name' not given, defaulted to 'NOBODY'. If you are analysing more than one dataset you will end up overwriting your output unless you provide taxon names")
      taxon_name<-'NOBODY'
    }
    if(class(data)=='character'&length(data)!=length(taxon_name)) stop("taxon_name and data are not the same length")
    if(is.null(sinkdir)) stop("Like Odysseus I could use some directions. I need to know the where to save output, use the 'sinkdir' arguement")
    dir.create(sinkdir,showWarnings = FALSE)
    if(is.null(time_periods)) stop('time_periods must be set')
    if(is.null(channel) & get_names_from_BRC) stop('frescalo needs channel to get names for concepts')
    if(!is.null(channel)) require(RODBC)
    if(is.null(taxon_reg)&!is.null(channel)) taxon_reg<-sqlQuery(channel, "select CONCEPT, CONCEPT_REC, NAME, NAME_ENGLISH, VALID from BRC.taxa_taxon_register where valid = 'V'")
    data(UK)
    #set up frescalo path
    frespath<-paste(normalizePath(.Library),'\\sparta\\exec\\Frescalo_2a.exe',sep='')
    #unpack weights file if needed
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
    if(class(Fres_weights)=='data.frame'){
      if(length(Fres_weights)!=3) stop('Fres_weights data.frame must have three columns: target, neighbour, weight')
      if(class(Fres_weights[,3])=='factor') stop('Weights column should not be a factor, it should be numeric')
      if(max(as.numeric(Fres_weights[,3]))>1|min(as.numeric(Fres_weights[,3]))<0) stop('Weight in Fres_weights should be between 0 and 1')
      round(as.numeric(Fres_weights[,3]),4)
      write.fwf(Fres_weights,colnames=FALSE,rownames=FALSE,width=c(9,9,7),file=paste(dirname(frespath),'/Custom_Wts.txt',sep=''))
      Fres_weights_name<-'Custom_Wts.txt'         
    }
    
    datecode <- format(Sys.Date(),'%y%m%d')
    print('loading raw data')
    if(class(data)=='data.frame'){
      taxa_data<-data
      rm(data)
    } else if(class(data)=='character'&grepl('.rdata',data,ignore.case=TRUE)){
      loaded<-load(data)
      if(sum(grepl('taxa_data',loaded))==0){
        stop('The .rdata file used does not contain an object called "taxa_data"')
      }
    }else if(grepl('.csv',data,ignore.case=TRUE)){
      taxa_data<-read.table(data,header=TRUE,stringsAsFactors=FALSE,sep=',')
      if('year' %in% names(taxa_data)) taxa_data$year <- as.numeric(taxa_data$year)
      taxa_data$CONCEPT<-as.factor(taxa_data$CONCEPT)
      if('Date' %in% colnames(taxa_data)) taxa_data$Date<-as.Date(taxa_data$Date)
    }
    
    if(!'year' %in% names(taxa_data)) taxa_data$year<-as.numeric(format(taxa_data$Date,'%Y'))     
    
    if(!is.null(species_to_include)) taxa_data<-taxa_data[taxa_data$CONCEPT %in% species_to_include,]
    if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$hectad)==1)
    if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$hectad)==FALSE)
    
    print(paste('Running Frescalo for',taxon_name))
    
    #If the data has a startdate and enddate ensure the dates are within one 
    #of the time periods, else if it just has a year, ensure this is in the
    #right time period
    if('TO_STARTDATE' %in% colnames(taxa_data)){
      for(ii in 1:length(time_periods[,1])){
        taxa_data$yearnew[as.numeric(format(taxa_data$TO_STARTDATE,'%Y'))>=time_periods[ii,1][[1]] &
                            taxa_data$year<=time_periods[ii,2][[1]]]<-rowMeans(time_periods[ii,])[[1]]
      }
    }else{
      for(ii in 1:length(time_periods[,1])){
        taxa_data$yearnew[taxa_data$year>=time_periods[ii,1][[1]] &
                            taxa_data$year<=time_periods[ii,2][[1]]]<-rowMeans(time_periods[ii,])[[1]]
      }
    }         
    
    #just hectad, concept, timeperiod
    taxa_data<-na.omit(unique(taxa_data[c('hectad','CONCEPT','yearnew')]))
    
    # Setup output
    fresoutput<-paste(sinkdir,'/',taxon_name,'_frescalo',datecode,sep='')
    dir.create(fresoutput,showWarnings = FALSE)
    
    #create non benchmark list if needed
    non_bench_txt<-NULL
    if(!is.null(non_benchmark_sp)){
      NonBenchPath<-paste(dirname(frespath),'/NonBench.txt',sep='')         
      write.table(non_benchmark_sp,sep='\n',file=NonBenchPath,row.names=F,col.names=F,quote=F)
      non_bench_txt<-'NonBench.txt'
    }
    
    spp_names<-NULL
    
    #Set up species names
    if(get_names_from_BRC==FALSE){
      new_names <- data.frame(SPECIES=paste('S',1:length(unique(taxa_data$CONCEPT)),sep=''),NAME=unique(taxa_data$CONCEPT))
      spp_names <- paste(fresoutput,'/species_names.csv',sep='')
      write.table(new_names,spp_names,sep=',',row.names=FALSE)
      taxa_data<-merge(taxa_data,new_names,by.x='CONCEPT',by.y='NAME',all=T)
      taxa_data<-taxa_data[c('hectad','SPECIES','yearnew')]
      if(!is.null(non_benchmark_sp)){
        write.table(new_names$SPECIES[new_names$NAME %in% non_benchmark_sp],sep='\n',file=NonBenchPath,row.names=F,col.names=F,quote=F)
      }
    }
    
    #If needed create a site filter
    fres_site_txt<-NULL
    if(!is.null(fres_site_filter)){
      fres_site_path<-paste(dirname(frespath),'/fres_site_filter.txt',sep='')         
      write.table(fres_site_filter,sep='\n',file=fres_site_path,row.names=F,col.names=F,quote=F)
      fres_site_txt<-'fres_site_filter.txt'          
    }         
    
    if(nrow(taxa_data)==0) stop("By Zeus' beard! The data heading into frescalo has 0 rows. Make sure your time periods match your years, and your not subsetting out all your data")
    fres_return<-run_fresc_file(channel=channel,fres_data=taxa_data,output_dir=fresoutput,frescalo_path=frespath,fres_f_wts=Fres_weights_name,
                                Plot=Plot_Fres,spp_names_file=spp_names,fres_f_nobench=non_bench_txt,fres_f_filter=fres_site_txt)
    class(fres_return)<-'frescalo'
    
    # Calculate z-values if only two time periods & lm_stats exists
    if(length(time_periods[,1])==2 & 'lm_stats' %in% names(fres_return)){
      fres_lm_path<-paste(sinkdir,'/',taxon_name,'_frescalo',datecode,'/Frescalo/Maps_Results/Frescalo Tfactor lm stats.csv',sep='')
      fres_lm<-read.csv(fres_lm_path)
      trendpath<-paste(sinkdir,'/',taxon_name,'_frescalo',datecode,'/Frescalo/Output/Trend.txt',sep='')
      zvalues<-fres_zvalues(trendpath)
      lm_z<-merge(x=fres_lm,y=zvalues,by='SPECIES',all=TRUE)
      write.csv(lm_z,fres_lm_path)
      fres_return$lm_stats<-lm_z
    }
    print('frescalo complete')
    return(fres_return)
  }