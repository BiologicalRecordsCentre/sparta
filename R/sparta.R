#' Trend analyses for unstructured data
#' 
#' A function that implements a range of methods used to analyses trends in unstructured
#' occurrence datasets. If only running \code{\link{frescalo}}, then that function is
#' recommended. This function additionally includes basic metrics such as Telfer and
#' mixed-model and list-length models. In the future this function is likely to be replaced
#' with a set of functions, one for each method since in its current state it is rather
#' cumbersome.
#'
#' @param data A dataframe object or a vector of strings giving the file path to the
#'        location(s) of data this can be in either .rdata or .csv format.
#'        For Frescalo required columns are: 'CONCEPT', 'hectad' and 'Date'. Optionally
#'        'TO_STARTDATE' can be included, if it is this is assumed to be the start date
#'        (from which start year is extracted) and year is assumed to be the end date.
#'        If \code{NULL} the user is prompted to select a .csv or .rdata file.
#'        For mixed models and list length models the following columns are required: 'CONCEPT',
#'        'kmsq' and 'Date', optionally 'TO_STARTDATE' can be added. For basic methods
#'        the following columns are required: 'CONCEPT','hectad' and 'Date', optionally
#'        'TO_STARTDATE' can be added.
#'
#' @param taxon_name A vector of strings giving the name(s) of data. This is used to name
#'        output files when written
#' @param Run_models Logical, if \code{FALSE} no models are run. This overrules \code{Run_MM, 
#'        Run_LL, Run_Fres} and \code{Run_Basic}. Default is \code{TRUE}
#' @param Run_MM Logical, if \code{TRUE} mixed model analyses are run. Default is \code{TRUE}
#' @param Run_LL Logical, if \code{TRUE} list-length model analyses are run. Default is \code{TRUE}
#' @param Run_Fres Logical, if \code{TRUE} Frescalo analyses are run. Default is \code{TRUE}
#' @param Run_Basic Logical, if \code{TRUE} basic trend analyses are run. This includes power law residual (plr), Telfer's change index and proportional
#'         difference. Default is \code{TRUE}
#' @param Create_persistance_table NOT IMPLEMENTED
#' @param Create_persistance_summary NOT IMPLEMENTED 
#' @param Calc_D_england_only NOT IMPLEMENTED 
#' @param Year.range The time period over which you wish to analyse your trends. This
#'        may or may not be the same as min(time_periods):max(time_periods). This is used
#'        for analyses that do not use time periods, such as the mixed models and list
#'        length models.
#' @param res The 'resolution' at which mixed models and list-length models analyse 
#'        the data. \code{'visit'} defines visits as unique combinations of km-square and date
#'        while \code{kmyr} defines visits as a unique combination of year km-square and
#'        year. Visit method is recommended.
#' @param min.L The minimum list length (number of species) required from a visit for it 
#'        to be included in the mixed model analysis. Default is 4 but should probably
#'        be changed dependent on the dataset the analysis is being applied to
#' @param min.yrs when \code{wellsamp} is \code{'visit'} then only sites for which there 
#'        \code{min.yrs} number of well.sampled visits (as defined by \code{res} and \code{min.L})
#'        are included in the mixed model analysis. If \code{wellsamp} is \code{'year'}
#'        then only sites with well sampled visits in \code{min.yrs} number of years are
#'        included in mixed model analysis.
#' @param od This option allows modelling overdispersion (\code{TRUE}) in mixed models
#' @param V This option, if \code{TRUE}, sets mixed model verbose to \code{TRUE}. Allowing
#'        the interations of each model to be veiwed.
#' @param split_yr USED IN PERSISTANCE TABLES, NOT IMPLEMENTED 
#' @param species_to_include A list of vector of strings (that match your CONCEPT column in
#'        data data) which are to be used. Species not in your list are ignored.
#'        This is useful if you are only interested in a subset of species, i.e. in red listing
#' @param ignore.ireland Logical, if \code{TRUE} Irish hectads are removed. Default
#'        is \code{FALSE}
#' @param ignore.channelislands Logical, if \code{TRUE} channel island hectads are 
#'        removed. Default is \code{FALSE}
#' @param sinkdir String giving the output directory for result
#' @param Log Logical, if \code{TRUE} log files are created in \code{sinkdir}
#' @param taxon_reg A lookup table that gives names to your concepts. If not given the
#'        names are gathered from the taxon_register on Oracle (where possible)
#' @param print_progress Logical, if \code{TRUE} progress is printed to console when
#'        running mixed models and list length models. Default is \code{TRUE}
#' @param time_periods A dataframe object with two columns. The first column contains the
#'        start year of each time period and the second column contains the end year of 
#'        each time period. This is required if running Frescalo or basic trends, but
#'        is not used in teh modelling methods.
#' @param channel An ODBC channel, creaded using odbcConnect(), this can be used to get spp
#'        names if get_names_from_BRC is \code{TRUE}
#' @param Plot_Fres Logical, if \code{TRUE} maps are produced by Frescalo
#' @param Fres_weights 'LC' specifies a weights file based on landcover data
#'        for the UK and 'VP' uses a weights file based on vascular plant data for the UK
#'        , both are included in the package. Alternativly a custom weights file can be
#'        given as a data.frame. This must have three columns: target cell, neighbour cell,
#'        weight. 
#' @param Telfer_min_sq The minimum number of squares occupied in the first time period
#'        in order for basic trends to be calculated for a species
#' @param non_benchmark_sp a character vector giving the concepts of species not to be
#'        used as benchmarks in Frescalo
#' @param non_benchmark_sp a character vector or data.frame with one column, giving the 
#'        concepts of species not to be used as benchmarks in Frescalo. Default is 
#'        \code{NULL} and all sites are used.
#' @param fres_site_filter Optionally a character vector or data.frame with one column, giving
#'        the names of sites to be used for in the trend analysis. Sites not include in this
#'        list are not used for estimating TFactors. Default is \code{NULL} and all sites are
#'        used.
#' @param phi Target frequency of frequency-weighted mean frequency. Default is 0.74 as in
#'        Hill (2011). If this value is smaller than the 98.5 percentile of input phi it is
#'        automatically increased and a warning message is generated.
#' @param alpha the proportion of the expected number of species in a cell to be treated as
#'        benchmarks. Default is 0.27 as in Hill (2011).
#' @return Results are saved to file and most are returned to R.
#'          
#'         A list is returned:
#'         
#'         \item{\bold{$basic_methods}}{This gives the results of the basic methods. This includes three
#'         measures: power law residual (plr), Telfer's change index and proportional
#'         difference. These methods can only compare two time periods and so if there are
#'         more than two time periods in 'time_periods' these metrics are calculated for all
#'         pairwise comparisons. The number after the column name gives the time periods
#'         being compared where 1 is the first time period, 2 the second etc.}
#'         \item{\bold{$model_methods}}{This gives the results from the mixed models and list length
#'         models. [TOM TO ADD MORE DETAILS HERE]}
#'         \item{\bold{$frescalo}}{This gives the results of the frescalo analysis. See
#'         \code{\link{frescalo}}}
#'         
#' @keywords trends
#' @import lme4 reshape2 sp RODBC
#' @examples
#' \dontrun{
#' #script for testing sparta
#' #data will be written to your working directory
#'
#' data(ex_dat)
#'
#' x<-sparta(data=ex_dat,
#'          taxon_name='EXAMPLE',
#'          time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
#'          sinkdir=paste(getwd(),'/example_sparta_output',sep=''),
#'          Run_Fres=T,Run_MM=T,Run_LL=T,Log=T,Run_Basic=T)
#' }

sparta <-
  function(data=NULL,#your data (.rdata files) as a file path (or list of file paths)
           taxon_name=NULL,#the name of your data (string) or list of names,
           #used to name your output files
           Run_models=T,#do you want to run models
           Run_MM=T,#do you want to run mixed models
           Run_LL=T,#do you want to run list length models
           Run_Fres=T,#do you want to run frescalo
           Run_Basic=T, #
           Create_persistance_table=F,#do you want to create persistance tables
           Create_persistance_summary=F,#do you want to create persistance summary tables
           #this can onlt be run if the models have been run
           Calc_D_england_only=F,#Do you want fractal D to be calculated for England only?
           Year.range=NULL,#used to subset data and in persistance tables
           res='visit', #'visit' or 'kmyr' resolution for data to be modelled
           min.L=4,#minimum list length for mixed models
           min.yrs=3,#minimum number of years for 'well sampled' sites (mixed models)
           od=F, #use overdispersion in MM
           V=F, #use verbose in MM
           split_yr=NULL, #First year of second time period
           species_to_include=NULL, #A species list with which to subset your data
           ignore.ireland=F,#do you want to remove Irish hectads?
           ignore.channelislands=F, ##do you want to remove Channel Islands (they are not UK)?
           sinkdir=NULL,#where is the data going to be saved
           Log=T,#do you want a log file
           taxon_reg=NULL, #additional info about each species which is merged to results
           print_progress=T,#do you want to print progress through species?
           time_periods=NULL, #a list of vector pairs used in frescalo (ie 'c((1990,1995),(1996,2000))')
           channel=NULL, #channel is needed to get a taxon_reg for frescalo if not given
           Plot_Fres=TRUE,#do you want to plot the maps and do linear regression for frescalo?
           Fres_weights='LC',#the name of the weights file in the frescalo directory to be used
           non_benchmark_sp=NULL, 
           Telfer_min_sq=5, #the min number of cells needed to be occupied in tp1 for Telfer to be run
           get_names_from_BRC=FALSE, #change to TRUE is using concepts and you want frescalo to output with names
           fres_site_filter=NULL, #optional list of sites not to be included in analysis
           phi = NULL, #phi value for frescalo
           alpha = NULL #alpha value for frescalo 
    ){
    
    required.packages <- c('lme4','reshape2','sp','RODBC','gdata','ggplot2')
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    if(is.null(data)){
      cat("Choose .csv or .rdata file. Else assign data.frame of data to 'data'")
      data<-choose.files()
    } 
    if(!'POSIXct' %in% class(data$Date) & !'Date' %in% class(data$Date)){
      stop('column Date is not in a date format. This should be of class "Date" or "POSIXct"')
    }
    if(is.null(taxon_name)){
      warning("'taxon_name' not given, defaulted to 'NOBODY'. If you are analysing more than one dataset you will end up overwriting your output unless you provide taxon names")
      taxon_name<-'NOBODY'
    }
    if(class(data)=='character'&length(data)!=length(taxon_name)) stop("taxon_name and data are not the same length")
    if(is.null(sinkdir)) stop("Like Odysseus I could use some directions. I need to know the where to save output, use the 'sinkdir' arguement")
    dir.create(sinkdir,showWarnings = FALSE)
    if(Run_Basic & is.null(time_periods)) stop('time_periods must be set for Basic trends analysis')
    if(Run_Fres & is.null(time_periods)) stop('time_periods must be set for Frescalo')
    if(Run_Fres & is.null(channel) & is.null(get_names_from_BRC)) stop('Frescalo needs channel to get names for concepts')
    if(!is.null(channel)) require(RODBC)
    if(is.null(taxon_reg)&!is.null(channel)) taxon_reg<-sqlQuery(channel, "select CONCEPT, CONCEPT_REC, NAME, NAME_ENGLISH, VALID from BRC.taxa_taxon_register where valid = 'V'")
    if(Run_Fres){
      data(UK)
      frespath<-paste(normalizePath(.Library),'\\sparta\\exec\\Frescalo_2a.exe',sep='')
      if(!is.null(phi)){
        if(phi>0.95|phi<0.5) stop("phi is outside permitted range of 0.50 to 0.95")
      }
      if(!is.null(alpha)){
        if(alpha>0.5|alpha<0.08) stop("alpha is outside permitted range of 0.08 to 0.50")
      }
      if(class(non_benchmark_sp)=='data.frame'){
        if(length(non_benchmark_sp)!=1){
          stop('data.frame "non_benchmark_sp" should only have one column')
        } else {
          non_benchmark_sp<-as.character(non_benchmark_sp[[1]])
        }
      }
      if(class(fres_site_filter)=='data.frame'){
        if(length(fres_site_filter)!=1){
          stop('data.frame "fres_site_filter" should only have one column')
        } else {
          fres_site_filter<-as.character(fres_site_filter[[1]])
        }
      }
    } 
    #unpack weights file if needed
    if(Run_Fres&class(Fres_weights)=='character'){
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
    if(Run_Fres&class(Fres_weights)=='data.frame'){
      if(length(Fres_weights)!=3) stop('Fres_weights data.frame must have three columns: target, neighbour, weight')
      if(class(Fres_weights[,3])=='factor') stop('Weights column should not be a factor, it should be numeric')
      if(max(as.numeric(Fres_weights[,3]))>1|min(as.numeric(Fres_weights[,3]))<0) stop('Weight in Fres_weights should be between 0 and 1')
      round(as.numeric(Fres_weights[,3]),4)
      write.fwf(Fres_weights,colnames=FALSE,rownames=FALSE,width=c(9,9,7),file=paste(dirname(frespath),'/Custom_Wts.txt',sep=''))
      Fres_weights_name<-'Custom_Wts.txt'         
    }
    if(class(data)=='data.frame'){
      length_data<-1
    }else {
      length_data<-length(data)
    }
    
    return_object=list()
    
    for(i in 1:length_data){
      
      print(paste('Starting',taxon_name[i]))
      
      datecode <- format(Sys.Date(),'%y%m%d')
      
      if(Log){
        logfilename<-paste(sinkdir,'/Log files/', taxon_name[i], '_', datecode, '.txt', sep='')
        dir.create(dirname(logfilename),showWarnings = FALSE)
        report(logfilename, paste('Starting models for',taxon_name[i]))
      }
      
      if(Run_models){
        #Read in data
        #The important columns in this data are CONCEPT,kmsq,Date and year
        print('loading raw data')
        if(class(data)=='data.frame'){
          taxa_data<-data
          rm(data)
        } else if(class(data)=='character'&grepl('.rdata',data[i],ignore.case=TRUE)){
          loaded<-load(data[i])
          if(class(data)=='character'&sum(grepl('taxa_data',loaded))==0){
            stop('The .rdata file used does not contain an object called "taxa_data"')
          }
        }else if(grepl('.csv',data[i],ignore.case=TRUE)){
          taxa_data<-read.table(data[i],header=TRUE,stringsAsFactors=FALSE,sep=',')
          if('year' %in% names(taxa_data)) taxa_data$year <- as.numeric(taxa_data$year)
          taxa_data$CONCEPT<-as.factor(taxa_data$CONCEPT)
          taxa_data$Date<-as.Date(taxa_data$Date)
        }
        
        
        if(Run_MM|Run_LL) if(!'kmsq' %in% names(taxa_data)) stop('kmsq column is needed for Mixed models or List length models. If using UK grid references this can be created using the reformat_gr() function')
        if(!'year' %in% names(taxa_data)) taxa_data$year<-as.numeric(format(taxa_data$Date,'%Y')) 
        
        if(!is.null(Year.range)) taxa_data<-taxa_data[taxa_data$year %in% Year.range,] #subset to year range
        if(!is.null(species_to_include)) taxa_data<-taxa_data[taxa_data$CONCEPT %in% species_to_include,] #subset to species in list
        if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$hectad)==1)
        if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$hectad)==FALSE)
        if(Log) report(logfilename, paste('Raw data loaded for', length(unique(taxa_data$CONCEPT)), 'species'))
        
        taxa_data_master<-taxa_data
        if(Run_Basic){
          if(Log) report(logfilename, 'Running Basic trends analysis')
          
          #Deal with date ranges
          if('TO_STARTDATE' %in% colnames(taxa_data)){
            for(ii in 1:length(time_periods[,1])){
              taxa_data$yearnew[as.numeric(format(taxa_data$TO_STARTDATE,'%Y'))>=time_periods[ii,1][[1]] &
                                  taxa_data$year<=time_periods[ii,2][[1]]]<-floor(rowMeans(time_periods[ii,])[[1]])
            }
          }else{
            for(ii in 1:length(time_periods[,1])){
              taxa_data$yearnew[taxa_data$year>=time_periods[ii,1][[1]] &
                                  taxa_data$year<=time_periods[ii,2][[1]]]<-floor(rowMeans(time_periods[ii,])[[1]])
            }
          }
          taxa_data$year<-taxa_data$yearnew
          taxa_data<-taxa_data[!is.na(taxa_data$year),]
          
          for(ii in 1:(length(time_periods[,1])-1)){
            for(j in (ii+1):length(time_periods[,1])){
              time_periods_temp<-time_periods[c(ii,j),] 
              taxon_temp<-paste(taxon_name[i],'_',ii,'_',j,sep='')
              basic_temp<-basic_trends(taxa_data,time_periods_temp,min_sq=Telfer_min_sq,splityr=NULL,sp_list=species_to_include)
              colnames(basic_temp)<-paste(colnames(basic_temp),'_',ii,'_',j,sep='')
              basic_temp$CONCEPT<-row.names(basic_temp)
              print(paste('Basic trends for tp',ii,'vs tp',j,'done',sep=' '))
              if(exists('basic_master')){
                basic_master<-merge(basic_master,basic_temp,by='CONCEPT',all=TRUE)
              }else{
                basic_master<-basic_temp
              }
            }
          }
          basic_master<-merge(basic_master,unique(taxa_data[c('CONCEPT')]),by='CONCEPT',all=TRUE)
          file_name<-paste('Basic_trends_',taxon_name[i],'_',datecode,'.csv',sep='')
          if (file.exists(file_name)){
            file_name<-paste('Basic_trends_',taxon_name[i],'_',datecode,'_',format(Sys.time(),'%H%M'),'.csv',sep='')
            warning(paste('Basic_trends_',taxon_name[i],'_',datecode,'.csv',' already exists.',
                          ' The new data is saved with the time appended to the file name',sep=''),call.=FALSE,immediate.=TRUE)
          }
          write.csv(basic_master,file_name,row.names=FALSE)
          if(Log) report(logfilename, 'Basic trend analysis complete')
          
          return_object[['basic_methods']]<-basic_master
        }
        
        
        if(Run_MM | Run_LL){
          
          taxa_data<-taxa_data_master
          
          #only data with year accuracy is included in these analyses 
          if('TO_STARTDATE' %in% colnames(taxa_data)) taxa_data <- taxa_data[format(taxa_data$TO_STARTDATE,'%Y')==format(taxa_data$Date,'%Y'),]
          
          if(Run_MM & Run_LL){
            if(Log) report(logfilename, 'Recasting data for List-length and mixed models')
            print('Recasting data for List-length and mixed models')
          } 
          if(Run_MM & !Run_LL){
            if(Log) report(logfilename, 'Recasting data for Mixed models')
            print('Recasting data for Mixed models')
          } 
          if(!Run_MM & Run_LL){
            if(Log) report(logfilename, 'Recasting data for List-length models')
            print('Recasting data for List-length models')
          } 
          
          space_time<-cast_recs(taxa_data[!is.na(taxa_data$Date)&!is.na(taxa_data$kmsq),][c('CONCEPT','Date','kmsq')],res)
          
          counter=1
          
          if(counter==1&!file.exists(paste(sinkdir,'/',taxon_name[i],'_ModelsL',min.L,'_',datecode,'.csv', sep=''))){
            file_name<-paste(sinkdir,'/',taxon_name[i],'_ModelsL',min.L,'_',datecode,'.csv', sep='')
          } else if (counter==1&file.exists(paste(sinkdir,'/',taxon_name[i],'_ModelsL',min.L,'_',datecode,'.csv', sep=''))){
            file_name<-paste(sinkdir,'/',taxon_name[i],'_ModelsL',min.L,'_',datecode,'_',format(Sys.time(),'%H%M'),'.csv', sep='')
            warning(paste('file ',taxon_name[i],'_ModelsL',min.L,'_',datecode,'.csv',' already exists.',
                          ' The new data is saved with the time appended to the file name',sep=''),call.=FALSE,immediate.=TRUE)
          }
          
          for (ii in unique(taxa_data$CONCEPT[!is.na(taxa_data$kmsq)&!is.na(taxa_data$year)])){
            if(print_progress) print(paste('Modelling',ii,'- Species',counter,'of',length(unique(taxa_data$CONCEPT[!is.na(taxa_data$kmsq)&!is.na(taxa_data$year)]))))
            y<-unique(taxa_data[taxa_data$CONCEPT==ii&!is.na(taxa_data$kmsq)&!is.na(taxa_data$year),][c('CONCEPT',colnames(space_time)[1],'kmsq')])
            species_space_time <- merge(x=space_time,y=y,all.x=T)
            species_space_time$CONCEPT <- as.character(species_space_time$CONCEPT)
            species_space_time$CONCEPT[is.na(species_space_time$CONCEPT)]<-0
            species_space_time$CONCEPT[species_space_time$CONCEPT==ii]<-1
            if(!grepl('year',colnames(species_space_time)[1])) species_space_time$year<-format(species_space_time$Date, '%Y')
            
            Mod_out<-t(as.data.frame(Models(species_space_time,min.L,min.yrs,MM=Run_MM,LL=Run_LL,od=F,V=F)))
            row.names(Mod_out)<-ii
            Mod_out<-as.data.frame(Mod_out)
            #if(Run_MM) Mod_out$MM_trend10<-pc.change(ilt(10*Mod_out$MM_trend))
            #if(Run_LL) Mod_out$LL_trend10<-pc.change(ilt(10*Mod_out$LL_trend))
            Mod_out$CONCEPT<-row.names(Mod_out)
            if(!is.null(taxon_reg)) Mod_out<-merge(x=Mod_out,y=taxon_reg,by='CONCEPT') #add taxon information
            
            counter=counter+1
            
            if(file.exists(file_name)){
              write.table(Mod_out, file=file_name, append=T,col.names=F,sep=',',row.names=FALSE)
            }else{
              write.table(Mod_out, file=file_name,col.names=T,sep=',',row.names=FALSE)
            }
            
          }
          if(Log) report(logfilename, paste('Models fitted for', counter, 'species'))
          
          model_methods<-read.csv(file_name)
          new_order<-c('CONCEPT',names(model_methods)[-length(names(model_methods))])
          model_methods<-model_methods[new_order]
          model_methods<-model_methods[with(model_methods, order(CONCEPT)),]
          return_object[['model_methods']]<-model_methods
          
          rm(list=c('Mod_out','model_methods'))
        }
        
        if(Run_Fres){
          
          taxa_data<-taxa_data_master
          
          print(paste('Running Frescalo for',taxon_name[i]))
          if(Log) report(logfilename, 'Running Frescalo')               
          
          #If the data has a startdate and enddate ensure the dates are within one 
          #of the time periods, else if it just has a year, enure this is in the
          #right time period
          if(sum(grepl('TO_STARTDATE',colnames(taxa_data)))>0){
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
          fresoutput<-paste(sinkdir,'/',taxon_name[i],'_frescalo',datecode,sep='')
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
                                      Plot=Plot_Fres,spp_names_file=spp_names,fres_f_nobench=non_bench_txt,fres_f_filter=fres_site_txt,
                                      fres_phi_val=phi,fres_bench_val=alpha)
          class(fres_return)<-'frescalo'
          
          # Calculate z-values if only two time periods
          if(length(time_periods[,1])==2 & 'lm_stats' %in% names(fres_return)){
            fres_lm_path<-paste(sinkdir,'/',taxon_name[i],'_frescalo',datecode,'/Maps_Results/Frescalo Tfactor lm stats.csv',sep='')
            fres_lm<-read.csv(fres_lm_path)
            trendpath<-paste(sinkdir,'/',taxon_name[i],'_frescalo',datecode,'/Output/Trend.txt',sep='')
            zvalues<-fres_zvalues(trendpath)
            lm_z<-merge(x=fres_lm,y=zvalues,by='SPECIES',all=TRUE)
            write.csv(lm_z,fres_lm_path,row.names=FALSE)
            fres_return$lm_stats<-lm_z
          }
          return_object[['frescalo']]<-fres_return
          if(Log) report(logfilename,'Frescalo complete')
        }
      }
      
      #####I HAVE NOT FUNCTIONALISED BEYOND HERE#####
      
      #Create persistance tables only for those species thought to be declining in models
      if(Create_persistance_table){
        
        report(logfilename, paste('Creating persistance tables'))
        #Find declining species
        print('read in trends')
        out_files<-dir(sinkdir)
        out_files<-out_files[grep(paste(taxon,'_Models',sep=''),out_files)]
        if(length(out_files)>1) stop(paste("There is more than one trends file for",taxon,'in',sinkdir))  
        
        trends<-read.csv(paste(sinkdir,'/',out_files,sep=''))
        declining_sp<-trends$CONCEPT[trends$MM_trend10<0 | trends$LL_trend10<0]
        
        #get english hectads
        eng.hectads<-read.csv(paste(datadir,'/','English_hectads.csv',sep=''))[,2]
        
        #get data, only for declining species
        print('read in taxa data')
        in_files<-dir(datadir)[grep(paste(taxon),dir(datadir))]
        in_files<-in_files[grep('.rdata',in_files)]
        if(length(in_files)>1) stop(paste("There is more than one raw data file for",taxon,'in',datadir))  
        load(paste(datadir,'/',in_files,sep=''))
        
        taxa_data<-taxa_data[taxa_data$CONCEPT %in% species_to_include,] #subset to species in list
        taxa_data<-taxa_data[taxa_data$CONCEPT %in% declining_sp,]  
        if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$hectad)==1)
        if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$hectad)==FALSE)
        
        #some datasets have start and end date here I restrict these to records that fall in the
        #time periods
        if(sum(grepl('TO_STARTDATE',colnames(taxa_data)))>0){
          tp_one<-min(Year.range):(split_yr-1)
          tp_two<-split_yr:max(Year.range)
          taxa_data<-taxa_data[((as.numeric(format(taxa_data$TO_STARTDATE,'%Y'))%in%tp_one) & (taxa_data$year %in% tp_one))|	
                                 ((as.numeric(format(taxa_data$TO_STARTDATE,'%Y'))%in%tp_two) & (taxa_data$year %in% tp_two)),]
          taxa_data$year[taxa_data$year<split_yr]<-mean(tp_one)
          taxa_data$year[taxa_data$year>=split_yr]<-mean(tp_two)
        }else{
          taxa_data<-taxa_data[taxa_data$year %in% Year.range,] #subset to year range
        }
        
        print('creating persistance tables')
        persistance_table(taxa_data,eng.hectads,homedir,year_col='year',concept_col='CONCEPT',
                          SQ_10_col='hectad',Year.range=Year.range,split_yr=split_yr,taxa=taxon)
        
        report(logfilename, paste('Persistance tables created for', length(declining_sp), 'species declining in models'))
        
        if(Create_persistance_summary){
          print('creating persistance summary table')
          persist_table<-read.csv(paste(homedir,'/Persistance tables/','persistance_',taxon,'_',datecode,'.csv',sep=''))
          persist_SP<-read.csv(paste(homedir,'/Persistance tables/','speciesData_',taxon,'_',datecode,'.csv',sep=''))
          #Add concept group
          group_names<-unique(taxa_data[c('CONCEPT','taxon')])
          persist_SP<-merge(x=persist_SP,y=group_names,by.x='concept',by.y='CONCEPT',all.x=TRUE,all.y=FALSE)
          #Add concept info and trend
          trends_merge<-trends[c('CONCEPT','MM_trend10','LL_trend10','CONCEPT_REC','NAME','NAME_ENGLISH','VALID')]
          persist_SP<-merge(x=persist_SP,y=trends_merge,by.x='concept',by.y='CONCEPT')
          
          #Fractal/Residual D for all time
          for (ii in unique(persist_SP$concept)){
            temp<-subset(taxa_data,CONCEPT==ii)
            if(Calc_D_england_only) temp<-temp[temp$hectad %in% eng.hectads,]
            allt_10<-(length(unique(temp$hectad))*100)#area of occupied 10km squares
            persist_SP$allt_AOO_10[persist_SP$concept==ii]<-allt_10
            temp$SQ_100 <- gsub(pattern='[0-9]', replacement='',temp$hectad)
            allt_100<-(length(unique(temp$SQ_100))*10000)#area of occupied 100km squares
            persist_SP$allt_AOO_100[persist_SP$concept==ii]<-allt_100
            persist_SP$allt_frac_D[persist_SP$concept==ii] <- 2 - (log10(allt_100) - log10(allt_10))#fractal dimension
          }
          
          m1<-lm(persist_SP$allt_frac_D~persist_SP$allt_AOO_10)
          persist_SP$allt_res_D<-m1$residuals
          
          #Fractal/Residual D for second time period only
          taxa_data_tp2<-subset(taxa_data,year>=split_yr)
          for (ii in unique(persist_SP$concept)){
            temp<-subset(taxa_data_tp2,CONCEPT==ii)
            if(Calc_D_england_only) temp<-temp[temp$hectad %in% eng.hectads,]
            tp2_10<-(length(unique(temp$hectad))*100)#area of occupied 10km squares
            persist_SP$tp2_AOO_10[persist_SP$concept==ii]<-tp2_10
            temp$SQ_100 <- gsub(pattern='[0-9]', replacement='',temp$hectad)
            tp2_100<-(length(unique(temp$SQ_100))*10000)#area of occupied 100km squares
            persist_SP$tp2_AOO_100[persist_SP$concept==ii]<-tp2_100
            persist_SP$tp2_frac_D[persist_SP$concept==ii] <- 2 - (log10(tp2_100) - log10(tp2_10))#fractal dimension
          }
          
          m2<-lm(persist_SP$tp2_frac_D~persist_SP$tp2_AOO_10)
          persist_SP$tp2_res_D<-m1$residuals
        }  
        
        write.csv(persist_SP,paste(homedir,'/Persistance tables/','speciesData_',taxon,'_',datecode,'.csv',sep=''),row.names=FALSE)
        report(logfilename, paste('Persistance summary tables finished'))
        cat(file=logfilename, append=T, '\n') # blank line
        
        rm(list=c('taxa_data'))
      }
    }
    print('Completed')
    return(return_object)
    if(Log) report(logfilename,'Finished')
  }
