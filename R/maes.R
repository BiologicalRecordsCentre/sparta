maes <-function(Data=NULL,#your data (.rdata files) as a file path (or list of file paths), or r object
           taxon_name=NULL,#the name of your data (string) or list of names,
           split_yr=NULL, #First year of second time period
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
    
    #clear warnings
    assign("last.warning", NULL, envir = baseenv())
    if(is.na(site_col)){
      warning('Site column not specified')
    }
    if(is.na(sp_col)){
      warning('Species column not specified')
    }
    if(is.na(year_col)){
      if(is.na(start_col)|is.na(end_col)){
        warning('year_col or start_col and end_col must be given')
      } 
    } else {
      if(!is.na(start_col)|!is.na(end_col)){
        warning('year_col cannot be used at the same time as start_col and end_col')
      }
    }
    new.colnames<-c(site_col,sp_col,year_col,start_col,end_col)
    missingColNames<-new.colnames[!new.colnames %in% names(Data)]
    if(length(missingColNames)>0) warning(paste(missingColNames, 'is/are not names of columns in your data'))
    
    if(length(warnings())>0) stop()
  
    required.packages <- c('reshape2')
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    if(is.null(Data)){
      cat("Choose .csv or .rdata file. Else assign data.frame of data to 'data'")
      Data<-choose.files()
    } 
    
    if(is.null(taxon_name)){
      warning("'taxon_name' not given, defaulted to 'NOBODY'. If you are analysing more than one dataset you will end up overwriting your output unless you provide taxon names")
      taxon_name<-'NOBODY'
    }
    
    if(class(Data)=='character'&length(Data)!=length(taxon_name)) stop("taxon_name and data are not the same length")
    
    if(!is.null(sinkdir)) dir.create(sinkdir,showWarnings = FALSE)
    
    #set up object to return
    return_object=list()
    
    #how many datasets are there?
    if(class(Data)=='data.frame'){
      length_data<-1
    }else {
      length_data<-length(Data)
    }
    
    for(i in 1:length_data){
      
      print(paste('Starting',taxon_name[i]))
      datecode <- format(Sys.Date(),'%y%m%d')
      if(class(Data)=='data.frame'){
        taxa_data<-Data
        rm(Data)
      } else if(class(Data)=='character'&grepl('.rdata',Data[i],ignore.case=TRUE)){
        print('loading raw data')
        loaded<-load(Data[i])
        if(class(Data)=='character'&sum(grepl('taxa_data',loaded))==0){
          stop('The .rdata file used does not contain an object called "taxa_data"')
        }
      }else if(grepl('.csv',Data[i],ignore.case=TRUE)){
        print('loading raw data')
        taxa_data<-read.table(Data[i],header=TRUE,stringsAsFactors=FALSE,sep=',')
      }
      
      #rename columns
      taxa_data<-maes_columns(taxa_data,list(year_col,site_col,sp_col,start_col,end_col))
    
      if(!is.na(year_col)) taxa_data$Year<-as.numeric(taxa_data$Year)
      if(!is.na(sp_col))taxa_data$Species<-as.factor(taxa_data$Species)
      if(!is.na(start_col)&!is.na(end_col)){      
        for( i in c('Start','End')){
          if(!'POSIXct' %in% class(taxa_data[[i]]) & !'Date' %in% class(taxa_data[[i]])){
            warning(paste('column',i,'Date is not in a date format. This should be of class "Date" or "POSIXct"'))
          }
        }
      }
      
      if(ignore.ireland) taxa_data <- subset(taxa_data, regexpr('^[A-Z]{2}', taxa_data$Site)==1)
      if(ignore.channelislands) taxa_data <- subset(taxa_data, grepl('^[Ww][[:alpha:]]{1}', taxa_data$Site)==FALSE)
    }
      
    maes_out<-fit_maes_trend(records=taxa_data,splityr=split_yr,min_sp=min_sp)
    
    return(maes_out)
  }