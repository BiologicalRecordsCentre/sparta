persistance_table <-
function(brc.data,eng.hectads,homedir,year_col='year',concept_col='CONCEPT',
                              SQ_10_col='hectad',Year.range=1970:2009,split_yr=1990,taxa='UNKNOWN'){
  
  require(reshape2)
  colnames(brc.data)[colnames(brc.data)==year_col]<-'YEAR'
  colnames(brc.data)[colnames(brc.data)==concept_col]<-'CONCEPT'
  colnames(brc.data)[colnames(brc.data)==SQ_10_col]<-'SQ_10'
  
  #Subset to year range desired and not Irish or W (Ireland and sea)
  brc.data<-subset(brc.data,YEAR<=max(Year.range) & YEAR>=min(Year.range) & nchar(as.character(SQ_10))>3 & substring(brc.data$SQ_10,1,1)!='W') 
  
  #create a new dataset with only english hectads for persistance tables
  data<-brc.data[brc.data$SQ_10 %in% eng.hectads,]
  data$tp[data$YEAR<split_yr]<-1 #create column indicating time period
  data$tp[data$YEAR>=split_yr]<-2
  data<-unique(data[c('CONCEPT','SQ_10','tp')]) #get unique combinations of concept, sq_10 and time-period
  data<-dcast(data, CONCEPT + SQ_10 ~ tp, value.var='tp') #cast the data to get two columns for timeperiod
  colnames(data)<-c('Concept','SQ_10','tp1','tp2')
  data$SQ_10<-as.character(data$SQ_10)
  
  # Remove hectads with no data in the second time period
  zero.sum<-function(x) sum(as.numeric(x),na.rm = TRUE)==0 # I use this function to look for hectads with no data in the second time period
  tp2sq_10<-tapply(data$tp2,data$SQ_10,zero.sum) #TRUE if there are no records in tp2
  names_notp2<-names(tp2sq_10[tp2sq_10==TRUE])#Names of hectads with no data in tp2
  data<-data[!data$SQ_10 %in% names_notp2,]#Remove these hectads
  
  #Get the Lat Long for all hectads in the dataset
  Hectads<-data.frame(Hectad=as.character(unique(data$SQ_10)))
  for (i in 1:length(Hectads$Hectad)){
    Hectads$Lat[i]<-gr2gps_latlon(as.character(Hectads$Hectad[i]),centre = TRUE)[[1]]
    Hectads$Long[i]<-gr2gps_latlon(as.character(Hectads$Hectad[i]),centre = TRUE)[[2]]
  }
  
  #Format data for persistance from records
  species_list<-data.frame(concept=unique(as.character(data$Concept)))
  
  for(i in unique(as.character(data$Concept))){
    
    concept_data<-subset(data,data$Concept==i)
    concept_data<-concept_data[!is.na(concept_data["tp1"]),][,c('Concept','SQ_10','tp2')]#remove cells with absence in first time period
    concept_data$tp2[concept_data$tp2==2]<-1 #covert tp2 to 1/0 presence/absence
    concept_data$tp2[is.na(concept_data$tp2)]<-0
    
    if(!exists('persist')){
      persist<-concept_data
    }else{
      persist<-rbind(persist,concept_data)
    }
    
    species_list$ncells[species_list$concept==i]<-length(concept_data$tp2) #output numbver of cells for this concept
    species_list$nextinct[species_list$concept==i]<-length(concept_data$tp2)-sum(concept_data$tp2)
  }
  
  data_out<-merge(x=persist,y=Hectads,by.x="SQ_10",by.y="Hectad",all.x=TRUE,all.y=FALSE)
  
  #write out file
  datecode<-format(Sys.Date(),'%y%m%d')
  dir.create(paste(homedir,'/Persistance tables',sep=''),showWarnings = FALSE)
  write.csv(data_out,paste(homedir,'/Persistance tables/','persistance_',taxa,'_',datecode,'.csv',sep=''),row.names=FALSE)
  write.csv(species_list,paste(homedir,'/Persistance tables/','speciesData_',taxa,'_',datecode,'.csv',sep=''),row.names=FALSE)
  #NOTE: for completeness species which were not found prior to 1989 are included in
  #the speciesData file but have a 0 in both columns
}
