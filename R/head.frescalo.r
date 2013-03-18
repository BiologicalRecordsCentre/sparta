head.frescalo<-function(x){
  
  elements<-names(x)
  element_names<-c("paths","trend","stat","freq","log","lm_stats")
  long_names<-c('file paths to frescalo log, stats, freq, trend and freq-quick load files',
               'trends file, giving the tfactor value for each species at each time period',
               'statistics for each hectad in the analysis',
               'rescaled frequencies for each location and species',
               'log file',
               'trends in tfactor over time')
    
  for(i in 1:length(elements)){
    loc<-match(elements[i],element_names)
    cat('\n')
    cat(paste('Preview of',long_names[loc],':\n\n',sep=' '))
    print(head(x[[i]]))
    cat('\n')
  }
}

