head.frescalo<-function(x){
  
  elements<-names(x)
  if('lm_stats' %in% elements){
    element_names<-c("paths","trend","stat","freq","log","lm_stats")
    long_names<-c('$paths - file paths to frescalo log, stats, freq, trend and freq-quick load files',
                 '$trend - trends file, giving the tfactor value for each species at each time period',
                 '$stat - statistics for each hectad in the analysis',
                 '$freq - rescaled frequencies for each location and species',
                 '$log - log file',
                 '$lm_stats - trends in tfactor over time')
      
    for(i in 1:length(elements)){
      loc<-match(elements[i],element_names)
      cat('\n')
      cat(paste('Preview of ',long_names[loc],':\n\n',sep=''))
      
      if(elements[i]=='log'){
          cat(x[[i]][37:48],sep='\n')
        }else{
          print(head(x[[i]]))
        }
      
      cat('\n')
    }
  } else {
    element_names<-c("paths","trend","stat","freq","log")
    long_names<-c('$paths - file paths to frescalo log, stats, freq, trend and freq-quick load files',
                  '$trend - trends file, giving the tfactor value for each species at each time period',
                  '$stat - statistics for each hectad in the analysis',
                  '$freq - rescaled frequencies for each location and species',
                  '$log - log file')
    
    for(i in 1:length(elements)){
      loc<-match(elements[i],element_names)
      cat('\n')
      cat(paste('Preview of ',long_names[loc],':\n\n',sep=''))
      if(elements[i]=='log'){
        cat(head(x[[i]],8),sep='\n')
      }else{
        print(head(x[[i]]))
      }
      cat('\n')
    }
  }
  
}

