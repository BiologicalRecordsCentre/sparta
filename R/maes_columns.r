maes_columns<-function(data=NULL,names=NULL)
  {
  new_names<-c('Year','Site','Species','Start','End')
  for(i in 1:length(names)){
    if(!is.null(names[[i]])){
      names(data)<-gsub(names[[i]],new_names[i],names(data))  
    }
  }
  return(data)  
}