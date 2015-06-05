create_index_filename <- function(sinkdir, datecode, fresoutput){
  
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
  return(fresoutput)
  
}