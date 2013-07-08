colToDate<-function(Data,column){
  
  # if the column is in POSIX format then converting it using as.Date can have
  # some unwanted effects, this method is more stable
  if('POSIXct' %in% class(Data[column][,1]) | 'POSIXlt' %in% class(Data[column][,1])){
    temp_date <- as.Date(format(Data[column][,1],'%Y/%m/%d'))
    Data[column] <- temp_date
  } else {
    try_out<-try(as.Date(Data[column][,1],format='%d/%m/%Y'),silent=TRUE)
    if(grepl('Error',try_out[1])){
      stop(paste('Column',column,'is not in a supported date format'))
    } else {
      Data[column] <- try_out
    }
    if(sum(is.na(Data[column][,1]))>0){
      warning(paste('There are', sum(is.na(Data[column][,1])),'NA values in',column, 'column. This may be because they are in an unsupported date format. These rows will be ignored.', sep=' '))
    }
  }
  return(Data)
}
