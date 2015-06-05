#' @method summary frescalo
#' @export

summary.frescalo<-function(x){
  
  start <- grep('Actual numbers in data',x$log)
  end <- grep('Filter locations included',x$log)
  cat(paste(x$log[start:end],'\n'))
  
}
