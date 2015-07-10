#' @method summary frescalo
#' @export

summary.frescalo<-function(object, ...){
  
  start <- grep('Actual numbers in data',object$log)
  end <- grep('Filter locations included',object$log)
  cat(paste(object$log[start:end],'\n'))
  
}
