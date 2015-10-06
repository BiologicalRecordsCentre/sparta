#' @method summary occDet
#' @export

summary.occDet <- function(object, ...){
    
  # gets summary output from the BUGS files 
  spp_data <- as.data.frame(object$BUGSoutput$summary)
  
  # get rows we are interested in
  ### take psi.fs rows - these are the yearly proportion of occupied cells ###
  spp_data$X <- row.names(spp_data)
  new_data <- spp_data[grepl("psi.fs",spp_data$X),]
  new_data$year <- (Year = (object$min_year - 1) + as.numeric(gsub("psi.fs", "", gsub("\\[|\\]","", row.names(new_data)))))
  
  # rename columns, otherwise ggplot doesn't work properly    
  names(new_data) <- gsub("2.5%","quant_025", names(new_data))
  names(new_data) <- gsub("97.5%","quant_975", names(new_data))
  
  return_data <- new_data[ ,c('mean', 'quant_025', 'quant_975')]
  
  row.names(return_data) <- new_data$year
  
  return(return_data)
}