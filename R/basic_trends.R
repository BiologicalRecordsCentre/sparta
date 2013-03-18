basic_trends <-
function(records,time_periods,min_sq=5,splityr=NULL,basictrendsdir,sp_list=NULL,taxon){
  
  require(reshape2)
  
  # Create a vector of all the years to include
  for (i in 1:length(row.names(time_periods))) {
    run<-time_periods[i,1]:time_periods[i,2]
    if (exists('all_years')){
      all_years <- c(all_years,run)
    }else{
      all_years <- run
    }
  }
  
  # Subset data to years needed
  records<-records[records$year %in% all_years,]
  
  # If split year is not set then it is eastimated
  if (is.null(splityr)) splityr <- mean(c(max(time_periods[1,]),min(time_periods[2,])))

  # Calculate the three measures of change
  gridcell_counts <- Convert_records_to_2tp(records, splityr)  #get the number of sites in each time periods
  prop.diff <- with(subset(gridcell_counts, n1>=min_sq), (n2-n1)/n1)
  plr <- rstandard(lm(log(n2) ~ log(n1), gridcell_counts, subset=gridcell_counts$n1 >= min_sq))
  Telfer <- fit_Telfer(gridcell_counts, min_sq)

  # Create an output dataframe
  output <- cbind(plr, Telfer,na.omit(as.data.frame(prop.diff)))
    
  attr(output, 'nSites') <- gridcell_counts[1,1:2]

  if(!is.null(sp_list)) output<-output[row.names(output) %in% sp_list,]
  
  # save all data (from linux machine)
  datecode<-format(Sys.Date(), "%y%m%d")
  setwd(basictrendsdir)
  return(output)

}
