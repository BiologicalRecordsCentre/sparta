#' @importFrom dplyr distinct
#' @importFrom reshape2 dcast

formatOccData <- function(taxa, site, time_period){

  # Create dataframe from vectors
  taxa_data <- distinct(data.frame(taxa, site, time_period))
  
  # time_period could be a numeric or a date. If it is a date extract the year
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    
    taxa_data$year <- as.numeric(format(taxa_data$time_period,'%Y')) # take year from date year 
  
  } else{
      
    stop('non-dates not yet supported for time_period')
    
  }
  
  # add list length column
  taxa_data$visit <- paste(taxa_data$site, taxa_data$time_period, sep="") # create a factor which combines date and site
  visit_Ls <- as.data.frame(table(taxa_data$visit))
  names(visit_Ls) <- c("visit", "L")
  taxa_data <- merge(taxa_data, visit_Ls)
  
  # create species_visit dataframe/matrix
  temp <- taxa_data[,c('taxa','visit')]
  names(temp)[1] <- "species_name"
  temp$pres <- TRUE # add TRUE column which will populate the spp with visit matrix/dataframe
  spp_vis <- dcast(temp, formula = visit ~ species_name, value.var = "pres", fill = FALSE) # This is the dataframe that contains a row per visit and a column for each species present or not.  USed to create the focal column in the next step
  
  # create occDetdata which is the main file sent to bugs (1 row per visist) - this will have "focal" added to it within the species loop
  occDetdata <- unique(taxa_data[,c("visit", "site", "L", "year")])
  
  return(list(spp_vis = spp_vis, occDetdata = occDetdata))
  
}