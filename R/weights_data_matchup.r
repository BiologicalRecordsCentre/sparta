weights_data_matchup <- function(weights_sites, data_sites){
  
  extra_sites_data <- data_sites[!data_sites %in% weights_sites]
  extra_sites_weights <- weights_sites[!weights_sites %in% data_sites]
  
  if(length(extra_sites_data) == length(data_sites)){
    stop('the sites in your data do not match those in your weights file')
  } 
  if(length(extra_sites_data) > 0) warning(length(extra_sites_data), ' sites appear your data but are not in your weights file, these will be ignored')
  #if(length(extra_sites_weights) > 0) warning(length(extra_sites_weights), ' sites appear your weights file but are not in your data file')
  
}