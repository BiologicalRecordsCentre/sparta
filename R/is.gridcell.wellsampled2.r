is.gridcell.wellsampled2 <- function(data, n=3){
  #takes the dataframe and returns a logical vector identifying well-sampled gridcells
  #this version copied from the Odonata trend analysis (late Feb 2013)
  #changed 4 March to filter on number of years, not number of visits
  require(reshape2)
  x <- acast(data, hectad~year, fun=length, value.var='L') # x is the number of visits
  num_yrs_visits  <- rowSums(x>0) # convert nVisits to binary and sum across years 
  sites_to_include <- num_yrs_visits >= n  
  return(data$hectad %in% dimnames(x)[[1]][sites_to_include])
}