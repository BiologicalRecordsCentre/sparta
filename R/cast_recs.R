cast_recs <-
function(records, resolution='visit'){
  #takes a set of records and returns a dataframe suitable for analysis
  #columns include the list length and presence/absence of the focal species
  #6 December: minor alterations to make it work with nonfocal species
  
  require(reshape2)
  
  if(resolution=='visit'){
    castrecs <- dcast(records, Date + kmsq ~ ., value.var='CONCEPT', fun=LenUniq)
  } else if(resolution=='kmyr'){
    castrecs <- dcast(records, year + kmsq ~ ., value.var='CONCEPT', fun=LenUniq)
  }
  
  names(castrecs)[ncol(castrecs)] <- 'L'
 
  return(castrecs) #list of number of unique concepts found at each 'visit'
}
