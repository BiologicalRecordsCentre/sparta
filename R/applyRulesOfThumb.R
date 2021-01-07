applyRuleOfThumb <- function(data_Metrics, criterion){
  if(criterion == "EqualWt")
    if(data_Metrics$prop_abs >= 0.990)
      pass <- data_Metrics$P90 >= 3.1
    else 
      pass <- data_Metrics$P90 >= 6.7
  else if(criterion == "HighSpec")  
    if(data_Metrics$prop_abs >= 0.958)
      pass <- data_Metrics$P90 >= 9.5
    else 
      pass <- data_Metrics$P90 >= 29
  else stop ("criterion not recognised")
  return(pass)
}