fit_Telfer <-
function(gridcell_counts, min_sq=5) {
  #takes output from Convert_to_2tp and fits the telfer model
  #convert to proportions  
  p1 <- gridcell_counts$n1/attr(gridcell_counts, 'denom')[1]
  p2 <- gridcell_counts$n2/attr(gridcell_counts, 'denom')[2]
    
  #Telfer's method is the *standardized* residual from a logit-logit regression
  model1 <- lm( log(p2/(1-p2)) ~ log(p1/(1-p1)), subset=gridcell_counts$n1 >=min_sq & p2 >0)
  return(rstandard(model1))
}
