formulaBuilder <- function(family, list_length, site_effect, overdispersion){

  # If we include list_length then the family must be Bernoulli
  if(list_length){
    
    family <- 'Bernoulli'
    
  } 
  
  # Build our formula depending on our arguements
  model_formula <- ifelse(family == 'Bernoulli',
                          'taxa ~ year',
                          'cbind(successes, failures) ~ year')
  if(list_length) model_formula <- paste(model_formula, '+ listLength')
  if(site_effect) model_formula <- paste(model_formula, '+ (1|site)')
  if(overdispersion) model_formula <- paste(model_formula, '+ (1|obs)')

  return(model_formula)
  
}