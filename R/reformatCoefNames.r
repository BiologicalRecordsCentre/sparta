reformatCoefNames <- function(names){

  names <- gsub(' ', '', names)
  names <- gsub('Pr\\(>\\|z\\|\\)', 'pvalue', names)
  names <- gsub('\\(Intercept\\)', 'intercept', names)
  names <- gsub('Std.Error', 'stderror', names)
  names <- tolower(names)

  return(names)
  
}