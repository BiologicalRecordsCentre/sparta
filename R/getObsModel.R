getObsModel <- function(modeltype, verbose = FALSE){
  
  basemodel <- "logit(p[j]) <-  alpha.p[Year[j]]"
  addVar <- NULL
  
  # if list length (categorical) is not a specification then we 
  
  for(mtype in modeltype){
    
    switch(EXPR = tolower(mtype),
           
           jul_date = {
             basemodel <- paste(
               basemodel,
               'beta1*JulDate[j] + beta2*pow(JulDate[j], 2)',
               sep = ' + ')
             addVar <- c(addVar,
                        "beta1 ~ dnorm(0, 0.0001)\nbeta2 ~ dnorm(0, 0.0001)\n")
           },
           
           catlistlength = {
             basemodel <- paste(
               basemodel,
               'dtype2.p*DATATYPE2[j] + dtype3.p*DATATYPE3[j]',
               sep = ' + ')
             addVar <- c(addVar,
                         "dtype2.p ~ dnorm(0, 0.01)\ndtype3.p ~ dnorm(0, 0.01)\n") 
           },
           
           intercept = {
             basemodel <- paste(
               basemodel,
               'eta.p0',
               sep = ' + ')},
           
           contlistlength = {
             basemodel <- paste(
               basemodel,
               'LL.p*logL[j]',
               sep = ' + ')
             addVar <- c(addVar,
                         "LL.p ~ dunif(dtype2p_min, dtype2p_max)\n")
             }
    )
  }
  
  fullModel <- paste0(paste(addVar, collapse = '\n'),
                     paste0('### Observation Model\n',
                            'for(j in 1:nvisit) {\n',
                            '  y[j] ~ dbern(Py[j])\n', 
                            '  Py[j]<- z[Site[j],Year[j]]*p[j]'),
                     '\n  ', basemodel, '\n',
                     '}')
  
  if(verbose) cat(paste('Observation model:\n', fullModel, '\n'))
  
  return(fullModel)
  
}