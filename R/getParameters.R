#' Modify the initial values object depending on the type of model we are running
#' 
#' This function is primarily for internal use within \code{occDetFunc}. It is used to 
#' return a vector of parameters to be monitored.
#' 
#' @param parameters A character vector of parameters you want to monitor.
#' @param modeltype Character, one of: indran, jul_date, catlistlength, contlistlength.
#' See \code{occDetFunc} for more information.
#' @param verbose Logical, if true progress is reported to the console
#' @return A character, of JAGS model code, that describes the observation model.
#' @export


# Modify the init object depending on the type of model we are running

getParameters <- function(parameters, modeltype, verbose = FALSE){
  
  switch(tolower(modeltype),
         
         indran = {
           if(verbose) cat('Adding parameters to monitor for indran\n')
           parameters <- c(parameters, "tau.a")
           return(parameters)
         },
         
         jul_date = {
           if(verbose) cat('Adding parameters to monitor for Julian Date\n')
           parameters <- c(parameters, "beta1", "beta2")
           return(parameters)
         },
         
         catlistlength = {
           if(verbose) cat('Adding parameters to monitor for Catagorical List Length\n')
           parameters <- c(parameters, "dtype2.p", "dtype3.p")
           return(parameters)
         },
         
         contlistlength = {
           if(verbose) cat('Adding parameters to monitor for Continious List Length\n')
           parameters <- c(parameters, "LL.p")
           return(parameters)
         },

         {
           if(verbose) cat(modeltype, 'monitors the basic parameters, nothing extra added\n')
           return(parameters)
         })
  
}