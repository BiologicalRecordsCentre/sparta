#' Modify the init object depending on the type of model we are running
#' 
#' This function is primarily for internal use within \code{occDetFunc}. It is used to 
#' update the initial values object according to the needs of each model type.
#' 
#' @param init An initial values object. As a minimum this is a list defined in \code{occDetFunc}
#' as \code{list(z = z, alpha.p = rep(runif(1, -2, 2), nTP), a = rep(runif(1, -2, 2), nTP),
#' eta = rep(runif(1, -2, 2), bugs_data$nsite))}. Where z is 1's/0's for whether the focal
#' species is present, alpha.p is the initial values for detectability in each year, a is 
#' the inital values for the occupancy probability in each year, eta is the initial values
#' for the site random effects.
#' @param modeltype one of: intercept, centering, contlistlength.
#' @param verbose Logical, if true progress is reported to the console
#' @return An updated \code{init} (initial values) object
#' @export

getInitValues <- function(init, modeltype, verbose = FALSE){
  
  switch(modeltype,

         intercept = {
           if(verbose) cat('Adding init values for intercept\n')
           init$psi0 <- runif(1, 0, 1)
           init$p0 <- runif(1, 0, 0.2)
           if(verbose) cat(init)
           return(init)
         },
         
         centering = {
           if(verbose) cat('Adding init values for centering\n')
           init$psi0 <- runif(1, 0, 1)
           init$p0 <- runif(1, 0, 0.2)
           if(verbose) cat(init)
           return(init)
         },
         
         contlistlength = {
           if(verbose) cat('Adding init values for Continious List Length\n')
           init$LL.p = runif(1, -2, 2)
           if(verbose) cat(init)
           return(init)
         },
         
         {
           if(verbose) cat(modeltype, 'uses the basic init object, nothing extra added\n')
           return(init)
         })
  
}