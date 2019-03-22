# Modify the init object depending on the type of model we are running

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
         
         jul_date = {
           if(verbose) cat('Adding init values for Julian Date\n')
           init$beta1 = runif(1, 0, 366)
           init$beta2 = runif(1, 0, 90)
           init$beta3 = rnorm(1, 0, 0.0001)
           if(verbose) cat(init)
           return(init)
         },
         
         {
           if(verbose) cat(modeltype, 'uses the basic init object, nothing extra added\n')
           return(init)
         })
  
}