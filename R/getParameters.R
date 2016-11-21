# Modify the init object depending on the type of model we are running

getParameters <- function(parameters, modeltype){
  
  switch(tolower(modeltype),
         
         indran = {
           cat('Adding parameters to monitor for indran\n')
           parameters <- c(parameters, "tau.a")
           return(parameters)
         },
         
         jul_date = {
           cat('Adding parameters to monitor for Julian Date\n')
           parameters <- c(parameters, "beta1", "beta2")
           return(parameters)
         },
         
         catlistlength = {
           cat('Adding parameters to monitor for Catagorical List Length\n')
           parameters <- c(parameters, "dtype2.p", "dtype3.p",
                           "pdet.d2", "pdet.d3")
           return(parameters)
         },
         
         contlistlength = {
           cat('Adding parameters to monitor for Continious List Length\n')
           parameters <- c(parameters, "LL.p")
           return(parameters)
         },

         {
           cat(modeltype, 'monitors the basic parameters, nothing extra added\n')
           return(parameters)
         })
  
}