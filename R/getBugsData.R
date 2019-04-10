#' Modify the bugs data object depending on the type of model you are running
#' 
#' This function is primarily for internal use within \code{occDetFunc}. It is used to 
#' update the bugs data according to the needs of each model type.
#' 
#' @param bugs_data The bugs data object. This is a list specified in \code{occDetFunc} as 
#' \code{list(y = as.numeric(focal), Year = TP, Site = rownum, nyear = nTP, nsite = nrow(zst),
#' nvisit = nrow(occDetdata[i,]))}. Where focal is a binary (0/1) of is the focal species is
#' present, Year is the time periods or survey periods, Site are the site identifiers, nyear is
#' the number of years in the data, nsite is hte number of sites and nVisit is the number of visits
#' @param modeltype Character, one of: intercept, centering, jul_date, catlistlength, contlistlength.
#' See \code{occDetFunc} for more information.
#' @param verbose Logical, if true progress is reported to the console
#' @param occDetData The 'raw' data used to create the \code{bugs_data}. This should have a 
#' column 'L' for list length and a column 'JulDate' for Julian date
#' @return An updated bugs_data object
#' @export

getBugsData <- function(bugs_data, modeltype, verbose = FALSE,
                        occDetData){
  
  switch(tolower(modeltype),
        
        # halfcauchy model has been simplified by removing scales 
        # halfcauchy = {
        #   cat('Adding bugs_data elements for halfcauchy\n')
        #   eta.scale <- lp.scale <- 1.6/qt(0.95, 1)
        #   bugs_data <- c(bugs_data, eta.scale = eta.scale, lp.scale = lp.scale)
        #   return(bugs_data)
        # },
         
         intercept = {
           if(verbose) cat('Adding bugs_data elements for intercept\n')
           psi0.a <- psi0.b <- 0.0001
           p0 <- beta.select(list(p=0.95, x=0.1), list(p=0.99, x=0.25))
           bugs_data <- c(bugs_data,
                          psi0.a = psi0.a, 
                          psi0.b = psi0.b, 
                          p0.a = p0[1], 
                          p0.b = p0[2])
           return(bugs_data)
         },
        
        centering = {
          if(verbose) cat('Adding bugs_data elements for centering\n')
          psi0.a <- psi0.b <- 0.0001
          p0 <- beta.select(list(p=0.95, x=0.1), list(p=0.99, x=0.25))
          bugs_data <- c(bugs_data,
                         psi0.a = psi0.a,
                         psi0.b = psi0.b,
                         p0.a = p0[1],
                         p0.b = p0[2])
          return(bugs_data)
        },
        
        jul_date = {
          if(verbose) cat('Adding bugs_data elements for Julian Date\n')

          JulDate <- occDetData$Jul_date #- 182 # centering has already been done in formatOccData!

          bugs_data <- c(bugs_data,
                         JulDate = list(as.numeric(JulDate)))
          return(bugs_data)
        },
        
        catlistlength = {
          if(verbose) cat('Adding bugs_data elements for Categorical List Length\n')
          DATATYPE2 <- occDetData$L %in% 2:3
          DATATYPE3 <- occDetData$L > 3
          bugs_data <- c(bugs_data,
                         DATATYPE2 = list(as.numeric(DATATYPE2)),
                         DATATYPE3 = list(as.numeric(DATATYPE3)))
          return(bugs_data)
        },
        
        contlistlength = {
          if(verbose) cat('Adding bugs_data elements for Continious List Length\n')
          logL <- log(occDetData$L)
          dtype2p_min <- -10 
          dtype2p_max <- 10
          bugs_data <- c(bugs_data,
                         logL = list(logL),
                         dtype2p_min = dtype2p_min,
                         dtype2p_max = dtype2p_max)
          return(bugs_data)
        },
        
        {
          if(verbose) cat(modeltype, 'uses the basic bugs_data object, nothing extra added\n')
          return(bugs_data)
        })
  
}