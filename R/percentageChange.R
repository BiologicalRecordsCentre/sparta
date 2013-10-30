percentageChange <-
  function(intercept=NULL, Ymin=NULL, Ymax=NULL, slope=NULL, NYears=10, option='arithmetic', probability = TRUE){
    
    if(is.null(intercept)) stop('No intercept value given')
    if(is.null(slope)) stop('No slope value given')
    if(is.null(Ymin)) stop('No Ymin value given')
    if(is.null(Ymax)) stop('No Ymax value given')    
   
    if(NA %in% c(intercept, Ymin, Ymax, slope)){
      
      change <- NA
      
    } else {
      
      # get the years range
      Yspan <- 1 + Ymax - Ymin
     
      # calculate the change in p(occupied) in the first and last years
      if(probability){ #when doing probabilities ilt must be used
        init <- ilt(intercept+slope*Ymin)
        final <- ilt(intercept+slope*Ymax)
      } else { # some methods such as Frescalo arent producing probabilities
        init <- intercept+slope*Ymin
        final <- intercept+slope*Ymax
      }
      # calculate the total change as a proportion of the initial
      prop.change <- (final - init) / init
      #attr(prop.change, 'span') <- Yspan
      
      if(option=='arithmetic'){
        # if arithmetic, we convert to a percentage and parcel this amount into equal segments
        change<-100* prop.change * NYears/Yspan
      } else if(option=='geometric'){
        # otherwise it's geometric, i.e. compound interest
        change<-100 *(((prop.change+1) ^ (NYears/Yspan)) - 1)      
      }
    }
    
    return(change)
    
  }
