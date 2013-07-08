#' Convergence codes for linear models
#' 
#' This function is an aid to interpreting cvg (convergence) codes.
#' 
#' @param x A number indicating a cvg code
#' @return The description of the code (x) as found in the source code of lme4.
#'         If x is not given all code descriptions are returned as a table. 
#' 
#' @examples
#' \dontrun{
#' library(sparta)
#' 
#' y<-cvg_code(4)
#' 
#' y
#' }

cvg_codes <-
  function(x=NULL){
    
    cvg_data<-data.frame(code=c(3,4,5,6,7,8,9,10,14,15,16,63,65),
                          description=c("X-convergence",
                                        "relative convergence",
                                        "both X-convergence and relative convergence",
                                        "absolute function convergence",
                                        "singular convergence",
                                        "false convergence",
                                        "function evaluation limit reached without convergence",
                                        "iteration limit reached without convergence",
                                        "storage has been allocated",
                                        "LIV too small",
                                        "LV too small",
                                        "fn cannot be computed at initial par",
                                        "gr cannot be computed at initial par"))
    if(is.null(x)) {
      return(cvg_data)
    } else {
      if(!x %in% cvg_data$code){
        stop(paste('Code',x,'not found in lookup table'))
      } else {
        return(as.character(cvg_data$description[cvg_data$code==x]))
      }
    }
  }