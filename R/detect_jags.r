# Internal function to detect JAGS installation. Note rjags is needed to detect the JAGS installation
#' @importFrom runjags testjags
#' @import rjags
#' 
detect_jags <- function(){
        return(suppressWarnings(runjags::testjags(silent = TRUE)$JAGS.found))
}