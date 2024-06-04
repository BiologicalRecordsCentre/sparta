# Internal function to detect JAGS installation.
#' @importFrom runjags testjags
#' 
detect_jags <- function(){
    return(suppressWarnings(runjags::testjags(silent = TRUE)$JAGS.found))
}