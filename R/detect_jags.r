# Internal function to detect JAGS installation.
detect_jags <- function(){
    return(suppressWarnings(runjags::testjags(silent = TRUE)$JAGS.found))
}