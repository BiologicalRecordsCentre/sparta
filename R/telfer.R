#' Telfer's change index
#' 
#' Telfers change index is designed to assess the relative change in range size of species 
#' between two time periods (Telfer et al 2002). This function can take multiple time periods
#' and will complete all pairwise comparisons.
#' 
#' @param taxa A character vector of taxon names
#' @param site A character vector of site names
#' @param time_period A numeric vector of user defined time periods, or a date vector
#' @param minL numeric, The minimum number of taxa recorded at a site at a given time period 
#' (list-length) for the visit to be considered well sampled.
#' @param minSite The minimum number of sites occupied in the first time period in
#'        order for a trend to be calculated for a taxon.
#' @param useIterations A logical variable indicating whether iterations should be used.
#'        Iterations are used to account for increased variation in the logit proportions
#'        close to zero and one (see Telfer et al 2002). Default is \code{TRUE}
#' @param iterations If \code{useIterations} is \code{TRUE}, then this parameter indicates
#'        the number of iterations to be used. In Telfer et al 2002 the number of iterations
#'        used were 7 and 8 for the two datasets for which it was applied. The defualt here
#'        is 10.
#' @import reshape2
#' @examples
#' 
#' # Create fake data
#' SS <- 1000 # number of observations
#' taxa <- sample(letters, SS, replace = TRUE)
#' site <- sample(paste('A', 1:20, sep = ''), SS, replace = TRUE)
#' time_period <- sample(1:3, SS, replace = TRUE)
#' 
#' TelferResult <- telfer(taxa, site, time_period)
#' 
#' @references Telfer, M.G., Preston, C.D., & Rothery, P. (2002) A general method for
#'             measuring relative change in range size from biological atlas data.
#'             Biological Conservation, 107, 99-109.

telfer <- function(taxa, site, time_period, minSite = 5, useIterations = TRUE, iterations = 10){
  
  # Perform error checks
  errorChecks(taxa = taxa, site = site, time_period = time_period, minSite = minSite,
              useIterations = useIterations, iterations = iterations)
 
  taxa_data <- data.frame(taxa, site, time_period)
  
  # Create a list of all pairwise comparisons of time periods
  TP_combos <- t(combn(x = sort(unique(time_period)), m = 2))
  
  # For each pair of time periods go through and compare them
  TelferList <- apply(X = TP_combos, MARGIN = 1, FUN = function(TPs){
          
    # Do the core Telfer analysis
    basic_temp <- telfer_func(taxa_data[taxa_data$time_period %in% TPs,], iterations = iterations,
                              useIterations = useIterations, minSite = minSite)[[1]]
    
    colnames(basic_temp)[2] <- paste('Telfer_', TPs[1], '_', TPs[2], sep = '')
    
  
    #Add in NAs
    basic_temp <- merge(basic_temp, data.frame(taxa = sort(unique(taxa))), all = TRUE)
        
    return(basic_temp)
    
  })

  Telfer_out <- Reduce(function(a,b) merge(a, b, all = TRUE, by = "taxa"), TelferList)  
  
  return(Telfer_out)
  
}