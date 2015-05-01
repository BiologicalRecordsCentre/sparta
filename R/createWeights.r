#' Create frescalo weights file
#' 
#' Create the weights file required to run frescalo, as outlined in (Hill,
#' 2011). For more information on frescalo see \code{\link{frescalo}}. This function
#' takes a table of geographical distances between sites and a table of numeric data
#' from which to calculate similarity (for example, landcover or abiotic data)
#'
#' @param dist a dataframe giving the distance between sites in a long format with three
#'        columns. The first column give the ID of the first site, the second column gives
#'        the ID of the second site and the third column gives the distance between them.
#'        The table should include reciprocal data i.e. both rows 'A, B, 10' and 'B, A, 10'
#'        should exist. 
#' @param sim a dataframe of numeric attributes of each site. The first column must contain
#'        the site IDs and all other columns are used to calculate the similarity between
#'        sites using dist() and method 'euclidean'.
#' @param dist_sub the number of neighbours to include after ranking by distance. In Hill
#'        (2011), this is set to 200 and is the default here
#' @param sim_sub the number of neighbours to include after ranking by similarity. This is
#'        the final number of sites that will be included in a neighbourhood. In Hill (2011),
#'        this is set to 100 and is the default here.
#' @param normalise Logical. If \code{TRUE} each attribute is divided by its maximum value to 
#'        produce values between 0 and 1. Default is \code{FALSE}
#' @return A dataframe is returned in a format that can be used directly in frescalo() or 
#'         sparta(). The dataframe has three columns giving the target cell, the neighbourhood
#'         cell, and the weight.
#' @keywords trends, frescalo, weights
#' @references Hill, Mark. Local frequency as a key to interpreting species occurrence data when
#' recording effort is not known. 2011. \emph{Methods in Ecology and Evolution}, 3 (1), 195-205.
#' @import sp
#' @importFrom reshape2 melt
#' @export
#' @examples
#' \dontrun{
#' library(sparta)
#'
#' # Load example distance and similarity data
#' data(sim)
#' data(dist)
#'
#' # Create the weights file
#' weights <- createWeights(dist=dist,
#'                           sim=sim,
#'                           dist_sub=20,
#'                           sim_sub=10)
#' }
 
createWeights<-function(dist,
                        sim,
                        dist_sub=200,
                        sim_sub=100,
                        normalise=FALSE){
  
  # Error checks
  errorChecks(dist = dist, sim = sim, dist_sub = dist_sub, sim_sub = sim_sub)

  # Ensure site names are characters not factors
  dist[,1] <- as.character(dist[,1])
  dist[,2] <- as.character(dist[,2])
  sim[,1] <- as.character(sim[,1])
  
  # Check that sites are in both drop those that are not - give a warning
  unique_dist_sites <- unique(c(dist[,1], dist[,2]))
  unique_sim_sites <- unique(sim[,1])
  distmiss <- unique_dist_sites[!unique_dist_sites %in% unique_sim_sites]
  simmiss <- unique_sim_sites[!unique_sim_sites %in% unique_dist_sites]
  missing <- unique(c(distmiss,simmiss))
  
  if(length(missing) > 0){
    warning(paste("The following sites were in only one of 'sim' and 'dist' and so have been excluded from the weights file:",toString(missing)))
    dist <- dist[!dist[,1] %in% missing,]
    dist <- dist[!dist[,2] %in% missing,]
    sim <- sim[!sim[,1] %in% missing,]
  }
  
  #normalise if required
  if(normalise){
    for(i in 2:length(colnames(sim))){
      mx <- max(sim[,i])
      sim[,i] <- sim[,i] / mx
    }
  }
  
  #convert attribute table into a long distance table
  cat('Creating similarity distance table...')
  row.names(sim) <- sim[,1]
  sim_distance <- dist(sim[,2:length(names(sim))], diag = TRUE, upper = TRUE) 
  sim_distance <- melt(as.matrix(sim_distance))
  sim_distance$value <- (sim_distance$value/max(sim_distance$value))
  sim_distance[,1] <- as.character(sim_distance[,1])
  sim_distance[,2] <- as.character(sim_distance[,2])
  cat('Complete\n')
  
  # Set up progress tracking
  if(length(unique(dist[,1]) >=10)){
    breaks_length <- 11
  } else {
    breaks_length <- length(unique(dist[,1]))
  }
  breaks <- round(seq(0,length(unique(dist[,1])), length.out = breaks_length))[-1]
  breakpoints <- unique(dist[,1])[breaks]
  progressDF <- data.frame(progress = paste(seq(from = 10, to = 100, length.out = length(breakpoints))), ID = breakpoints)
  cat('Creating weights file...\n0%\n')
  
  # Taking each cell in turn calculate the weights
  weights_list <- lapply(unique(dist[,1]), function(i){
    
    # select for target cell
    sim_foc <- subset(sim_distance, sim_distance[,1] == i)
    dist_foc <- subset(dist, dist[,1] == i)
    
    # For this focal cell rank all others by distance
    dist_foc$rankdist <- rank(dist_foc[,3], ties.method = "first")
    
    # Take the top 'dist_sub' closest (dist_sub defaults to 200)
    dist_foc <- subset(dist_foc, rankdist <= dist_sub)
    
    # Of these take the 'sim_sub' top by similarity distance (sim_sub defaults to 100)
    ranks <- merge(x = dist_foc, y = sim_foc, by = c("Var1", "Var2"), all.x = TRUE, all.y = FALSE)
    ranks$rankflor <- rank(ranks$value.y, ties.method = "first")
    ranks <- subset(ranks, rankflor <= sim_sub)
    
    # Calculate similarity by distance and flora
    ranks$distsim <- (1 - (((ranks$rankdist - 1)^2) / (dist_sub)^2))^4
    ranks$florsim <- (1 - (((ranks$rankflor - 1)^2) / (sim_sub)^2))^4
    
    # Calculate weights
    ranks$weight <- ranks$distsim*ranks$florsim
    
    # report on progress
    if(i %in% progressDF$ID){
      cat(paste(progressDF$progress[progressDF$ID == i],'%\n',sep=''))
    }
    
    # Merge back with all data
    return(data.frame(target=ranks$Var1,
                      neighbour=ranks$Var2,
                      weight=round(ranks$weight,4)))
    
    })

  weights_master <- do.call(rbind, weights_list)
  
  cat('Complete\n')
  
  return(weights_master) 
  
}