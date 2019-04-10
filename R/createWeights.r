#' Create frescalo weights file
#' 
#' Create the weights file required to run frescalo, as outlined in (Hill,
#' 2011). For more information on frescalo see \code{\link{frescalo}}. This function
#' takes a table of geographical distances between sites and a table of numeric data
#' from which to calculate similarity (for example, landcover or abiotic data)
#'
#' @param distances a dataframe giving the distance between sites in a long format with three
#'        columns. The first column give the ID of the first site, the second column gives
#'        the ID of the second site and the third column gives the distance between them.
#'        The table should include reciprocal data i.e. both rows 'A, B, 10' and 'B, A, 10'
#'        should exist. 
#' @param attributes a dataframe of numeric attributes of each site. The first column must contain
#'        the site IDs and all other columns are used to calculate the similarity between
#'        sites using dist() and method 'euclidean'.
#' @param dist_sub the number of neighbours to include after ranking by distance. In Hill
#'        (2011), this is set to 200 and is the default here
#' @param sim_sub the number of neighbours to include after ranking by similarity. This is
#'        the final number of sites that will be included in a neighbourhood. In Hill (2011),
#'        this is set to 100 and is the default here.
#' @param normalise Logical. If \code{TRUE} each attribute is divided by its maximum value to 
#'        produce values between 0 and 1. Default is \code{FALSE}
#' @param verbose Logical, should progress be printed to console. Defaults to TRUE
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
#'
#  # I'm going to create some made up data
#' mySites <- paste('Site_', 1:100, sep = '')
#'
#' # Build a table of distances
#' myDistances <- merge(mySites, mySites) 
#' 
#' # add random distances
#' myDistances$dist <- runif(n = nrow(myDistances), min = 10, max = 10000) 
#'
#' # to be realistic the distance from a site to itself should be 0
#' myDistances$dist[myDistances$x == myDistances$y] <- 0
#'
#' # Build a table of attributes
#' myHabitatData <- data.frame(site = mySites,
#'                             grassland = runif(length(mySites), 0, 1),
#'                             woodland = runif(length(mySites), 0, 1),
#'                             heathland = runif(length(mySites), 0, 1),
#'                             urban = runif(length(mySites), 0, 1),
#'                             freshwater = runif(length(mySites), 0, 1))
#'
#' # This pretend data is supposed to be proportional cover so lets 
#' # make sure each row sums to 1
#' multiples <- apply(myHabitatData[,2:6], 1, sum)
# 
#' for(i in 1:length(mySites)){
#'   myHabitatData[i,2:6] <- myHabitatData[i,2:6]/multiples[i]
#' }
#'
#' # Create the weights file
#' weights <- createWeights(distances = myDistances,
#'                           attributes = myHabitatData,
#'                           dist_sub = 20,
#'                           sim_sub = 10)
#' }
 
createWeights<-function(distances,
                        attributes,
                        dist_sub=200,
                        sim_sub=100,
                        normalise=FALSE,
                        verbose=TRUE){
  
  # Error checks
  errorChecks(dist = distances, sim = attributes, dist_sub = dist_sub, sim_sub = sim_sub)

  # Ensure site names are characters not factors
  distances[,1] <- as.character(distances[,1])
  distances[,2] <- as.character(distances[,2])
  attributes[,1] <- as.character(attributes[,1])
  
  # rename the columns
  colnames(distances) <- c('site1', 'site2' ,'distance')
    
  # Check that sites are in both drop those that are not - give a warning
  unique_dist_sites <- unique(c(distances[,1], distances[,2]))
  unique_sim_sites <- unique(attributes[,1])
  distmiss <- unique_dist_sites[!unique_dist_sites %in% unique_sim_sites]
  simmiss <- unique_sim_sites[!unique_sim_sites %in% unique_dist_sites]
  missing <- unique(c(distmiss,simmiss))
  
  if(length(missing) > 0){
    warning(paste("The following sites were in only one of 'attributes' and 'distances' and so have been excluded from the weights file:",toString(missing)))
    distances <- distances[!distances[,1] %in% missing,]
    distances <- distances[!distances[,2] %in% missing,]
    attributes <- attributes[!attributes[,1] %in% missing,]
  }
  
  #normalise if required
  if(normalise){
    for(i in 2:length(colnames(attributes))){
      mx <- max(attributes[,i])
      attributes[,i] <- attributes[,i] / mx
    }
  }
  
  #convert attribute table into a long distance table
  if(verbose) cat('Creating similarity distance table...')
  row.names(attributes) <- attributes[,1]
  sim_distance <- dist(attributes[,2:length(names(attributes))], diag = TRUE, upper = TRUE) 
  sim_distance <- melt(as.matrix(sim_distance))
  sim_distance$value <- (sim_distance$value/max(sim_distance$value))
  sim_distance[,1] <- as.character(sim_distance[,1])
  sim_distance[,2] <- as.character(sim_distance[,2])
  colnames(sim_distance) <- c('site1', 'site2', 'similarity')
  if(verbose) cat('Complete\n')
  
  if(verbose) cat('Creating weights file...\n0%\n')
  
  total <- length(unique(distances[,1]))
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  # Taking each cell in turn calculate the weights
  weights_list <- lapply(unique(distances[,1]), function(i){
    
    # select for target cell
    sim_foc <- sim_distance[sim_distance$site1 == i, ]
    dist_foc <- distances[distances$site1 == i, ]
    
    # For this focal cell rank all others by distance
    dist_foc$rankdist <- rank(dist_foc$distance, ties.method = "first")
    
    # Take the top 'dist_sub' closest (dist_sub defaults to 200)
    dist_foc <- dist_foc[dist_foc$rankdist <= dist_sub, ]
    
    # Of these take the 'sim_sub' top by similarity distance (sim_sub defaults to 100)
    ranks <- merge(x = dist_foc, y = sim_foc, by = c('site1', 'site2'), all.x = TRUE, all.y = FALSE)
    ranks$rankflor <- rank(ranks$similarity, ties.method = "first")
    ranks <- ranks[ranks$rankflor <= sim_sub, ]
    
    # Calculate similarity by distance and flora
    ranks$distsim <- (1 - (((ranks$rankdist - 1)^2) / (dist_sub)^2))^4
    ranks$florsim <- (1 - (((ranks$rankflor - 1)^2) / (sim_sub)^2))^4
    
    # Calculate weights
    ranks$weight <- ranks$distsim*ranks$florsim
    
    if(verbose) setTxtProgressBar(pb, grep(paste0('^', i, '$'), unique(distances[,1])))
    
    # Merge back with all data
    return(data.frame(target = ranks$site1,
                      neighbour = ranks$site2,
                      weight = round(ranks$weight, 4)))
    
    })

  weights_master <- do.call(rbind, weights_list)
  close(pb)
  
  if(verbose) cat('Complete\n')
  
  return(weights_master) 
  
}