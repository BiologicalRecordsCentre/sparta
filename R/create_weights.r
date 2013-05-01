#' create frescalo weights file
#' 
#' This file creates the weights file required to run frescalo, as outlined in (Hill,
#'  2011). For more information on frescalo see \code{\link{frescalo}}. This function
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
#'        the final number of site that will be included in a neighbourhood. In Hill (2011),
#'        this is set to 100 and is the default here.
#' @param normalise Logical. If \code{TRUE} each attribute is divided by its maximum value to 
#'        produce vlues between 0 and 1. Default is \code{FALSE}
#' @return A dataframe is returned in a format that can be used directly in frescalo() or 
#'         sparta(). The dataframe has three columns giving the target cell, the neighbourhood
#'         cell, and the weight.
#' @keywords trends, frescalo, weights
#' @references Hill, Mark. Local frequency as a key to interpreting species occurrence data when
#' recording effort is not known. 2011. \emph{Methods in Ecology and Evolution}, 3 (1), 195-205.
#' @examples
#' \dontrun{
#' library(sparta)
#'
#' # Load example distance and similarity data
#' data(sim)
#' data(dist)
#'
#' # Create the weights file
#' weights<-create_weights(dist=dist,
#'                        sim=sim,
#'                        dist_sub=20,
#'                        sim_sub=10)
#'
#' # Set up sinkdir to run frescalo with our new weights file  
#' sinkdir <- getwd()
#'
#' # Load example data for frescalo
#' data(ex_dat)
#'
#' # Run frescalo
#'   fres_out <- frescalo(data = ex_dat,
#'                     taxon_name = 'Unicorns',
#'                     time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
#'                     sinkdir = sinkdir,
#'                     Fres_weights=weights)
#'  }
 
create_weights<-function(dist=NULL,
                         sim=NULL,
                         dist_sub=200,
                         sim_sub=100,
                         normalise=FALSE){
  
  #check that both tables exist
  if(is.null(dist) & is.null(sim)) stop("both 'dist' and 'sim' are missing, these must be supplied")
  if(is.null(dist)) stop("'dist' is missing, this must be supplied")
  if(is.null(sim)) stop("'sim' is missing, this must be supplied")
  
  #check that dist_sub is larger than sim_sub 
  if(dist_sub<=sim_sub) stop("'dist_sub' cannot be smaller than or equal to 'sim_sub'")
  
  # Ensure site names are characters not factors
  dist[,1]<-as.character(dist[,1])
  dist[,2]<-as.character(dist[,2])
  sim[,1]<-as.character(sim[,1])
  
  #check that sites are in both and give a warning if not
  unique_dist_sites<-unique(c(dist[,1],dist[,2]))
  unique_sim_sites<-unique(sim[,1])
  distmiss<-unique_dist_sites[!unique_dist_sites %in% unique_sim_sites]
  simmiss<-unique_sim_sites[!unique_sim_sites %in% unique_dist_sites]
  missing<-unique(c(distmiss,simmiss))
  if(length(missing)>0){
    warning(paste("The following sites were in only one of 'sim' and 'dist' and so have been excluded from the weights file:",toString(missing)))
    dist<-dist[!dist[,1] %in% missing,]
    dist<-dist[!dist[,2] %in% missing,]
    sim<-sim[!sim[,1] %in% missing,]
  }
  
  #normalise if required
  if(normalise){
    for(i in 2:length(colnames(sim))){
      mx<-max(sim[,i])
      sim[,i]<-sim[,i]/mx
    }
  }
  
  #convert attribute table into a long distance table
  cat('Creating similarity distance table...')
  sim_distance <- dist(sim[,2:length(names(sim))],diag=TRUE,upper=TRUE) 
  sim_distance <- melt(as.matrix(sim_distance))
  sim_distance$value <- (sim_distance$value/max(sim_distance$value))
  sim_distance[,1]<-as.character(sim_distance[,1])
  sim_distance[,2]<-as.character(sim_distance[,2])
  cat('Complete\n')  
  
  # Set up progress tracking
  count=0
  breaks<-round(seq(0,length(unique(dist[,1])),length.out=11))[-1]
  cat('Creating weights file...\n0%\n')
  
  # Taking each cell in turn calculate the weights
  for (i in unique(dist[,1])){
    
    # select for target cell
    sim_foc<-subset(sim_distance,sim_distance[,1]==i)
    dist_foc<-subset(dist,dist[,1]==i)
    
    # For this focal cell rank all others by distance
    dist_foc$rankdist<-rank(dist_foc[,3],ties.method="first")
    
    # Take the top 'dist_sub' closest (dist_sub defaults to 200)
    dist_foc <- subset(dist_foc,rankdist<=dist_sub)
    
    # Of these take the 'sim_sub' top by similarity distance (sim_sub defaults to 100)
    ranks <- merge(x=dist_foc,y=sim_foc,by=c("Var1","Var2"),all.x=TRUE,all.y=FALSE)
    ranks$rankflor<-rank(ranks$value.y,ties.method="first")
    ranks <- subset(ranks,rankflor<=sim_sub)
    
    # Calculate similarity by distance and flora
    ranks$distsim<-(1-(((ranks$rankdist-1)^2)/(dist_sub)^2))^4
    ranks$florsim<-(1-(((ranks$rankflor-1)^2)/(sim_sub)^2))^4
    
    # Calculate weights
    ranks$weight<-ranks$distsim*ranks$florsim
    
    # Merge back with all data
    if (!exists('weights_master')){
      weights_master<-data.frame(target=ranks$Var1,
                                 neighbour=ranks$Var2,
                                 weight=round(ranks$weight,4))
    } else {
      weights_master<-rbind(data.frame(target=ranks$Var1,
                                       neighbour=ranks$Var2,
                                       weight=round(ranks$weight,4))
                            ,weights_master)
      }
    
    # report on progress
    count=count+1
    if(count %in% breaks){
      cat(paste(match(count,breaks)*10,'%\n',sep=''))
    }
  }
  cat('Complete')
  return(weights_master) 
}