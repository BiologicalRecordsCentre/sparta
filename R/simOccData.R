#' Simulate for Occupancy detection models
#' 
#' Simulates some data suitable for use in sparta
#' The user defines the parameters for the data generation
#' At present it works with just one species and generates the list length probabalistically
#'
#' @return A list, the first two elements of which ('spp_vis' & 'occDetData') mimic the output of occDetFunc.
#' The third element ('Z') is the presence-absence state variable and the fourth ('p') is the true probability of detection.
#' 
#' @examples
#' \dontrun{
#' 
#' # set the sparta options
#'sparta_options <- c('ranwalk', # prior on occupancy is set by last year's posterior
#'                    'jul_date', # use the Julian date as a covariate on the detection probability
#'                    'catlistlength', # categorises the visits into three sets of 'qualities'
#'                    'halfcauchy') # prior on the precisions
#'
#' # simulate some data
#'mydata <- simOccData(nvisit=200, nsite=10, nTP=5, psi=0.5, beta1=182, beta2=20, beta3=100)
#'with(mydata, plot(occDetdata$Jul_date, p))
#'
#'# run the occupancy model model
#'out <- occDetFunc('mysp', mydata$occDetdata, mydata$spp_vis, n_iter = 1e4, 
#'                  modeltype = sparta_options, return_data=TRUE)
#'
#'out$BUGSoutput
#'detection_phenology(out)
#'
#'qplot(data=melt(out$BUGSoutput$sims.array), geom='line',
#'      x=Var1, col=factor(Var2), y=value) +
#'  facet_wrap(~Var3, ncol=4, scales='free')
#'
#' }
#' @importFrom reshape2 melt
#' @importFrom boot inv.logit
#' @export

simOccData <- function(
                nsites = 20,
                nvisits = 100,
                nTP = 10,
                psi = 0.5,
                trend = -0.01, # this proportion of sites changes state each TP           
                mu.lp = -1,
                tau.lp = 10,
                beta1 = 182,
                beta2 = 20,
                beta3 = 100,
                dtype2.p = 3,
                dtype3.p = 10
              ){

  #-------------------------- State variable
  Z <- matrix(NA, nrow=nsites, ncol=nTP)
  
  # set the initial distribution
  Z[,1] <- rbinom(n=nsites, size=1, prob=psi)
  
  for(t in 2:nTP){
    transition <- rbinom(n=nsites, size=1, prob=abs(trend))
    if(trend > 0) Zt <- apply(cbind(Z[,(t-1)], transition), 1, max)
    else if(trend < 0) Zt <- apply(cbind(Z[,(t-1)], transition), 1, min)
    Z[,t] <- Zt
  }
  
  mZ <- melt(Z)
  names(mZ) <- c('site', 'TP', "Z") 
  
  #-------------------------- observations
  
  alpha.p <- rnorm(n = nTP, mean = mu.lp, sd = sqrt(1/tau.lp))
  
  # visits are randomly allocated to sites and TPs
  # there is a nonzero probability that some TPs will have no visits, which could be a problem
  occDetdata <- data.frame(visit = 1:nvisits,
                        site = sample.int(n=nsites, size=nvisits, repl=TRUE),
                        L = sample(c(1,2,4), size=nvisits, repl=TRUE),
                        TP = sample.int(n=nTP, size=nvisits, repl=TRUE),
                        Jul_date = sample.int(n=365, size=nvisits, repl=TRUE)
                        )
  # probability of detection
  p <- inv.logit( alpha.p[occDetdata$TP] + 
                  beta3 * (1/((2*pi)^0.5 * beta2) * exp(-((occDetdata$Jul_date - beta1)^2 / (2* beta2^2)))) +
                  dtype2.p * (occDetdata$L %in% 2:3) + 
                  dtype3.p * (occDetdata$L == 4)
  )
  
  # true occupancy
  mZ <- merge(occDetdata, mZ, all.x=TRUE)$Z
  # Detection history
  Y <- rbinom(n=nvisits, size=1, p= p*mZ)
  spp_vis <- data.frame(visit = 1: nvisits, mysp = Y)

  return(list(spp_vis = spp_vis, 
              occDetdata = occDetdata,
              Z=Z, p=p))
  
}

