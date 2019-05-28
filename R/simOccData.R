#' Simulate for Occupancy detection models
#' 
#' Simulates some data suitable for use in sparta
#' The user defines the parameters for the data generation
#' At present it works with just one species and generates the list length probabalistically
#'
#'@param nsites numeric, the number of sites from which the simulated data is sampled.
#'@param nvisits numeric, the number of visits from which the simulated data is sampled.
#'@param nTP numeric, the number of time periods from which the simulated data is sampled.
#'@param psi probability of a site being occupied in time period 1.
#'@param trend the proportion of sites that change state each time period.
#'       For decreasing trends the probability of persistence from one time period to the next is 1 + the trend value.
#'       Probability of colonisation is 0.
#'       For increasing trends the probability of an unoccupied site being colonised from one time period to the next is the trend value.
#'       Probability of persistence is 1.
#'@param mu.lp the mean value for the normal distribution for the year effect (alpha.p) on the observation model.
#'@param tau.lp the precision value for the normal distribution for the year effect (alpha.p) on the observation model. 
#'@param beta1 the mean value for the normal distribution for the effect of Julian day on observation model.This must be a valid Julian date
#'@param beta2 the standard deviation for the normal distribution for the effect of Julian day on observation model.
#'@param beta3 parameter on the logit scale governing the magnitude of the Julian date effect on the observation model.
#'@param dtype2.p parameter (logit scale) for list length 2-3 on the observation model.
#'@param dtype3.p parameter (logit scale) for list length 4 on the observation model.
#'@param JD_range range of Julian dates upon which visits can take place. If this is null Julian date ranges between 1 and 365.
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
                dtype3.p = 10,
                JD_range = NULL
              ){

  #-------------------------- State variable
  Z <- matrix(NA, nrow=nsites, ncol=nTP)
  
  # set the initial distribution
  Z[,1] <- rbinom(n=nsites, size=1, prob=psi)
  
  for(t in 2:nTP){
    if(trend > 0) {
      transition <- rbinom(n=nsites, size=1, prob= trend) # prob defined here as colonization
      Zt <- apply(cbind(Z[,(t-1)], transition), 1, max)
    } else if(trend < 0) {
      transition <- rbinom(n=nsites, size=1, prob= 1+trend) # prob defined here as persistence
      Zt <- apply(cbind(Z[,(t-1)], transition), 1, min)
    }
    Z[,t] <- Zt
  }
  
  mZ <- melt(Z)
  names(mZ) <- c('site', 'TP', "Z") 
  
  #-------------------------- observations
  
  alpha.p <- rnorm(n = nTP, mean = mu.lp, sd = sqrt(1/tau.lp))
  
  # visits are randomly allocated to sites and TPs
  # there is a nonzero probability that some TPs will have no visits, which could be a problem
  if(is.null(JD_range)){occDetdata <- data.frame(visit = 1:nvisits,
                        site = sample.int(n=nsites, size=nvisits, replace = TRUE),
                        L = sample(c(1,2,4), size=nvisits, replace=TRUE),
                        TP = sample.int(n=nTP, size=nvisits, replace=TRUE),
                        Jul_date = sample.int(n=365, size=nvisits, replace=TRUE)
                        )
  }else{
    if(any( !(JD_range %in% c(1:366)))){
      stop('Invalid Julian date range')}
    potential_JD <- seq(as.integer(JD_range[1]),as.integer(JD_range[2]),by=1L)
    occDetdata <- data.frame(visit = 1:nvisits,
                  site = sample.int(n=nsites, size=nvisits, replace=TRUE),
                  L = sample(c(1,2,4), size=nvisits, replace=TRUE),
                  TP = sample.int(n=nTP, size=nvisits, replace=TRUE),
                  Jul_date = sample(x=potential_JD, size=nvisits, replace=TRUE)
    ) 
  }
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

