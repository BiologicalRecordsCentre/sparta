#' Diagnostics for the detectability with respect to Julian Date 
#' 
#' Creates a plot of detectability over the season and calculates some simple statistics
#'
#' @param model a fitted sparta model of class \code{OccDet}.
#' @param spname optional name of the species (used for plotting)
#' @param bins number of points to estimate across the year. Defaults to 12
#' 
#' @details 
#' Takes a object of \code{OccDet} fitted with the \code{jul_date} option
#' 
#' Calculate the phenology of detection and produces a plot of detectability over time for the reference data type.
#'
#' @return Some numbers.
#' 
#' @references van Strien, A.J., Termaat, T., Groenendijk, D., Mensing, V. & Kéry, M. (2010) 
#'             Site-occupancy models may offer new opportunities for dragonfly monitoring based on daily species lists. 
#'             \emph{Basic and Applied Ecology}, 11, 495–503.
#' @export


###########################################
# calculate some stats
# test
# correct for starting on 1 July
###########################################


detection_phenology <- function(model, spname=NULL, bins=12){

  require(sparta)
  require(reshape2)
  require(plyr)
  require(boot)
  require(ggplot2)
  
  data <- model$BUGSoutput$sims.list
  
  if(!"beta1" %in% names(data))
    stop("no phenological effect was modelled!")

  # the base: alpha.p is common to all models: 
  # it's the logit probability of detection on a single species list
  # use the average value across years
  pDet1 <- rowMeans(data$alpha.p)
  # pDet1 is an array of dims equal to (niter, nyr)
  
  # Julian dates are 1:
  jul_dates <- seq(from=1, to=365, length.out=bins)
  
  cjd <- jul_dates - 182
  # we ran the Julian Date option
  # So let's scale the detection probabilities to end June (day 180)
  pDet <- melt(sapply(cjd, function(jd){
    pDet1 + jd * data$beta1[,1] +  jd^2 * data$beta2[,1]
  }))
  
  names(pDet) <- c("it", "jd","lgt_pDet")
  pDet$pDet <- inv.logit(pDet$lgt_pDet)
  
  # now summarize these posterior distributions
  pDet_summary <-ddply(
    pDet, .(jd), summarise, 
    mean_pDet = mean(pDet),
    lower95CI = quantile(pDet, 0.025),
    upper95CI = quantile(pDet, 0.975))
  
  # now convert the jds back to their equivalent Julian Dates
  pDet_summary$JulianDay <- jul_dates[pDet_summary$jd] 

  # now plot the detection over time
  gp <- ggplot(data=pDet_summary, x=JulianDay, y=mean_pDet) +
    geom_line(aes(x=JulianDay, y=mean_pDet)) +
    geom_ribbon(aes(x=JulianDay, ymin=lower95CI, ymax=upper95CI), alpha=0.2) +
    ylab("Detection probability") +
    ggtitle(spname) +
    theme_bw()
  gp 

  # calculate some simple stats
  
}

