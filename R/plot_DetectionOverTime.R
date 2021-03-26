#' Diagnostics for the detection model with respect to Length 
#' 
#' Creates a plot of detectability by year for differing list lengths from an occupancy model output.

#' @param model a fitted sparta model of class \code{OccDet}.
#' @param spname optional name of the species (used for plotting)
#' @param bins number of points to estimate across the year. Defaults to 12
#' @param density_function whether the model used a density function to fit Julian date. This form was implemented from version 0.1.48 onwards. For models ran using earlier versions of the package this should be set to FALSE
#' 
#' @details 
#' Takes a object of \code{OccDet}
#' 
#' Calculates the detection probability and produces a plot of detectability over time for the reference data type.
#'
#' @return This function returns plot showing the detection probability on the y axis and year on the x.
#'
#' @importFrom reshape2 melt 
#' @importFrom plyr ddply
#' @importFrom ggplot2 ggplot
#' @importFrom boot inv.logit
#' @export


plot_DetectionOverTime <- function(model, spname = NULL, min.yr = NULL, CI=95){

  
  # convert the CI into quantiles
  # first check that CI is sensible
  if((CI > 100) | (CI <= 0)) stop("Credible intervals must be between 0 and 100")
  CI2q <- function(CI) {
    q <- (1 - CI/100)/2
    return(c(q, 1-q))
  }
  q <- CI2q(CI)
  
  sims_list <- model$BUGSoutput$sims.list
  
  # the base: alpha.p is common to all models: 
  # it's the logit probability of detection on a single species list
  pDet1 <- sims_list$alpha.p
  # pDet1 is an array of dims equal to (niter, nyr)
  
  if("beta1" %in% names(sims_list)){
    # we ran the Julian Date option
    # So let's scale the detection probabilities to end June (day 180)
    pDet1 <- apply(pDet1, 2, function(x) 
      x + 180 * sims_list$beta1[,1] +  180^2 * sims_list$beta2[,1]
      )
  }
  
  # now calculate the equivalent values for lists of length 2 and 4 
  if("LL.p" %in% names(sims_list)){
    # the model was fitted with continuous list length
    pDet2 <- pDet1 + sims_list$LL.p * log(2)
    pDet4 <- pDet1 + sims_list$LL.p * log(4)
  } else if("dtype2.p" %in% names(sims_list)){
    # the model was fitted with categorical list length
    pDet2 <- pDet1 + sims_list$dtype2.p[,1]
    pDet4 <- pDet1 + sims_list$dtype3.p[,1]
  } else {
    # there is also an option to ignore list length, 
    # in which case the probability of detection is assumed to be constant across surveys
    # i.e. if the survey was systematic
    # in this case the there are no estimates for the other pDet values.
    pDet2 <- pDet4 <- NA
  }

  pDet <- melt(list(pDet1, pDet2, pDet4))
  names(pDet) <- c("it", "year", "lgt_pDet", "ListLength")
  pDet$ListLength[pDet$ListLength==3] <- 4 # the "third" category is for a list of length 4
  
  pDet$pDet <- inv.logit(pDet$lgt_pDet)
  
  # now summarize these posterior distributions
  pDet_summary <-ddply(
        pDet, .(year, ListLength), summarise, 
        mean_pDet = mean(pDet),
        lower95CI = quantile(pDet, 0.025),
        upper95CI = quantile(pDet, 0.975))
  
  # if the user has supplied a year then switch the x axis to start at that minimum
  if(!is.null(min.yr)) pDet_summary$year <- pDet_summary$year + min.yr - 1
  
  # now plot the detection over time
  gp <- ggplot(data=pDet_summary, x=year, y=mean_pDet) +
    geom_line(aes(x=year, y=mean_pDet, col=factor(ListLength))) +
    geom_ribbon(aes(x=year, ymin=lower95CI, ymax=upper95CI, fill=factor(ListLength)), alpha=0.2) +
    ylab("Detection probability") +
    ggtitle(spname) +
    theme_bw()
  gp 

}


