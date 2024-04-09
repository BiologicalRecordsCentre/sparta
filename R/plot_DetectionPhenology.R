#' Diagnostics for the detectability with respect to Julian Date
#'
#' Creates a plot of detectability over the season and calculates some simple statistics
#'
#' @param model a fitted sparta model of class \code{OccDet}.
#' @param spname optional name of the species (used for plotting)
#' @param bins number of points to estimate across the year. Defaults to 12
#' @param density_function whether the model used a density function to fit Julian date. This form was implemented from version 0.1.48 onwards. For models ran using earlier versions of the package this should be set to FALSE
#'
#' @details
#' Takes a object of \code{OccDet} fitted with the \code{jul_date} option
#'
#' Calculates the phenology of detection and produces a plot of detectability over time for the reference data type.
#'
#' @return This function returns plot showing the detection probability on the y axis and Julian day on the x.
#'         The data within the output list shows the Julian day for each point estimated (equal to the number of bins)
#'         and the mean detection probability with 95% credible intervals.
#'
#' @references van Strien, A.J., Termaat, T., Groenendijk, D., Mensing, V. & Kéry, M. (2010)
#'             Site-occupancy models may offer new opportunities for dragonfly monitoring based on daily species lists.
#'             \emph{Basic and Applied Ecology}, 11, 495–503.
#'
#' @importFrom reshape2 melt
#' @importFrom plyr ddply
#' @importFrom ggplot2 ggplot
#' @importFrom boot inv.logit
#' @export


###########################################
# calculate some stats
# test
# correct for starting on 1 July
###########################################


plot_DetectionPhenology <- function(model, spname = NULL, bins = 12, density_function = TRUE) {
  data <- model$BUGSoutput$sims.list

  if (!"beta1" %in% names(data)) {
    stop("no phenological effect was modelled!")
  }

  # the base: alpha.p is common to all models:
  # it's the logit probability of detection on a single species list
  # use the average value across years
  pDet1 <- rowMeans(data$alpha.p)
  # pDet1 is an array of dims equal to (niter, nyr)

  # Julian dates are 1:
  jul_dates <- seq(from = 1, to = 365, length.out = bins)

  if (density_function == TRUE) {
    if (!"beta3" %in% names(data)) {
      stop("beta3 not found. Please check that the density function method was used")
    }
    # we ran the Julian Date option
    # So let's calculate the detection probabilities

    pDet <- melt(sapply(jul_dates, function(jd) {
      pDet1 + data$beta3[, 1] * (1 / ((2 * pi)^0.5 * data$beta2[, 1]) * exp(-((jd - data$beta1[, 1])^2 / (2 * data$beta2[, 1]^2))))
    }))
  } else {
    cjd <- jul_dates - 182
    # we ran the Julian Date option
    # So let's scale the detection probabilities to end June (day 180)
    pDet <- melt(sapply(cjd, function(jd) {
      pDet1 + jd * data$beta1[, 1] + jd^2 * data$beta2[, 1]
    }))
  }

  names(pDet) <- c("it", "bin", "lgt_pDet")
  pDet$pDet <- inv.logit(pDet$lgt_pDet)

  # now summarize these posterior distributions
  pDet_summary <- ddply(
    pDet, .(bin), summarise,
    mean_pDet = mean(pDet),
    lower95CI = quantile(pDet, 0.025),
    upper95CI = quantile(pDet, 0.975)
  )

  # now convert the jds back to their equivalent Julian Dates
  if (density_function == FALSE) {
    pDet_summary$cJulDate <- cjd[pDet_summary$bin]
  }
  pDet_summary$JulianDay <- jul_dates[pDet_summary$bin]

  # now plot the detection over time
  gp <- ggplot(data = pDet_summary, x = JulianDay, y = mean_pDet) +
    geom_line(aes(x = JulianDay, y = mean_pDet)) +
    geom_ribbon(aes(x = JulianDay, ymin = lower95CI, ymax = upper95CI), alpha = 0.2) +
    ylab("Detection probability") +
    ggtitle(spname) +
    theme_bw()

  return(gp)
}
