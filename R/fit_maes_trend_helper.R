#'Generate a new set of relative distributions based on a given set of relative distributions
#'@author Thierry Onkelinx
#'@export
#'@param rd a matrix with relative distributions. The first column contains the relative distribution at the first timepoint. The column column those of the second timepoint. Other columns are ignored.
#'@param n a vector with the number of sampled gridcells in each period. Only the first two are used.
#'@param rd.names an optinal vector with the species names
#'@return a matrix with four columns and as much rows as in rd. The columns contain: the resampled relative distribution at timepoint 1, the resampled relative distribution at timepoint 2, the index from Maes et al assuming the null hypothesis and the observed index from Maes et al based on the new sample.
sampleRd <- function(rd, n, rd.names = factor(seq_len(nrow(rd)))){
  t1 <- sample(rd.names, size = n[1], prob = rd[, 1], replace = TRUE)
  t2.0 <- sample(rd.names, size = n[2], prob = rd[, 1], replace = TRUE)
  t2.a <- sample(rd.names, size = n[2], prob = rd[, 2], replace = TRUE)
  t1 <- table(t1) / n[1]
  t2.0 <- table(t2.0) / n[2]
  t2.a <- table(t2.a) / n[2]
  cbind(t1, t2.0, t2.a, (t2.0 - t1) / t1, (t2.a - t1) / t1)
}

#'Calculate two sided p-values for the index from Maes et al based on their bootstrapped distribution under the null hypothesis
#' @author Thierry Onkelinx
#' @export
#' @param rd a matrix with relative distributions. The first column contains the relative distribution at the first timepoint. The column column those of the second timepoint. Other columns are ignored.
#'@param n a vector with the number of sampled gridcells in each period. Only the first two are used. Ignored when distribution is given.
#' @param distribution a list with at least one item H0 contains a matrix with as many rows as rd containing resampled values of the index under the null hypothesis. Will be calculated if missing.
#' @return a vector with two sided p-values for each row of rd
twoSidedPValue <- function(rd, n, distribution){
  if(missing(distribution)){
    if(missing(n)){
      stop("either n or distribution must be defined")
    } else {
      distribution <- list(
        H0 = replicate(1e3, {
          x <- sampleRd(rd = rd, n = n)[, 4]
        })
      )
    }
  }
  sapply(seq_len(nrow(rd)), function(i){
    if(is.infinite(rd[i, 3]) | is.na(rd[i, 3])){
      NA
    } else if(rd[i, 3] > 0){
      mean(distribution$H0[i, ] >= rd[i, 3]) * 2
    } else {
      mean(distribution$H0[i, ] <= rd[i, 3]) * 2
    }
  })
}

#'Calculate confidence intervals for the index from Maes et al based on their bootstrapped distribution under the alternative hypothesis
#' @author Thierry Onkelinx
#' @export
#'@param rd a matrix with relative distributions. The first column contains the relative distribution at the first timepoint. The column column those of the second timepoint. Other columns are ignored.
#'@param n a vector with the number of sampled gridcells in each period. Only the first two are used. Ignored when distribution is given.
#'@param distribution a list with at least one item Ha contains a matrix with as many rows as rd containing resampled values of the index under the alternative hypothesis. Will be calculated if missing.
#'@param alpha the errorlevel
#'@return a matrix with observed index, lower confidence limits, upper confidence limit and median.
confintRd <- function(rd, n, distribution, alpha = 0.05){
  if(missing(distribution)){
    if(missing(n)){
      stop("either n or distribution must be given.")
    } else {
      distribution <- list(
        Ha = replicate(1e3, {
          x <- sampleRd(rd = rd, n = n)[, 5]
        })
      )
    }
  }
  x <- t(apply(
    distribution$Ha, 
    1,
    quantile,
    c(alpha / 2, 1 - alpha / 2, 0.5)
  ))
  x <- cbind(rd[, 3], x)
  colnames(x) <- c("observed", "lcl", "ucl", "median")
  x
}
