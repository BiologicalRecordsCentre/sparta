fit_maes_trend <- function(records, time_periods = NULL, min_sp = 5){
  
  # fits the method described in Maes et al 2012 (Biol Cons 145: 258-266)
  # I checked this by comparing the results against those in Maes et al SI
  # I've added a sampling theory to estimate p-values.
  # the function returns numbers of sites in 2 time periods (out of the well-sampled ones), the %trend and p-value
  require(reshape2)
  
  if(is.null(time_periods)) stop('No time periods supplied to fit_maes_trend')

  # Create a vector of all the years to include
  all_years <- unlist(
    apply(
      time_periods, 
      1, 
      function(x){
        list(x[1]:x[2])
      }
    )
  )
  
  #Ensure data is unique
  records <- unique(records)
  
  # Subset data to years needed
  records <- records[records$tpnew %in% all_years, ]
  splityr <- mean(c(time_periods$end[1], time_periods$start[2]))
    
  # which time period is each record in?
  records$tp <- cut(records$tpnew,
    sort(
      c(0, splityr, Inf)
    )
  )
    
  records <- records[!is.na(records$tp), ]
  
  # convert the records into a 3D array
  rc <- acast(records, Species ~ Site ~ tp, fun = occurrence, value.var = 2)

  # what is the observed species richness in each cell in each year
  rc1 <- apply(rc, c(2,3), sum)

  # which sites are well-sampled? (defined as having at least min_sp species in BOTH time periods)
  well_sampled <- which(apply(rc1 >= min_sp, 1, all))
  
  # look at just the data for these well-sampled cells
  rc2 <- rc[, well_sampled, ]
  
  # how many sites for each species in each time period?
  rc3 <- apply(rc2, c(1,3), sum)

  # calculate the relative distribution in each time period
  # the denominator is the total number of unqiue site-species combos within the well-sampled set (for each period)
  rd <- apply(rc3, 2, function(x) x/sum(x))

  # the trend is the % difference in the two relative distributions
  trend <- 100 * (rd[, 2] - rd[, 1]) / rd[, 1]
  
  # we can assess the significance of this method as follows
  # first we assume the distribution of each species is equal among poorly-sampled and well-sampled sites
  # Under the null hypothesis that total proportion of sites has not changed,
  # we can calculate the binomial probability of the 'estimated number of successes
  # estimated number of successes is defined here as nsr2 (number of sites recorded in t2)
  # where the number of trials = nS[2]
  
  n <- colSums(rc3)
  #confidence intervals under the alternative hypothesis
  CI <- t(apply(rd, 1, function(x){
    quantile(
      na.omit(#ignore trends where the species is absent in both time periods
        apply(
          #simulate under the alternative hypothesis
          matrix(
            rbinom(2e3, size = n, prob = x) / n,
            nrow = 2
          ),
          2,
          #calculate simulated trend under the alternative hypothesis
          function(y){
            (y[2] - y[1]) / y[1]
          }
        )
      ),
      prob = c(0.025, 0.975)
    )
  }))

  #p-values under the null hypothesis
  p.values <- t(apply(rd, 1, function(x){
    trend <- (x[2] - x[1]) / x[1]
    trend.null <- na.omit(apply(
      #simulate under the null hypothesis x[1] == x[2]
      matrix(
        rbinom(2e3, size = n, prob = x[1]) / n,
        nrow = 2
      ),
      2,
      #calculate simulated trend under the null hypothesis
      function(y){
        (y[2] - y[1]) / y[1]
      }
    ))
    c(
      trend = unname(trend),
      p.negative.trend.one.sided = mean(trend.null <= trend),
      p.positive.trend.one.sided = mean(trend.null >= trend),
      p.absolute.trend.two.sided = mean(abs(trend.null) >= abs(trend))
    )
  }))

  Maes <- data.frame(
    gridcells1 = rc3[, 1], 
    relDist1 = rd[, 1], 
    gridcells2 = rc3[, 2], 
    relDist2 = rd[, 1], 
    change = p.values[, "trend"], 
    pval = p.values[, "p.absolute.trend.two.sided"]
  )
  attr(Maes, 'GridCellSums') <- colSums(rc3)
  attr(Maes, 'wellsampled') <- length(well_sampled)
  return(Maes)
}
