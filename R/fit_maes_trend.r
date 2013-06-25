fit_maes_trend<-function(records, timeperiods=NULL, splityr=NULL, min_sp=5){
  
  
  # SOMETHING IS GOING WRONG AT AROUND LINE 35
  # make this work with time periods if needed
  # fits the method described in Maes et al 2012 (Biol Cons 145: 258-266)
  # I checked this by comparing the results against those in Maes et al SI
  # I've added a sampling theory to estimate p-values. We'll see whether it's robust!
  # the function returns numbers of sites in 2 time periods (out of the well-sampled ones), the %trend and p-value
  require(reshape2)
  
  # which time period is each record in?
  if('Start' %in% names(records) & 'End' %in% names(records)){
    records$tp[as.numeric(format(records$Start,'%Y'))<splityr&as.numeric(format(records$End,'%Y'))<splityr]<-1
    records$tp[as.numeric(format(records$Start,'%Y'))>=splityr & as.numeric(format(records$End,'%Y'))>=splityr]<-2
    
  }else if('Year' %in% names(records)){
    records$tp[records$Year<splityr]<-1
    records$tp[records$Year>=splityr]<-2
  }
  records<-records[!is.na(records$tp),]

  print(head(records))
  
  stop()  
  # convert the records into a 3D array
  rc <- acast(records, Species ~ Site ~ tp, fun=occurrence, value.var=2)
  #print(head(rc))
  
  # what is the observed species richness in each cell in each year
  rc1 <- apply(rc, c(2,3), sum)
  #print(head(rc1))
  
  # the number of sites in each time period
  nSites <- colSums(rc1>0)
  
  # which sites have are well-sampled? (defined as having at least min_sp species in BOTH time periods)
  well_sampled <- as.numeric(dimnames(rc1)[[1]][apply(rc1, 1, function(x) all(x>=min_sp))])
  #print(head(well_sampled))
  
  # look at just the data for these well-sampled cells
  rc2 <- rc[,dimnames(rc)[[2]] %in% well_sampled,]
  
  # how many sites for each species in each time period?
  rc3 <- apply(rc2, c(1,3), sum)
  
  # calculate the relative distribution in each time period
  rd1 <- rc3[,1]/nSites[1]
  rd2 <- rc3[,2]/nSites[2]
  
  trend <- 100 * (rd2-rd1)/rd1
  
  # we can assess the significance of this method as follows
  # first we assume the distribution of each species is equal among poorly-sampled and well-sampled sites
  # Under the null hypothesis that total proportion of sites has not changed,
  # we can calculate the binomial probability of the 'estimated number of successes
  # estimated number of successes is defined here as nsr2 (number of sites recorded in t2)
  # where the number of trials = nSites[2]
  
  nsr2 <- nSites[2] * rc3[,2] / length(well_sampled)
  true_probs <- rc3[,1]/length(well_sampled)
  
  pval <- mapply(FUN=pbinom, q=nsr2, prob=true_probs, MoreArgs=list(size=nSites[2]))
  
  #these are one-tailed: convert them to one-tailed
  pval <- one_to_two_tail(pval)
  
  Maes <- data.frame(N1=rc3[,1], N2=rc3[,2], trend=trend, pval=pval) 
  attr(Maes, 'nSites') <- nSites
  attr(Maes, 'wellsampled') <- length(well_sampled)
  return(Maes)
}

one_to_two_tail <- function(p) ifelse(p<0.5, p*2, (1-p)*2)