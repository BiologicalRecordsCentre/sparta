#simple occupancy model with data in long format (one line per visit) 
#this version has List Length as a covariate and Site effect
occDetBUGScode <- function(){  
  # Priors 
  # state model priors
  for(t in 1:nyear){
    a[t] ~ dunif(-10,10)   
  }                 
  
  # RANDOM EFFECT for SITE
  for (i in 1:nsite) {
    eta[i] ~ dnorm(0, tau2)       # extra random site-effect op occ. (previously it was mu2: changed Jan 2014)
  } 
  
  tau2 <- 1/(sigma2 * sigma2) # NOT NEEDED IN SIMPLE MODEL
  sigma2 ~ dunif(0, 5)
  
  
  # observation model priors 
  for (t in 1:nyear) {
    alpha.p[t] ~ dnorm(mu.lp, tau.lp)            # p random year
  }
  
  mu.lp ~ dnorm(0, 0.01)                         
  tau.lp <- 1 / (sd.lp * sd.lp)                 
  sd.lp ~ dunif(0, 5)   
  
  LL.p ~ dunif(dtype2p_min, dtype2p_max)
  
  # State model
  for (i in 1:nsite){ 
    for (t in 1:nyear){   
      z[i,t] ~ dbern(muZ[i,t]) # True occupancy z at site i
      logit(muZ[i,t])<- a[t] + eta[i] # plus random site effect
    }}   
  
  # Observation model 
  # go through the visits and find the matching year and site identity
  for(j in 1:nvisit) {
    #for each visit, find the matching site and year identities
    Py[j]<- z[Site[j],Year[j]]*p[j]      
    logit(p[j]) <- alpha.p[Year[j]] + LL.p*logL[j]
    y[j] ~ dbern(Py[j])  
    Presi[j] <- abs(y[j]-p[j])
    y.new[j] ~ dbern(Py[j]) 
    Presi.new[j] <- abs(y.new[j]-p[j])
  }
  
  # Bayesian Goodness-of-Fit
  fit <- sum(Presi[])
  fit.new <- sum(Presi.new[])
  
  # Derived parameters state model
  
  # Finite sample occupancy
  for (t in 1:nyear) {  
    psi.fs[t] <- sum(z[1:nsite,t])/nsite
  } 
  
  # Overall trend in occpuancy
  sumY <- sum(psi.fs[1:nyear])
  for (t in 1:nyear) {
    sumxy[t] <- psi.fs[t]*t
  }
  sumXY <- sum(sumxy[1:nyear])
  regres.psi <- (sumXY - ((sumX*sumY)/nyear))/(sumX2 - ((sumX*sumX)/nyear))
  
  # Derived parameters observation model
  for (t in 1:nyear) {          
    pdet.alpha[t] <- exp(alpha.p[t])/(1 + exp(alpha.p[t])) 
  }
  
  # overall trend in pdet.alpha
  sumYpdet <- sum(pdet.alpha[1:nyear])  
  for (t in 1:nyear) {          
    sumxypdet[t] <- pdet.alpha[t]*t
  }
  sumXYpdet <- sum(sumxypdet[1:nyear])
  regres.pdet <- (sumXYpdet - ((sumX*sumYpdet)/nyear))/(sumX2 - ((sumX*sumX)/nyear))
  
  # end of model formulation
}