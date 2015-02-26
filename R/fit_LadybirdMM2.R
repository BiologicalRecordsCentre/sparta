fit_LadybirdMM2 <-
function(indata, od=F, V=F, pvis=NA, family = "Binomial"){ 
  
  #07/05/2013: Updated with NI code 
  
  require(lme4)
  if(class(indata$year)!='numeric') indata$year <- as.numeric(indata$year)
  if(class(indata$CONCEPT)!='numeric') indata$CONCEPT <- as.numeric(indata$CONCEPT)
  
  #Ensure data is unique
  data <- unique(indata)
  
#   #subset the data: remove the short lists (defined by nsp)
#   data <- subset(indata, L >= nsp)
#   
#   # of these visits, which are on well-sampled sites?
#   data <- subset(data, is.gridcell.wellsampled2(data, n = nyr))
    
  #if there is no well sampled data, capture what info you can and dont bother modelling
  if(nrow(data)==0){ 
    
      coefs <- c(rep(NA,9),pvis,length(data$L),sum(as.numeric(data$CONCEPT)), 'Error : no data in well sampled sites')
    
    }else{
      
    if(family == "Binomial"){
      
      MMdata <- dcast(data, year + site ~ ., fun=length, value.var='L') #how many lists for each year?
      names(MMdata)[ncol(MMdata)] <- 'nVis'  
      
      MMdata$nVR <- as.numeric(acast(data, year + site ~ ., fun=sum, value.var='CONCEPT'))
      
      # to fit a binomial model, we define a new column containing the number of visits without an observaiton of the focal
      MMdata$failures <- with(MMdata, nVis - nVR)
      
      # Centre the Year on the median value (for numerical stability)
      yearZero <- median(unique(as.numeric(MMdata$year)))
      MMdata$cYr <- MMdata$year - yearZero
  
      if(od){
        MMdata$obs <- 1:nrow(MMdata)
        MM <- try(glmer(cbind(nVR, failures) ~ cYr + (1|site) + (1|obs), data=MMdata, family=binomial, verbose=V),silent=TRUE)
      } else {
        MM <- try(glmer(cbind(nVR, failures) ~ cYr + (1|site), data=MMdata, family=binomial, verbose=V),silent=TRUE)      
      }
    
    } else if(family == "Bernoulli"){
      
      # Centre the Year on the median value (for numerical stability)
      yearZero <- median(unique(as.numeric(data$year)))
      
      MM <- try(glmer(as.numeric(CONCEPT) ~ I(as.numeric(year)-yearZero) + (1|site), data, family=binomial), silent = TRUE)
            
    } else {
      
      stop('Family not known')
      
    }
    if(class(MM)[1]=="try-error"){
      coefs <- c(rep(NA,9),pvis,length(data$L),sum(as.numeric(data$CONCEPT)))
      coefs <- c(coefs, MM[1])
    }
    if(sum(as.numeric(data$CONCEPT))==0){ #ie if there are no observations
      coefs <- c(rep(NA,9),pvis,length(data$L),sum(as.numeric(data$CONCEPT)),'Error : no data in well sampled sites')
    }
#     if(as.numeric(MM@dims['cvg'])==65){ #catches a specific convergence error (not caught above)
#       coefs <- c(rep(NA,9),as.numeric(MM@dims['cvg']),length(data$L)/length(indata$L),length(data$L),sum(as.numeric(data$CONCEPT)))
#     }
    if(class(MM)[1]!="try-error" & !exists('coefs')){
      coefs <- as.numeric(summary(MM)$coefficients[2,])
      coefs <- c(coefs,as.numeric(summary(MM)$coefficients[1,1:2]),yearZero)
      coefs <- c(coefs,min(MM@frame[,2]),max(MM@frame[,2]))
      #coefs <- c(coefs,as.numeric(MM@dims['cvg']))
      coefs <- c(coefs,pvis,length(data$L),sum(as.numeric(data$CONCEPT)))
      coefs <- c(coefs, NA)
      }
  }  
   # keep this a two step process in case we later decide to extract other info
  return(coefs)
}
