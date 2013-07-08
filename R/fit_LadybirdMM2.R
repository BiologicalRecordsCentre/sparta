fit_LadybirdMM2 <-
function(indata, nsp=2, nyr=3, od=F, V=F){ 
  
  #07/05/2013: Updated with NI code 
  
  require(lme4)
  if(class(indata$year)!='numeric') indata$year<-as.numeric(indata$year)
  if(class(indata$CONCEPT)!='numeric') indata$CONCEPT<-as.numeric(indata$CONCEPT)
  
  #subset the data: remove the short lists (defined by nsp)
  data <- subset(indata, L>=nsp)
  
  # of these visits, which are on well-sampled sites?
  data <- subset(data, is.gridcell.wellsampled2(data, n=nyr))
    
  #if there is no well sampled data, capture what info you can and dont bother modelling
  if(nrow(data)==0){ 
      coefs <- c(rep(NA,8),length(data$L)/length(indata$L),length(data$L),sum(as.numeric(data$CONCEPT)))
    }else{
      
    MMdata <- dcast(data, year + hectad ~ ., fun=length, value.var='L') #how many lists for each year?
    names(MMdata)[ncol(MMdata)] <- 'nVis'  
    
    MMdata$nVR <- as.numeric(acast(data, year + hectad ~ ., fun=sum, value.var='CONCEPT'))
    
    # to fit a binomial model, we define a new column containing the number of visits without an observaiton of the focal
    MMdata$failures <- with(MMdata, nVis - nVR)
    
    # centre the Year on the median value (for numerical stability)
    MMdata$cYr <- MMdata$year - median(unique(as.numeric(MMdata$year)))

    if(od){
      MMdata$obs <- 1:nrow(MMdata)
      MM <- tryCatch(glmer(cbind(nVR, failures) ~ cYr + (1|hectad) + (1|obs), data=MMdata, family=binomial, verbose=V))
    } else {
      MM <- tryCatch(glmer(cbind(nVR, failures) ~ cYr + (1|hectad), data=MMdata, family=binomial, verbose=V))      
    }
    
    if(class(MM)[1]=="try-error"){
      coefs <- rep(NA,13)
    }
    if(sum(as.numeric(data$CONCEPT))==0){ #ie if there are no observations
      coefs <- c(rep(NA,10),length(data$L)/length(indata$L),length(data$L),sum(as.numeric(data$CONCEPT)))
    }
    if(as.numeric(MM@dims['cvg'])==65){ #catches a specific convergence error (not caught above)
      coefs <- c(rep(NA,9),as.numeric(MM@dims['cvg']),length(data$L)/length(indata$L),length(data$L),sum(as.numeric(data$CONCEPT)))
    }
    if(class(MM)[1]!="try-error" & !exists('coefs')){
      coefs <- as.numeric(summary(MM)@coefs[2,])
      coefs <- c(coefs,as.numeric(summary(MM)@coefs[1,1:2]),median(unique(as.numeric(MMdata$year))))
      coefs <- c(coefs,min(MM@frame[,2]),max(MM@frame[,2]))
      coefs <- c(coefs,as.numeric(MM@dims['cvg']))
      coefs <- c(coefs,length(data$L)/length(indata$L),length(data$L),sum(as.numeric(data$CONCEPT)))
      }
  }  
   # keep this a two step process in case we later decide to extract other info
  return(coefs)
}
