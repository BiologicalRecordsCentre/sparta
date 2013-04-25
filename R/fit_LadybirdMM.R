fit_LadybirdMM <-
function(MMdata, nsp=2, nyr=3,wellsamp='visit'){ #0.44 seconds
  # this version returns only the coefficients and the proportion of observations that were 'well-sampled'
  # 14 December: minor change: I removed the call to MMdata to outside the function.
  #   this Gives extra flexibility to analyse disaggregated data.
  # 4th Feb: year is subtracted from 1990 to avoid false convergences
  # 6th MArch 13: call to is.gridcell.wellsampled changed so that instead of getting
  # cells that have been well sampled three times, as it was, there is now the choice to
  # have it as well sampled in three different years.
  require(lme4)
  i <- MMdata$L >= nsp
  if(wellsamp=='year'){
    tempi <- unique(MMdata[i,][c('kmsq','year')])[['kmsq']]
    tempi <- names(table(tempi)[table(tempi)>=3])
    i[i==T]<-MMdata$kmsq[i] %in% tempi
  } 
  if(wellsamp=='visit') i[i==T] <- is.gridcell.wellsampled(MMdata$kmsq[i], n=nyr)
  #if there is no well sampled data, capture what info you can and dont bother modelling
  if(dim(MMdata[i,])[1]==0){ 
      coefs <- c(NA,NA,NA,NA,NA,NA,NA,NA,sum(i)/length(i),sum(i),sum(as.numeric(MMdata$CONCEPT[i])))
    }else{  
    MM <- tryCatch(glmer(as.numeric(CONCEPT) ~ I(as.numeric(year)-1990) + (1|kmsq), MMdata, subset=i, family=binomial))
    #This catches model errors, there was one in the BRYOPHYTE data
    if(class(MM)[1]=="try-error"){
      coefs <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    }
    if(as.numeric(MM@dims['cvg'])==65){ #catches a specific convergence error (not caught above)
      coefs <- c(NA,NA,NA,NA,NA,NA,NA,as.numeric(MM@dims['cvg']),sum(i)/length(i),sum(i),sum(as.numeric(MMdata$CONCEPT[i])))
    }
    if(class(MM)[1]!="try-error" & !exists('coefs')){
      coefs <- as.numeric(summary(MM)@coefs[2,])
      coefs <- c(coefs,as.numeric(summary(MM)@coefs[1,1:2]),as.integer(1990))
      coefs <- c(coefs,as.numeric(MM@dims['cvg']))
      coefs <- c(coefs,sum(i)/length(i),sum(i),sum(as.numeric(MMdata$CONCEPT[i])))
      }
  }  
   # keep this a two step process in case we later decide to extract other info
  return(coefs)
}
