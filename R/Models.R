Models <-
function (MMdata,min.L,nyr,MM=T,LL=T,wellsamp='visit'){
  # the standard MM, as year-monad (ym) resolution
  if (MM){
    MM <- fit_LadybirdMM(MMdata, nsp=min.L, nyr,wellsamp=wellsamp)[c(1,3:8)]
    names(MM) <- c('trend','zscore','p','cvg','pCombosUsed','SiteDatecombos','n_Obs')
    names(MM) <- paste('MMsp_', names(MM), sep='')
    output <- MM
  }
  if(LL){
    LL_model <- summary(glm(as.numeric(CONCEPT) ~ as.numeric(year) + log2(L), binomial, data=MMdata, subset = L>0))$coef
    if(exists('output')){
       output <- c(output, LL_trend=LL_model[2,1],LL_z=LL_model[2,3], LL_p=LL_model[2,4])
    }else{
       output <- c(LL_trend=LL_model[2,1],LL_z=LL_model[2,3], LL_p=LL_model[2,4])
    }
  }
  return(output)
}
