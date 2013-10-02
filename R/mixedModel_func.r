mixedModel_func <-
  function(MMdata, min.L, nyr, od=F, V=F){
    MM <- fit_LadybirdMM2(MMdata, nsp=min.L, nyr, od=od, V=F)
    names(MM) <- c('year','year_SE','year_zscore','year_pvalue','intercept','intercept_SE','yearZero','Ymin','Ymax','pVisitsUsed','nVisitsUsed','nSpeciesObs','errorMsg') # cvg_code removed after Y_min
    return(MM)
  }
