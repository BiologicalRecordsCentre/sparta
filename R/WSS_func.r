WSS_func <-
  function(MMdata, od=F, V=F, pvis = NA, family = "Binomial"){
    MM <- fit_LadybirdMM2(MMdata, od=od, V=V, pvis=pvis, family = family)
    names(MM) <- c('year','year_SE','year_zscore','year_pvalue','intercept','intercept_SE','yearZero','Ymin','Ymax','pVisitsUsed','nVisitsUsed','nSpeciesObs','errorMsg') # cvg_code removed after Y_min
    return(MM)
  }
