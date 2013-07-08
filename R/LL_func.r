LL_func <-
  function (MMdata){
    # Create a new value of year centred on the median year
    # This increases the stability of the model
    medianYear <- median(unique(as.numeric(MMdata$year)))
    MMdata$cYr <- MMdata$year - medianYear
    
    #Concept is numeric, as we are looking at presence and absence of the target species
    LLM <- glm(as.numeric(CONCEPT) ~ cYr + log2(L), binomial, data=MMdata, subset = L>0)
    Ymin <- min(LLM$data['year']) 
    Ymax <- max(LLM$data['year']) 
    
    LL_model <- summary(LLM)$coef

    output <- c(year=LL_model[2,1],year_SE=LL_model[2,2],year_zscore=LL_model[2,3],
                year_pvalue=LL_model[2,4],intercept=LL_model[1,1],intercept_SE=LL_model[1,2],
                intercept_zscore=LL_model[1,3],intercept_pvalue=LL_model[1,4],Log2LL=LL_model[3,1],
                Log2LL_SE=LL_model[3,2],Log2LL_zscore=LL_model[3,3],Log2LL_pvalue=LL_model[3,4],
                yearZero=medianYear, Ymin=Ymin-medianYear, Ymax=Ymax-medianYear)
    
    return(output)
}
