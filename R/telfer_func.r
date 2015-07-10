# Internal function that uses the implementation of Telfer written by Gary Powney

telfer_func <- function (taxa_data, iterations = 10, useIterations = TRUE, minSite = 5){
  
  # Check we have two time periods
  TPs <- sort(unique(taxa_data$time_period))
  if(length(unique(taxa_data$time_period)) != 2) stop('There are not two time periods in the data')
    
  # subset data to one object for each time perod
  T1 <- unique(taxa_data[taxa_data$time_period == TPs[1], c('taxa', 'site')])
  T2 <- unique(taxa_data[taxa_data$time_period == TPs[2], c('taxa', 'site')])
  
  # remove gridcells that are only found in one time period
  T1cells <- unique(T1$site)
  T2cells <- unique(T2$site)
  allGood <- T1cells[T1cells %in% T2cells]
  T1 <- T1[T1$site %in% allGood,]
  T2 <- T2[T2$site %in% allGood,]
  
  ### Identify the number of sites each taxa occupies in each time period. ###
  T1_range <- data.frame(taxa = names(table(T1$taxa)), T1Nsites = as.numeric(table(T1$taxa)))
  T2_range <- data.frame(taxa = names(table(T2$taxa)), T2Nsites = as.numeric(table(T2$taxa)))
  
  ### Remove taxa which have less than minSite grid cells in first period ###
  T1_range_good <- T1_range[T1_range$T1Nsites >= minSite,]
  if(nrow(T1_range_good) == 0) stop(paste('No taxa satisfy the minSite criteria when comparing time',
                                          'period', TPs[1], 'and', TPs[2]))
  
  ### Link the two tables by taxa ###
  spp_table <- merge(T1_range_good, T2_range, by = 'taxa')
  
  ### Identify the simple difference between the two time periods ###
  spp_table$range_change <- spp_table$T2Nsites - spp_table$T1Nsites 

  ### convert the grid cell number to proportion of total number of cells surveyed ###
  total.cells <- length(allGood)
  
  # To avoid the problems associated with 0 proportions they were calculated as (x + 0.5) / (n + 1).
  spp_table$T1_range_prop <- (spp_table$T1Nsites + 0.5) / (total.cells + 1)	
  spp_table$T2_range_prop <- (spp_table$T2Nsites + 0.5) / (total.cells + 1)
  
  
  spp_table$T1_logit_range <- log(spp_table$T1_range_prop / (1 - spp_table$T1_range_prop))  # logit transform the proportions
  spp_table$T2_logit_range <- log(spp_table$T2_range_prop / (1 - spp_table$T2_range_prop))	# logit transform the proportions
  
  ### To account for non constant variance we must do the following steps to create a variable to weight the final regression ###
  row.names(spp_table) <- spp_table$taxa # name rows helps make sense of the model output
  m1 <- lm(T2_logit_range ~ T1_logit_range, data = spp_table)  	# linear regression of two time periods

  if(!useIterations){
    
    spp_table$change_index <- rstandard(m1)
    final_output_table <- spp_table[,c(1,9)]
    return(list(final_output_table, spp_table))
    
  } else if(useIterations){
    
    spp_table$sq_residual_m1 <- resid(m1)^2 # square the residuals
    spp_table$fitted_proportions <- ilt(m1$fitted) # exponential function, overwrite fitted values to fitted proportions
    
    total.cell.1 <- total.cells + 1								# total grid cells surveyed + 1 
    
    spp_table$NP_test <- 1 / (total.cell.1 * spp_table$fitted_proportions * 
                                (1 - spp_table$fitted_proportions))	# 1 / [NP (1 - P)] in telfer paper
    
    # second model which is the squared residuals of m1 on 1/[NP (1-P)]
    m2 <- lm(sq_residual_m1 ~ NP_test, data = spp_table)
    
    # second model which is the squared residuals of m1 on 1/[NP (1-P)]
    c_ <- coef(m2)[1] # take the intercept of m2 
    d_ <- coef(m2)[2] # take the slope of m2
    
    V_ <- NULL												# prepare variance vector
    rep_loop <- 1:(iterations-1)		# repeat the process 
    C_all <- c_												# prepare c vector
    D_all <- d_												# prepare d vector
    V_all <- V_												# prepare variance vector
    
    
    for (i in rep_loop){  							
      
      # c_ + d_ should not be in brackets as they were in a previous version
      V_ <- c_ + d_ / (total.cell.1 * spp_table$fitted_proportions * (1 - spp_table$fitted_proportions))	# work out the variance (modification of the binomial proportion variance structure)
      
      # take the inverse of variance squared.
      inv_sq_V <- 1 / (V_^2)										
      
      # run the new model weighting by the inverse variance identifeid above.
      error <- try(m4 <- lm(sq_residual_m1 ~ NP_test, weights = inv_sq_V, data = spp_table), silent = TRUE)
      if(class(error) == 'try-error') stop('Model failed in iteration, too little data?')
      
      c_ <- coef(m4)[1] # take the intercept of the new model
      d_ <- coef(m4)[2] # take the slope of the new model
      
      C_all <- c(C_all,c_) # build a vector with all of the new intercepts
      D_all <- c(D_all,d_) # build a vector with all of the new slopes
      V_all <- c(V_all,V_) # build a vector with all of the new inverse variances squared
      
    }
  
    ## final model to take the residuals from
    # take the reciprocal of the "settled" variance (Telfer et al 2002)
    spp_table$recip_V <- 1 / V_  										
    # use the reciprocal of the "settled" variance as weight for the final model.
    m1 <- lm(T2_logit_range ~ T1_logit_range, weights = spp_table$recip_V, data = spp_table)
    # take the standardised residuals from the model.
    # For each taxa, the standardised residual from the fitted regression line provides
    # the index of relative change in range size. A taxa with a negative change index has
    # been recorded in relatively fewer grid cells in the later period, whereas a taxa with
    # a positive change index has been recorded in relatively more.  
    spp_table$change_index <- rstandard(m1)								
    final_output_table <- spp_table[,c(1,13)]	

    return(list(final_output_table, spp_table))
    
  }

}
