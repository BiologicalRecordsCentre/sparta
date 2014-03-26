# Internal function that uses the implementation of Telfer written by Gary Powney

GP_telfer <- function (taxa_data,time_periods,iterations=10,useIterations=TRUE,min_sq=5){
  
  # Create a vector of all the years to include
  for (i in 1:length(row.names(time_periods))) {
    run<-time_periods[i,1]:time_periods[i,2]
    if (exists('all_years')){
      all_years <- c(all_years,run)
    }else{
      all_years <- run
    }
  }
  
  # Subset data to years needed
  taxa_data<-taxa_data[taxa_data$year %in% all_years,]
  
  # The year to split is taken to be the average of the max of earlier time period
  # and min of the later time period
  splityr <- mean(c(max(time_periods[1,]),min(time_periods[2,])))
    
  # subset data to one object for each time perod
  T1 <- taxa_data[taxa_data$year<splityr,]
  T2 <- taxa_data[taxa_data$year>splityr,]
  
  # remove gridcells that are only found in one time period
  T1cells <- unique(T1$site)
  T2cells <- unique(T2$site)
  allGood <- T1cells[T1cells %in% T2cells]
  T1 <- T1[T1$site %in% allGood,]
  T2 <- T2[T2$site %in% allGood,]

  # Change to GP format (2 col, $species and $gcell)
  T1 <- unique(T1[c('CONCEPT','site')])
  T2 <- unique(T2[c('CONCEPT','site')])
  names(T1) <- c('species','gcell')
  names(T2) <- c('species','gcell')
  
  ### Identify the number of grid cells each species occupies in each time period. ###
  T1_species<-as.vector(unique(T1$species)) # creates species list for time period 1
  
  rangesT1<-NULL   					# create the vector to be filled	
  for (x in T1_species){				# for each species in time period 1
    temp<-T1[T1$species==x,]			# create a mini table for species x in time period 1
    size<-length(temp[,1])				# identify the number of cells the species occupies (species range size)
    rangesT1<-c(rangesT1,size)				# concatenate all species range sizes
  }
  T1_range<-data.frame(T1_species,rangesT1) # create a dataframe of species and range size for time period 1
  
  ## Do the same for the second period, but using the species list for the first period this will put 0 in counts for species with no record in second period ##
  
  rangesT2<-NULL   					# create the vector to be filled
  for (x in T1_species){				# for each species in time period 1
    temp<-T2[T2$species==x,]			# create a mini table for species x in time period 2
    size<-length(temp[,1])			# identify the number of cells the species occupies (species range size)
    rangesT2<-c(rangesT2,size)		# concatenate all species range sizes
  }
  T2_range<-data.frame(T1_species,rangesT2) # create a dataframe of species and range size for time period 2
  
  ### Remove species which have less than min_sq grid cells in first period ###
  T1_range_good<-T1_range[T1_range$ranges>=min_sq,]
  
  ### Link the two tables on species then identify the change in number of grid squares between time periods. ###
  names(T1_range_good)[1]<-"CONCEPT"  		# re-naming columns
  names(T2_range)[1]<-"CONCEPT"				# re-naming columns
  spp_table<-merge(T1_range_good,T2_range)	# merge T1 with T2 removing species with less than 5 gridcells in the first period.
  
  ### Identify the simple difference between the two time periods ###
  spp_table$range_change<-(spp_table$rangesT2-spp_table$rangesT1) # identify the difference between T1 and T2

  ### convert the grid cell number to proportion of total number fo cells surveyed ###
  total.cells<-length(unique(T1$gcell))  		# this is the total number of cells surveyed
  spp_table$T1_range_prop<-(spp_table$rangesT1+0.5)/(total.cells+1)			# identify the proportion of number of cells in T1 of the total cells surveyed.  To avoid the problems associated with 0 proportions they were calculated as (x+0.5)/(n+1).
  spp_table$T2_range_prop<-(spp_table$rangesT2+0.5)/(total.cells+1)				# identify the proportion of number of cells in T2 of the total cells surveyed
  
  
  spp_table$T1_logit_range<-log(spp_table$T1_range_prop/(1-spp_table$T1_range_prop))  # logit transform the proportions
  spp_table$T2_logit_range<-log(spp_table$T2_range_prop/(1-spp_table$T2_range_prop))	# logit transform the proportions
  
  ### To account for non constant variance we must do the following steps to create a variable to weight the final regression ###
  row.names(spp_table) <- spp_table$CONCEPT #name rows helps make sense of the model output
  m1<-lm(T2_logit_range~T1_logit_range,data=spp_table)  	# linear regression of two time periods

  if(!is.logical(useIterations)){
    stop('useIterations must be a logical variable')
  } else if(!useIterations){
    spp_table$change_index<-rstandard(m1)
    final_output_table<-spp_table[,c(1,9)]
    return(final_output_table)
    
  } else if(useIterations){
    
    residual_m1<-resid(m1)  								# take the residuals from the lm
    spp_table$sq_residual_m1<-residual_m1^2					# square the residuals
    
    spp_table$fitted_proportions<-m1$fitted					# calculated the fitted proportions and add them into the dataframe
    
    P<-ilt(spp_table$fitted_proportions)					# exponential function, shown above.
    spp_table$fitted_proportions<-P							# overwrite fitted values to fitted proportions
    
    total.cell.1<-total.cells+1								# total grid cells surveyed +1 
    
    spp_table$NP_test<-1/(total.cell.1*spp_table$fitted_proportions*(1-spp_table$fitted_proportions))	# 1/[NP (1-P)] in telfer paper
    
    m2<-lm(sq_residual_m1~NP_test,data=spp_table)  		# second model which is the squared residuals of m1 on 1/[NP (1-P)]
     
    c_<-coef(m2)[1]   									# take the intercept of m2 
    d_<-coef(m2)[2]											# take the slope of m2
    
    V_<-NULL												# prepare variance vector
    rep_loop<-1:(iterations-1)			# repeat the process 
    C_all<-c_												# prepare c vector
    D_all<-d_												# prepare d vector
    V_all<-V_												# prepare variance vector
    
    
    for (i in rep_loop){  								# start loop
      
      # c_ + d_ should not be in brackets as they were in a previous version
      V_<- c_ + d_ / (total.cell.1*spp_table$fitted_proportions*(1-spp_table$fitted_proportions))	# work out the variance (modification of the binomial proportion variance structure)
      inv_sq_V<-1/(V_^2)										# take the inverse of variance squared.
      
      m4<-lm(sq_residual_m1~NP_test,weight=inv_sq_V,data=spp_table)	# run the new model weighting by the inverse variance identifeid above.
      
      c_<-coef(m4)[1]											# take the intercept of the new model
      d_<-coef(m4)[2]											# take the slope of the new model
      
      C_all<-c(C_all,c_)										# build a vector with all of the new intercepts
      D_all<-c(D_all,d_)										# build a vector with all of the new slopes
      V_all<-c(V_all,V_)										# build a vector with all of the new inverse variances squared
    }
  
    ## final model to take the residuals from
    
    spp_table$recip_V<-1/V_  										# take the reciprocal of the "settled" variance (Telfer et al 2002)
    
    m1<-lm(T2_logit_range~T1_logit_range,weight=recip_V,data=spp_table)	# use the reciprocal of the "settled" variance as weight for the final model.
    spp_table$change_index<-rstandard(m1)								# take the standardised residuals from the model.  For each species, the standardised residual from the fitted regression line provides the index of relative change in range size. A species with a negative change index has been recorded in relatively fewer grid cells in the later period, whereas a species with a positive change index has been recorded in relatively more.  
    
    final_output_table<-spp_table[,c(1,13)]	

  }

}
