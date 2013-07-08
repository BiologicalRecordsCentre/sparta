stat_Tfactor <-
  function(trend,spp_col = "Species__", time_col = "Time______", Tfactor_col = "TFactor", stdev_col = "St_Dev", spp_name = NULL, errorbars = TRUE, fit_smooth = TRUE, fit_lm = TRUE, ylim = NULL, ... ){  
    # Remove any incomplete rows (i.e. those with NA)
    trend = trend[complete.cases(trend),]
    # Remove rows where time_col = 0
    trend = trend[trend[,time_col] != 0,]
    # If spp_name null then assign concept code from 1st row of trend data
    if(is.null(spp_name)){
      spp_name = trend[1,spp_col]
    } 
    # Check that there are some rows left after removal of NAs
    if(nrow(trend) > 0){
      # Continue and plot
          
      # Check that there is at least one none zero Tfactor value
      if(nrow(trend[trend[,Tfactor_col] > 0,]) > 0){
        # Add linear tend
        if(fit_lm & nrow(trend) >= 2){
          lm_mod = as.formula(paste(Tfactor_col,"~", time_col))
          spp_lm = try(lm(lm_mod, data = trend))
          if(class(spp_lm) != "try-error" & nrow(trend[trend[,Tfactor_col] > 0,]) > 0){
            # Setup data.frame of results to be saved
            lm_summ = summary(spp_lm)
            lm_coefs = coef(lm_summ)
            
            stat = data.frame(
              SPECIES = trend[1,spp_col],
              NAME = spp_name, 
              b = lm_coefs[2,1], 
              a = lm_coefs[1,1], 
              b_std_err = lm_coefs[2,2], 
              b_tval = lm_coefs[2,3], 
              b_pval = lm_coefs[2,4], 
              a_std_err = lm_coefs[1,2], 
              a_tval = lm_coefs[1,3], 
              a_pval = lm_coefs[1,4],
              adj_r2 = lm_summ$adj.r.squared,
              r2 = lm_summ$r.squared,
              F_val = lm_summ$fstatistic[1],
              F_num_df = lm_summ$fstatistic[2],
              F_den_df = lm_summ$fstatistic[3],
              fres_trend10 = pc.change(ilt(10*lm_coefs[2,1])),
              Ymin = min(spp_lm$model[time_col]),
              Ymax = max(spp_lm$model[time_col])
            )
                    
          } else {
            stat = data.frame(
              SPECIES = trend[1,spp_col],
              NAME = spp_name, 
              b = NA, 
              a = NA, 
              b_std_err = NA, 
              b_tval = NA, 
              b_pval = NA, 
              a_std_err = NA, 
              a_tval = NA, 
              a_pval = NA,
              adj_r2 = NA,
              r2 = NA,
              F_val = NA,
              F_num_df = NA,
              F_den_df = NA,
              fres_trend10 = NA,
              row.names = trend[1,spp_col]
            )
          }
        
        invisible(stat)
      }else{
        stat = data.frame(
          SPECIES = trend[1,spp_col],
          NAME = spp_name, 
          b = NA, 
          a = NA, 
          b_std_err = NA, 
          b_tval = NA, 
          b_pval = NA, 
          a_std_err = NA, 
          a_tval = NA, 
          a_pval = NA,
          adj_r2 = NA,
          r2 = NA,
          F_val = NA,
          F_num_df = NA,
          F_den_df = NA,
          fres_trend10 = NA,
          row.names = trend[1,spp_col]
        )
        invisible(stat)
      }
    }else{
      stat = data.frame(
        SPECIES = trend[1,spp_col],
        NAME = spp_name, 
        b = NA, 
        a = NA, 
        b_std_err = NA, 
        b_tval = NA, 
        b_pval = NA, 
        a_std_err = NA, 
        a_tval = NA, 
        a_pval = NA,
        adj_r2 = NA,
        r2 = NA,
        F_val = NA,
        F_num_df = NA,
        F_den_df = NA,
        fres_trend10 = NA,
        row.names = trend[1,spp_col]
      )
      invisible(stat)
    }
  }else{
    stat = data.frame(
      SPECIES = trend[1,spp_col],
      NAME = spp_name, 
      b = NA, 
      a = NA, 
      b_std_err = NA, 
      b_tval = NA, 
      b_pval = NA, 
      a_std_err = NA, 
      a_tval = NA, 
      a_pval = NA,
      adj_r2 = NA,
      r2 = NA,
      F_val = NA,
      F_num_df = NA,
      F_den_df = NA,
      fres_trend10 = NA,
      row.names = trend[1,spp_col]
    )
    invisible(stat)
  }
}