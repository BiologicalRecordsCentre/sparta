plot_Tfactor <-
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
				# Determine ylims to allow full extent of error bars to be plotted (if errorbars are to be plotted)
					# Determine smoothing y min/max
						smooth_vals = suppressWarnings(try(loess.smooth(trend[,time_col], trend[,Tfactor_col]), silent = TRUE))
				if(is.null(ylim) & errorbars){
						if(fit_smooth & class(smooth_vals) != "try-error"){
							ylim = c(min(trend[,Tfactor_col] - trend[,stdev_col], smooth_vals$y, na.rm =TRUE), max(trend[,Tfactor_col] + trend[, stdev_col], smooth_vals$y, na.rm = TRUE))
						} else {
							ylim = c(min(trend[,Tfactor_col] - trend[,stdev_col], na.rm =TRUE), max(trend[,Tfactor_col] + trend[, stdev_col], na.rm = TRUE))
						}
				}
				# plot Figure
				plot(trend[,time_col], trend[,Tfactor_col], type='o', pch = 16, xlab = "Time", ylab = "TFactor", ylim = ylim, ...)
				# Add Y error bars
				if(errorbars){
					error_bars(trend[,time_col], trend[,Tfactor_col], y_error = trend[,stdev_col])
				}
				# Add smoothing line
				if(fit_smooth & class(smooth_vals) != "try-error"){
					points(smooth_vals, type='l', lwd = 2, col="red")
				}
			} else {
				# Blank Plot
				plot(1, type="n", xaxt = "n", yaxt = "n", xlab = "", ylab ="", bty = "n")
			}
			# Add linear tend
			if(fit_lm & nrow(trend) >= 2){
				lm_mod = as.formula(paste(Tfactor_col,"~", time_col))
				spp_lm = try(lm(lm_mod, data = trend))
				if(class(spp_lm) != "try-error" & nrow(trend[trend[,Tfactor_col] > 0,]) > 0){
					# If fitting linear model worked then add trendline and save results to output file
					abline(spp_lm, lwd = 2)
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
						row.names = trend[1,spp_col]
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
						fres_trend10 = NA
					)
				}
				invisible(stat)
			} else {
			  # Blank Plot
			  plot(1, type="n", xaxt = "n", yaxt = "n", xlab = "", ylab ="", bty = "n")
			  
			  #return NA stats
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
			    fres_trend10 = NA
			  )
			  invisible(stat)
			}
		} else {
			# Blank Plot
			plot(1, type="n", xaxt = "n", yaxt = "n", xlab = "", ylab ="", bty = "n")
			
      #return NA stats
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
			  fres_trend10 = NA
			)
			invisible(stat)
		}
	}
