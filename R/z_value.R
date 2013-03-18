z_value <-
function(trend, spp_names = NULL, Tfactor_col = "TFactor", sd_col = "St_Dev", tp_col = "Time______", spp_col = "Species__"){
	# Remove zero time periods
	trend = trend[trend[,tp_col] != 0,]
	
	# Check that trend only includes two time periods (once zero time periods have been removed)
	tp = unique(trend[,tp_col])
	if(length(tp) != 2){
		stop("Z value can only be calculated when there are only two time periods")
	}
	
	# Check trend is in species,time_period order
	trend = trend[order(trend[,spp_col], trend[,tp_col]),]
	
	# Calculate Z values
	z_val = ( trend[,Tfactor_col][trend[,tp_col] == tp[2]] - trend[,Tfactor_col][trend[,tp_col] == tp[1]] ) / sqrt( trend[,sd_col][trend[,tp_col] == tp[2]]^2 + trend[,sd_col][trend[,tp_col] == tp[1]]^2 )
	
	# Build data.frame to return
	ret_obj = data.frame(SPECIES = trend[,spp_col][trend[,tp_col] == tp[1]], Z_VAL = z_val, SIG_95 = abs(z_val) > 1.96, stringsAsFactors = FALSE)
	
	# If spp_names supplied then merge these
		if(!is.null(spp_names)){
			ret_obj = merge(ret_obj, spp_names, by.x = "SPECIES", by.y = eval(names(spp_names)[1]), all.x = TRUE)
			# Reorder columns so name next to species column and reorder rows so in alphabetic order
			ret_obj = ret_obj[order(ret_obj[,4]),c(1,4,2,3)]
		}

	# Return
	return(ret_obj)
}
