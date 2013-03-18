cat_breaks <-
function(x, n_cat = 10, breaks = NULL, cat_type = "data", whole_breaks = FALSE, min_val = NULL, max_val = NULL, rnd_digits = NULL){
		# Setup variable to take catergorical version
			ret_obj = rep(NA, length(x))
		# # breaks
			# if min and max are null then set to min and max of dataset
			if(is.null(min_val)){
				min_val = min(x, na.rm=TRUE)
			}
			if(is.null(max_val)){
				max_val = max(x, na.rm =TRUE)
			}
			x_inds = which(x>= min_val & x <= max_val)
			
			
			if(is.null(breaks)){
				# If breaks not supplied then assume breaks to be determined by splitting into n categories
				if(is.null(n_cat)){
					stop("ERROR: Either n_cats or breaks must be specified")
				} else {
					if(cat_type == "data"){
						# Splitting into n categories is done by choosing n breaks that assign as even proportion of the data into each category
						# NOTE when whole breaks == TRUE or rounding used then proportions in categories will be less even due to rounding
						breaks = quantile(x[x_inds], probs = seq(0,1,length.out= n_cat + 1), na.rm = TRUE)
					} else if(cat_type == "spacing"){
						breaks = seq(from = min_val, to = max_val, length.out = n_cat)
					} else {
						stop("ERROR: cat_type must be either \"data\" or \"spacing\"")
					}
				}
			} else {
				# If breaks is specified then check whether min_val and max_val extend beyond specified breaks if so extend breaks
				if(min_val <= min(breaks)){
					breaks = c(min_val,breaks)
				}
				if(max_val >= max(breaks)){
					breaks = c(breaks, max_val)
				}
			}
			
		# Round breaks if whole_breaks is true
		if(whole_breaks){
			# Lower first break to nearest integer
			breaks[1] = floor(breaks[1])
			# Increase last break to nearest integer
			breaks[length(breaks)] = ceiling(breaks[length(breaks)])
			# Round other breaks to nearest integer
			breaks[2:(length(breaks)-1)] = round(breaks[2:(length(breaks)-1)])
		} else {
			if(is.null(rnd_digits) == FALSE){
				breaks = round(breaks,rnd_digits)
			}
		}
		
		# Return breaks
		return(breaks)
	}
