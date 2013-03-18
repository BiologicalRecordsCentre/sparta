error_bars <-
function(x,y, x_error = NULL, y_error = NULL, bar_len = 0.05, angle = 90, code = 3, ...){

		# Add x error bars if not null
		if(is.null(x_error) == FALSE){
			# Determine format of x_error (single value, two values, vector, or two column data.frame with 1 or n number of rows)	
			if( is.vector(x_error) ){
				if( length(x_error) == 2 & length(x) != 2 ){
					# x_error is a vector of two (1st value negative deviation, 2nd value positive deviation)
						suppressWarnings(arrows(x - x_error[1], y, x + x_error[2], y, length = bar_len, angle = angle, code = code, ...))
				} else {
					if( length(x_error) == 1 | length(x_error) == length(x) ){
						# x_error is a vector (i.e either 1 value applied symetrically to all points or an n length vector applied symetrically to the points)
							suppressWarnings(arrows(x - x_error, y, x + x_error, y, length = bar_len, angle = angle, code = code, ...))
					} else {
						stop("ERROR - Lengths of x and x_error do not match")
					}
				}
			} else { 
				# x_error is not a vector check that is a data.frame with 2 columns
				if( is.data.frame(x_error) & ncol(x_error) == 2 ){
					if( nrow(x_error) == 1 | nrow(x_error) == length(x)) {
						# x_error is data.frame with 1 or n rows (where n is length of x/y) set of +/- value to be applied to the data points
							suppressWarnings(arrows(x - x_error[,1], y, x + x_error[,2], y, length = bar_len, angle = angle, code = code, ...))
					} else {
						stop("ERROR - Lengths of x and x_error do not match")
					}
				}
			} # If x_error vector
		} # if x_error null
		
		# Add y error bars if not null
		if(is.null(y_error) == FALSE){
			# Determine format of y_error (single value, two values, vector, or two column data.frame with 1 or n number of rows)	
			if( is.vector(y_error) ){
				if( length(y_error) == 2 & length(x) != 2){
					# y_error is a vector of two (1st value negative deviation, 2nd value positive deviation)
						suppressWarnings(arrows(x, y - y_error[1], x, y + y_error[2], length = bar_len, angle = angle, code = code, ...))
				} else {
					if( length(y_error) == 1 | length(y_error) == length(y) ){
						# y_error is a vector (i.e either 1 value applied symetrically to all points or an n length vector applied symetrically to the points)
							suppressWarnings(arrows(x, y - y_error, x, y + y_error, length = bar_len, angle = angle, code = code, ...))
					} else {
						stop("ERROR - Lengths of y and y_error do not match")
					}
				}
			} else { 
				# y_error is not a vector check that is a data.frame with 2 columns and either 1 or n rows
				if( is.data.frame(y_error) & ncol(y_error) == 2 ){
					if( nrow(y_error) == 1 | nrow(y_error) == length(y)) {
						# y_error is data.frame with 1 or n rows (where n is length of x/y) set of +/- value to be applied to the data points
							suppressWarnings(arrows(x, y - y_error[,1], x, y + y_error[,2], length = bar_len, angle = angle, code = code, ...))
					} else {
						stop("ERROR - Lengths of y and y_error do not match")
					}
				}
			} # If x_error vector
		} # if x_error null
		
		if(is.null(x_error) & is.null(y_error)){
			stop("ERROR - x_error and y_error are both null (No error to plot)")
		}
	}
