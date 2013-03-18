gr_let2num <-
function(gridref, centre = FALSE, gr_prec = NULL, return_projection = FALSE){
	# Function required to calculate easting in Letter Grid
	spmod = function(x, mod){
		ret_obj = x %% mod
		ret_obj[ret_obj == 0] = mod
		return(ret_obj)
	}
	
	# Remove any spaces from gridref
		gridref = gsub("[ -]","",toupper(gridref))
	
	# Setup variable to hold output
		len_grvec = length(gridref)
		if(return_projection){
			ret_obj = data.frame( EASTING = rep(NA,len_grvec), NORTHING = rep(NA, len_grvec), PROJECTION = rep(NA, len_grvec), row.names = NULL ) # row.names set to null to stop duplicate row names error
		} else {
			ret_obj = data.frame( EASTING = rep(NA,len_grvec), NORTHING = rep(NA, len_grvec), row.names = NULL ) # row.names set to null to stop duplicate row names error
		}
		
	# First find all British Gridrefs
		cty_inds = which(grepl('^[[:upper:]]{2}[[:digit:]]{2,}$',gridref) & !grepl('^(WA)|(WV)[[:digit:]]{2,}$',gridref))

		# If British Gridrefs found then calc easting and northings
		if(length(cty_inds) > 0){
			# Get position of gridref letters in grid
				l1 = match(substr(gridref[cty_inds],1,1), LETTERS[-9])
				l2 = match(substr(gridref[cty_inds],2,2), LETTERS[-9])
			# Determine initial easting northing digits based on 500km square
				e = (spmod(l1,5) - 1)*5
				n = floor(abs(l1 - 25)/5)*5
			# Modify intial easting/northing digits based on 100km square
				e = e + (spmod(l2,5) - 1)
				n = n + floor(abs(l2 - 25)/5)
			# Recalulate for false origin (SV) of British Grid
				e = e - 10
				n = n - 5
			# skip grid letters to get numeric part of grid ref
				# Extract digits
				gr_nums = substr(gridref[cty_inds],3,nchar(gridref[cty_inds]))
				# Seperate easting and northing digits
				east_num = substr(gr_nums,1,nchar(gr_nums)/2)
				north_num = substr(gr_nums,(nchar(gr_nums)/2)+1, nchar(gr_nums))
			# Extend so nchars of east_num/north_num = 5 right padded with zeros
				east_num = gsub(" ","0", format(east_num, width = 5))
				north_num = gsub(" ","0", format(north_num, width = 5))
			# append numeric part of references to grid index
				e = paste(e,east_num, sep="")
				n = paste(n,north_num, sep="")
			# Overwrite placeholder values in ret_obj
				ret_obj[cty_inds,c("EASTING", "NORTHING")] = c(as.numeric(e), as.numeric(n))
				if(return_projection){
					ret_obj[cty_inds,"PROJECTION"] = "OSGB"
				}
		}
		
	# Find all Irish Gridrefs
		cty_inds = which(grepl('^[[:upper:]]{1}[[:digit:]]{2,}$',gridref))
		# If Irish Gridrefs found the calc easting and northings
		if(length(cty_inds) > 0){
			# Get position of gridref letters in grid
				l2 = match(substr(gridref[cty_inds],1,1), LETTERS[-9])
			# Determine intial easting/northing digits based on 100km square
				e = spmod(l2,5) - 1
				n = floor(abs(l2 - 25)/5)
			# skip grid letters to get numeric part of grid ref
				# Extract digits
					gr_nums = substr(gridref[cty_inds],2,nchar(gridref[cty_inds]))
				# Seperate easting and northing digits
					east_num = substr(gr_nums,1,nchar(gr_nums)/2)
					north_num = substr(gr_nums,(nchar(gr_nums)/2)+1, nchar(gr_nums))
			# Extend so nchars of east_num/north_num = 5 right padded with zeros
				east_num = gsub(" ","0", format(east_num, width = 5))
				north_num = gsub(" ","0", format(north_num, width = 5))
			# append numeric part of references to grid index
				e = paste(e,east_num, sep="")
				n = paste(n,north_num, sep="")
			# Overwrite placeholder values in ret_obj
				ret_obj[cty_inds,c("EASTING", "NORTHING")] = c(as.numeric(e), as.numeric(n))
				if(return_projection){
					ret_obj[cty_inds,"PROJECTION"] = "OSNI"
				}
		}
	
	# Find all Channel Islands Gridrefs
		cty_inds = which(grepl('^(WA)|(WV)[[:digit:]]{2,}$',gridref))
		# If CI gridrefs found then calc easting and northings
		if(length(cty_inds) > 0){
			# Determine intial easting/northing based on letters
				e = rep(5, length(cty_inds))
				n = ifelse(grepl('^(WA)[[:digit:]]{2,}$',gridref[cty_inds]),55,54)
			# skip grid letters to get numeric part of grid ref
				# Extract digits
				gr_nums = substr(gridref[cty_inds],3,nchar(gridref[cty_inds]))
				# Seperate easting and northing digits
				east_num = substr(gr_nums,1,nchar(gr_nums)/2)
				north_num = substr(gr_nums,(nchar(gr_nums)/2)+1, nchar(gr_nums))
			# Extend so nchars of east_num/north_num = 5 right padded with zeros
				east_num = gsub(" ","0", format(east_num, width = 5))
				north_num = gsub(" ","0", format(north_num, width = 5))
			# append numeric part of references to grid index
				e = paste(e,east_num, sep="")
				n = paste(n,north_num, sep="")
			# Overwrite placeholder values in ret_obj
				ret_obj[cty_inds,c("EASTING", "NORTHING")] = c(as.numeric(e), as.numeric(n))
				if(return_projection){
					ret_obj[cty_inds,"PROJECTION"] = "UTM30"
				}
		}
	# If centre is true the determine precision and give easting and northing for centre of gridref
	if(centre){
		# Determine precision (if gr_prec not supplied)
			if(is.null(gr_prec)){
				gr_prec = det_gr_precision(gridref)
			}
		# Add half of precision to easting and northing
			ret_obj[,c("EASTING", "NORTHING")] = ret_obj[,c("EASTING", "NORTHING")] + (gr_prec/2)
	}
	# Return easting and Northings	
	return(ret_obj)
}
