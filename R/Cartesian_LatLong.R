Cartesian_LatLong <-
function(x,y,z, Datum = "OSGB", datum_params = datum_vars, full_output = FALSE ){
	# Determine length of input vars x,y,z and check all 3 variables are same length
		len_x = length(x)
		if(length(y) != len_x | length(z) != len_x){
			stop("ERROR: input variables 'x', 'y' and 'z' are of different lengths")
		}
		
	# Get list of Datums to be converted from Datum
		# If length of Datum input not 1 or same length as input vars then stop
		if(!length(Datum) %in% c(1,len_x)){
			stop("ERROR: Length of 'Datum' does not match length of input 'x','y' & 'z' values")
		}
		datum_list = unique(Datum)
		# Check all Datum present in data are in datum_params data frame
		miss_datum = which(!datum_list %in% datum_params$Datum)
		if(length(miss_datum) > 0){
			stop(paste("ERROR: Datum present in data for which parameters have not been given (",paste(sQuote(datum_list[miss_datum]), collapse=","), ")", sep=""))
		}
		
	# Setup object to hold output data (Latitude/Longitude)
		if(full_output){
			ret_obj = data.frame(x = x, y = y, z = z, DATUM = Datum, LATITUDE = rep(NA, len_x), LONGITUDE = rep(NA, len_x), HEIGHT = rep(NA, len_x), stringsAsFactors = FALSE)
		} else {
			ret_obj = data.frame(LATITUDE = rep(NA, len_x), LONGITUDE = rep(NA, len_x), stringsAsFactors = FALSE)
		}
		
	# Setup precision variable (used to determine when to stop iteration)
		prec = 1
		
	# Loop through datum and extract params and then calculate Latitude and Longitude
	for(i_datum in 1:length(datum_list)){
	
		# Extract indices of lat/long values corresponding to current datum
		if(length(Datum) == 1){
			dat_inds = 1:len_x
		} else {
			dat_inds = which(Datum == datum_list[i_datum])
		}
		
		# Extract datum params from datum_params (at same time covert lat0 and lon0 from degrees to radians
			par_ind = which(datum_params$Datum == datum_list[i_datum])
		# Check only one row of datum parameters data frame matches current datum
		if(length(par_ind) > 1){
			stop(paste("ERROR: More than one match for current Datum (",sQuote(datum_list[i_datum]),") found in datum_params data frame",sep=""))
		}
			a = datum_params$a[par_ind]
			b = datum_params$b[par_ind]

	
		# Calculate eccentricity squared (e2)
			e2 = (a^2 - b^2)/a^2
			
		# Determine longitude
			lon = atan(y[dat_inds]/x[dat_inds])
		# Iteratively determine latitude
			p = sqrt(x[dat_inds]^2 + y[dat_inds]^2)
			lat = atan( z[dat_inds] / (p * (1 - e2)) )
			v = a / sqrt(1 - e2*sin(lat)^2)
			lat2 = atan( (z[dat_inds] + e2*v*sin(lat))/p )
			d_lat = lat - lat2
			iter_inds = which(d_lat > prec)
			while(length(iter_inds) > 0){
				# Change lat to lat2 and recalculate lat2
				lat[iter_inds] = lat2[iter_inds]
				v[iter_inds] = a[dat_inds][iter_inds] / sqrt(1 - e2*sin(lat[iter_inds])^2)
				lat2[iter_inds] = atan( (z[dat_inds][iter_inds] + e2*v[iter_inds]*sin(lat[iter_inds]))/p[iter_inds] )
				d_lat[iter_inds] = lat[iter_inds] - lat2[iter_inds]
				# Recalculate which rows need further iteration
				iter_inds = which(d_lat > prec)
			}
		# Calculate Height (although in majority of cases not really needed/wanted)
		H = (p / cos(lat)) - v
		# Convert Lat / long back to degrees
			lat = lat * (180/pi)
			lon = lon * (180/pi)
			
		# Setup return object
		if(full_output){
			ret_obj[dat_inds,c("LATITUDE","LONGITUDE", "HEIGHT")] = data.frame(lat, lon, H)
		} else {
			ret_obj[dat_inds,c("LATITUDE","LONGITUDE")] = data.frame(lat, lon)
		}
		
	}
	# Return output object
		return(ret_obj)
}
