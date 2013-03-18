LatLong_Cartesian <-
function(Latitude, Longitude, Datum = "OSGB", datum_params = datum_vars, H = NULL, full_output = FALSE){
	# Determine length of Latitude & check same as Longitude
		lat_len = length(Latitude)
		if(length(Longitude) != lat_len){
			stop("ERROR: 'Latitude' & 'Longitude' are of different lengths")
		}
		
	# Get list of Datums to be converted from Datum
		# If length of Datum input not 1 or same length as Latitude/Longitude then stop
		if(!length(Datum) %in% c(1,lat_len)){
			stop("ERROR: Length of 'Datum' does not match length of Latitude/Longitude values")
		}
		datum_list = unique(Datum)
		# Check all Datum are in datum_params data frame
		miss_datum = which(!datum_list %in% datum_params$Datum)
		if(length(miss_datum) > 0){
			stop(paste("ERROR: Datum present in data for which parameters have not been given (",paste(sQuote(datum_list[miss_datum]), collapse=","), ")", sep=""))
		}
		
	# Setup object to hold output data (Cartesian Coordinates)
		if(full_output){
			ret_obj = data.frame(LATITUDE = Latitude, LONGITUDE = Longitude, DATUM = Datum, x = rep(NA, lat_len), y = rep(NA, lat_len), z = rep(NA, lat_len),  stringsAsFactors = FALSE)
		} else {
			ret_obj = data.frame(x = rep(NA, lat_len), y = rep(NA, lat_len), z = rep(NA, lat_len), stringsAsFactors = FALSE)
		}
		
	# Loop through datum and extract params and then calculate Cartesian Coordinates
	for(i_datum in 1:length(datum_list)){
	
		# Extract indices of lat/long values corresponding to current datum
		if(length(Datum) == 1){
			dat_inds = 1:lat_len
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
		
			# Height (H) assummed to be 0 unless specified
				if(is.null(H)){
					H = 0
				} else {
					H = H
				}

		# Convert latitude/longitude values into radians
			lat = Latitude[dat_inds] * (pi/180)
			lon = Longitude[dat_inds] * (pi/180)
			
		# Calculate eccentricity squared (e2)
			e2 = (a^2 - b^2)/a^2
		
		# Calculate Cartezian coordinates 
			v = a / sqrt(1 - e2*sin(lat)^2)
			x = (v + H)*cos(lat)*cos(lon)
			y = (v + H)*cos(lat)*sin(lon)
			z = (((1-e2)*v) + H)*sin(lat)
			
		# Write values to ret_obj
			ret_obj[dat_inds,c("x","y","z")] = data.frame(x,y,z)
		
	}
	return(ret_obj)
}
