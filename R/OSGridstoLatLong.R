OSGridstoLatLong <-
function(Easting, Northing, Datum = "OSGB", datum_params = datum_vars, full_output = FALSE) {

	# Determine length of Easting & check same as Northing
		east_len = length(Easting)
		if(length(Northing) != east_len){
			stop("ERROR: 'Easting' & 'Northing' are of different lengths")
		}
		
	# Get list of Datums to be converted from Datum
		# If length of Datum input not 1 or same length as Easting/Northing then stop
		if(!length(Datum) %in% c(1,east_len)){
			stop("ERROR: Length of 'Datum' does not match length of Easting/Northing values")
		}
		datum_list = unique(Datum)
		# Check all Datum are in datum_params data frame
		miss_datum = which(!datum_list %in% datum_params$Datum)
		if(length(miss_datum) > 0){
			stop(paste("ERROR: Datum present in data for which parameters have not been given (",paste(sQuote(datum_list[miss_datum]), collapse=","), ")", sep=""))
		}
		
	# Setup object to hold output data
		if(full_output){
			ret_obj = data.frame(EASTING = Easting, NORTHING = Northing, DATUM = Datum, LATITUDE = rep(NA, east_len), LONGITUDE =rep(NA, east_len), stringsAsFactors = FALSE)
		} else {
			ret_obj = data.frame(LATITUDE = rep(NA, east_len), LONGITUDE =rep(NA, east_len), stringsAsFactors = FALSE)
		}
	
	# Loop through datum and extract params and then calculate lat long
	for(i_datum in 1:length(datum_list)){
	
	# Extract indices of easting/northing values corresponding to current datum
		if(length(Datum) == 1){
			dat_inds = 1:east_len
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
		F0 = datum_params$F0[par_ind]
		lat0 = datum_params$lat0[par_ind] * (pi/180) # Converting to radians
		lon0 = datum_params$lon0[par_ind] * (pi/180) # Converting to radians
		N0 = datum_params$N0[par_ind]
		E0 = datum_params$E0[par_ind]

	
	# Calculate other derived variables for projection/datum
		e2 = 1 - (b^2)/(a^2)		# eccentricity squared
		n = (a-b)/(a+b)
		n2 = n^2   					# Calculate n squared (used several times so store as variable)
		n3 = n^3					# Calculate n cubed (used several times so store as variable)
	
	# Iterate to estimate value of lat and M
		# Intial values
		# Lat
			lat = ( (Northing[dat_inds] - N0) / (a * F0) ) + lat0
		# Meridional Arc
			Ma = ( 1 + n + ((5/4)*n2) + ((5/4)*n3) ) * (lat-lat0)
			Mb = ( (3*n) + (3*n2) + ((21/8)*n3) ) * sin(lat-lat0) * cos(lat+lat0)
			Mc = ( ((15/8)*n2) + ((15/8)*n3) ) * sin(2*(lat-lat0)) * cos(2*(lat+lat0))
			Md = ( (35/24)*n3) * sin(3*(lat-lat0)) * cos(3*(lat+lat0))
			M = (b * F0) * (Ma - Mb + Mc - Md)			# Calculate Developed Meridional arc (M)
		# Setup index for lats that need further iteration
			iter_inds = which(abs( Northing[dat_inds] - N0 - M) >= 0.01)
	while(length(iter_inds) > 0){
		# Lat
			lat[iter_inds] = ( (Northing[dat_inds][iter_inds] - N0 - M[iter_inds]) / (a * F0) ) + lat[iter_inds]
		# Meridional Arc
			Ma[iter_inds] = ( 1 + n + ((5/4)*n2) + ((5/4)*n3) ) * (lat[iter_inds]-lat0)
			Mb[iter_inds] = ( (3*n) + (3*n2) + ((21/8)*n3) ) * sin(lat[iter_inds]-lat0) * cos(lat[iter_inds]+lat0)
			Mc[iter_inds] = ( ((15/8)*n2) + ((15/8)*n3) ) * sin(2*(lat[iter_inds]-lat0)) * cos(2*(lat[iter_inds]+lat0))
			Md[iter_inds] = ( (35/24)*n3) * sin(3*(lat[iter_inds]-lat0)) * cos(3*(lat[iter_inds]+lat0))
			M[iter_inds] = (b * F0) * (Ma[iter_inds] - Mb[iter_inds] + Mc[iter_inds] - Md[iter_inds])			# Calculate Developed Meridional arc (M)
		# Recalculate iteration index
			iter_inds = which(abs( Northing[dat_inds] - N0 - M) >= 0.01)
	}
	
	sinLat = sin(lat)
	tanLat = tan(lat)
	tan2Lat = tanLat^2
	tan4Lat = tanLat^4
	tan6Lat = tanLat^6
	secLat = 1 / cos(lat)
	
	elon = Easting[dat_inds] - E0
	
    nu = (a * F0)*(1-e2*sinLat^2)^-0.5              # transverse radius of curvature
    rho = (a * F0) * (1-e2)*(1-e2*sinLat^2)^-1.5   # meridional radius of curvature
    eta2 = (nu/rho)-1								# East-west component of the deviation of the vertical squared
  
	VII = tanLat / (2*rho*nu)
	VIII = ( tanLat / (24*rho*nu^3) ) * ( 5 + (3*tan2Lat) + eta2 - (9 * tan2Lat * eta2) )
	IX = (tanLat/(720*rho*nu^5)) * (61 + (90*tan2Lat) + (45*tan4Lat))
	X = secLat / nu
	XI = (secLat / (6 * nu^3)) * ( (nu/rho) + 2*tan2Lat)
	XII = (secLat / (120 * nu^5)) * (5 + (28*tan2Lat) + (24*tan4Lat))
	XIIA = (secLat / (5040 * nu^7)) * (61 + (662*tan2Lat) + (1320*tan4Lat) + (720*tan6Lat))
	
	Lat_rad = lat - (VII*elon^2) + (VIII*elon^4) - (IX*elon^6)
	Long_rad = lon0 + (X*elon) - (XI*elon^3) + (XII*elon^5) - (XIIA*elon^7)
	
	Latitude = Lat_rad * (180/pi)
	Longitude = Long_rad * (180/pi)
	
    ret_obj[dat_inds,c("LATITUDE","LONGITUDE")] = data.frame(Latitude, Longitude)
  }
  return (ret_obj)
}
