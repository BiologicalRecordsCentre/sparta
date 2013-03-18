LatLongtoOSGrids <-
function(Latitude, Longitude, Datum = "OSGB", datum_params = datum_vars, full_output = FALSE) {
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
		
	# Setup object to hold output data
		if(full_output){
			ret_obj = data.frame(LATITUDE = Latitude, LONGITUDE = Longitude, DATUM = Datum, EASTING = rep(NA, lat_len), NORTHING =rep(NA, lat_len), stringsAsFactors = FALSE)
		} else {
			ret_obj = data.frame(EASTING = rep(NA, lat_len), NORTHING = rep(NA, lat_len), stringsAsFactors = FALSE)
		}
	
	# Loop through datum and extract params and then calculate easting and northing
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
		F0 = datum_params$F0[par_ind]
		lat0 = datum_params$lat0[par_ind] * (pi/180) # Converting to radians
		lon0 = datum_params$lon0[par_ind] * (pi/180) # Converting to radians
		N0 = datum_params$N0[par_ind]
		E0 = datum_params$E0[par_ind]
	
	# Calculate other derived variables for given datum/projection
	e2 = 1 - (b^2)/(a^2)		# eccentricity squared
	n = (a-b)/(a+b)
	n2 = n^2   					# Calculate n squared (used several times so store as variable)
	n3 = n^3					# Calculate n cubed (used several times so store as variable)
  
	# Convert input Latitude and Longitude (for current datum) to Radians
	lat = Latitude[dat_inds]*(pi/180)		# Convert Latitude to Radians
    lon = Longitude[dat_inds]*(pi/180)		# Convert Longitude to Radians

    cosLat = cos(lat)								# Calculate cos of radian latitude
    sinLat = sin(lat)								# Calculate cos of radian latitude
    nu = (a * F0)*(1-e2*sinLat^2)^-0.5              # transverse radius of curvature
    rho = (a * F0) * (1-e2)*(1-e2*sinLat^2)^-1.5   	# meridional radius of curvature
    eta2 = (nu/rho)-1								# East-west component of the deviation of the vertical squared
  
	# Calculate Meridional Arc
    Ma = ( 1 + n + ((5/4)*n2) + ((5/4)*n3) ) * (lat-lat0)
    Mb = ( (3*n) + (3*n2) + ((21/8)*n3) ) * sin(lat-lat0) * cos(lat+lat0)
    Mc = ( ((15/8)*n2) + ((15/8)*n3) ) * sin(2*(lat-lat0)) * cos(2*(lat+lat0))
    Md = ( (35/24)*n3) * sin(3*(lat-lat0)) * cos(3*(lat+lat0))
    M = (b * F0) * (Ma - Mb + Mc - Md)			# Calculate Developed Meridional arc (M)
  
	# Calculate and store powers of cos and tan of latitude as used serveral times below
    cos3lat = cosLat^3
    cos5lat = cosLat^5
    tan2lat = tan(lat)^2
    tan4lat = tan(lat)^4
  
	# Main subequations to determine Northing & Easting
    I = M + N0;
    II = (nu/2)*sinLat*cosLat
    III = (nu/24)*sinLat*cos3lat*(5-tan2lat+(9*eta2))
    IV = (nu/720)*sinLat*cos5lat*(61-(58*tan2lat)+tan4lat)
    V = nu*cosLat
    VI = (nu/6)*cos3lat*( (nu/rho)-tan2lat)
    VII = (nu/120) * cos5lat * (5 - (18*tan2lat) + tan4lat + (14*eta2) - (58*tan2lat*eta2))
  
    dLon = lon-lon0

  
    N = floor(I + (II*dLon^2) + (III*dLon^4) + (IV*dLon^6))
    E = floor(E0 + V*dLon + VI*dLon^3 + VII*dLon^5)
	
	# Add Eastings and Northings to ret_obj
	ret_obj[dat_inds,c("EASTING","NORTHING")] = data.frame(E,N)
  }
  return (ret_obj)
}
