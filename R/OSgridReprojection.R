OSgridReprojection <-
function(Easting, Northing, org_grid = "OSNI", out_grid = "OSGB", full_output = FALSE){

  # Determine length of easting and check same as northing
		len_east = length(Easting)
		if(length(Northing) != len_east){
			stop("ERROR: 'Easting' and 'Northing' differ in length")
		}
		
	# Setup object to hold output variable
		if(full_output){
			ret_obj = data.frame(ORG_EASTING = Easting, ORG_NORTHING = Northing, ORG_GRID = org_grid, NEW_GRID = out_grid, EASTING = rep(NA, len_east), NORTHING = rep(NA, len_east), stringsAsFactors = FALSE)
		} else {
			ret_obj = data.frame(EASTING = rep(NA, len_east), NORTHING = rep(NA, len_east), stringsAsFactors = FALSE)
		}
		
	# Determine Lat/Lon (Original projection)
		org_latlon = OSGridstoLatLong(Easting, Northing, org_grid, datum_vars)
	# Determine Cartesian (Original projection)
		org_cart = LatLong_Cartesian(org_latlon$LATITUDE, org_latlon$LONGITUDE, gsub("WGS84","UTM30", org_grid), datum_vars)
	# Apply Helmert transformation to convert orginal projections to WGS84 (unless already WGS84/UTM30)
	if(!org_grid %in% c("WGS84","UTM30")){
		helm_tran = helmert_trans(x =org_cart$x, y = org_cart$y, z = org_cart$z, trans = paste(org_grid,"toWGS84", sep=""))
	} else {
		helm_tran = org_cart
	}
	# If out_grid is not WGS84 or WGS84 elipsoid based (UTM30) then perform secondary convertion to covert WGS84 to out_grid
	if(!out_grid %in% c("WGS84","UTM30")){
		helm_tran = helmert_trans(x = helm_tran$x, y = helm_tran$y, z = helm_tran$z, trans = paste("WGS84to",out_grid, sep=""))
	}
	# Convert Cartesian coordinates to Latitude/Longitude (New projection)
		out_latlon = Cartesian_LatLong(helm_tran$x, helm_tran$y, helm_tran$z, out_grid, datum_vars)
	# Convert Latitude/Longitude to Eastings and Northings (New projection)
		out_en = LatLongtoOSGrids(out_latlon$LATITUDE, out_latlon$LONGITUDE, out_grid, datum_vars)
	# Write to output variable
		ret_obj[,c("EASTING","NORTHING")] = data.frame(out_en$EASTING, out_en$NORTHING)
	return(ret_obj)
}
