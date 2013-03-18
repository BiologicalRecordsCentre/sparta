CI_insert_en <-
function(easting, northing, origin= c(-180000,30000)){
	# Convert values to new origin
		easting = (easting - 500000) + origin[1]
		northing = (northing - 5430000) + origin[2]
		
	# Return new values
	ret_obj = data.frame(EASTING = easting, NORTHING = northing, stringsAsFactors = FALSE)
	return(ret_obj)
}
