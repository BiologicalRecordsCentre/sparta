CI_insert_gr <-
function(gridref, origin = c(-180000,30000)){
	# Check that gridrefs are for channel islands
		
	# Determine easting & northing on native grid
		org_en = gr_let2num(gridref)
	# Convert values to new origin
		org_en$EASTING = (org_en$EASTING - 500000) + origin[1]
		org_en$NORTHING = (org_en$NORTHING - 5430000) + origin[2]
	return(org_en)	
}
