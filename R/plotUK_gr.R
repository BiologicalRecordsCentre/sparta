plotUK_gr <-
function(gridref, gr_prec = 10000, ci_insert = TRUE, ci_origin = c(-180000,30000), unit_conv = NULL, ...){
	# Extract only unique gridrefs
		gridref = unique(gridref)	
		
	
	# Determine number of unique gridrefs
		gr_len = length(gridref)
		
	# If gr_prec is null then determine precision from gridref
		if(is.null(gr_prec)){
			gr_prec = det_gr_precision(gridref)
		}
		
	# Setup dataframe to hold data
		gr_poly = data.frame(EASTING = rep(NA,gr_len*5), NORTHING = rep(NA,gr_len*5))
	
	# Determine origin of square and asign one value to every 5th row
		gr_poly[seq(1,by=5, length.out=gr_len),] = gr_let2num(gridref)
		
	# Determine other three corners and assign to every fith row in data.frame (i.e. 2nd, 7th for TL corner, 3rd, 8th for TR, etc)
		# Top Leftprin    
		gr_poly[seq(2,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(0,gr_len),rep(gr_prec,gr_len))
		# Top Right
		gr_poly[seq(3,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(gr_prec,gr_len),rep(gr_prec,gr_len))
		# Bottom Right
		gr_poly[seq(4,by=5, length.out=gr_len),] = gr_poly[seq(1,by=5, length.out=gr_len),] + data.frame(rep(gr_prec,gr_len), rep(0,gr_len))
		    
	# Find Irish gridrefs
		ir_inds = which(grepl("(^[[:upper:]]{1}[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^[[:upper:]]{1}[[:digit:]]{2,}$)", rep(gridref,each=5)) & !is.na(gr_poly$EASTING))
		if(length(ir_inds) > 0){
			gr_poly[ir_inds,] = OSgridReprojection(gr_poly$EASTING[ir_inds], gr_poly$NORTHING[ir_inds], org_grid = "OSNI", out_grid = "OSGB")
		}
		
	# If ci_insert = TRUE then find channel islands gridrefs and convert to insert position
		if(ci_insert){
			# Find channel islands grid refs
			ci_inds = which(grepl("(^(WA|WV)[[:digit:]]{2}([[:upper:]]?|[[:upper:]]{2})$)|(^(WA|WV)[[:digit:]]{2,}$)", rep(gridref,each=5)) & !is.na(gr_poly$EASTING))
			
			if(length(ci_inds) > 0){
				# Convert eastings and northings for these gridrefs to the insert positions
				gr_poly[ci_inds,] = CI_insert_en(gr_poly$EASTING[ci_inds], gr_poly$NORTHING[ci_inds])
			}
		}
		
	# Apply unit conversion if necessary
		if(!is.null(unit_conv)){
			gr_poly = gr_poly * unit_conv
		}
		
	# Plot polygons
    polygon(gr_poly$EASTING, gr_poly$NORTHING, ...) 
	      
	# Return gr_poly invisibly
		invisible(data.frame(GRIDREF = rep(gridref, each = 5), gr_poly))
}
