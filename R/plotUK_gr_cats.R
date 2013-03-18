plotUK_gr_cats <-
function(gridref, att, breaks, legend_pos = "topleft", leg_cex = 0.7, ...){
	# Check gridref is same length as att
	if(length(gridref) != length(att)){
		stop("Length of Grid Refs does not match length of Attribute")
	}
	# Get colours from heat.colours
		att_cols = rev(heat.colors(length(breaks)-1))
		#att_cols = rev(rainbow(length(breaks)))
	# Loop through breaks (exluding last value) and find gridrefs where att >= breaks[i] but < breaks[i+1]
	for(i in 1:(length(breaks)-1)){
		if(i == length(breaks)-1){
			row_inds = which(att >= breaks[i] & att <= breaks[i+1])
		} else {
			row_inds = which(att >= breaks[i] & att < breaks[i+1])
		}
		if(length(row_inds) > 0){
			plotUK_gr(gridref[row_inds], col = att_cols[i], ...)
		}
	}
	# Add legend
	if(is.null(legend_pos) == FALSE){
		legend(x = legend_pos, legend = paste(breaks[1:(length(breaks)-1)],"-", breaks[2:length(breaks)]), fill=att_cols, cex = leg_cex, bg = "white", inset = 0.015)
	}
}
