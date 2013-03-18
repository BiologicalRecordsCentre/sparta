add_channel_isles <-
function(CI_outline_gr, insert_origin = c(-180000,30000), insert_dimen = c(100000,100000), ins_border = NULL, ins_col = "white", col = NA, border = NULL, show_gridline = FALSE){
	# Add insert box
		rect(insert_origin[1], insert_origin[2], insert_origin[1]+insert_dimen[1], insert_origin[2] + insert_dimen[2], border = ins_border, col = ins_col)
	
	# Add gridline showing seperation between WA and WV
	if(show_gridline){
		# Determine coords for gridline
			gr_line = CI_insert_gr("WA00",origin = insert_origin)$NORTHING
		segments(x0 = insert_origin[1], gr_line, insert_origin[1]+insert_dimen[1], gr_line, col= "grey")
	}
	
	# Plot CI outline coverting gridrefs using CI_insert_gr
		polygon(CI_insert_gr(CI_outline_gr, origin = insert_origin), col = col, border = border)
}
