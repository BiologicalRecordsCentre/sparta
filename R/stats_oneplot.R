stats_oneplot <-
function(stats, title_main = "Summary Statistics"){
		# Setup outer margin area
		par(oma = c(1,0,2,0))
		
		# Subdivide device into 4 figures
		layout(matrix(c(1,2,3,4), ncol = 2, nrow = 2, byrow = TRUE))
		
		# Setup margins
		par(mar = c(0,0,2,0))
		
		# PLOT SUBPLOTS
		# No. Species
		map_data_R(gridrefs = stats$Location, attribute = stats$No_spp, breaks = cat_breaks(stats$No_spp, n_cat = 10, whole_breaks = TRUE), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.5, sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(a) No. Species", adj = 0.05, font = 1, cex = 0.7)
		
		# Rescaled No. Species
		map_data_R(gridrefs = stats$Location, attribute = stats$Spnum_out, breaks = cat_breaks(stats$Spnum_out, n_cat = 10, whole_breaks = TRUE), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft",leg_cex = 0.5,  sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(b) Rescaled No. Species", adj = 0.05, font = 1, cex = 0.7)
		
		# Alpha (Derivied Breaks)
		map_data_R(gridrefs = stats$Location, attribute = stats$Alpha, breaks = cat_breaks(stats$Alpha, n_cat = 10, whole_breaks = FALSE, rnd_digits = 2), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.5,  sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(c) Alpha (Derived Breaks)", adj = 0.05, font = 1, cex = 0.7)
		
		# Alpha (Specified Breaks)
		map_data_R(gridrefs = stats$Location, attribute = stats$Alpha, breaks = cat_breaks(stats$Alpha, n_cat = NULL, breaks = c(0,1,2,4,8,16,32), whole_breaks = TRUE), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.5,  sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(d) Alpha (Specified Breaks)", adj = 0.05, font = 1, cex = 0.7)
		
		# Add main title to device
		mtext(title_main ,outer=TRUE, font = 2)
	}
