sp_oneplot <-
function(trend, freq, spp_name = NULL){
		# Setup outer margin area
		par(oma = c(1,0,2,0))
		
		# Subdivied device into 5 figures
		layout(matrix(c(1,2,3,4,3,5,3,5), ncol=2, nrow = 4, byrow = TRUE), heights = c(0.5,0.1, 0.2, 0.2))
		# PLOT MAP BASED FIGURES
		
		par(mar = c(0,0,2,0))
		
    # Actual Distribution
		# Note at present this is a fudge until Mark adapts Frescalo to output original dist column
		map_data_R(gridrefs = freq$Location[freq$Pres == 1],attribute = NULL, show.axis = FALSE, show.grid = FALSE, sq_col="darkolivegreen", sq_border = NA, bg.col = NULL, shape_data = UK)
		mtext("(a) Actual Distribution", adj = 0.05, font = 1, cex = 0.7)
		
    
		# Neighbourhood Frequency
		map_data_R(gridrefs = freq$Location,attribute = freq$Freq__ ,breaks = seq(0.0, 1.0, 0.1), show.axis = FALSE, show.grid = FALSE, legend_pos = NULL, sq_border = NA, bg.col = NULL, shape_data = UK)
		mtext("(b) Neighbourhood Frequency", adj = 0.05, font = 1, cex = 0.7)
		
		# Adjusted Frequency
		map_data_R(gridrefs = freq$Location,attribute = freq$Freq_1 ,breaks = seq(0.0, 1.0, 0.1), show.axis = FALSE, show.grid = FALSE, legend_pos = NULL, sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(c) Adjusted Frequency",adj = 0.05, font = 1, cex = 0.7)
		
    
		par(mar = c(0,0,0,0))
		plot(1, type="n", axes = FALSE, frame.plot = FALSE, xlab ="", ylab ="")
		legend("center",legend = c("0.0 - <0.1","0.1 - <0.2","0.2 - <0.3","0.4 - <0.5","0.5 - <0.6","0.6 - <0.7","0.7 - <0.8","0.8 - <0.9","0.9 - 1.0"), fill = rev(heat.colors(9)), title="Frequency", ncol = 3, bty="n")
		
		# TFACTOR PLOT
		par(mar = c(5,4,2,2)+ 0.1)
		lm_stats = plot_Tfactor(trend, spp_name = spp_name)
		mtext("(d) Tfactor", adj = 0.05, font = 1, cex = 0.7, line = 1)
		
		# Try printing title to top of figure
		mtext(spp_name,outer=TRUE, font = 4)
		
		# Return lm_stats if assigned
		invisible(lm_stats)
	}
