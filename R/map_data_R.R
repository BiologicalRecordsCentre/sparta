map_data_R <-
function(gridrefs, attribute = NULL, breaks = NULL, shape_data = UK, xlim = NULL, ylim = NULL, grid.div = 100000, gr_prec = 10000, sq_border = NULL,sq_col = NA, legend_pos = "topleft", leg_cex = 0.4, ...){
		# Setup blank map of shape_data
		plot_GIS(shape_data, new.window = FALSE, blank.plot = TRUE, xlim = xlim, ylim = ylim, grid.div = grid.div, set.margin=FALSE, ...)
		
		# Plot grid squares onto shape_data
		if(!is.null(attribute) & !is.null(breaks)){
			plotUK_gr_cats(gridrefs, attribute, breaks = breaks, gr_prec = gr_prec, border = sq_border, legend_pos = legend_pos, leg_cex = leg_cex)
		} else {
			plotUK_gr(gridrefs, gr_prec = gr_prec, col = sq_col, border = sq_border)
		}
		
		# Plot UK outline over mapping
		plot_GIS(shape_data, new.window = FALSE, additions = TRUE, xlim = xlim, ylim = ylim, grid.div = grid.div, set.margin=FALSE, ...)
	}
