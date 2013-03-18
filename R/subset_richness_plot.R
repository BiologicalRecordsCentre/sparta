subset_richness_plot <-
function(freq, spp_subset, spp_names = NULL, species_col = "Species", location_col = "Location", freq_col = "Freq__", adj_freq_col = "Freq_1", title_main = "Subset Species Richness", save_data = TRUE, dir_path = getwd()){

		# Restrict data to only species in subset
			freq = freq[freq[,species_col] %in% spp_subset,]
		# Determine real number of subsset species at each square
			act_nsp = sapply(tapply(freq[freq$Pres == 1,species_col], freq[freq$Pres == 1,location_col], unique),length)
		# Determine unadjusted neighbourhood species richness
			neigh_rich = tapply(freq[,freq_col], freq[,location_col], sum, na.rm = TRUE)
		# Determine adjusted neighbourhood species richness
			adj_rich = tapply(freq[,adj_freq_col], freq[,location_col], sum, na.rm = TRUE)
		
		# If save_data = TRUE then write values to text file
		if(save_data){
			# Create data.frame to hold data
			ret_obj = data.frame(LOCATION = unique(freq[,location_col]), ACT_N_SPP = NA, NEIGH_N_SPP = NA, ADJ_N_SPP = NA, row.names = unique(freq[,location_col]), stringsAsFactors = FALSE)
			
			# Add values to data.frame
			ret_obj[names(act_nsp),"ACT_N_SPP"] = act_nsp
			ret_obj[names(neigh_rich),"NEIGH_N_SPP"] = neigh_rich
			ret_obj[names(adj_rich),"ADJ_N_SPP"] = adj_rich
			
			# Write to file
			write.table(ret_obj, file=file.path(dir_path, paste(title_main, ".csv", sep="")), sep=",", quote = TRUE, col.names = TRUE, row.names = FALSE, na = "")
		}
		
		# Mapping the data
		# Setup outer margin area
		par(oma = c(1,0,2,0))
		
		# Subdivide device into 4 figures
		layout(matrix(c(1,2,3,4), ncol = 2, nrow = 2, byrow = TRUE))
		
		# Setup margins
		par(mar = c(0,0,2,0))
		
		# PLOT SUBPLOTS
		# Actual No. Species
		map_data_R(gridrefs = names(act_nsp), attribute = act_nsp, breaks = cat_breaks(act_nsp, n_cat = 10, whole_breaks = TRUE), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.5, sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(a) No. Species (Actual)", adj = 0.05, font = 1, cex = 0.7)
		
		# Neighbourhood Richness
		map_data_R(gridrefs = names(neigh_rich), attribute = neigh_rich, breaks = cat_breaks(neigh_rich, n_cat = 10, whole_breaks = TRUE, rnd_digits = 1), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft",leg_cex = 0.5,  sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(b) No. Species (Neighbourhood)", adj = 0.05, font = 1, cex = 0.7)
		
		# Adjusted Neighbourhood Richness
		map_data_R(gridrefs = names(adj_rich), attribute = adj_rich, breaks = cat_breaks(adj_rich, n_cat = 10, whole_breaks = TRUE, rnd_digits = 1), show.axis = FALSE, show.grid = FALSE, legend_pos = "topleft", leg_cex = 0.5,  sq_border = NA, bg.col = NULL,shape_data = UK)
		mtext("(c) No. Species (Adjusted)", adj = 0.05, font = 1, cex = 0.7)
		
		# Blank Plot
		plot(1:10,1:10, type="n", xaxt = "n", yaxt = "n", bty="n")
		
		# Add main title to device
		mtext(title_main ,outer=TRUE, font = 2)
		
		# Return data if assigned
		invisible(ret_obj)
	}
