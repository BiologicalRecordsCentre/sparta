all_oneplots <-
function(stat, trend, freq, spp_names = NULL, onefile = TRUE, onefile_name = NULL, dev_type = "pdf", dir_path = getwd(), dev_args = list(height = 11, width = 7), save_lm_stats = TRUE, stats_fname = "Frescalo Tfactor lm stats.csv", plot= TRUE){
		# Print Progress to screen
		cat("Building Species List")
		# Get list of species in trend data
		 spp_list = unique(trend$Species__)
		# Check no extra species in frequency data
			miss_spp = freq$Species[!unique(freq$Species) %in% spp_list]
			if(length(miss_spp) > 0){
				spp_list = c(spp_list,miss_spp)
			}
			rm(miss_spp)
		# Check that all species in spp_names if not null
		if(!is.null(spp_names)){
			# Find species in datasets but not in spp_names 
			miss_names = spp_list[!spp_list %in% spp_names[,1]]
			# Add miss_names to spp_names
			if(length(miss_names) > 0){
				spp_names = rbind(spp_names, data.frame(CONCEPT = miss_names, NAME = paste("Unnamed Species (", miss_names,")", sep=""), stringsAsFactors = FALSE))
			}
			# Remove any species from spp_names where not in dataset
			spp_names = spp_names[spp_names[,1] %in% spp_list,]
		} else {
			# Create spp_names using values in species column as name and code
			spp_names = data.frame(SPECIES = spp_list, NAME = spp_list, stringsAsFactors = FALSE)
		}
		# Reorder spp_names according to NAME
		spp_names = spp_names[order(spp_names[,2], spp_names[,1]),]
		
		# Calculate no. of species
		spp_tot = nrow(spp_names)
		
		# Print Progress
		cat(" - Complete\n")
    
    if(plot){
  		# Setup plotting Device parameters
  			# Print Progress to screen
  			cat("Setting Up Plotting Device")
  			# Dev type extension lookup
  				dev_ext = switch(dev_type, png = "png",jpeg = "jpg", bmp = "bmp",tiff = "tif",postscript = "eps",pdf = "pdf", win.metafile = "emf")
  				if(is.null(dev_ext)){
  					stop(paste("device type", dev_type, "not recognised (choose from png, jpeg, bmp, tiff, postscript, pdf"))
  				}
  			# Check the onefile and dev_type values are compatible
  			if(onefile & !dev_type %in% c("pdf","postscript")){
  				stop(paste("Device type '",dev_type, "' not compatible with plotting multiple figure into one file", sep=""))
  			}
  			# Create filename for device if one_file == TRUE
  			if(onefile & is.null(onefile_name)){
  				onefile_name = paste("Frescalo Plots ", format(Sys.time(),"%Y %m %d %H_%M_%S"), ".", dev_ext, sep="")
  			} else if (onefile & !is.null(onefile_name)){
  				# check that file has valid extension
  				if(grepl("[.]", onefile_name)){
  					if(!grepl(paste("[.]", dev_ext,"$",sep=""), onefile_name)){
  						stop(paste("Extension given in file name ('",onefile_name,"') is not valid for device type specified (device: ",dev_type,", expecting extension '.", dev_ext, "')", sep=""))
  					}
  				} else {
  					onefile_name = paste(onefile_name,".",dev_ext, sep="")
  				}
  			}
  			
  			# Add onefile and file to dev_args
  			if(onefile){
  				dev_args["file"] = file.path(dir_path, onefile_name)
  				dev_args["onefile"] = onefile
  			}
  			
  		  # Open device if onefile == TRUE	
  			if(onefile){
  				do.call(dev_type, dev_args)
  			}
  			
  			# Print Progress
  			cat(" - Complete\n")
  		  # Plot General Frescalo summary Statistics
  			# Print Progress
  			cat("Plotting General Summary Statistics")
  			# If not one file then open device
  			if(!onefile){
  				dev_args["filename"] = file.path(dir_path, paste("Summary Statistics.",dev_ext,sep=""))
  				do.call(dev_type, dev_args)
  			}
  			
  			# Plot Stats
  			stats_oneplot(stat)
  			
  			# Close stats device if onefile FALSE
  			if(!onefile){
  				dev.off()
  			}
  			
  			# Print Progress
  			cat(" - Complete\n")
      }
		  # Loop through each species and plot sp_oneplot
			# Setup variable to determine if lm stats file already created
				lm_file_created = FALSE
			# Print Progress
			cat("Outputting Species Results\n")
			for(i_spp in 1:nrow(spp_names)){
				cat("\tSpecies ", i_spp, " of ", spp_tot, " - ",spp_names[i_spp,2]," - ", format(Sys.time(),"%d/%m/%Y %H:%M:%S"), "\n", sep="")
				# If not one file then open device
				if(plot & !onefile){
					dev_args["filename"] = file.path(dir_path, paste(spp_names[i_spp,2]," multiplot.",dev_ext,sep=""))
					do.call(dev_type, dev_args)
				}
				
				# Plot Species Maps/Plots
				if(plot){
          sp_lmstat = sp_oneplot(trend[trend$Species__ == spp_names[i_spp,1],], freq[freq$Species == spp_names[i_spp,1],], spp_names[i_spp,2])
				} else {
          sp_lmstat = stat_Tfactor(trend[trend$Species__ == spp_names[i_spp,1],], spp_name = spp_names[i_spp,2])
				}
				# Close stats device if onefile FALSE
				if(plot & !onefile){
					dev.off()
				}
				
				# If lm stats to be saved then write results to file
				if(save_lm_stats){
					if(lm_file_created){
						# Append current spp data to stats file
						write.table(sp_lmstat,file = file.path(dir_path,stats_fname), col.names = FALSE, row.names = FALSE, quote=TRUE, sep=",", append = TRUE)
					} else {
						# Create/Overwrite stats file
						write.table(sp_lmstat,file = file.path(dir_path,stats_fname), col.names = TRUE, row.names = FALSE, quote=TRUE, sep=",", append = FALSE)
						# Update stat_file_created to prevent overwriting
						lm_file_created = TRUE
					}
				}
			}
		
		# Close onefile device
			if(plot & onefile){
				temp = dev.off()
			}
    
    # pick up lm file and sort by species
    lm_stats<-read.csv(file = file.path(dir_path,stats_fname))
    lm_stats<-lm_stats[with(lm_stats,order(NAME)),]
		write.table(lm_stats,file = file.path(dir_path,stats_fname), col.names = TRUE, row.names = FALSE, quote=TRUE, sep=",", append = FALSE)
		
		return(lm_stats)	
}
