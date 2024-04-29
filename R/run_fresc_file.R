run_fresc_file <-
function(
	fres_data,
	output_dir,
	frescalo_path,
	fres_f_foc = "FocDist.txt", # File name to give the focal data for the frescalo analysis
	fres_f_wts = "Wts.txt", # file name for the frescalo weighting file (needs to already be in directory with frescalo exe file)
	fres_f_log = "Log.txt", # file name for log file created by frescalo (cannot exist in folder already or frescalo will crash)
	fres_f_stat = "Stats.txt", # file name for stats file created by frescalo (cannot exist in folder already or frescalo will crash)
	fres_f_freq = "Freq.txt", # file name for frequencies file created by frescalo (cannot exist in folder already or frescalo will crash)
	fres_f_trend = "Trend.txt", # file name for trend file created by frescalo (cannot exist in folder already or frescalo will crash)
	fres_f_filter = NULL, # file containing sites to be filtered (if required)
	fres_f_nobench = NULL, # file name containing list of species codes of species to exclude from benchmarking (if required needs to already be in directory with frescalo exe file)
	fres_phi_val = NULL, # Parameter value for phi val (NULL equals default of 0.74)
	fres_bench_val = NULL, # Paramter value for local benchmarking threshold (NULL equals default of 0.27)
	spp_names_file = NULL, # Path to csv file containing species names in 2 column format, 1) "SPECIES", which contains codes, and 2) "NAME", name belonging to code (if null will assume standard BRC concepts and look them up directly)
	Plot=TRUE
	
){

if (!detect_os_compat()) {
    stop("Apologies, Frescalo is currently only avaiable on mac and Windows operating systems.")
}

# BODY OF FUNCTION
	# Print status to screen 
		cat("\nSAVING DATA TO FRESCALO WORKING DIRECTORY\n",rep("*",20),"\n\n", sep="")
	
  # Ensure data is unique
		fres_data<-unique(fres_data)
    
  # Check that frescalo_path exists
		org_wd<-getwd()
    
    if(!file.exists(frescalo_path)){
			stop("ERROR: supplied frescalo path is not valid")
		}

	# Check output directory exists
		if(!file.exists(output_dir)){
			# If full directory given does not exist but only last folder is missing then create missing directory
			if(file.exists(dirname(output_dir))){
				dir.create(output_dir)
			} else {
				stop("ERROR: output directory path is not valid")
			}
		}
	
  # Check phi value is within permitted range
    if(!is.null(fres_phi_val)){
      if(fres_phi_val>0.95|fres_phi_val<0.5) stop("phi is outside permitted range of 0.50 to 0.95")
    }
    
  # Check alpha value is within permitted range
		if(!is.null(fres_bench_val)){
		  if(fres_bench_val>0.5|fres_bench_val<0.08) stop("alpha is outside permitted range of 0.08 to 0.50")
		}
  	
		# Within folder setup input, output and maps_results folders
			# Build paths for directories
			fres_sub_dir = file.path(output_dir, c("Input","Output","Maps_Results"))
			names(fres_sub_dir) = c("INPUT","OUTPUT","RESULTS")
			# Create all/missing directories (hiding output from function)
				invisible(sapply(fres_sub_dir[!file.exists(fres_sub_dir)], dir.create))
				
	# Save frescalo formatted data (GRIDREF, SPECIES_CODE, TIMEPERIOD) to space delimited file in frescalo working directory
		write.table(fres_data, file = file.path(fres_sub_dir["INPUT"], fres_f_foc), sep=" ", row.names = FALSE, col.names = FALSE, quote=FALSE)
		
	
	# Copy Frescalo file created above to folder where frescalo.exe is stored
		# Extract folder path for the folder in which the frescalo exe is stored
			exe_dir = dirname(frescalo_path)
		# Copy the frescalo file created above into the directory containing the exe
			invisible(file.copy(from = file.path(fres_sub_dir["INPUT"], "FocDist.txt"), to = file.path(exe_dir, fres_f_foc), overwrite = TRUE ))
		# Determine if output files from frescalo exist in exe dir 
			f_out_exists = file.exists(file.path(exe_dir, c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend)))
			# If so then delete
			if(any(f_out_exists)){
				invisible(file.remove(file.path(exe_dir, c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend))[f_out_exists]))
			}
		
	# Create frescalo parameter file (by line)
		# 1) Log fie, 2) data file, 3) weights file, 4) ?no bench file, 5) Site Filter, 6), Stats out file, 7) Freq out file, 8) Trend out file, 9) Phi value, 10) Benchmark value
		# Log file
		cat(fres_f_log,"\n", file=file.path(exe_dir, "params.txt"), append = FALSE)
		# Data File
		cat(fres_f_foc,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Weigths
		cat(fres_f_wts,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# No bench
		cat(fres_f_nobench,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Filter
		cat(fres_f_filter,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Stats
		cat(fres_f_stat,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Freq
		cat(fres_f_freq,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Trend
		cat(fres_f_trend,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Phi val
		cat(fres_phi_val,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		# Benchmark val
		cat(fres_bench_val,"\n", file=file.path(exe_dir, "params.txt"), append = TRUE)
		
		
	# Run Frescalo using parameter file
		# Print progress
			cat("\nRUNNING FRESCALO\n",rep("*",20),"\n\n", sep="")
		# Create batch file to change directory and then call frescalo
		if (grepl("linux", R.version$platform)){
		  setwd(dirname(frescalo_path))
		  system(paste('"',frescalo_path,'"',sep=""))
		  setwd(org_wd) 
		}else{
		  cat( paste("cd", normalizePath(dirname(frescalo_path))), "\n", file = file.path(exe_dir, "wincomm.cmd"), append = FALSE)
		  cat( basename(frescalo_path), "\n", file = file.path(exe_dir, "wincomm.cmd"), append = TRUE)
		  # Run batch file through shell
		  setwd(dirname(frescalo_path))
		  shell(shQuote(file.path(exe_dir, "wincomm.cmd")), intern = FALSE)
		  setwd(org_wd) 
		}	
    
		# Check log file from frescalo and determine if value from Phi was too low if so then run again with value of Phi + 1 (only if fres_phi_val is NULL)
		if(is.null(fres_phi_val)){
			# Setup inital values of loop variables to ensure entry into while loop (NOTE: only used to enter loop!)
				fres_warn = TRUE
				act_phi = 1.0
				tar_phi = 0.74
			# Setup up while loop to keep going through as long as frescalo gives warning and target is less or equal to than actual value of Phi
			while(tar_phi <= act_phi & fres_warn & tar_phi < 0.95){
				# Read log file using readLines
				log_out = readLines(file.path(exe_dir, fres_f_log))
				
				# Look for warning in log file
					fres_warn = any(grepl(" [*]{3}[ ]BEWARE[ ][*]{3} ",log_out))
				# Find row stating actual value of phi
					act_txt = log_out[grep("(98.5 percentile of input phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})",log_out)]
				# Find row stating target value of phi
					tar_txt = log_out[grep("(Target value of phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})", log_out)]
				# Read value of Phi
					act_phi = as.numeric(gsub("(98.5 percentile of input phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})","\\2", act_txt))
				# Read Target value of phi
					tar_phi = as.numeric(gsub("(Target value of phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})","\\2", tar_txt))
					
				if(tar_phi <= act_phi & fres_warn & tar_phi < 0.95){
					# Print notice to screen to say will need to rerun
						cat("NOTE: Targest value of phi may be too small, frescalo will be rerun with a larger target value of Phi\n\n")
					# Read in params file and alter 9 value (Phi value line)
						param_temp = readLines(file.path(exe_dir,"params.txt"))
					# Set target phi to act_phi + 0.01 to make sure rounding issues don't cause failure again
					  new_phi = act_phi + 0.01
            warning(paste('input value of phi',tar_phi,'is smaller than the 98.5 percentile of input phi',act_phi,'. Phi was changed to',new_phi,'and re-run'))
            
          # make sure new phi is no greater than 0.95
            if(new_phi>0.95){
              new_phi<-0.95
              warning('phi has been set to the maximum value of 0.95')
            }
            param_temp[9] = new_phi
					# Write lines to param file
						writeLines(param_temp, file.path(exe_dir,"params.txt"))
						
					# Determine if output files from frescalo exist in exe dir 
						f_out_exists = file.exists(file.path(exe_dir, c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend)))
						# If so then delete
						if(any(f_out_exists)){
							invisible(file.remove(file.path(exe_dir, c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend))[f_out_exists]))
						}
						
					# Rerun frescalo
						# Create batch file to change directory and then call frescalo
						if (grepl("linux", R.version$platform)){
						  setwd(dirname(frescalo_path))
						  system(paste('"',frescalo_path,'"',sep=""))
						  setwd(org_wd) 
						}else{
						  cat( paste("cd", normalizePath(dirname(frescalo_path))), "\n", file = file.path(exe_dir, "wincomm.cmd"), append = FALSE)
						  cat( basename(frescalo_path), "\n", file = file.path(exe_dir, "wincomm.cmd"), append = TRUE)
						  # Run batch file through shell
						  setwd(dirname(frescalo_path))
						  shell(shQuote(file.path(exe_dir, "wincomm.cmd")), intern = FALSE)
						  setwd(org_wd) 
						}
				  }
			  }
		  }else{
		    log_out = readLines(file.path(exe_dir, fres_f_log))
		    
        # Look for warning in log file
		    fres_warn = any(grepl(" [*]{3}[ ]BEWARE[ ][*]{3} ",log_out))
		    # Find row stating actual value of phi
		    act_txt = log_out[grep("(98.5 percentile of input phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})",log_out)]
		    # Find row stating target value of phi
		    tar_txt = log_out[grep("(Target value of phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})", log_out)]
		    # Read value of Phi
		    act_phi = as.numeric(gsub("(98.5 percentile of input phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})","\\2", act_txt))
		    # Read Target value of phi
		    tar_phi = as.numeric(gsub("(Target value of phi[ ]*)([[:digit:]]{1}[.][[:digit:]]{2})","\\2", tar_txt))
		    
        if(fres_warn & act_phi > tar_phi){
          warning('Your value of phi (',tar_phi,') is smaller than the 98.5 percentile of input phi (',act_phi,'). It is reccommended your phi be similar to this value. For more information see Hill (2011) reference in frescalo help file',sep='')
        } else if (fres_warn){
          warning('Frescalo has a warning message. Please check the log file [$log]')
        }
        
		  }
			
	
	# Copy output from Frescalo to results folder
		invisible(file.copy(from = file.path(exe_dir, c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend)), to = file.path(fres_sub_dir["OUTPUT"], c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend)), overwrite = TRUE ))
	
  # Read in output to return
		read_in <- read_frescalo(stat_path = file.path(fres_sub_dir["OUTPUT"], fres_f_stat), trend_path = file.path(fres_sub_dir["OUTPUT"], fres_f_trend), freq_path = file.path(fres_sub_dir["OUTPUT"], fres_f_freq))
    trend <- read_in$trend
    freq <- read_in$freq
    stats <- read_in$stats
    
	# Run alloneplots to create maps etc
	if(Plot){
		# Print progress to screen
			cat("\nREAD FRESCALO OUTPUT\n",rep("*",20),"\n\n", sep="")
					
		# Extract species names
			spp_names = read.table(spp_names_file, sep=",", header=TRUE, stringsAsFactors = FALSE)
			
		# Make standard Frescalo plots	
			# Print progress to screen
				cat("\nPLOT MAPS AND CALCULATE RESULTS\n",rep("*",20),"\n\n", sep="")
			# Do plots
			lm_stats<-all_oneplots(stat = stats, trend = trend, freq = freq, spp_names = spp_names, onefile_name = "Standard Frescalo Plots", dir_path = fres_sub_dir["RESULTS"], plot = Plot)
	}	else {
  	  # Extract species names
  	  spp_names = read.table(spp_names_file, sep=",", header=TRUE, stringsAsFactors = FALSE)
  	  
      #Run tfactor stats without plotting
	    lm_stats<-all_oneplots(stat = stats, trend = trend, freq = freq, spp_names = spp_names, dir_path = fres_sub_dir["RESULTS"], plot = Plot)
  }
	
	# Make mapping file for NBN validation stuff (essentially freq file but quicker to load?)
	#	write.table(freq, file = file.path(fres_sub_dir["RESULTS"],"Freq_quickload.txt"), sep="\t", col.names = TRUE, row.names = FALSE, quote=FALSE)
		
	# Return results file paths via invisble
		fpaths_out = file.path(fres_sub_dir["OUTPUT"], c(fres_f_log, fres_f_stat, fres_f_freq, fres_f_trend,"Freq_quickload.txt"))
		    
  # format files for returning
    if(!exists('spp_names')) spp_names = read.table(spp_names_file, sep=",", header=TRUE, stringsAsFactors = FALSE)
    
    trend <- as.data.frame(trend)
		names(trend) <- gsub('_', '', names(trend))
		for(i in unique(trend$Species)){
		  trend$Species[trend$Species==i]<-spp_names$NAME[spp_names$SPECIES==i]
		}
				
    stats <- as.data.frame(stats)
    freq <- as.data.frame(freq)
		names(freq) <- gsub('_', '', names(freq))

  for(i in unique(freq$Species)){
		  freq$Species[freq$Species==i]<-spp_names$NAME[spp_names$SPECIES==i]
		}
    
    write.table(trend,file=paste(output_dir,'/Output/Trend.csv',sep=''),sep=',',row.names=FALSE)
		write.table(stats,file=paste(output_dir,'/Output/Stats.csv',sep=''),sep=',',row.names=FALSE)
		write.table(freq,file=paste(output_dir,'/Output/Freq.csv',sep=''),sep=',',row.names=FALSE)
		
  # Create list to return
		if(!exists('log_out')) log_out <- readLines(file.path(exe_dir, fres_f_log))
    frescalo<-list(paths=invisible(fpaths_out),trend=trend,stat=stats,freq=freq,log=log_out)
    if(exists('lm_stats')) frescalo[["lm_stats"]] = lm_stats
    
  # Remove unwanted objects  
		unwanted<-c("stats", "freq", "trend","UK","GB_LC_Wts","Wts")
		rm(list=unwanted[unwanted %in% ls(pos = ".GlobalEnv")],pos = ".GlobalEnv")
    		
  # Return output
    class(frescalo) <- 'frescalo'
    return(frescalo)
}
