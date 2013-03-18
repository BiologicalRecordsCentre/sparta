get_spp_names <-
function(channel,brc_code, add_cols = NULL, output_list = FALSE, old_code = FALSE, block = 500, silent=FALSE){
	# Set up object to store names
		spp_list = NULL
	# Set-up object to store position of current batch in species code vector
		i_spp = 1
		end_spp = 0
	# Determine number of codes passed
		no_codes = length(brc_code)
		
	# Setup while loop to look up species names in BRC database in batches
	while (i_spp < no_codes){
		end_spp = i_spp + block
		if(end_spp > no_codes){
			end_spp = no_codes
		}
		if(silent == FALSE){
			cat("Getting names for species",i_spp,"to",end_spp,"\n")
		}
		if(old_code){
			cur_batch = sqlQuery(channel, 
				paste(
					"select old_brc_code, name",ifelse(is.null(add_cols),"",paste(",",paste(add_cols, collapse=",")))," from brc.taxa_taxon_register where valid in ('V','P') and old_brc_code in ("
					, paste(shQuote(brc_code[i_spp:end_spp], type="sh"), collapse=","), ")", sep=""
				)
				, stringsAsFactors = FALSE
				)
		} else {
			cur_batch = sqlQuery(channel, 
				paste(
					"select concept, name",ifelse(is.null(add_cols),"",paste(",",paste(add_cols, collapse=",")))," from brc.taxa_taxon_register where valid in ('V','P') and concept in ("
					, paste(shQuote(brc_code[i_spp:end_spp], type="sh"), collapse=","), ")", sep=""
				)
				, stringsAsFactors = FALSE
				)
		}
		# Add current batch of names to species list
		spp_list = rbind(spp_list,cur_batch)
		i_spp = end_spp + 1
	}
	if(output_list | !is.null(add_cols)){
		names_out = spp_list
	} else {
		# Setup output object
			names_out = NULL
		# Reorder names to match brc_code
		for(i in 1:no_codes){
			names_out[i] = spp_list$NAME[spp_list[,1] == brc_code[i]]
		}
	}
	return(names_out)
}
