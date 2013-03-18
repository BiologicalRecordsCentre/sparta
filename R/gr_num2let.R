gr_num2let <-
function(easting,northing, OSgrid = "OSGB", keep_precision = TRUE){
	# Check easting and northing are same length
	if(length(easting)!=length(northing)){
		stop("Length of Easting & Northing Vectors do not match")
	}
	
	# Check OSgrid is valid
	inv_grid = which(!OSgrid %in% c("OSGB","OSNI","UTM30"))
	if(length(inv_grid) > 0){
		stop(paste("Invalid Grid type (",OSgrid[inv_grid],")", sep=""))
	}
	
	#If OSgrid is length 1 then expand to be same length as easting/northing
	if(length(OSgrid) == 1){
		OSgrid = rep(OSgrid, length(easting))
	}

	# Setup variable to store gridrefs
		ret_obj = rep(NA, length(easting))

	# Convert easting/northing to strings (to avoid sci notation problems)
		easting = format(easting, scientific = FALSE, trim = TRUE)
		northing = format(northing, scientific = FALSE, trim = TRUE)
		
		
	# Extract component of easting/northing that will remain as digits
		dig_east = substr(easting,(nchar(easting)-5)+1,nchar(easting))
		dig_north = substr(northing,(nchar(northing)-5)+1,nchar(northing))
	
	# Determine if precision of easting/northing to be kept or if can trim gridref to only positve value
		#(i.e. easting/northing 507000, 281000 could be presented as TL0700081000 (keeping the precision) or presented as TL0781 (removing matching zero pairs)
	if(!keep_precision){
		# Strip excess digits from ends of dig_east/dig_north (So gridrefs donn't come out with loads of zeros)
			# Can only trim digit from east/north if both east and northing end in a 0
			pair_trim = pmin( nchar(dig_east) - nchar(gsub("[0]*$","", dig_east)), nchar(dig_north) - nchar(gsub("[0]*$","", dig_north)) )
			dig_east[pair_trim > 0] = substr(dig_east[pair_trim > 0],1,nchar(dig_east[pair_trim > 0])-pair_trim[pair_trim > 0])
			dig_north[pair_trim > 0] = substr(dig_north[pair_trim > 0], 1, nchar(dig_north[pair_trim > 0])-pair_trim[pair_trim > 0])
	}
		
	# Determine gridref of British Easting Northings
	cty_inds = which(OSgrid == "OSGB")
	if(length(cty_inds) > 0){
		
		# Determine easting and northing from orgin in 500km squares
		eX500k = as.numeric(easting[cty_inds])/500000
		nX500k = as.numeric(northing[cty_inds])/500000
		
		# Determine first letter based on 500km easting/northing (-3 at end is to deal with the 500km grid starting at s
		L500k = (21 - (5*floor(nX500k)) + floor(eX500k))-3
		
		# Determine easting and northing within given 500km square (round to 2 decimal places to ensure floor in next step works (issues with HP60 gridref)
		eX100k = round((eX500k - floor(eX500k)) * 5,5)
		nX100k = round((nX500k - floor(nX500k)) * 5,5)
		
		# Determine second letter based on 100km easting/northing
		L100k = 21 - (5*floor(nX100k))  + floor(eX100k)
		
		# Fill ret_obj with grid references using letters and digits (not exlude 9th letter from LETTERS as I not used)
		ret_obj[cty_inds] = paste(LETTERS[-9][L500k], LETTERS[-9][L100k], dig_east[cty_inds], dig_north[cty_inds], sep="")
		
	}
	
	# Determine gridref of Irish Easting/Northings
	cty_inds = which(OSgrid == "OSNI")
	if(length(cty_inds) > 0){
		# Determine easting and northing from orgin in 100km squares (even though Ireland doesn't use 500km squares
		eX100k = round(as.numeric(easting[cty_inds])/100000,2)
		nX100k = round(as.numeric(northing[cty_inds])/100000,2)
		
		# Determine second letter based on 100km easting/northing
		L100k = 21 - (5*floor(nX100k))  + floor(eX100k)
		ret_obj[cty_inds] = paste(LETTERS[-9][L100k], dig_east[cty_inds], dig_north[cty_inds], sep="")
	}
	
	# Determine gridref of Channel Islands Easting/Northings
	cty_inds = which(OSgrid == "UTM30")
	if(length(cty_inds) > 0){
		chr_east = substr(easting,1,(nchar(easting)-5))
		chr_north = substr(northing,1,(nchar(northing)-5))
		# First square
		part_inds = which(OSgrid == "UTM30" & chr_north == 55)
		if(length(part_inds) > 0){
			ret_obj[part_inds] = paste("WA",dig_east[part_inds],dig_north[part_inds], sep="")
		}
		# Second square
		part_inds = which(OSgrid == "UTM30" & chr_north == 54)
		if(length(part_inds) > 0){
			ret_obj[part_inds] = paste("WV",dig_east[part_inds],dig_north[part_inds], sep="")
		}
	}
	
	return(ret_obj)
}
