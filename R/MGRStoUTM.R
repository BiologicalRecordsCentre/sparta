MGRStoUTM <-
function(gridref, zone = 30, zdl = "U", spos = TRUE){
	zdl_chr = c("C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X")
	mgrs_chr = c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X","Y","Z");
	for(i in seq(1, along.with=gridref)){
		# Get median lat from ZDL
			if(zdl == "X"){
				med_lat = 78 - 1
			} else {
				med_lat = which(zdl_chr == zdl) - 1
			}
			# Modify med_lat to get value for median
			med_lat = -76 + (8*med_lat)
		# Remove any spaces from gridref
			temp_gridref = gsub(" ","",toupper(gridref[i]))
			temp_gridref = gsub("-","",temp_gridref)			
		# Extract Chars from gridref
			l1 = which(mgrs_chr == substr(temp_gridref,1,1)) - 1
			l2 = which(mgrs_chr == substr(temp_gridref,2,2)) - 1
		# Extact east and north digits from gridref
			gr_nums = substr(temp_gridref,3,nchar(temp_gridref))
			east_num = substr(gr_nums,1,nchar(gr_nums)/2)
			north_num = substr(gr_nums,(nchar(gr_nums)/2)+1, nchar(gr_nums))
			d_len = nchar(east_num)
			if(nchar(north_num) != d_len){
				stop("Easting and Northing Digits must be the same length")
			}
		# Determine intial easting and northing
			E0 = 1 + l1%%8
			N0 = (20 + l2 - ifelse(zone%%2 != 0,0,5))%%20
		# Determine approximate median northing of ZDL (100km)
			appN = med_lat*100/90
		# Modify N0
			N0 = N0 + round((appN - N0)/20)*20
		# Modify E0 and N0 to get E and N
			E = E0 * 100000 + as.numeric(east_num) * 10^(5-d_len)
			N = N0 * 100000 + as.numeric(north_num) * 10^(5-d_len)
		# If spos is TRUE and ZDL <= N then grid in Southern Hemisphere and Northing to be corrected to be possitive (by ading 10 Million)
			if(spos & which(zdl_chr == zdl) <= 11){
				N = N + 10000000
			}
		# Setup return object
		if(i == 1){
			ret_obj = data.frame(Easting = E, Northing = N)
		} else {
			ret_obj[i,] = data.frame(Easting = E, Northing = N)
		}	
		return(ret_obj)
	}
}
