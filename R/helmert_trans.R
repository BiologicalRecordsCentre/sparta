helmert_trans <-
  function(x, y, z, trans = "OSNItoOSGB", trans_params = NULL, full_output = FALSE){
    # If trans_params is null then check where helmert_trans_vars data.frame has been loaded from package
    if(is.null(trans_params)){
      
      # set trans_params = helmert_trans_vars
      trans_params = helmert_trans_vars
    }
    
    # Determine length of input vars x,y,z and check all 3 variables are same length
    len_x = length(x)
    if(length(y) != len_x | length(z) != len_x){
      stop("ERROR: input variables 'x', 'y' and 'z' are of different lengths")
    }
    
    # Get list of trans to be performed from trans
    # If length of trans input not 1 or same length as input vars then stop
    if(!length(trans) %in% c(1,len_x)){
      stop("ERROR: Length of 'trans' does not match length of input 'x','y' & 'z' values")
    }
    trans_list = unique(trans)
    # Check all Datum present in data are in datum_params data frame
    miss_trans = which(!trans_list %in% trans_params$TRANS)
    if(length(miss_trans) > 0){
      stop(paste("ERROR: Transformation requested for which parameters have not been given (",paste(sQuote(trans_list[miss_trans]), collapse=","), ")", sep=""))
    }
    
    # Setup object to hold output data (x,y,z)
    if(full_output){
      ret_obj = data.frame(org_x = x, org_y = y, org_z = z, TRANS = trans, x = rep(NA, len_x), y = rep(NA, len_x), z = rep(NA, len_x), stringsAsFactors = FALSE)
    } else {
      ret_obj = data.frame(x = rep(NA, len_x), y = rep(NA, len_x), z = rep(NA, len_x), stringsAsFactors = FALSE)
    }
    
    # Loop through transformations and extract params and then perform helmert transformation
    for(i_trans in 1:length(trans_list)){
      # Extract indices of x,y,z values corresponding to current transformation
      if(length(trans) == 1){
        dat_inds = 1:len_x
      } else {
        dat_inds = which(trans == trans_list[i_trans])
      }
      # Select relevant helmert transformation parameters
      tp = trans_params[trans_params$TRANS == trans_list[i_trans],]
      # Convert trans paras for rotation from degress to radians
      tp[,c("rx","ry","rz")] = (tp[,c("rx","ry","rz")]/3600) * (pi/180)
      # Normalise s to ppm
      tp[,"s"] = tp$s*1e-6
      # Apply transformation
      x_out = x[dat_inds] + (x[dat_inds]*tp$s) - (y[dat_inds]*tp$rz) + (z[dat_inds]*tp$ry) + tp$tx
      y_out = (x[dat_inds]*tp$rz) + y[dat_inds] + (y[dat_inds]*tp$s) - (z[dat_inds]*tp$rx) + tp$ty
      z_out = (-1*x[dat_inds]*tp$ry) + (y[dat_inds]*tp$rx) + z[dat_inds] + (z[dat_inds]*tp$s) + tp$tz
      
      # Insert transformed cartesian coordincates into output variable
      ret_obj[dat_inds,c("x","y","z")] = data.frame(x = x_out, y = y_out, z = z_out)
    }
    return(ret_obj)
  }
