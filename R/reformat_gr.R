reformat_gr <-
function(gridref, prec_out = 10000, precision = NULL){
  # Setup output object to store reformated grid refs
  gr_out = rep(NA, length(gridref))
  
  # if precision not supplied then calculate from gridref
  if(is.null(precision)){
    gr_prec = det_gr_precision(gridref)
  } else {
    if(length(precision) == 1){
      gr_prec = rep(precision, length(gridref))
    } else if ( length(precision) == length(gridref) ) {
      gr_prec = precision
    } else {
      stop("Precision and Gridref vectors are of different lengths")
    }
  }
  
  # Will need to deal with tetrad/quadrant gridrefs/precision later!
  
  # Determine if required precision is possible from gridref
  
  if (prec_out %in% c(100000,10000,5000,2000,1000,100, 10, 1)) {
    
    # Extract components from gridref
    # Characters
    gr_chars = fmt_gridref(gridref, 2)
    # Digit pairs (
    gr_digits = fmt_gridref(gridref,3)
    
    # Check that grid ref digits are in pairs
    gr_dig_len = nchar(gr_digits)
    
    # Determine number of digits to give required precision
    dig_req = 5 - log10(prec_out)
    
    # Find gridrefs for which new precision can be calculated
    i_ref = which(gr_prec <= prec_out &  gr_dig_len %% 2 == 0)
    
    if(length(i_ref) > 0 ){
      # For gridrefs to be reformated then determine mid point and then split into east_digits and north_digits of required length for new precision
      dig_mid = gr_dig_len[i_ref]/ 2
      east_digit = substr(gr_digits[i_ref], 1, dig_req)
      north_digit = substr(gr_digits[i_ref], dig_mid+1, dig_mid + dig_req)
      
      # Combine new components to create grid ref of required precision and insert inot gr_out
      gr_out[i_ref] = paste(gr_chars[i_ref],east_digit, north_digit, sep="")
    }
  }
  
  # Return output string
  return(gr_out)
}
