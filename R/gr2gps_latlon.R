#' Covert grid reference to latitude and longitude
#' 
#' @param gridref a vector of strings giving grid references to be changed 
#' @param precision gives the precision of the grid references. Must be of
#'        the same length as gridref. precision is given in meters as a numeric
#'        i.e. 10km square = 10000, 1km square = 1000. If NULL then the function
#'        tries to work out the precision itself.
#' @param projection specifies the projection of the input and can either be \code{"OSGB"} 
#'        or \code{"OSNI"}. Defaults to \code{"OSGB"}
#' @param centre If \code{TRUE} the coordinates for the centre of the cell are given
#'        else if \code{FALSE} the coordinates for the bottom left corner are given. 
#' @return A dataframe of results are returned
#' @keywords mapping latitude longitude grid-reference
#' @export 
#' @examples
#' 
#' gr2gps_latlon('SU616896')

gr2gps_latlon <-
function(gridref, precision = NULL, projection = 'OSGB', centre = TRUE){
  
  # Setup up variable to hold final output
  out_latlon = data.frame(LATITUDE = rep(NA, length(gridref)), LONGITUDE = rep(NA, length(gridref)))
  
  # Get rid of dodgy gridrefs
  gridref = fmt_gridref(gridref)
  
  # determine precision if not supplied
  if(is.null(precision)){
    precision = det_gr_precision(gridref)
  } else if (length(precision) != length(gridref)){
    stop("Length of precision does not match length of gridref")
  }
  
  # Determine projection if not supplied
  if(is.null(projection)){
    projection = gr_det_country(gridref)
  } else if( length(projection) != length(gridref) ){
    stop("Length of projection does not match length of gridref")
  }
  
  # Determine rows are complete
  i_comp = which(complete.cases(cbind(gridref, precision, projection)))
  
  # Determine eastings & northings
  org_en = gr_let2num(gridref[i_comp], centre = centre)
  
  # Determine lat lon (original projection)
  out_latlon[i_comp,] = OSGridstoLatLong(org_en$EASTING, org_en$NORTHING, projection[i_comp], datum_vars)
  
  # Determine indices of gridrefs that are not UTM30 (which will need transformed)
  i_trans = which(!projection %in% c("UTM30","WGS84") & complete.cases(cbind(gridref, precision, projection)))
  
  # Determine Cartesian (Original projection)
  org_cart = LatLong_Cartesian(out_latlon$LATITUDE[i_trans], out_latlon$LONGITUDE[i_trans], projection[i_trans], datum_vars)
  
  # Apply Helmert transformation to convert orginal projections to WGS84 (Cartesian in WGS84)
  helm_tran = helmert_trans(x =org_cart$x, y = org_cart$y, z = org_cart$z, trans = paste(projection[i_trans],"toWGS84", sep=""))
  
  # Convert Cartesian coordinates to Latitude/Longitude (Lat Lon in WGS84)
  out_latlon[i_trans,] = Cartesian_LatLong(helm_tran$x, helm_tran$y, helm_tran$z, "UTM30", datum_vars)
  
  # Return output
  return(out_latlon)
}
