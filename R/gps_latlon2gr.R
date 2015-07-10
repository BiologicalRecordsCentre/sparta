#' Covert latitude and longitude to other formats
#' 
#' Coverts latitude and longitude to a grid reference and/or easting and northing.
#' 
#' @param latitude a vector of numerics giving latitudes
#' @param longitude a vector of numerics giving longitudes
#' @param out_projection a string giving the desired output projection, either \code{"OSGB"}
#'        or \code{"OSNI"}. Defaults to \code{"OSGB"}.
#' @param return_type a string defining what information the funtion should return. If
#'        \code{"both"} (default), grid reference and easting/northing are returned. If 
#'        \code{"en"}, only easting and northing are returned. If \code{gr}, only grid
#'        reference is returned
#' @return A dataframe of results are returned
#' @keywords mapping latitude longitude grid-reference
#' @export
#' @examples
#' 
#' gps_latlon2gr(51.60233, -1.111254)

gps_latlon2gr <-
function(latitude, longitude, out_projection = "OSGB", return_type = "both"){

  # Determine number of coordinates
  n_coords = length(latitude)
  # Check lengths are the same if not stop
  if(n_coords != length(longitude)){
    stop("ERROR - Lenghts of latitude and longitude do not match")
  }
  
  
  # Convert to Cartesian
  org_cart = LatLong_Cartesian(latitude, longitude, "UTM30", datum_vars)
  
  # At some point should set up some ranges to split UK, Irish & Channel Islands grid refs (and do follow stages in loop for different projections)
  # If doing multiple projections will need to setup output variable before hand!
  
  # Perform projection/datum transformation (helmert)
  helm_tran = helmert_trans(x =org_cart$x, y = org_cart$y, z = org_cart$z, trans = paste("WGS84to", out_projection, sep=""))
  
  # Convert to UK lat lon
  out_latlon = Cartesian_LatLong(helm_tran$x, helm_tran$y, helm_tran$z, out_projection, datum_vars)
  
  # Convert to UK easting and northing
  out_en = LatLongtoOSGrids(out_latlon$LATITUDE, out_latlon$LONGITUDE, out_projection, datum_vars)
  
  # Convert to UK gridref
  out_gr = gr_num2let(out_en$EASTING, out_en$NORTHING, OSgrid = out_projection)
  
  # Determine output
  if(return_type == "both"){
    out_obj = data.frame(GRIDREF = out_gr, EASTING = out_en$EASTING, NORTHING = out_en$NORTHING, stringsAsFactors = FALSE)
  } else if(return_type == "en"){
    out_obj = data.frame(EASTING = out_en$EASTING, NORTHING = out_en$NORTHING, stringsAsFactors = FALSE)
  } else if(return_type == "gr"){
    out_obj = data.frame(GRIDREF = out_gr, stringsAsFactors = FALSE)
  } else {
    stop("ERROR - Unknown return type (valid entries are \"both\", \"en\", \"gr\")")
  }
  
  # Return output object
  return(out_obj)
}
