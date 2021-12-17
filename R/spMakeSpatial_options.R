#' Make SpatialPoints options
#' 
#' Returns a list of user-supplied parameters and parameter values for making
#' SpatialPoints.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param xvar String. Name of variable in xyplt defining x coordinate.
#' @param yvar String. Name of variable in xyplt defining y coordinate.
#' @param xy.crs PROJ.4 String or CRS object or Integer EPSG code defining
#' Coordinate Reference System. (e.g., EPSG:4269-Geodetic coordinate system for
#' North America, NAD83).
#' @param prj String. Projection, or coordinate system of the X/Y coordinates
#' ("longlat", "utm", "aea"). If other, include PROJ.4 string in prj4str.
#' @param datum String. Datum of projection ("WGS84", "NAD83", "NAD27").
#' @param zone Integer. If prj="utm", the UTM zone.
#' @param zoneS Logical. If prj="utm", if the UTM zone is in the Southern
#' hemisphere.
#' @param aea.param String. If prj="aea", the associated lat/lon parameters
#' (USGS: " +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0").  If
#' other, include PROJ.4 string in prj4str.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' spMakeSpatial_options()
#' 
#' @export spMakeSpatial_options
spMakeSpatial_options <- function(xvar=NULL, yvar=NULL, xy.crs=4269,
                                  prj=NULL, datum=NULL, zone=NULL,
                                  zoneS=FALSE, aea.param="USGS", ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(spMakeSpatial_options)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}
