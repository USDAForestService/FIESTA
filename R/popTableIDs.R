#' List of population table unique IDs.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' table unique IDs to be supplied to *pop functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param cuniqueid String. Unique identifier of plot in cond.
#' @param puniqueid String. Unique identifier of plot in plt.
#' @param tuniqueid String. Unique identifier of plot in tree and seed.
#' @param vuniqueid String.
#' @param subpuniqueid String.
#' @param lulcuniqueid String. 
#' @param condid String.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords list
#' @examples
#' 
#' popTableIDs()
#' 
#' @export popTableIDs

popTableIDs <- function(cuniqueid="PLT_CN",  puniqueid="CN", 
                              tuniqueid="PLT_CN",
                               vuniqueid="PLT_CN", subpuniqueid="PLT_CN",
                               lulcuniqueid="PLT_CN", condid="CONDID", ...) {
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}

