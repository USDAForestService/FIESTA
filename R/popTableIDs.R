#' List of population table unique IDs.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' table unique IDs to be supplied to *pop functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param cond String. Unique identifier of plot in cond.
#' @param plt String. Unique identifier of plot in plt.
#' @param tree String. Unique identifier of plot in tree and seed.
#' @param seed String.
#' @param vsubpspp String.
#' @param vsubpstr String.
#' @param subplot String.
#' @param subp_cond String. 
#' @param lulc String. 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords list
#' @examples
#' 
#' popTableIDs()
#' 
#' @export popTableIDs

popTableIDs <- function(cond="PLT_CN", plt="CN", tree="PLT_CN", seed="PLT_CN",
                        vsubpspp="PLT_CN", vsubpstr="PLT_CN", subplot="PLT_CN",
                        subp_cond="PLT_CN", lulc="PLT_CN",...) {
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}

