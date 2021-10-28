#' List of data table unique IDs.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' table unique IDs to be supplied to *pop functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param cuniqueid String. Unique identifier of plot in cond.
#' @param puniqueid String. Unique identifier of plot in plt.
#' @param pltassgnid String. 
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
#' data_uniqueid_list()
#' 
#' @export data_uniqueids_list

data_uniqueids_list <- function(cuniqueid="PLT_CN",  puniqueid="CN", 
                               pltassgnid="PLT_CN", tuniqueid="PLT_CN",
                               vuniqueid="PLT_CN", subpuniqueid="PLT_CN",
                               lulcuniqueid="PLT_CN", condid="CONDID", ...) {
  # set up list of parameters
  l <- as.list(match.call())
  l <- l[-1]
  
  # this evaluates objects in the user's global environment and saves them back
  # into the list in order to pass them correctly to other functions
  objs <- ls(envir = globalenv())
  if (length(l) > 0) {
    for (i in 1:length(l)) {
      if (class(l[[i]]) == "name") {
        if (l[i] %in% objs) {
          l[i] <- eval(l[i][[1]], envir = globalenv())
        }
      }
    }
  }
  
  # returns the list
  return(l)
}

