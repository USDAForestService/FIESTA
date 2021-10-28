#' Strata options.
#' 
#' Returns a list of user-supplied parameters and parameter values for strata.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param stratalut DF/DT. If strata=TRUE, look-up table with pixel counts or
#' area by strata or proportion or area ('strwt') by strata (and estimation
#' unit).  If 'strwt' is not included, set getwt=TRUE and getwtvar as the name
#' of variable to calculate weights from (e.g., pixel counts).
#' @param strvar String. If strata=TRUE, name of the strata variable in
#' stratalut and cond or pltassgn data frame with stratum assignment for each
#' plot (Default = 'STRATUMCD').
#' @param getwt Logical. If TRUE, calculates strata weights from stratatlut
#' getwtvar.  If FALSE, strwtvar variable must be in stratalut.
#' @param getwtvar String. If getwt=TRUE, name of variable in stratalut to
#' calculate weights (Default = 'P1POINTCNT').
#' @param strwtvar String. If getwt=FALSE, name of variable in stratalut with
#' calculated weights (Default = 'strwt').
#' @param stratcombine Logical. If TRUE, and strata=TRUE, automatically combines
#' strata categories if less than minplotnum.strat plots in any one stratum. 
#' See notes for more info.
#' @param minplotnum.strat Integer. Minimum number of plots for a stratum
#' within an estimation unit.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' strata_options(getwt = FALSE)
#' 
#' @export strata_options

strata_options <- function(stratalut=NULL, strvar="STRATUMCD", getwt=TRUE,
                           getwtvar="P1POINTCNT", strwtvar="strwt", # are these right?
                           stratcombine=TRUE, minplotnum.strat=2, ...) {
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

