#' Unit options.
#' 
#' Returns a list of user-supplied parameters and parameter values for unit.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param unitvar2 String. Name of a second level estimation unit variable in
#' unitarea and cond or pltassgn with assignment for each plot (e.g.,
#' 'STATECD').
#' @param areaunits String. Units of areavar in unitarea ('acres', 'hectares').
#' @param minplotnum.unit Integer. Minimum number of plots for estimation unit.
#' @param unit.action String. What to do if number of plots in an estimation
#' unit is less than minplotnum.unit ('keep', 'remove' 'combine'). If
#' unit.action='combine', combines estimation unit to the following estimation
#' unit, ordered in unitzonal or stratalut.
#' @param npixelvar String. Name of variable in unitlut defining number of
#' pixels by estimation unit.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' unit_options()
#' 
#' @export unit_options

unit_options <- function(unitvar2 = NULL, areaunits = "acres", 
                         minplotnum.unit = 10, unit.action = "keep", 
                         npixelvar = "npixels",  ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::unit_options)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}

