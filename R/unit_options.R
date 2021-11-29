#' Unit options.
#' 
#' Returns a list of user-supplied parameters and parameter values for unit.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param unitvar2 
#' @param areavar 
#' @param areaunits
#' @param minplotnum.unit 
#' @param unit.action 
#' @param npixelvar 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' unit_options()
#' 
#' @export unit_options

unit_options <- function(unitvar2 = NULL, areavar = "ACRES",
                         areaunits = "acres", minplotnum.unit = 10, 
                         unit.action = "keep", npixelvar = "npixels",  ...) {
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

