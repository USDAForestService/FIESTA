#' Unit options.
#' 
#' Returns a list of user-supplied parameters and parameter values for unit.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param dunitvar2 
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

unit_options <- function(dunitvar2 = NULL, areavar = "ACRES",
                         areaunits = "acres", minplotnum.unit = 10, 
                         unit.action = "keep", npixelvar = "npixels",  ...) {
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}

