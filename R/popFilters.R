#' Population data filters.
#' 
#' Returns a list of user-supplied parameters and parameter values for population
#' data filters.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param evalid 
#' @param invyrs 
#' @param intensity 
#' @param measCur 
#' @param measEndyr 
#' @param measEndyr.filter 
#' @param ACI 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for population
#' data filters.
#' @author Grayson W. White
#' @keywords filters
#' @examples
#' 
#' popFilters(ACI = TRUE)
#' 
#' @export popFilters

popFilters <- function(evalid = NULL, invyrs = NULL, intensity = NULL,
                       measCur = NULL, measEndyr = NULL, 
                       measEndyr.filter = NULL, ACI = FALSE, ...) {
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}