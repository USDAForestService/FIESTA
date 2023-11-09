#' Population data filters.
#' 
#' Returns a list of user-supplied parameters and parameter values for population
#' data filters.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param evalid Numeric. FIA Evaluation identifier for subsetting plots for
#' population.
#' @param invyrs Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)).
#' @param intensity Integer code. Code(s) indicating intensity to use for
#' population.
#' @param ACI Logical. If TRUE, including All Condition Inventory (ACI) plots.
#' @param AOIonly Logical. If TRUE, and there is an AOI (1/0) attribute in the
#' population data, only AOI=1 are used for estimation.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for population
#' data filters.
#' @author Grayson W. White
#' @keywords filters
#' @examples
#' popFilters(ACI = TRUE)
#' @export popFilters

popFilters <- function(evalid = NULL, invyrs = NULL, intensity = NULL,
                       ACI = FALSE, AOIonly = FALSE, ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(popFilters)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # removes input parameters to create l correctly
  rm(input.params, formallst)
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}