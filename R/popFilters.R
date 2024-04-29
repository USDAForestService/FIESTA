#' Population data filters.
#' 
#' Returns a list of user-supplied parameters and parameter values for population
#' data filters.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param evalid Numeric. FIA Evaluation identifier for subsetting plots for
#' population.
#' @param states String or numeric vector. Name (e.g., 'Arizona','New Mexico')
#' or code (e.g., 4, 35) of state(s) for evalid. If all states in one or more
#' FIA Research Station is desired, set states=NULL and use RS argument to
#' define RS.
#' @param evalCur Logical. If TRUE, the most current FIA Evaluation is extracted
#' for state(s).
#' @param evalEndyr Number. The end year of the FIA Evaluation of interest.
#' Selects only sampled plots and conditions for the evaluation period. If
#' more than one state, create a named list object with evalEndyr labeled for
#' each state (e.g., list(Utah=2014, Colorado=2013).
#' @param measCur Logical. If TRUE, the most current sampled plots available
#' for state(s).
#' @param measEndyr Number. The most current sampled plots measured before or
#' during end given..
#' @param invyrs Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)).
#' @param measyrs Integer vector. Measurement year(s) (e.g., c(2000, 2001, 2002)).
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

popFilters <- function(evalid = NULL, 
                       states = NULL,
                       evalCur = FALSE,
                       evalEndyr = NULL,
                       measCur = FALSE,
                       measEndyr = NULL,
                       invyrs = NULL,
                       measyrs = NULL,
                       intensity = NULL,
                       ACI = FALSE,
                       AOIonly = FALSE,
                       ...) {
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