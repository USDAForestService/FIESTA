#' List of population tables.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' tables to be supplied to *pop functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param cond DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Condition-level data with one record for each condition, including or
#' excluding nonsampled conditions. Plot variables and strata/estimation unit
#' variable(s) may be included if plt and pltassgn=NULL. See details for
#' necessary variables to include.
#' @param plt DF/DT, Optional. R object, sf R object, comma-delimited
#' file(*.csv), layer or spatial layer in dsn, or shapefile(*.shp).  Plot-level
#' data with one record for each plot, including or excluding nonsampled
#' conditions. If nonsampled plots are included, PLOT_STATUS_CD variable must
#' be in table or a filter defined in plt.nonsamp.filter.
#' @param tree DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level data with one record for each tree. Tree data are aggregated to
#' condition-level. See details for necessary variables to include.
#' @param seed DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Seedling data with one record for each seedling count.
#' @param vsubpspp DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Vegetation species-level data with one record for each species
#' (P2VEG_SUBPLOT_SPP).
#' @param vsubpstr DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Vegetation species-structure data with one record for each species
#' (P2VEG_SUBP_STRUCTURE).
#' @param subplot DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Subplot-level data with one record for each species (SUBPLOT).
#' @param subp_cond DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Subplot condition-level data with one record for each species
#' (SUBP_COND).
#' @param lulc DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Land use/Land cover data with current and previous observations.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords list
#' @examples
#' popTables(cond = FIESTA::WYcond, seed = FIESTA::WYseed)
#' @export popTables

popTables <- function(cond=NULL, plt=NULL, tree=NULL,
                             seed=NULL, vsubpspp=NULL, vsubpstr=NULL, 
                             subplot=NULL, subp_cond=NULL, lulc=NULL, ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(popTables)))
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

