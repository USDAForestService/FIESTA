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
#' @param p2veg_subplot_spp DF/DT, R object, comma-delimited file(*.csv), or 
#' layer in dsn. Vegetation species-level data with one record for each species
#' (P2VEG_SUBPLOT_SPP).
#' @param vsubpstr DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn. Vegetation species-structure data with one record for each species
#' (P2VEG_SUBP_STRUCTURE).
#' @param p2veg_subp_structure DF/DT, R object, comma-delimited file(*.csv), 
#' or layer in dsn. Vegetation species-structure data with one record for each 
#' species (P2VEG_SUBP_STRUCTURE).
#' @param invsubp DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn. Invasive species data with one record for each species
#' (INVASIVE_SUBPLOT_SPP).
#' @param invasive_subplot_spp DF/DT, R object, comma-delimited file(*.csv), 
#' or layer in dsn. Invasive species data with one record for each species
#' (INVASIVE_SUBPLOT_SPP).
#' @param subplot DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn. Subplot-level data with one record for each species (SUBPLOT).
#' @param subp_cond DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn. Subplot condition-level data with one record for each species
#' (SUBP_COND).
#' @param dwm DF/DT, R object, comma-delimited file(*.csv), or layer 
#' in dsn. Calculated down woody material (COND_DWM_CALC).
#' @param cond_dwm_calc DF/DT, R object, comma-delimited file(*.csv), or layer 
#' in dsn. Calculated down woody material (COND_DWM_CALC).
#' @param sccm DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Subplot-level data (SUBP_COND_CHNG_MTRX).
#' @param subp_cond_chng_mtrx DF/DT, R object, comma-delimited file(*.csv), 
#' or layer in dsn. Subplot-level data (SUBP_COND_CHNG_MTRX).
#' @param grm DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level growth, removal, and mortality data (TREE_GRM_COMPONENT).
#' @param tree_grm_component DF/DT, R object, comma-delimited file(*.csv), 
#' or layer in dsn. Tree-level growth, removal, and mortality data (TREE_GRM_COMPONENT).
#' @param begin DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level growth, removal, and mortality data (TREE_GRM_BEGIN).
#' @param tree_grm_begin DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level growth, removal, and mortality data (TREE_GRM_BEGIN).
#' @param midpt DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level growth, removal, and mortality data (TREE_GRM_MIDPT).
#' @param tree_grm_midpt DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level growth, removal, and mortality data (TREE_GRM_MIDPT).
#' @param pltu DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Plot data unioned with remeasured plot data.
#' @param condu DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Cond data unioned with remeasured cond data.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords list
#' @examples
#' popTables(cond = FIESTA::WYcond, seed = FIESTA::WYseed)
#' @export popTables

popTables <- function(cond = "COND", 
                      plt = "PLOT", 
                      tree = "TREE", 
                      seed = "SEEDLING", 
                      vsubpspp = "P2VEG_SUBPLOT_SPP", 
					  p2veg_subplot_spp = "P2VEG_SUBPLOT_SPP",
                      vsubpstr = "P2VEG_SUBP_STRUCTURE", 
					  p2veg_subp_structure = "P2VEG_SUBP_STRUCTURE",
                      invsubp = "INVASIVE_SUBPLOT_SPP",
					  invasive_subplot_spp = "INVASIVE_SUBPLOT_SPP",
                      subplot = "SUBPLOT", 
                      subp_cond = "SUBP_COND", 
                      dwm = "COND_DWM_CALC",
					  cond_dwm_calc = "COND_DWM_CALC",
                      sccm = "SUBP_COND_CHNG_MTRX",
					  subp_cond_chng_mtrx = "SUBP_COND_CHNG_MTRX",
                      grm = "TREE_GRM_COMPONENT",
					  tree_grm_component = "TREE_GRM_COMPONENT",
                      begin = "TREE_GRM_BEGIN",
					  tree_grm_begin = "TREE_GRM_BEGIN",
                      midpt = "TREE_GRM_MIDPT",
					  tree_grm_midpt = "TREE_GRM_MIDPT",
					  pltu = "pltu",
					  condu = "condu",
                      ...) {
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

