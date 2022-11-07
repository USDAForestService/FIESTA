#' List of population tables.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' tables to be supplied to *DB functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param plot_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Plot-level data. 
#' @param cond_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Condition-level data. 
#' @param seed_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Seedling data. 
#' @param plotgeom_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Plot-level GIS extracted data. 
#' @param vsubpspp_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Understory vegetation species data. 
#' @param vsubpstr_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Understory vegetation structure data. 
#' @param invsubp_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Understory vegetation invasives data. 
#' @param subplot. R object, comma-delimited file(*.csv), or name of
#' layer in database. Subplot-level data. 
#' @param subpc_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Subplot condition-level data. 
#' @param dwm_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Down wood material data. 
#' @param sccm_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Subplot-level change matrix data. 
#' @param grm_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Tree growth, removal, mortality data. 
#' @param survey_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Population survey data. 
#' @param ppsa_layer. R object, comma-delimited file(*.csv), or name of
#' layer in database. Population plot stratum assignment data. 
#' @param other_layers. R object, comma-delimited file(*.csv), or name of
#' layer in database. Other layers to extract from database. 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Tracey S. Frescino
#' @keywords list
#' @examples
#' dbTables(sccm_layer)
#' @export dbTables

dbTables <- function(plot_layer = "PLOT",
                     cond_layer = "COND",
                     tree_layer = "TREE",
                     seed_layer = "SEEDLING",
                     plotgeom_layer = "PLOTGEOM",
                     vsubpspp_layer = "P2VEG_SUBPLOT_SPP",
                     vsubpstr_layer = "P2VEG_SUBP_STRUCTURE",
                     invsubp_layer = "INVASIVE_SUBPLOT_SPP",
                     subplot_layer = "SUBPLOT",
                     subpc_layer = "SUBP_COND",
                     dwm_layer = "COND_DWM_CALC",
                     sccm_layer = "SUBP_COND_CHNG_MTRX",
                     grm_layer = "TREE_GRM_COMPONENT",
                     survey_layer = "SURVEY",
                     ppsa_layer = "POP_PLOT_STRATUM_ASSGN",
                     ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(dbTables)))
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

