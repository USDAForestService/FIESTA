#' List of population tables.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' tables to be supplied to *DB functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param plot_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Plot-level data (PLOT). 
#' @param cond_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Condition-level data (COND). 
#' @param seed_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Seedling data (SEEDLING). 
#' @param tree_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Tree-level data (TREE). 
#' @param plotgeom_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Plot-level GIS extracted data (PLOTGEOM). 
#' @param vsubpspp_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Understory vegetation species data 
#' (P2VEG_SUBPLOT_SPP). 
#' @param vsubpstr_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Understory vegetation structure data 
#' (P2VEG_SUBP_STRUCTURE). 
#' @param invsubp_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Understory vegetation invasives data 
#' (INVASIVE_SUBPLOT_SPP). 
#' @param subplot_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Subplot-level data (SUBPLOT). 
#' @param subpcond_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Subplot condition-level data (SUBP_COND). 
#' @param dwm_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Down wood material data (COND_DWM_CALC)
#' @param sccm_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Subplot-level change matrix data
#' (SUBP_COND_CHNG_MTRX). 
#' @param grm_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Tree growth, removal, mortality data 
#' (TREE_GRM_COMPONENT). 
#' @param grmb_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Tree growth, removal, mortality begin data 
#' (TREE_GRM_BEGIN). 
#' @param grmm_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Tree growth, removal, mortality midpoint data 
#' (TREE_GRM_MIDPT). 
#' @param survey_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population survey (SURVEY) data. 
#' @param popeval_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population evaluation (POP_EVAL) data. 
#' @param popevalgrp_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population evaluation group data (POP_EVAL_GRP). 
#' @param popevaltyp_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population evaluation type data (POP_EVAL_TYP). 
#' @param popstratum_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population stratum data (POP_STRATUM). 
#' @param popestnunit_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population estimation unit data (POP_ESTN_UNIT).
#' @param ppsa_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Population plot stratum assignment data (
#' (POP_PLOT_STRATUM_ASSGN). 
#' @param refspp_layer R object, comma-delimited file(*.csv), or name of
#' layer in database. Reference table for species (REF_SPECIES). 
#' @param other_layers R object, comma-delimited file(*.csv), or name of
#' layer in database. Other layers to extract from database. 
#' @param other_layers String. Other layer(s) in database to clip and/or
#' extract from database (Note: must include PLT_CN variable as unique
#' identifier).
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Tracey S. Frescino
#' @keywords list
#' @examples
#' dbTables(plot_layer = FIESTA::WYplt)
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
                     subpcond_layer = "SUBP_COND",
                     dwm_layer = "COND_DWM_CALC",
                     sccm_layer = "SUBP_COND_CHNG_MTRX",
                     grm_layer = "TREE_GRM_COMPONENT",
					 grmb_layer = "TREE_GRM_BEGIN",
					 grmm_layer = "TREE_GRM_MIDPT",
                     survey_layer = "SURVEY",
                     popeval_layer = "POP_EVAL",
                     popevalgrp_layer = "POP_EVAL_GRP",
                     popevaltyp_layer = "POP_EVAL_TYP",
                     popstratum_layer = "POP_STRATUM",
                     popestnunit_layer = "POP_ESTN_UNIT",
                     ppsa_layer = "POP_PLOT_STRATUM_ASSGN",
                     refspp_layer = "REF_SPECIES",
                     other_layers = NULL,
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


addftypgrp <- function(x) {
  ## DESCRIPTION: appends fortypgrpcd and fortypgrpnm to table
  ftypnm <- findnm("FORTYPCD", names(x), returnNULL=TRUE)
  if (is.null(ftypnm)) {
    stop("FORTYPCD not in x")
  }
  ftypgrpnm <- findnm("FORTYPGRPCD", names(x), returnNULL=TRUE)
  if (!is.null(ftypgrpnm)) {
    stop("FORTYPGRPCD already in x")
  }
  ref_fortypcd <- ref_codes[ref_codes$VARIABLE == "FORTYPCD", c("VALUE", "GROUPCD", "GROUPNM")]
  names(ref_fortypcd) <- c("FORTYPCD", "FORTYPGRPCD", "FORTYPGRPNM")

  x <- datLUTnm(x, xvar=ftypnm, LUT=ref_fortypcd, LUTvar="FORTYPCD")$xLUT
  return(x)
}

