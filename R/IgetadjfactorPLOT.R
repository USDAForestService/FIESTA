getadjfactorPLOT <- function(esttype, treex, condx=NULL, tuniqueid="PLT_CN", 
	cuniqueid="PLT_CN", checkNA=TRUE){

  ####################################################################################
  ## DESCRIPTION: 
  ## Calculates adjustment factors for plots to account for nonsampled conditions.
  ## Creates an adjusted condition proportion by dividing 1 by summed proportions in plot.
  ## NOTE: The following variables must be included in your dataset:
  ##    TPA_UNADJ; 
  ##    SUBPPROP_UNADJ (if you have TPA_UNADJ values > 5 and < 10); 
  ##    MICRPROP_UNADJ (if you have TPA_UNADJ values > 50); 
  ##    MACRPROP_UNADJ (if you have TPA_UNADJ values < 5) 
  ##
  ## VALUE:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC) by  
  ##     strata and /or estunit (*PROP_UNADJ_SUM / n.strata or n.total, if strvars=NULL)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ####################################################################################
 
  ## Set global variables
  CONDPROP_ADJ=CONDPROP_UNADJ=CONDPROP_ADJFAC=tadjfac=cadjfac=TPA_UNADJ=
	MICRPROP_UNADJ_SUM=MACRPROP_UNADJ_SUM=SUBPPROP_UNADJ_SUM=MICRPROP_ADJFAC_SUM=
	MACRPROP_ADJFAC_SUM=SUBPPROP_ADJFAC_SUM=CONDPROP_UNADJ_SUM=PROP_BASIS <- NULL    
  keycondx <- key(condx)

  if ("COND_STATUS_CD" %in% names(condx)) {
    if (any(condx$COND_STATUS_CD == 5)) {
      message("removing nonsampled plots (COND_STATUS_CD == 5) from cond")
      condx <- condx[condx$COND_STATUS_CD != 5, ]
    } 
  } else {
    message("COND_STATUS_CD not in dataset... assuming all conditions are sampled")
  }

  varlst <- "CONDPROP_UNADJ"
  varlstadj <- sapply(varlst, function(x) sub("UNADJ", "ADJFAC", x) )

  ## Get list of condition-level variables to calculate adjustments for
  if (esttype %in% c("TREE", "RATIO")) {  
    tvarlst <- check.PROP(treex, condx, checkNA=checkNA)
    varlst <- c(varlst, tvarlst)

    ## Names for new, condition-level adjusted variables (_UNADJ to _ADJ) 
    tvarlstadj <- sapply(tvarlst, function(x){sub("UNADJ", "ADJFAC", x) })
    varlstadj <- c(varlstadj, tvarlstadj)
  }
  varsumlst <- paste0(varlst, "_SUM")

  ###############################################################################
  ## Calculate adjustment factors by plot
  ## Sum condition variable(s) in varlst by plot
  ###############################################################################

  ## Sum condition variable(s) in varlst by plot and rename varlst to *_sum
  pltadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=cuniqueid, .SDcols=varlst]
  setnames(pltadj, varlst, varsumlst)
  setkeyv(pltadj, cuniqueid)

  ## Calculate adjusted condition proportion for plots
#  pltadj$CONDPROP_ADJ <- 1 / pltadj$CONDPROP_UNADJ_SUM
#  pltx <- pltadj[, c(cuniqueid, "CONDPROP_ADJ"), with=FALSE]
#  setkeyv(pltx, cuniqueid)


  ## Calculate adjusted condition proportions for different size plots for trees
  if (esttype %in% c("TREE", "RATIO")) {
    ## Merge condition adjustment factors to tree table to get plot identifiers.
    ## Define a column in tree table, adjfact, to specify adjustment factor based on
    ##	the size of the plot it was measure on (identified by TPA_UNADJ)
    ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ=0.999188
    setkeyv(treex, tuniqueid)

    if ("PROP_BASIS" %in% names(treex)) {
      treex[pltadj, tadjfac := ifelse(PROP_BASIS == "MICR", MICRPROP_UNADJ_SUM, 
		ifelse(PROP_BASIS == "MACR", MACRPROP_UNADJ_SUM,
		SUBPPROP_UNADJ_SUM))]
    } else {
      treex[pltadj, tadjfac := ifelse(TPA_UNADJ > 50, MICRPROP_UNADJ_SUM, 
 		ifelse(TPA_UNADJ > 0 & TPA_UNADJ < 5, MACRPROP_UNADJ_SUM,
 		SUBPPROP_UNADJ_SUM))]
    }
    treex[, tadjfac := 1 / tadjfac]
  } 

  ## Calculate adjusted condition proportions 
  condx[pltadj, cadjfac := 1/CONDPROP_UNADJ_SUM]
  condx[, CONDPROP_ADJ := CONDPROP_UNADJ * cadjfac]


  adjfacdata <- list(condadj = condx[, c(cuniqueid, "CONDPROP_ADJ"), with=FALSE])
  if (esttype %in% c("TREE", "RATIO")) adjfacdata$treeadj <- treex 

  return(adjfacdata)
}

