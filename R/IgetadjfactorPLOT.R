getadjfactorPLOT <- function(condx=NULL, treex=NULL, seedx=NULL, cuniqueid="PLT_CN", 
	tuniqueid="PLT_CN", checkNA=TRUE){

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
  ##     estunit (*PROP_UNADJ_SUM / n.total)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ####################################################################################
 
  ## Set global variables
  CONDPROP_ADJ=CONDPROP_UNADJ=CONDPROP_ADJFAC=tadjfac=cadjfac=TPA_UNADJ=
	MICRPROP_UNADJ_SUM=MACRPROP_UNADJ_SUM=SUBPPROP_UNADJ_SUM=MICRPROP_ADJFAC_SUM=
	MACRPROP_ADJFAC_SUM=SUBPPROP_ADJFAC_SUM=CONDPROP_UNADJ_SUM=TPROP_BASIS <- NULL    
  keycondx <- key(condx)

  ## Condition proportion variable
  varlst <- "CONDPROP_UNADJ"

  ## Get list of condition-level variables to calculate adjustments for
  if (!is.null(treex)) {  
    tvarlst <- c("SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
    tvarlst2 <- tvarlst[which(tvarlst%in% names(condx))]
    if (length(tvarlst2) == 0) stop("must include *PROP_UNADJ variables in cond")
    varlst <- c(varlst, tvarlst2)
  }
  varsumlst <- paste0(varlst, "_SUM")

  ## Names for new, condition-level adjusted variables (_UNADJ to _ADJ) 
  varlstadj <- sapply(varlst, function(x) sub("PROP_UNADJ", "", x) )
  varlstadj <- paste0("ADJ_FACTOR_", varlstadj)


  ###############################################################################
  ## Calculate adjustment factors by plot
  ## Sum condition variable(s) in varlst by plot
  ###############################################################################
  setkeyv(condx, cuniqueid)

  ## Sum condition variable(s) in varlst by plot and rename varlst to *_sum
  pltadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=cuniqueid, .SDcols=varlst]
  setnames(pltadj, varlst, varsumlst)
  setkeyv(pltadj, cuniqueid)

  ## Calculate adjusted condition proportion for plots
#  pltadj$CONDPROP_ADJ <- 1 / pltadj$CONDPROP_UNADJ_SUM
#  pltx <- pltadj[, c(cuniqueid, "CONDPROP_ADJ"), with=FALSE]
#  setkeyv(pltx, cuniqueid)


  ## Calculate adjustment factor for conditions
  condx[pltadj, cadjfac := 1/CONDPROP_UNADJ_SUM]

  ## Calculate adjustment factors for different size plots for trees
  if (!is.null(treex)) {
    ## Merge condition adjustment factors to tree table to get plot identifiers.
    ## Define a column in tree table, adjfact, to specify adjustment factor based on
    ##	the size of the plot it was measure on (identified by TPA_UNADJ)
    ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ=0.999188
    setkeyv(treex, tuniqueid)

    if ("TPROP_BASIS" %in% names(treex)) {
      treex[pltadj, tadjfac := ifelse(TPROP_BASIS == "MICR", MICRPROP_UNADJ_SUM, 
		ifelse(TPROP_BASIS == "MACR", MACRPROP_UNADJ_SUM,
		SUBPPROP_UNADJ_SUM))]
    } else {
      treex[pltadj, tadjfac := ifelse(TPA_UNADJ > 50, MICRPROP_UNADJ_SUM, 
 		ifelse(TPA_UNADJ > 0 & TPA_UNADJ < 5, MACRPROP_UNADJ_SUM,
 		SUBPPROP_UNADJ_SUM))]
    }
    treex[, tadjfac := ifelse(tadjfac > 0, tadjfac, 1)]

    if (!is.null(seedx)) {
      setkeyv(seedx, c(tuniqueid))
      seedx[pltadj, tadjfac := MICRPROP_UNADJ_SUM]
      seedx[, tadjfac := ifelse(tadjfac > 0, tadjfac, 1)]
    }  
  } 

  ## Calculate adjusted condition proportions 
  condx[, CONDPROP_ADJ := CONDPROP_UNADJ * cadjfac]

  adjfacdata <- list(condadj = condx)
  if (!is.null(treex)) adjfacdata$treeadj <- treex 
  if (!is.null(seedx)) adjfacdata$seedx <- seedx 

  return(adjfacdata)
}

