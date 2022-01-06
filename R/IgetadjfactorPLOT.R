getadjfactorPLOT <- function(condx=NULL, treex=NULL, seedx=NULL, cuniqueid="PLT_CN", 
	tuniqueid="PLT_CN", condid="CONDID", checkNA=TRUE, areawt="CONDPROP_UNADJ", 
	tpropvars=list(SUBP="SUBPPROP_UNADJ", MICR="MICRPROP_UNADJ", MACR="MACRPROP_UNADJ")){

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

  ## Define function
  adjnm <- function(nm) {
    ## DESCRIPTION: changes name of variable
    if (length(grep("UNADJ", nm)) == 1) {
      sub("UNADJ", "ADJ", nm)
    } else {
      paste0(nm, "_ADJ")
    }
  }
  
  keycondx <- key(condx)

  ## Condition proportion variable
  varlst <- areawt
  areasum <- paste0(areawt, "_SUM")
  areaadj <- paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", areawt))
  varsumlst <- areasum
  varadjlst <- areaadj

  ## Check tables
  condx <- pcheck.table(condx)
  treex <- pcheck.table(treex)
  seedx <- pcheck.table(seedx)

  ## Get list of condition-level variables to calculate adjustments for
  if (!is.null(treex)) {  
    tvarlst <- unlist(tpropvars)
    tvarlst2 <- tvarlst[which(tvarlst%in% names(condx))]
    if (length(tvarlst2) == 0) {
      stop("must include unadjusted variables in cond")
    }
    tvarsum <- lapply(tpropvars, function(x) paste0(x, "_SUM"))
    tvaradj <- lapply(tpropvars, function(x) paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)))
    varlst <- c(varlst, tvarlst)
    varsumlst <- c(varsumlst, unlist(tvarsum))
    varadjlst <- c(varadjlst, unlist(tvaradj))
  }


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

  pltadj[, (varadjlst) := lapply(.SD, 
	function(x) ifelse((is.na(x) | x==0), 0, 1/x)), .SDcols=varsumlst]
  condx <- condx[pltadj]


  ## Change name of condition adjustment factor to cadjfac
  ## Note: CONDPPROP_UNADJ is the same as below (combination of MACR and SUBP)
  cadjfacnm <- suppressMessages(checknm("cadjfac", names(condx)))
  setnames(condx, areaadj, cadjfacnm)


  ## Calculate adjusted condition proportion for plots
  areawtnm <- adjnm(areawt)
  condx[, (areawtnm) := get(areawt) * get(cadjfacnm)]
  setkeyv(condx, c(cuniqueid, condid))


  ## Calculate adjustment factors for different size plots for trees
  if (!is.null(treex)) {
    ## Merge condition adjustment factors to tree table to get plot identifiers.
    ## Define a column in tree table, adjfact, to specify adjustment factor based on
    ##	the size of the plot it was measure on (identified by TPA_UNADJ)
    ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ=0.999188
    setkeyv(treex, tuniqueid)

    if ("TPROP_BASIS" %in% names(treex)) {
      treex[pltadj, tadjfac := ifelse(TPROP_BASIS == "MICR", get(tvaradj[["MICR"]]), 
		ifelse(TPROP_BASIS == "MACR", get(tvaradj[["MACR"]]),
		get(tvaradj[["SUBP"]])))]
    } else {
      treex[pltadj, tadjfac := ifelse(TPA_UNADJ > 50, get(tvaradj[["MICR"]]), 
 		ifelse(TPA_UNADJ > 0 & TPA_UNADJ < 5, get(tvaradj[["MACR"]]),
 		get(tvaradj[["SUBP"]])))]
    }
    treex[, tadjfac := ifelse(tadjfac > 0, tadjfac, 1)]

    if (!is.null(seedx)) {
      setkeyv(seedx, c(tuniqueid))
      seedx[pltadj, tadjfac := get(tvaradj[["MICR"]])]
      seedx[, tadjfac := ifelse(tadjfac > 0, tadjfac, 1)]
    }  
  } 
  ## Remove summed variables from condx
  vars2remove <- c(varsumlst, cadjfacnm)
  vars2remove <- vars2remove[vars2remove %in% names(condx)]
  if (length(vars2remove) > 0) {
    condx[, (c(varsumlst, cadjfacnm)) := NULL]
  }

  adjfacdata <- list(condx = condx)
  if (!is.null(treex)) adjfacdata$treex <- treex 
  if (!is.null(seedx)) adjfacdata$seedx <- seedx 

  return(adjfacdata)
}

