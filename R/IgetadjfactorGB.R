getadjfactorGB <- function(condx=NULL, treex=NULL, seedx=NULL, vcondsppx=NULL,
	vcondstrx=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", vuniqueid="PLT_CN", 
	condid="CONDID", unitlut=NULL, unitvars=NULL, strvars=NULL, unitarea=NULL, 
	areavar=NULL, areawt="CONDPROP_UNADJ", cvars2keep=NULL){

  ####################################################################################
  ## DESCRIPTION: 
  ## Calculates adjustment factors for area and trees by strata (and estimation unit)
  ##		to account for nonsampled plots and conditions.
  ## Creates an adjusted condition proportion by merging strata-level adjustment
  ##		factors to cond and dividing CONDPROP_UNADJ by adjustment factor.
  ## NOTE: The following variables must be included in your dataset:
  ##    unitvar (if there is more that 1 estimation unit)
  ##    strvar (defining strata)
  ##    CONDID; 
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
  CONDPROP_ADJ=CONDPROP_UNADJ=ADJ_FACTOR_COND=cadjfac=tadjfac=TPAGROW_UNADJ=
	ADJ_FACTOR_MICR=ADJ_FACTOR_MACR=ADJ_FACTOR_SUBP=expfac=expcond=expcondtab=
	n.strata=TPROP_BASIS=EXPNS=strwt=Prop <- NULL
    
  strunitvars <- c(unitvars, strvars)
  keycondx <- key(condx)

  ## Condition proportion variable
  varlst <- areawt
  #varlst <- c(areawt, "SUBPPROP_UNADJ", "MACRPROP_UNADJ")
 

  ## Get list of condition-level variables to calculate adjustments for
  if (!is.null(treex)) {  
    tvarlst <- c("SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
    tvarlst2 <- tvarlst[which(tvarlst%in% names(condx))]
    if (length(tvarlst2) == 0) stop("must include *PROP_UNADJ variables in cond")
    varlst <- unique(c(varlst, tvarlst2))
  }
  varsumlst <- paste0(varlst, "_SUM")

  ## Names for new, condition-level adjusted variables (_UNADJ to _ADJ) 
  varlstadj <- sapply(varlst, function(x) sub("PROP_UNADJ", "", x) )
  varlstadj <- paste0("ADJ_FACTOR_", varlstadj)


  ###############################################################################
  ## Calculate adjustment factors by strata (and estimation unit) for variable list
  ## Sum condition variable(s) in varlst and divide by total number of plots in strata
  ###############################################################################
  ## Sum condition variable(s) in varlst by strata and rename varlst to *_sum
  cndadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=strunitvars, .SDcols=varlst]
  setnames(cndadj, varlst, varsumlst)
  setkeyv(cndadj, strunitvars)
  setkeyv(unitlut, strunitvars)

  ## Merge condition adjustment factors to strata table.
  unitlut <- unitlut[cndadj]
  n <- ifelse(is.null(strvars), "n.total", "n.strata")

  ## Calculate adjustment factor for conditions 
  ## (divide summed condition proportions by total number of plots in strata)
  unitlut[, (varlstadj) := lapply(.SD, 
	function(x, n) ifelse((is.na(x) | x==0), 0, get(n)/x), n), .SDcols=varsumlst]

  ## Merge condition adjustment factors to cond table to get plot identifiers.
  setkeyv(condx, strunitvars)
  condx <- condx[unitlut[,c(strunitvars, varlstadj), with=FALSE]]

  ## Change name of condition adjustment factor to cadjfac
  ## Note: CONDPPROP_UNADJ is the same as below (combination of MACR and SUBP)
  cadjfactnm <- paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", areawt))
  setnames(condx, cadjfactnm, "cadjfac")
  setnames(unitlut, cadjfactnm, "cadjfac")

  ## This is the same as above
  #if ("PROP_BASIS" %in% names(condx)) {
  #  condx[, cadjfac2 := ifelse(PROP_BASIS == "MACR", ADJ_FACTOR_MACR, ADJ_FACTOR_SUBP)]
  #}

  ## Calculate adjusted condition proportion for plots
  condx[, CONDPROP_ADJ := get(areawt) * cadjfac]
  setkeyv(condx, c(cuniqueid, condid))


  ## Calculate adjusted condition proportions for different size plots for trees
  if (!is.null(treex)) {
    setkeyv(treex, c(tuniqueid, condid))

    ## Merge condition adjustment factors to tree table to get plot identifiers.
    ## Define a column in tree table, adjfact, to specify adjustment factor based on
    ##	the size of the plot it was measure on (identified by TPA_UNADJ)
    ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ>6

#    if ("PROP_BASIS" %in% names(condx)) {
#      treex[condx, tadjfac := ifelse(PROP_BASIS == "MICR", ADJ_FACTOR_MICR, 
#		ifelse(PROP_BASIS == "MACR", ADJ_FACTOR_MACR,
#		ADJ_FACTOR_SUBP))]
    if ("TPROP_BASIS" %in% names(treex)) {
      treex[condx, tadjfac := ifelse(TPROP_BASIS == "MICR", ADJ_FACTOR_MICR, 
		ifelse(TPROP_BASIS == "MACR", ADJ_FACTOR_MACR,
		ADJ_FACTOR_SUBP))]
    } else {
      treex[condx, tadjfac := ifelse(TPAGROW_UNADJ > 50, ADJ_FACTOR_MICR, 
 		ifelse(TPAGROW_UNADJ > 0 & TPAGROW_UNADJ < 5, ADJ_FACTOR_MACR,
 		ADJ_FACTOR_SUBP))]
    }

    if (!is.null(seedx)) {
      setkeyv(seedx, c(tuniqueid, condid))
      seedx[condx, tadjfac := ADJ_FACTOR_MICR]
    }  
  }     

  if (!is.null(vcondsppx)) { 
    setkeyv(vcondsppfx, c(vuniqueid, condid))
    vcondsppx <- merge(vcondsppx, condx[, c(key(condx), "cadjfac"), with=FALSE], 
		by=key(condx))
    vcondsppx[, COVER_PCT_SUM_ADJ := COVER_PCT_SUM * cadjfac]
  } 
  if (!is.null(vcondstrx)) { 
    setkeyv(vcondstrx, c(vuniqueid, condid))
    vcondstrx <- merge(vcondstrx, condx[, c(key(condx), "cadjfac"), with=FALSE], 
		by=key(condx))
    vcondstrx[, COVER_PCT_SUM_ADJ := COVER_PCT_SUM * cadjfac]
  } 

  ## Calculate expansion factors (strata-level and cond-level)
  if (!is.null(unitarea)) {
    tabs <- FIESTA::check.matchclass(unitlut, unitarea, unitvars)
    unitlut <- tabs$tab1
    unitarea <- tabs$tab2

    ## Merge unitlut with unitarea
    setkeyv(unitarea, unitvars)
    setkeyv(unitlut, unitvars)
    unitlut <- unitlut[unitarea]

    ## Expansion factors - average area by strata
    if (any(c("strwt", "Prop") %in% names(unitlut))) {
      if ("strwt" %in% names(unitlut)) {
        unitlut[, expfac:= get(areavar)/get(n)][, EXPNS := expfac * strwt]
      } else {
        unitlut[, expfac:= get(areavar)/get(n)][, EXPNS := expfac * Prop]
      } 

      ## Condition-level expansion factors
      setkeyv(condx, strunitvars)
      expcondtab <- merge(condx, unitlut[,c(strunitvars, "EXPNS"), with=FALSE],
			by=strunitvars)
      expcondtab <- expcondtab[, expcond:=(CONDPROP_ADJ * EXPNS)][order(get(cuniqueid))][
    		, EXPNS := NULL][, (strunitvars) := NULL]
      setkeyv(condx, c(cuniqueid, condid))
      setkeyv(expcondtab, c(cuniqueid, condid))
    }
  } 

  ## Remove *_ADJFAC and *_UNADJ columns in condx 
  #condx[, names(condx)[grep("ADJ_FACTOR_", names(condx))]:= NULL] 
  #condx[, names(condx)[grep("_UNADJ", names(condx))]:= NULL] 

  unitlut <- setkeyv(unitlut, strunitvars)
  adjfacdata <- list(condx=condx, unitlut=unitlut)
  adjfacdata$expcondtab <- expcondtab
  if (!is.null(treex)) adjfacdata$treex <- treex 
  if (!is.null(seedx)) adjfacdata$seedx <- seedx
  if (!is.null(vcondsppx)) adjfacdata$vcondsppx <- vcondsppx
  if (!is.null(vcondstrx)) adjfacdata$vcondstrx <- vcondstrx
 
  adjfacdata$cvars2keep <- names(condx)[names(condx) != "CONDPROP_ADJ"]

  return(adjfacdata)
}
