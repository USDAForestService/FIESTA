getadjfactorGB <- function(condx=NULL, treex=NULL, seedx=NULL, vcondsppx=NULL,
	vcondstrx=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", vuniqueid="PLT_CN", 
	condid="CONDID", unitlut=NULL, unitvars=NULL, strvars=NULL, unitarea=NULL, 
	areavar=NULL, areawt="CONDPROP_UNADJ", cvars2keep=NULL,
	tpropvars=list(SUBP="SUBPPROP_UNADJ", MICR="MICRPROP_UNADJ", MACR="MACRPROP_UNADJ")){

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
	n.strata=TPROP_BASIS=EXPNS=strwt=Prop=COVER_PCT_SUM <- NULL

  ## Define function
  adjnm <- function(nm) {
    ## DESCRIPTION: changes name of variable
    if (length(grep("UNADJ", nm)) == 1) {
      sub("UNADJ", "ADJ", nm)
    } else {
      paste0(nm, "_ADJ")
    }
  }

  ## check tables
  unitlut <- pcheck.table(unitlut)
  unitarea <- pcheck.table(unitarea)
    
  strunitvars <- c(unitvars, strvars)
  keycondx <- key(condx)

  ## Condition proportion variable
  varlst <- areawt
  areasum <- paste0(areawt, "_SUM")
  areaadj <- paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", areawt))
  varsumlst <- areasum
  varadjlst <- areaadj

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
  unitlut[, (varadjlst) := lapply(.SD, 
	function(x, n) ifelse((is.na(x) | x==0), 0, get(n)/x), n), .SDcols=varsumlst]

  ## Merge condition adjustment factors to cond table to get plot identifiers.
  setkeyv(condx, strunitvars)
  condx <- condx[unitlut[,c(strunitvars, varadjlst), with=FALSE]]

  ## Change name of condition adjustment factor to cadjfac
  ## Note: CONDPPROP_UNADJ is the same as below (combination of MACR and SUBP)
  cadjfacnm <- suppressMessages(checknm("cadjfac", names(condx)))
  setnames(condx, areaadj, cadjfacnm)
  setnames(unitlut, areaadj, cadjfacnm)
      
  ## Calculate adjusted condition proportion for plots
  areawtnm <- adjnm(areawt)
  condx[, (areawtnm) := get(areawt) * get(cadjfacnm)]
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
      treex[condx, tadjfac := ifelse(TPROP_BASIS == "MICR", get(tvaradj[["MICR"]]), 
		ifelse(TPROP_BASIS == "MACR", get(tvaradj[["MACR"]]),
		get(tvaradj[["SUBP"]])))]
#    } else if ("TPAGROW_UNADJ" %in% names(treex)) {
#      treex[condx, tadjfac := ifelse(TPAGROW_UNADJ > 50, get(tvaradj[["MICR"]]), 
# 		ifelse(TPAGROW_UNADJ > 0 & TPAGROW_UNADJ < 5, get(tvaradj[["MACR"]]),
# 		get(tvaradj[["SUBP"]])))]
    } else {
      treex[condx, tadjfac := 1]
    }

    if (!is.null(seedx)) {
      setkeyv(seedx, c(tuniqueid, condid))
      seedx[condx, tadjfac := get(tvaradj[["MICR"]])]
    }  
  }     

  if (!is.null(vcondsppx)) { 
    setkeyv(vcondsppx, c(vuniqueid, condid))
    vcondsppx <- merge(vcondsppx, condx[, c(key(condx), cadjfacnm), with=FALSE], 
		by=key(condx))
    vcondsppx[, COVER_PCT_SUM := COVER_PCT_SUM]
  } 
  if (!is.null(vcondstrx)) { 
    setkeyv(vcondstrx, c(vuniqueid, condid))
    vcondstrx <- merge(vcondstrx, condx[, c(key(condx), cadjfacnm), with=FALSE], 
		by=key(condx))
    vcondstrx[, COVER_PCT_SUM := COVER_PCT_SUM]
  } 
  if (!is.null(vcondsppx) || !is.null(vcondstrx)) { 
    setnames(unitlut, cadjfacnm, "ADJ_FACTOR_P2VEG_SUBP")
  }

  ## Calculate expansion factors (strata-level and cond-level)
  if (!is.null(unitarea)) {
    tabs <- check.matchclass(unitlut, unitarea, unitvars)
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

  ## Remove summed variables from condx
  vars2remove <- c(varsumlst, cadjfacnm)
  vars2remove <- vars2remove[vars2remove %in% names(condx)]
  if (length(vars2remove) > 0) {
    condx[, (varsumlst) := NULL]
  }

  ## Remove *_ADJFAC and *_UNADJ columns in condx 
  #condx[, names(condx)[grep("ADJ_FACTOR_", names(condx))]:= NULL] 
  #condx[, names(condx)[grep("_UNADJ", names(condx))]:= NULL] 

  adjfacdata <- list(condx=condx, unitlut=unitlut)
  adjfacdata$expcondtab <- expcondtab
  if (!is.null(treex)) adjfacdata$treex <- treex 
  if (!is.null(seedx)) adjfacdata$seedx <- seedx
  if (!is.null(vcondsppx)) adjfacdata$vcondsppx <- vcondsppx
  if (!is.null(vcondstrx)) adjfacdata$vcondstrx <- vcondstrx
 
  adjfacdata$cvars2keep <- names(condx)[names(condx) != "CONDPROP_ADJ"]

  return(adjfacdata)
}
