getadjfactorGB <- function(esttype, treex, condx=NULL, tuniqueid="PLT_CN", 
	cuniqueid="PLT_CN", condid="CONDID", unitlut=NULL, unitvars=NULL, strvars=NULL, 
	unitarea=NULL, areavar=NULL, cvars2keep=NULL){

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
  CONDPROP_ADJ=CONDPROP_UNADJ=CONDPROP_ADJFAC=tadjfac=TPA_UNADJ=MICRPROP_ADJFAC=
   	MACRPROP_ADJFAC=SUBPPROP_ADJFAC=expfac=expcond=n.strata=PROP_BASIS=EXPNS=strwt <- NULL
    
  strunitvars <- c(unitvars, strvars)
  keycondx <- key(condx)

  varlst <- "CONDPROP_UNADJ"
  varlstadj <- sapply(varlst, function(x) sub("UNADJ", "ADJFAC", x) )

  ## Get list of condition-level variables to calculate adjustments for
  if (esttype %in% c("TREE", "RATIO")) {  
    tvarlst <- c("SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
    tvarlst2 <- tvarlst[which(tvarlst%in% names(condx))]
    if (length(tvarlst2) == 0) stop("must include *PROP_UNADJ variables in cond")
    varlst <- c(varlst, tvarlst2)

    ## Names for new, condition-level adjusted variables (_UNADJ to _ADJ) 
    tvarlstadj <- sapply(tvarlst2, function(x){sub("UNADJ", "ADJFAC", x) })
    varlstadj <- c(varlstadj, tvarlstadj)
  }
  varsumlst <- paste0(varlst, "_SUM")


  ###############################################################################
  ## Calculate adjustment factors by strata (and estimation unit) for variable list
  ## Sum condition variable(s) in varlst and divide by total number of plots in strata
  ###############################################################################
 
  ## Sum condition variable(s) in varlst by strata and rename varlst to *_sum
  cndadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=strunitvars, .SDcols=varlst]
  setnames(cndadj, varlst, varsumlst)
  setkeyv(cndadj, strunitvars)

  ## Merge condition adjustment factors to strata table.
  unitlut <- unitlut[cndadj]
  n <- ifelse(is.null(strvars), "n.total", "n.strata")

  ## Divide summed conditions by total number of plots in strata
  unitlut[, (varlstadj) := lapply(.SD, function(x) get(n)/x), .SDcols=varsumlst]

  ## Merge condition adjustment factors to cond table to get plot identifiers.
  setkeyv(condx, strunitvars)
  condx <- condx[unitlut[,c(strunitvars, varlstadj), with=FALSE]]

  ## Calculate adjusted condition proportion for plots
  condx[, CONDPROP_ADJ := CONDPROP_UNADJ * CONDPROP_ADJFAC]

  ## Calculate adjusted condition proportions for different size plots for trees
  if (esttype %in% c("TREE", "RATIO")) {
    setkeyv(condx, c(cuniqueid, condid))
    setkeyv(treex, c(tuniqueid, condid))

    ## Merge condition adjustment factors to tree table to get plot identifiers.
    ## Define a column in tree table, adjfact, to specify adjustment factor based on
    ##	the size of the plot it was measure on (identified by TPA_UNADJ)
    ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ=0.999188

    if ("PROP_BASIS" %in% names(condx)) {
      treex[condx, tadjfac := ifelse(PROP_BASIS == "MICR", MICRPROP_ADJFAC, 
		ifelse(PROP_BASIS == "MACR", MACRPROP_ADJFAC,
		SUBPPROP_ADJFAC))]
    } else if ("PROP_BASIS" %in% names(treex)) {
      treex[condx, tadjfac := ifelse(PROP_BASIS == "MICR", MICRPROP_ADJFAC, 
		ifelse(PROP_BASIS == "MACR", MACRPROP_ADJFAC,
		SUBPPROP_ADJFAC))]
    } else {
      treex[condx, tadjfac := ifelse(TPA_UNADJ > 50, MICRPROP_ADJFAC, 
 		ifelse(TPA_UNADJ > 0 & TPA_UNADJ < 5, MACRPROP_ADJFAC,
 		SUBPPROP_ADJFAC))]
    }
  }

  ## Remove *_ADJFAC and *_UNADJ columns in condx 
  condx[, names(condx)[grep("_ADJFAC", names(condx))]:= NULL] 
  condx[, names(condx)[grep("_UNADJ", names(condx))]:= NULL] 
  

  ## Calculate expansion factors (strata-level and cond-level)
  if (!is.null(unitarea)) {
    tabs <- FIESTA::check.matchclass(unitlut, unitarea, unitvars)
    unitlut <- tabs$tab1
    unitarea <- tabs$tab2

    unitlut <- unitlut[unitarea]

    ## Expansion factors - average area by strata
    unitlut[, expfac:= get(areavar)/get(n)][, EXPNS := expfac * strwt]

    ## Condition-level expansion factors
    setkeyv(condx, strunitvars)
    expcondtab <- merge(condx, unitlut[,c(strunitvars, "expfac"), with=FALSE])
    expcondtab <- expcondtab[, expcond:=(CONDPROP_ADJ * expfac)][order(get(cuniqueid))][
    , expfac := NULL][, (strunitvars) := NULL]
    setkeyv(condx, c(cuniqueid, condid))
    setkeyv(expcondtab, c(cuniqueid, condid))
    unitlut[, expfac := NULL]
  } else {
    expcondtab <- NULL
  }

  setkeyv(condx, keycondx)
  adjfacdata <- list(condx=condx, unitlut=unitlut)
  adjfacdata$expcondtab <- expcondtab
  if (esttype %in% c("TREE", "RATIO")) adjfacdata$treex <- treex 
  adjfacdata$cvars2keep <- names(condx)[names(condx) != "CONDPROP_ADJ"]

  return(adjfacdata)
}
