strat.collapse <- function(stratacnt, errtab, pltstratx, minplotnum.unit=10, 
	minplotnum.strat=2, unitarea, areavar, unitvar, unitvar2=NULL, strvar, 
	getwt=FALSE, stratcombine=TRUE, unitcombine=FALSE, vars2combine=NULL) {
  ## unitcombine - If TRUE, combine estimation units, If FALSE, only combine strata

  ## Set global variables
  n.strata=n.total=puniqueid=unitstrgrplut=UNITCD <- NULL
  addUNITCD <- FALSE

  if (!"data.table" %in% class(stratacnt)) stratacnt <- setDT(stratacnt)
  if (!"data.table" %in% class(unitarea)) unitarea <- setDT(unitarea)


  ## If number of plots per strata <= minplotnum.strat is more than 50%, collapse to 1 strata.
#  if (stratcombine && sum(errtab$n.strata < minplotnum.strat)/nrow(stratacnt) >= .5) {
#    message(paste("number of plots per strata <=", minplotnum, 
#		"is greater than 50%... collapsing strata with less than", 
#		minplotnum, "plots in a strata to 1 strata"))
#	
#    unitvars <- c(unitvar, unitvar2)
#    strunitvars <- c(unitvars, strvar)
#    errtab[, MATCH := do.call(paste, .SD), .SDcols=unitvars]
#    stratacnt[, MATCH := do.call(paste, .SD), .SDcols=unitvars]
#    stratacnt[MATCH %in% unique(errtab$MATCH), (strvar) := 1]
#    stratacnt[, MATCH := NULL]
#
#    strsumvars <- c("n.strata", "n.total") 
#    if (getwt) {
#      strsumvars <- c(vars2combine, strsumvars) 
#    } else {
#      strsumvars <- c("strwt", strsumvars)
#    }
#    strlut <- stratacnt[, lapply(.SD, sum, na.rm=TRUE), by=strunitvars, .SDcols=strsumvars]
#    pltstratx[, (strvar) := 1]
#    strlut[, n.strata := NULL][, n.total := NULL]
#
#    ## Check again for number of plots by strata. If < 2 plots still with 1 strata, stop.
#    stratacnts2 <- FIESTA::check.pltcnt(pltx=pltstratx, puniqueid=puniqueid, 
#		unitlut=strlut, unitvars=unitvar, strvars=strvar)
#    stratacnt <- stratacnts2$unitlut
#    errtab <- stratacnts2$errtab
#  }

  ## Stop and send message if stratcombine=FALSE
  ######################################################################################
  if (!stratcombine) {
    if (any(unique(stratacnt$n.total) < minplotnum.unit))
      stop("estimation unit has less than ", minplotnum.unit, " plots", 
		"... must combine estimation units")
    if ("n.strata" %in% names(stratacnt) && 
			any(unique(stratacnt$n.strata) < minplotnum.strat))
      stop("strata has less than ", minplotnum.strat, " plots", 
		"... must combine strata")
  }

  ## Stop and send message if unitcombine=FALSE and total plots less than minplotnum.unit
  #######################################################################################
  if (!unitcombine) {
    if (any(unique(stratacnt$n.total) < minplotnum.unit)) {
      estunits <- unique(stratacnt[stratacnt$n.total < minplotnum.unit, unitvar, with=FALSE][[1]])
      message("estimation unit has less than ", minplotnum.unit, " plots", 
		"... remove or combine estimation units")
      print(paste(estunits, collapse="; "))
    }
  }


  #############################################################################
  ## If stratcombine=TRUE and unitcombine=TRUE and number of total plots is less 
  ## than minplotnum.unit.
  #############################################################################
  if (unitcombine && any(unique(stratacnt$n.total) < minplotnum.unit)) {
    message("collapsing estimation units")

    ## Define a variable to restrain collapsing by. Use unitvar2 if exists.
    if (is.null(unitvar2)) {
      if (unitvar != "UNITCD" && !"UNITCD" %in% names(stratacnt)) {
        stratacnt[, UNITCD := 1]
        addUNITCD <- TRUE
      }
      unitcombinevar <- "UNITCD"
    } else {
      unitcombinevar <- unitvar2
    }
         
    ## Group estimation units if less than minplotnum
    unitvarnew <- paste0(unitvar, ".1")
    stratacnt <- setDF(stratacnt)
    unitcnt <- setDF(stratacnt[, list(n.total = sum(n.total)), by=c(unitcombinevar, unitvar)])
    unitgrp <- by(unitcnt, unitcnt[, unitcombinevar], groupEstunit, 
			unitvar=unitvar, unitvarnew=unitvarnew, minplotnum=minplotnum.unit, 
			unitgrpnm=TRUE, n="n.total")
    unitgrp.dt <- setDT(data.frame(do.call(rbind, unitgrp), stringsAsFactors=FALSE, row.names=NULL))
    stratacnt <- merge(stratacnt, unitgrp.dt[, c(unitcombinevar, unitvar, unitvarnew), with=FALSE],
		by=c(unitcombinevar, unitvar))
    SDcols <- c(vars2combine, "n.strata", "n.total")
    SDcols <- SDcols[SDcols %in% names(stratacnt)]
    unitgrpsum <- stratacnt[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(unitcombinevar, unitvarnew, strvar), .SDcols=SDcols]
    setorderv(unitgrpsum, c(unitcombinevar, unitvarnew, strvar))

    if (addUNITCD) {
      unitgrpsum[, (unitcombinevar) := NULL]
      unitjoinvars <- c(unitvar)
    } else {
      unitjoinvars <- c(unitcombinevar, unitvar)
    }

    ## Create look up table with original classes and new classes
    unitgrpvars <- c(unitjoinvars, unitvarnew)
    unitgrplut <- unique(unitgrp.dt[, unitgrpvars, with=FALSE])
    unitstrgrplut <- unitgrp.dt[, c(unitgrpvars, strvar), with=FALSE]
  

    if (!is.null(unitarea)) {
      ## unitarea: Check if estunit1nm class match
      tabs <- FIESTA::check.matchclass(unitarea, unitgrplut, unitjoinvars)
      unitarea <- tabs$tab1
      unitgrplut <- tabs$tab2

      ## Merge new estimation unit to dat, unitarea, strlut
      unitarea <- merge(unitarea, unitgrplut, by=unitjoinvars)
      unitarea[, (unitvar) := NULL]
      unitvar <- unitvarnew
      unitarea <- unitarea[, sum(get(areavar)), by=unitvar]
      setnames(unitarea, "V1", areavar)
      setkeyv(unitarea, unitvar)
    }

    ## Merge new unitvar to pltstratx
    pltstratx <- merge(pltstratx, unitgrplut, by=unitjoinvars)
    unitvar <- unitvarnew

  } else {
    unitgrpsum <- stratacnt
    unitgrplut <- stratacnt
  }


  #############################################################################
  ## If stratcombine=TRUE and number of total plots is less than minplotnum.strat
  #############################################################################
  if ("n.strata" %in% names(stratacnt) && 
		any(unique(stratacnt$n.strata) < minplotnum.strat)) {

    ## Group strata classes by estimation unit
    strvarnew <- paste0(strvar, ".1")
    unitgrpsum <- setDF(unitgrpsum)
    stratgrp <- by(unitgrpsum, unitgrpsum[, unitvar], groupStrata, 
			strvar=strvar, strvarnew=strvarnew, minplotnum=minplotnum.strat)
    stratgrp.dt <- setDT(data.frame(do.call(rbind, stratgrp), stringsAsFactors=FALSE, 
		row.names=NULL))
    strlut <- stratgrp.dt[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, strvarnew), .SDcols=c(vars2combine, "n.strata")]
    strlut[, n.total := stratgrp.dt[match(strlut[[unitvar]], stratgrp.dt[[unitvar]]), 
		"n.total"]]

    ## Create look up table with original classes and new classes
    unitstrjoinvars <- c(unitvar, strvar)
    if (!is.null(unitstrgrplut)) {
      unitstrgrplut <- merge(unitstrgrplut, 
			stratgrp.dt[, c(unitvar, strvar, strvarnew), with=FALSE], 
			by=unitstrjoinvars)
      unitstrgrplut <- unitstrgrplut[, c(unitgrpvars, strvar, strvarnew), with=FALSE]
    } else {
      unitstrgrplut <- stratgrp.dt[, c(unitvar, strvar, strvarnew), with=FALSE]
    }

    ## Merge new strata to look up table with original classes and new classes
    keyvars <- unitstrjoinvars
    setkeyv(setDT(unitstrgrplut), keyvars)

    ## Merge new unitvar to pltstratx
    pltstratx <- merge(pltstratx, 
		unique(unitstrgrplut[,c(unitstrjoinvars, strvarnew), with=FALSE]), 
		by=unitstrjoinvars)
    strvar <- strvarnew
    strunitvars=c(unitvar, strvar)

  } else {
    strlut <- unitgrpsum
  }
 
  returnlst <- list(pltstratx=pltstratx, strlut=strlut, unitvar=unitvar)
  if (!is.null(strvar)) returnlst$strvar <- strvar
  if (stratcombine && !is.null(unitstrgrplut))
    returnlst$unitstrgrplut <- unitstrgrplut

  return(returnlst)
}
