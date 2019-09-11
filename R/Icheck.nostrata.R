check.nostrata <- function(pltstratx, puniqueid, unitvars=NULL, minplotnum=10, 
	autocombine=FALSE, unitarea=NULL, unitvar=NULL, areavar="ACRES", tabtype="AREA",
	strlut=NULL, getnpixels=FALSE, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION: 
  ## Add variable ONESTRAT=1 to pltstratx
  ## Create strlut table with variable ONESTRAT=1 and strwt=1
  ## Check number of plots by estimation unit. If < 2 plots, stop, with message to 
  ##	combine estimation units.
  ##################################################################################

  ## Get variable list
  pltnmlst <- names(pltstratx)

  ## Set global variables
  strwt=ONESTRAT=n.strata=n.total=UNITCD=STATECD=PLOT_STATUS_CD=npixels <- NULL

  message("no strata")
  strvar <- "ONESTRAT"
  pltstratx[, ':=' (ONESTRAT=1)]
  strvars <- strvar 
  strunitvars <- c(unitvars, strvars)

#  if ("UNITCD" %in% names(pltstratx)) 
#    strunitvars <- c("UNITCD", strunitvars)

  if (is.null(strlut)) {
    ## Create strata table with 1 strata per unit        
    strlut <- unique(pltstratx[, strunitvars, with=FALSE])
  } else {

    ## Check stratalut
    ############################################################################
    strlut <- FIESTA::pcheck.table(strlut, gui=gui, tabnm="strlut",
 		caption="Strata table?", nullcheck=TRUE)
    strlut[, ':=' (ONESTRAT=1)]

    ## Check npixels
    if (getnpixels && "npixels" %in% names(strlut)) 
      npixels <- unique(strlut[, c(unitvars, "npixels"), with=FALSE])

  }
  #strlut[, strwt := 1]    
  setkeyv(strlut, strunitvars)


  ###################################################################################
  ## CHECKS NUMBER OF PLOTS BY STRATA AND/OR ESTIMATION UNIT 
  ##	 (including partially sampled plots - COND_STATUS_CD=5) 
  ##	 (excluding totally nonsampled plots - PLOT_STATUS_CD=1) 
  ###################################################################################
  ## If < 2 plots, an error occurs, must collapse plots.
  ## If 2-10 plots, a warning is displayed, suggesting to collapse plots. 
  ## Returns:
  ## - strlut including number of plots by strata (n.strata) and total number of 
  ##	plots by estimation unit (n.total)
  #################################################################################
  stratacnts <- FIESTA::check.pltcnt(pltx=pltstratx, puniqueid=puniqueid, 
		unitlut=strlut, unitvars=unitvars, strvars=strvars)
  stratacnt <- stratacnts$unitlut
  errtab <- stratacnts$errtab

  unitstrgrplut <- NULL
  unitsplit <- FALSE
  addUNITCD <- FALSE

  if (!is.null(errtab) && any(errtab[["errtyp"]] == "stop")) {

    if (!autocombine) {
      stop("estimation unit has less than 2 plots... must combine units")

    } else {
      if (is.null(unitvar)) stop("unitvar is invalid")
      message("collapsing estimation units")

      ## Generate column of STATECD and UNITCD to handle multiple states
      if ("UNITCD" %in% names(stratacnt)) {
        if ("STATECD" %in% names(stratacnt) && length(unique(stratacnt[["STATECD"]])) > 1) {
          stratacnt[, UNITCD := paste(STATECD, UNITCD)] 
          unitsplit <- TRUE
        }
      } else {
        stratacnt[, UNITCD := 1]
        addUNITCD <- TRUE
      }
      unitcdvar <- "UNITCD"

      ## Group estimation units if less than minplotnum
      unitvarnew <- paste0(unitvar, ".1")
      stratacnt <- setDF(stratacnt)
      unitgrp <- by(stratacnt, stratacnt[, "UNITCD"], groupEstunit, unitvar=unitvar,
			unitvarnew=unitvarnew, minplotnum=minplotnum, unitgrpnm=TRUE)
      unitgrp.dt <- setDT(data.frame(do.call(rbind, unitgrp), stringsAsFactors=FALSE, 
			row.names=NULL))
      SDcols <- c("n.strata", "n.total")
      unitgrpsum <- unitgrp.dt[, lapply(.SD, sum, na.rm=TRUE), 
			by=c("UNITCD", unitvarnew, strvars), .SDcols=SDcols]
      setorderv(unitgrpsum, c("UNITCD", unitvarnew, strvars))

      if (addUNITCD) {
        stratacnt[["UNITCD"]] <- NULL
        unitstrgrpvars <- c(unitvar, unitvarnew, strvars)
      } else {
        unitstrgrpvars <- c("UNITCD", unitvar, unitvarnew, strvars)
      }

      ## Create look up table with original classes and new classes
      unitstrgrplut <- unitgrp.dt[, unitstrgrpvars, with=FALSE]


      if (!is.null(unitarea)) {
        if (tabtype=="AREA") stop("unitarea is invalid")

        ## unitarea: Check if estunit1nm class match
        tabs <- FIESTA::check.matchclass(unitarea, unitstrgrplut, unitvars)
        unitarea <- tabs$tab1
        unitstrgrplut <- tabs$tab2

        ## Merge new estimation unit to dat, unitarea, strlut
        unitarea <- merge(unitarea, 
			unique(unitstrgrplut[, c(unitvars, unitvarnew), with=FALSE]), by=unitvars)
        unitarea[, (unitvar) := NULL]
        unitvars <- c(unitvars[unitvars != unitvar], unitvarnew)
        unitarea <- unitarea[, sum(get(areavar)), by=unitvars]
        setnames(unitarea, "V1", areavar)
        setkeyv(unitarea, unitvars)
      }
    } 

    if (unitsplit) {
      stunit <- strsplit(unitstrgrplut$UNITCD, " ")
      unitstrgrplut <- data.table(cbind(STATECD=unlist(stunit)[c(TRUE,FALSE)], 
			UNITCD=unlist(stunit)[c(FALSE,TRUE)],
 			unitstrgrplut[, c(unitvar, strvar, unitvarnew), with=FALSE]))
      #keyvars <- c("STATECD", "UNITCD", unitvar)
      keyvars <- c("STATECD", unitvar)
      setkeyv(unitstrgrplut, keyvars)
    } else {
      unitstrgrplut <- setDT(unitstrgrplut)
      #keyvars <- c(unitcdvar, unitvar)
      keyvars <- unitvar
      setkeyv(unitstrgrplut, keyvars)
    }


    ## Merge new estimation unit to pltstratx
    ##################################################################################

    ## Check if estunit1nm and strata1nm classes match
    if ("UNITCD" %in% keyvars & !"UNITCD" %in% names(pltstratx)) {
      keyvars <- keyvars[keyvars != "UNITCD"]
      unitstrgrplut[, UNITCD := NULL]
    }
    tabs <- FIESTA::check.matchclass(pltstratx, unitstrgrplut, keyvars)
    pltstratx <- tabs$tab1
    unitstrgrplut <- tabs$tab2

    pltstratx <- merge(pltstratx, unitstrgrplut[,c(keyvars, unitvarnew), 
			with=FALSE], by=keyvars)
    setkeyv(pltstratx, puniqueid)


    ## Merge new estimation unit to strlut
    ##################################################################################

    ## Check if estunit1nm and strata1nm classes match
    if ("UNITCD" %in% keyvars & !"UNITCD" %in% names(stratacnt)) {
      keyvars <- keyvars[keyvars != "UNITCD"]
      unitstrgrplut[, UNITCD := NULL]
    }
    tabs <- FIESTA::check.matchclass(stratacnt, unitstrgrplut, keyvars)
    stratacnt <- tabs$tab1
    unitstrgrplut <- tabs$tab2

    stratacnt <- setDT(merge(stratacnt, unitstrgrplut[,c(keyvars, unitvarnew), 
			with=FALSE], by=keyvars))
    keyvars[keyvars == unitvar] <- unitvarnew

    strlut <- stratacnt[, lapply(.SD, sum, na.rm=TRUE), by=keyvars, 
		.SDcols=c("n.strata", "n.total")]
    setkeyv(strlut, keyvars)

    unitvars[unitvars == unitvar] <- unitvarnew
    strunitvars[strunitvars == unitvar] <- unitvarnew
    unitvar <- unitvarnew

  } else {
    strlut <- stratacnt
  }

  ## Add a strata weight = 1
  strlut[, strwt := 1]    


  returnlst <- list(pltstratx=pltstratx[,c(puniqueid, strunitvars), with=FALSE], 
	unitarea=unitarea, strlut=strlut, strvars=strvars, unitvars=unitvars,
	npixels=npixels)
  if (autocombine) returnlst$unitstrgrplut <- unitstrgrplut


  return(returnlst)

}

