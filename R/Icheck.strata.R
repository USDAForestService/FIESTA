check.strata <- function(pltstratx=NULL, puniqueid=NULL, stratalut=NULL, strvar="STRATA", 
	unitarea=NULL, unitvar=NULL, unitvar2=NULL, unitvars=NULL, areavar=NULL, getwt=FALSE, 
	getwtvar=NULL, sumunits=FALSE, nonresp=FALSE, substrvar=NULL, minplotnum=10, 
	autocombine=FALSE, gui=FALSE, tabtype="AREA"){

  ##################################################################################
  ## DESCRIPTION: Check strata information. 
  ##
  ## Import and check tables (cond, pltstrat, stratalut)
  ## If ONEUNIT, add ONEUNIT to stratalut
  ## Check strata variables in stratalut and in cond or pltstrat
  ## - Check if strvar is in stratalut
  ## - Check if strvar is in cond or pltstrat
  ##    - If strvar in cond, check if 1 value per plot and merge to pltstrat
  ## If nonresp, check substrvar
  ## - Check if substrvar is in stratalut
  ## - Check if substrvar is in cond or pltstrat
  ##    - If substrvar in cond, check if 1 value per plot and merge to pltstrat
  ## Check class and values of stratalut match pltstrat
  ## Check number of plots by strata and estimation unit.
  ## 	  If sumunits=FALSE, but number of plots per strata <= minplotnum (10) is more than 20%,
  ##   collapse to 1 strata. Then check again. If after collapsing to 1 strata, there 
  ##   is < 2 plots in an estimation unit, stop with a warning to combine estimation units. 
  ## Get strata weight information
  ## - Check if strwt or STRWT exists
  ## - If  strwt does not exist or if getwt=TRUE:
  ##    - Check areavar in stratalut
  ##    - Calculate strata weights (strwt = areavar/TOTAREA) 
  ## Return data and variable names
  ## - Generate table of nonsampled plots (PLOT_STATUS_CD = 3)
  ##################################################################################

  ## Set global variables
  PLOT_STATUS_CD=ONEUNIT=V1=TOTAREA=strwt=n.total=n.strata=UNITCD=STATECD=
	unitcdvar=MATCH <- NULL

  ## Check stratalut
  ############################################################################
  stratalut <- FIESTA::pcheck.table(stratalut, gui=gui, tabnm="stratalut",
 	caption="Strata table?", nullcheck=TRUE)
  if (is.null(stratalut)) stop("must include stratalut if strata=TRUE")
  strlutnmlst <- names(stratalut)

  ## Check for a total value in the last row of table..  If exists, exclude.
  lastrow <- stratalut[nrow(stratalut),]
  if (length(grep("Total", lastrow, ignore.case=TRUE)) > 0)
    stratalut <- stratalut[-nrow(stratalut)]

  ## Check unitvars in stratalut
  ############################
  if (any(unitvars == "ONEUNIT") && !"ONEUNIT" %in% names(stratalut)) 
    stratalut[, ONEUNIT := 1]


  ## Check strvar
  ############################
  strvar <- FIESTA::pcheck.varchar(var2check=strvar, varnm="strvar", gui=gui, 
	checklst=c("NONE", strlutnmlst), caption="Strata variable?", 
	warn="strata variable not in stratalut", stopifnull=TRUE)
  strvars <- strvar

  ## Check if class of strunitvars in stratalut matches class of strunitvars in pltstratx
  strunitvars <- c(unitvars, strvars)
  tabs <- FIESTA::check.matchclass(pltstratx, stratalut, strunitvars,
		tab1txt="pltstrat", tab2txt="stratalut")
  pltstratx <- tabs$tab1
  stratalut <- tabs$tab2

  ## Check that the strunitvars in pltstratx are all in stratalut
  missval <- FIESTA::check.matchval(pltstratx, stratalut, var1=strunitvars, 
	tab1txt="plt", tab2txt="stratalut", returnvals=TRUE)
  if (!is.null(missval) && length(missval) > 0) {
    stratalut[, MATCH := do.call(paste, .SD), .SDcols=unitvars]
    pltstratx[, MATCH := do.call(paste, .SD), .SDcols=unitvars]
    stratalut <- stratalut[MATCH %in% pltstratx$MATCH,]
    stratalut[, MATCH := NULL]
    message("subsetting stratalut to match plt")
  }

  ## Check getwt
  ###################################################################################
  getwt <- FIESTA::pcheck.logical(getwt, varnm="getwt", title="Get strata weights?", 
		first="YES", gui=gui, stopifnull=TRUE)

  if (getwt) {
    if (!getwtvar %in% strlutnmlst) {
      if ("strwt" %in% strlutnmlst) {
        message("using strwt column for strata weights")
        getwt <- FALSE
      } else {
        stop("getwtvar not in stratalut")
      }
    }
  } else {
    ## Check for strwt
    if (!"strwt" %in% strlutnmlst) {
      if ("STRWT" %in% strlutnmlst) {
        setnames(stratalut, "STRWT", "strwt") 
      } else {
        stop("strwt variable not in stratalut... include getwtvar and getwt=TRUE")
      }
    }
    getwtvar <- NULL
  } 

  #######################################################################
  ## NONRESPONSE
  #######################################################################

  ## GET substrvar FROM STRATA TABLE
  if (nonresp) {
    ## Remove strvar from strlutnmlst
    allnmlst <- allnmlst[which(!allnmlst %in% strvar)]
    substrvar <- FIESTA::pcheck.varchar(var2check=substrvar, varnm="substrvar", gui=TRUE, 
		checklst=strlutnmlst, caption="Substrata variable?", 
		warn="substrata variable not in strata table" )
    strvars <- c(strvars, substrvar)

    ## Generate table of nonsampled plots (PLOT_STATUS_CD = 3)
    if ("PLOT_STATUS_CD" %in% names(pltstratx)) {
      if (!3 %in% unique(pltstratx[["PLOT_STATUS_CD"]])) 
        message("there are no nonsampled plots in dataset (PLOT_STATUS_CD = 3)")    
      nonsampplots <- pltstratx[PLOT_STATUS_CD == 3, list(Freq=.N), by=c(strvar, substrvar)]
    }
  } else {
    nonsampplots <- NULL
  }

  ## Caculate weight
  ## Sum pixel weights first
  ###########################################################################
  if (getwt) {
    stratalut <- stratalut[, sum(get(getwtvar), na.rm=TRUE), by=strunitvars]
    setnames(stratalut, c(strunitvars, getwtvar))
  } else {
    stratalut <- stratalut[, sum(strwt/.N, na.rm=TRUE), by=strunitvars]
    setnames(stratalut, c(strunitvars, "strwt"))
  }

  ## Set key for stratalut
  setkeyv(stratalut, strunitvars)


  ###################################################################################
  ## CHECKS NUMBER OF PLOTS BY STRATA AND/OR ESTIMATION UNIT 
  ##	 (including partially sampled plots - COND_STATUS_CD=5) 
  ##	 (excluding totally nonsampled plots - PLOT_STATUS_CD=1) 
  ###################################################################################
  ## If < 2 plots, an error occurs, must collapse plots.
  ## If 2-10 plots, a warning is displayed, suggesting to collapse plots. 
  ## Returns:
  ## - strlut including number of plots by strata (n.strata) and total number of plots
  ##		by estimation unit (n.total).
  ## - error tab with warnings.
  #################################################################################
  stratacnts <- check.pltcnt(pltx=pltstratx, puniqueid=puniqueid, 
		unitlut=stratalut, unitvars=unitvars, strvars=strvars)
  stratacnt <- stratacnts$unitlut
  errtab <- stratacnts$errtab

  ###################################################################################
  ## Check for collapsing strata and/or estimation units
  ## If sumunits=FALSE, but number of plots per strata <= minplotnum (10) is more than 50%,
  ## collapse all estimation units with less than minplotnum (10) in a strata to 1 strata. 
  ## Then check again. If after collapsing to 1 strata, there is < 2 plots in an 
  ## estimation unit, stop with a warning to combine estimation units.
  ###################################################################################
  strsumvars <- c("n.strata", "n.total") 
  if (getwt) {
    strsumvars <- c(getwtvar, strsumvars) 
  } else {
    strsumvars <- c("strwt", strsumvars)
  }

  unitstrgrplut <- NULL
  unitsplit <- FALSE
  addUNITCD <- FALSE
   
#  if (!sumunits) {
  if (!is.null(errtab)) {

    ## If number of plots per strata <= minplotnum is more than 50%, collapse to 1 strata.
    if (autocombine && nrow(errtab)/nrow(stratacnt) >= .5) {
      message(paste("number of plots per strata <=", minplotnum, 
		"is greater than 50%... collapsing strata with less than", 
		minplotnum, "plots in a strata to 1 strata"))

      errtab[, MATCH := do.call(paste, .SD), .SDcols=unitvars]
      stratacnt[, MATCH := do.call(paste, .SD), .SDcols=unitvars]
      stratacnt[MATCH %in% unique(errtab$MATCH), (strvar) := 1]
      stratacnt[, MATCH := NULL]

      strlut <- stratacnt[, lapply(.SD, sum, na.rm=TRUE), by=strunitvars, .SDcols=strsumvars]
      pltstratx[, (strvar) := 1]
      strlut[, n.strata := NULL][, n.total := NULL]

      ## Check again for number of plots by strata. If < 2 plots still with 1 strata, stop.
      stratacnts2 <- FIESTA::check.pltcnt(pltx=pltstratx, puniqueid=puniqueid, 
		unitlut=strlut, unitvars=unitvars, strvars=strvars)
      stratacnt <- stratacnts2$unitlut
      errtab <- stratacnts2$errtab
    }

    if (any(errtab[["errtyp"]] == "stop")) {
      if (sumunits) {
        if (!autocombine) {
          stop("estimation unit has 2 plots or less... must combine estimation units")
        } else {
          if (any(stratacnt[["n.total"]] < 2)) {
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
            unitgrp <- by(stratacnt, stratacnt[, "UNITCD"], FIESTA::groupEstunit, 
			unitvar=unitvar, unitvarnew=unitvarnew, minplotnum=minplotnum, 
			unitgrpnm=TRUE)
            unitgrp.dt <- setDT(data.frame(do.call(rbind, unitgrp), 
			stringsAsFactors=FALSE, row.names=NULL))
            SDcols <- c(getwtvar, "n.strata", "n.total")
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

          } else {
            unitgrpsum <- stratacnt
            unitstrgrplut <- stratacnt
            unitvarnew <- unitvar
          }
        } 
          
        ## Group strata classes by estimation unit
        strvarnew <- paste0(strvar, ".1")
        unitgrpsum <- setDF(unitgrpsum)
        stratgrp <- by(unitgrpsum, unitgrpsum[, unitvarnew], groupStrata, 
			strvar=strvar, strvarnew=strvarnew, minplotnum=minplotnum)
        stratgrp.dt <- setDT(data.frame(do.call(rbind, stratgrp), stringsAsFactors=FALSE, 
		row.names=NULL))
        strlut <- stratgrp.dt[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitcdvar, unitvarnew, strvarnew), .SDcols=c(getwtvar, "n.strata")]
        strlut[, n.total := stratgrp.dt[match(strlut[[unitvarnew]], stratgrp.dt[[unitvarnew]]), 
		"n.total"]]

        ## Merge new strata to look up table with original classes and new classes
        unitstrgrplut <- merge(unitstrgrplut, 
		stratgrp.dt[, c(unitcdvar, unitvarnew, strvar, strvarnew), with=FALSE])
        if (unitsplit) {
          stunit <- strsplit(unitstrgrplut$UNITCD, " ")
          unitstrgrplut <- data.table(cbind(STATECD=unlist(stunit)[c(TRUE,FALSE)], 
			UNITCD=unlist(stunit)[c(FALSE,TRUE)],
 			unitstrgrplut[, c(unitvar, strvar, unitvarnew, strvarnew), with=FALSE]))
          keyvars <- c("STATECD", "UNITCD", unitvar, strvar)
          setkeyv(unitstrgrplut, keyvars)
        } else {
          unitstrgrplut <- setDT(unitstrgrplut)
          keyvars <- c(unitcdvar, unitvar, strvar)
          setkeyv(unitstrgrplut, keyvars)
        }

        ## Merge new estimation unit and strata values to pltstratx
        ##################################################################################

        ## Check if estunit1nm and strata1nm classes match
        if ("UNITCD" %in% keyvars & !"UNITCD" %in% names(pltstratx)) {
          keyvars <- keyvars[keyvars != "UNITCD"]
          unitstrgrplut[, UNITCD := NULL]
        }
        tabs <- FIESTA::check.matchclass(pltstratx, unitstrgrplut, keyvars)
        pltstratx <- tabs$tab1
        unitstrgrplut <- tabs$tab2

        pltstratx <- merge(pltstratx, unitstrgrplut[,unique(c(keyvars, unitvarnew, strvarnew)), 
			with=FALSE], by=keyvars)
        setkeyv(pltstratx, puniqueid)
    
        strvars[strvars == strvar] <- strvarnew
        strvar <- strvarnew
      } else { # if sumunits
        unitvarnew <- unitvar
        strlut <- stratacnt
      }

      unitvars[unitvars == unitvar] <- unitvarnew
      strunitvars[strunitvars == unitvar] <- unitvarnew
      unitvar <- unitvarnew

    } else {
      strlut <- stratacnt
    } 
  } else {
    strlut <- stratacnt
  }
  
  ###########################################################################
  ## Get strata weight information
  ###########################################################################
  ## Check for strwt
  if ("strwt" %in% names(strlut)) {
    test <- strlut[, round(sum(strwt, na.rm=TRUE)), by=unitvars]
    testlt1 <- test[V1 < 1]
    if (nrow(testlt1) > 0) {
      warning("strwts should add to 1")
      print(testlt1)
    } else {
      getwt <- FALSE
    }
  }

  ## Get strata weights from getwtvar
  if (getwt) {
    ## Check getwtvar from strata table.
    getwtvar <- FIESTA::pcheck.varchar(var2check=getwtvar, varnm="getwtvar", gui=gui, 
		checklst=names(strlut), caption="Acre variable?")
    if (is.null(getwtvar)) stop("getwtvar must be in strlut to calculate weights")

    #message(paste("calculating weights from", getwtvar))
    strlut[, strwt:=prop.table(get(getwtvar)), by=unitvars]
  }  
 

  returnlst <- list(pltstratx=pltstratx[,c(puniqueid, c(unitvars, strvars)),
 	with=FALSE], strlut=strlut, strvars=strvars, nonsampplots=nonsampplots)
  if (!is.null(unitarea)) returnlst$unitarea <- unitarea
  if (autocombine) returnlst$unitstrgrplut <- unitstrgrplut

  return(returnlst)
}

