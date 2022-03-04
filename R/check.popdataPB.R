check.popdataPB <- function(gui, pnt=NULL, pltpct=NULL, pltpctvars=NULL,
	plt=NULL, pltassgn=NULL, plotid="plot_id", pntid="dot_cnt",
 	puniqueid="CN", pltassgnid="CN", nonsamp.pfilter=NULL,
	unitvar=NULL, unitvar2=NULL, auxvars=NULL, unitarea=NULL, areavar="ACRES",
	areaunits="acres", unit.action="keep", removetext="unitarea",
	strata=FALSE, strvar=NULL, strtype="POST", stratcombine=TRUE,
	sumunits=FALSE, pvars2keep=NULL){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs
  ## Import and check pnt or pltpct tables
  ##	- check uniqueid of plot and points
  ##	- check NA values
  ##	- check pltpctvars (if pltpct table)
  ## Import and check plot and pltassgn tables
  ##	- check uniqueid of plot
  ##	- check NA values
  ## Merge plot data to points
  ## Apply plot-level nonsample filter (nonsamp.pfilter - default=NULL)
  ## Check for missing variables in pvars2keep (unitvar, strvar)
  ## Generate table of sampled/nonsampled plots (PLOT_STATUS_CD, if exists)
  ## Subset variables for PBx and pltassgnx
  ## Check unitarea (if esttype='AREA')
  ## Check if strvar is.factor
  ## VALUE:
  ## Return checked data
  ###################################################################################

  ## Define necessary plot and condition-level variables:
  ## - plt (pvars2keep) - "STATECD", unitvars, auxvars
  ## Check tabtype, module, and MAmethod
  ## - module in("GB", "MA")
  ## - MAmethod in("HT", "PS", "GREG")
  ## Check logical parameters: ratio, strata
  ## - If strata, only 1 auxvar allowed
  ## Import pnt or pltpct table and check unique identifiers (plotid):
  ## If pltpct,
  ## - Check for NA values in uniqueid (plotid)
  ## - Check pltpctvars and transpose to rows
  ## - Apply landarea and pnt filters
  ## - Add a column, TOTAL=1 to pntprop table for total estimate
  ## - Get filter ids for NOTinDOMAIN points
  ## If pnt,
  ## - Check for NA values in uniqueids (plotid, pntid)
  ## - Create table of sampled/nonsampled points
  ## - Add a column, TOTAL=1 to pnt table for total estimate
  ## - Apply nonsample, landarea, and pnt filters
  ## - Get filter ids for NOTinDOMAIN points
  ## Import and check plot table (optional)
  ## - Check unique identifier of plot
  ## - Check class of unique identifier of plot and pnt tables
  ## - If no plot table, create one from pnt table's unique identifier
  ## - Apply plot filters (pfilter)
  ## Apply point nonsample filter (pnt.nonsample.filter)
  ###################################################################################

  ## Set global variables
  pltdomainlst=pntprop=sampcnt=condid=value=ONEUNIT=plotsampcnt=p.pltdom=predfac=
	PBvars2keep <- NULL

  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  pdoms2keep <- unique(c("STATECD", "UNITCD", "COUNTYCD", "INVYR",
	"MEASYEAR", "PLOT_STATUS_CD", "PSTATUSCD", "RDDISTCD", "WATERCD", "ELEV",
	"COND_STATUS_CD", "ELEV_PUBLIC", "ECOSUBCD", "CONGCD", "INTENSITY", "DESIGNCD"))
  pvars2keep <- unique(c(pvars2keep, unitvar, unitvar2, auxvars))

  ###################################################################################
  ## Check logical parameters: strata, ACI
  ###################################################################################


  ## Check unit.action
  ########################################################
  unit.actionlst <- c("keep", "remove", "combine")
  unit.action <- pcheck.varchar(var2check=unit.action, varnm="unit.action", gui=gui,
		checklst=unit.actionlst, caption="unit.action", multiple=FALSE, stopifnull=TRUE)

  ## Check sumunits
  ########################################################
  sumunits <- pcheck.logical(sumunits, varnm="sumunits",
		title="Sum estimation units?", first="YES", gui=gui, stopifnull=TRUE)


  ## Check strata, strvars
  ###################################################################################
  strata <- pcheck.logical(strata, varnm="strata",
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)
  if (strata) {
    if (is.null(strvar)) stop("must include strvar for post-strat estimates")
    if (length(strvar) > 1) stop("invalid strvar... only 1 variable allowed")
    pvars2keep <- unique(c(pvars2keep, strvar))

    ## Check strtype
    strtype <- pcheck.varchar(var2check=strtype, varnm="strtype", gui=gui,
		checklst=c("POST", "PRE"), caption="Strata type",
		warn="invalid strtype")

    ## Check stratcombine
    stratcombine <- pcheck.logical(stratcombine, varnm="stratcombine",
		title="Combine strata?", first="YES", gui=gui, stopifnull=TRUE)

  } else {
    strvar <- NULL
  }


  ## Check auxvars
  ###################################################################################
#  if ((module == "MA" && MAmethod != "PS"))
#    if (!is.null(auxvars)) pvars2keep <- pvars2keep[pvars2keep != auxvars]


  ##################################################################
  ## Import and check pnt or pltpct tables
  ##################################################################
  pntx <- pcheck.table(pnt, gui=gui, tabnm="pnt", caption="Point table?",
		nullcheck=TRUE)
  pltpctx <- pcheck.table(pltpct, gui=gui, tabnm="pltpct",
		caption="Point counts?", nullcheck=TRUE)

  if (is.null(pntx) && is.null(pltpctx))
    stop("must include pnt or pltpct")

  if (!is.null(pntx) && !is.null(pltpctx))
    stop("only input pnt or pltpct")

  if (!is.null(pltpctx)) {
    getprop <- FALSE
    pltpctvarlst <- names(pltpctx)

    ## Check unique identifiers of pnt table (puniqueid and condid)
    #############################################################################
    plotid <- pcheck.varchar(var2check=plotid, varnm="plotid", gui=gui,
		checklst=pltpctvarlst, caption="UniqueID variable for pltpct",
		warn="plotid not in pltpct table", stopifnull=TRUE)
    setkeyv(pltpctx, plotid)

    ## Check for NA values in unique identifier
    ##############################################################################
    navars <- c(plotid)
    pltpctx.na <- sapply(navars,
		function(x, pltpctx){ sum(is.na(pltpctx[,x, with=FALSE])) }, pltpctx)
    if (any(pltpctx.na) > 0)
      stop(pltpctx.na[pltpctx.na > 0], " NA values in variable: ",
		paste(names(pltpctx.na[pltpctx.na > 0]), collapse=", "))

    ## Check pltpctvarsT1
    ##############################################################################
    pltpctvarlst <- pltpctvarlst[pltpctvarlst != plotid]
    pltpctvars <- pcheck.varchar(pltpctvars, varnm="pltpctvars",
		checklst=pltpctvars, gui=gui, caption="Variable list",
		warn="invalid variable names", stopifnull=TRUE, multiple=TRUE)
    if (is.null(pltpctvars)) {
      pltpctvars <- pltpctvarlst
      warning("pltpctvars=NULL... using all variables in pltpct for estimation")
    }

    ## Transpose pltpctvars to rows
    ##############################################################################
    pntprop <- transpose2row(pltpctx, plotid, tvars=pltpctvars, na.rm=TRUE)
    pntprop[, p.pltdom := value/100][, value := NULL]
    rowvar <- "variable"
    PBvars2keep <- "p.pltdom"

    PBx <- pntprop
    rowvar <- "variable"

  } else if (!is.null(pntx)) {

    getprop <- TRUE
    pntnmlst <- names(pntx)

    ## Check unique identifiers of pnt table (puniqueid and pntid)
    #############################################################################
    plotid <- pcheck.varchar(var2check=plotid, varnm="plotid", gui=gui,
		checklst=pntnmlst, caption="UniqueID variable for plot - pnt",
		warn="plotid not in pnt table", stopifnull=TRUE)

    pntid <- pcheck.varchar(var2check=pntid, varnm="pntid", gui=gui,
		checklst=pntnmlst[pntnmlst != plotid], caption="UniqueID variable for points - pnt",
		warn="pntid not in pnt table", stopifnull=TRUE)
    setkeyv(pntx, c(plotid, pntid))

    ## Check if there are any NA values in these variables
    ##############################################################################
    navars <- c(plotid, pntid, PBvars2keep)
    pntx.na <- sapply(navars,
		function(x, pntx){ sum(is.na(pntx[,x, with=FALSE])) }, pntx)
    if (any(pntx.na) > 0)
      stop(pntx.na[pntx.na > 0], " NA values in variable: ",
		paste(names(pntx.na[pntx.na > 0]), collapse=", "))

    ## Get table of sampled / nonsampled points
    ##############################################################################
    #sampcnt <- pntx[, .N, by=c(rowvar, colvar)]

    PBx <- pntx
  }


  ####################################################################################
  ## Import and check plot and pltassgn tables (optional), check unique identifiers
  ####################################################################################
  pltx <- pcheck.table(plt, gui=gui, tabnm="plt",
	caption="plt table?", nullcheck=TRUE)
  pltassgnx <- pcheck.table(pltassgn, tabnm="pltassgn",
		caption="plot assignments?", nullcheck=TRUE, gui=gui)

  ## Check plot-level variables
  ###################################################################################
  if (!is.null(pltx) || !is.null(pltassgnx)) {
    if (!is.null(pltx)) {
      pltnmlst <- names(pltx)
      puniqueid <- pcheck.varchar(var2check=puniqueid, varnm="puniqueid", gui=gui,
		checklst=names(pltx), caption="UniqueID variable of plot",
		warn=paste(puniqueid, "not in plt table"), stopifnull=TRUE)
      if (any(duplicated(pltx[[puniqueid]]))) {
        warning("plt records are not unique in: plt")
      }
      ## Check for NA values in necessary variables in plt table
      pltx.na <- sum(is.na(pltx[[puniqueid]]))
      if (pltx.na > 0) stop("NA values in ", puniqueid)

      ## Set key
      setkeyv(pltx, puniqueid)
    }

    if (!is.null(pltassgnx)) {
      pltassgnid <- pcheck.varchar(var2check=pltassgnid, varnm="pltassgnid", gui=gui,
		checklst=names(pltassgnx), caption="UniqueID variable of plot",
		warn=paste(pltassgnid, "not in, warntab"), stopifnull=TRUE)
      if (any(duplicated(pltassgnx[[pltassgnid]])))
        warning("plot records are not unique in pltassgn")

      ## Check for NA values in necessary variables in plt table
      pltassgnx.na <- sum(is.na(pltassgnx[[pltassgnid]]))
      if (pltassgnx.na > 0) stop("NA values in ", pltassgnid)

      ## Set key
      setkeyv(pltassgnx, pltassgnid)
    }

    if (!is.null(pltx) && !is.null(pltassgnx)) {
      pltassgnx <- pltassgnx[, unique(c(pltassgnid,
		names(pltassgnx)[!names(pltassgnx) %in% names(pltx)])), with=FALSE]
      setkeyv(pltassgnx, pltassgnid)

      ## Check if class of puniqueid in pltx matches class of puniqueid in condx
      tabs <- check.matchclass(pltx, pltassgnx, puniqueid, pltassgnid)
      pltx <- tabs$tab1
      pltassgnx <- tabs$tab2

      ## Check for matching unique identifiers of condx and pltx
      pltx <- check.matchval(pltx, pltassgnx, puniqueid, pltassgnid, tab1txt="plt",
			tab2txt="pltassgn", subsetrows=TRUE)
      pltx <- pltx[pltassgnx]

    } else if (is.null(pltx)) {
      pltx <- pltassgnx
      puniqueid <- pltassgnid
    }

    ## Merge plot data to pnts
    ######################################################
    ## Check if class of puniqueid in pltx matches class of puniqueid in condx
    tabs <- check.matchclass(PBx, pltx, plotid, puniqueid)
    PBx <- tabs$tab1
    pltx <- tabs$tab2

    ## Check for matching unique identifiers of PBx and pltx
    PBx <- check.matchval(PBx, pltx, plotid, puniqueid, tab1txt="pnt",
			tab2txt="plt", subsetrows=TRUE)
    PBx <- PBx[pltx]

    ##############################################################################
    ## Apply plot-level nonsample filter
    ##############################################################################
    PBx <- FIESTA::datFilter(PBx, nonsamp.pfilter, title.filter="plt.nonsample filter",
			filternm="nonsamp.pfilter")$xf


    ######################################################################################
    ## Check for missing variables
    ###########################################################################
    pltnmlst <- names(PBx)
    pvarsmiss <- pvars2keep[which(!pvars2keep %in% pltnmlst)]
    if (length(pvarsmiss) > 0)
      stop("missing variables: ", paste(pvarsmiss, collapse=", "))
    pvars <- pltnmlst[which(pltnmlst %in% c(pvars2keep, pdoms2keep))]


    ######################################################################################
    ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
    ######################################################################################
    if (any(c("PLOT_STATUS_CD", "PSTATUSCD") %in% pltnmlst)) {
      if ("PSTATUSCD" %in% names(PBx))
        names(PBx)[names(PBx) == "PSTATUSCD"] <- "PLOT_STATUS_CD"
      ref_plot_status_cd <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "PLOT_STATUS_CD", ]
      plotsampcnt <- PBx[, list(NBRPLOT=uniqueN(get(puniqueid))), by="PLOT_STATUS_CD"]
    }

    ## Check for NA values in pvars2keep variables
    pvars.na <- sapply(pvars2keep, function(x, PBx){
					sum(is.na(PBx[, x, with=FALSE])) }, PBx)
    if (any(pvars.na > 0)) {
      stop(paste(pvars.na[pvars.na > 0], "NA values in variable:",
		paste(names(pvars.na[pvars.na > 0]), collapse=", ")))
    }
  }

  ############################################################################
  ## Subset variables for pltassgnx, condx, and pltcondx
  ############################################################################
  pvars2keep <- pvars2keep[pvars2keep %in% names(PBx)]
  if (length(pvars2keep) > 0) {
    pltassgnx <- unique(PBx[, c(plotid, pvars2keep), with=FALSE])
    PBx[, (pvars2keep) := NULL]
    pltassgnid <- plotid
  } else {
    pltassgnx <- unique(PBx[, plotid, with=FALSE])
    pltassgnid <- plotid
  }


  ## Add unitvar to plt table if NULL
  if (is.null(unitvar)) {
    unitvar <- checknm("ONEUNIT", names(PBx))
    pltassgnx[, (unitvar) := 1]
    unitvar <- unitvar
  }

  ###################################################################################
  ## CHECK unitarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and area by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  if (is.null(unitarea)) {
    if (sumunits) {
      stop("need unitarea to combine estimation units")
    }
    unitarea <- NULL
  } else {
    removeunits <- ifelse(unit.action == "remove", TRUE, FALSE)
    unitvars <- c(unitvar, unitvar2)
    unitdat <- check.unitarea(unitarea=unitarea, pltx=pltassgnx,
		unitvars=unitvars, areavar=areavar, areaunits=areaunits,
		removeunits=removeunits, removetext=removetext, gui=gui)
    unitarea <- unitdat$unitarea
    areavar <- unitdat$areavar
    areaunits <- unitdat$areaunits
  }


  ## Subset plot-level variables from PBx
  #pdoms2keep <- pdoms2keep[!pdoms2keep %in% pvars2keep]
  #pdoms2keep <- pdoms2keep[pdoms2keep %in% names(PBx)]
  #if (length(pdoms2keep) > 0) {
  #  pltx <- PBx[, unique(c(puniqueid, pdoms2keep)), with=FALSE]
  #} else {
  #  pltx <- NULL
  #}

  ###########################################################################
  ## Check predfac variable(s) and strvar - for factor status
  ###########################################################################
  if (!is.null(predfac) && !is.character(predfac))
    stop("invalid predfac... must be character string")
  if (any(!is.null(c(predfac, strvar)))) {
    notfac <- predfac[!pltassgnx[, lapply(.SD, is.factor), .SDcols=c(predfac, strvar)][[1]]]
    if (length(notfac) > 0) {
      #pltassgnx[, (predfac) := lapply(.SD, as.factor), .SDcols=notfac]

      for (fac in notfac) {
        fac.levels <- sort(unique(pltassgnx[[fac]]))
        pltassgnx[[fac]] <- factor(pltassgnx[[fac]], levels=fac.levels)
      }
    }
  }

  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(PBx=PBx, pltassgnx=pltassgnx, plotid=plotid, pntid=pntid,
	pltassgnid=pltassgnid, unitvar=unitvar, unitvar2=unitvar2, unitarea=unitarea,
	areavar=areavar, areaunits=areaunits, unit.action=unit.action,
	strata=strata, strvar=strvar, strtype=strtype, stratcombine=stratcombine,
 	plotsampcnt=plotsampcnt, getprop=getprop)

  if (!is.null(pltpctx)) {
    returnlst$rowvar <- rowvar
  }

  return(returnlst)
}
