check.popdata <- function(module="GB", method="GREG", tree=NULL, cond, 
	plt=NULL, pltassgn=NULL, dsn=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", 
	condid="CONDID", puniqueid="CN", pltassgnid="CN", pjoinid="CN", evalid=NULL, 
	measCur=FALSE, measEndyr=NULL, measEndyr.filter=NULL, invyrs=NULL, 
	adj="samp", strata=TRUE, unitvar=NULL, unitvar2=NULL, unitarea=NULL, 
	areavar="ACRES", unitcombine=FALSE, strvar="STRATUMCD", getwt=TRUE, 
	getwtvar="P1POINTCNT", nonresp=FALSE, substrvar=NULL, stratcombine=TRUE, 
	prednames=NULL, predfac=NULL, ACI=FALSE, plt.nonsamp.filter=NULL, 
	cond.nonsamp.filter=NULL, nullcheck=FALSE, pvars2keep=NULL, 
	cvars2keep=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs 
  ## Define necessary plot and condition-level variables: 
  ## - plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD, 
  ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC, 
  ##		ECOSUBCD, CONGCD, INTENSITY, DESIGNCD
  ## - plt (pvars2keep) - unitvars, auxvars
  ## - cond (cvars2keep) - CONDPROP_UNADJ
  ## Check module, method (if MA,SA), adj
  ## - module in("GB", "MA", "SA")
  ## - if (module="MA") method in("HT", "PS", "GREG")
  ## - if (module="SA") SApackage <- c("JoSAE", "sae"); method <- c("unit", "area")
  ## Check logical parameters: ACI, unitcombine, strata stratcombine (if strata=TRUE)
  ## - If ACI, add NF_PLOT_STATUS_CD to pvars2keep and NF_COND_STATUS_CD to cvars2keep
  ## - If unitcombine, estimation units are combined if less than 10 plots
  ## - If strata, only 1 auxvar allowed, add to pvars2keep
  ## - If module = SA or MA-GREG, add prednames to pvars2keep
  ## - If adj="samp", nonsample adjustment factors calculated at strata level
  ## - If adj="plot", nonsample adjustment factors calculated at plot level
  ## Check predfac, if module = SA, MA-GREG
  ## Import and check cond, plt, pltassgn tables 
  ## Check corresponding unique identifiers (cuniqueid, puniqueid, pltassgnid)
  ## Merge cond, plt, pltassgn tables to check necessary variables
  ## Check plot data
  ## - Check for missing pvars2keep variables
  ## - Check for missing pdoms2keep variables (STATE, INTENSITY, INVYR, DESIGNCD)
  ## - Get state(s) and inventory year(s) (for titles)
  ## - Generate table of sampled/nonsampled plots (if PLOT_STATUS_CD included)
  ## - If ACI, add table of sampled/nonsampled nonforest plots (if NF_PLOT_STATUS_CD included)
  ## - Generate table of plots by strata, including nonsampled plots (P2POINTCNT)
  ## - If nonresp, generate table of nonsampled plots by strata, substrvar 
  ## - Generate and apply plt.nonsamp.filter (PLOT_STATUS_CD != 3)
  ## - Check for NA values in pvars2keep variables
  ## - If unitvar = NULL, add unitvar=ONEUNIT
  ## Check condition data
  ## - Check condid (NA values and duplicate records (cuniqueid, condid)
  ## - Check for CONDPROP_UNADJ (if not included, add CONDPROP_UNADJ=1 
  ## - Check for COND_STATUS_CD (if not included, add COND_STATUS_CD = PLOT_STATUS_CD with 3=5)
  ## - Generate table of sampled/nonsampled plots and conditions (if COND_STATUS_CD included)
  ## - If ACI, add table of sampled/nonsampled nonforest conditions (if NF_COND_STATUS_CD included)
  ## - IF ACI=FALSE, create ACI.filter="COND_STATUS_CD == 1"
  ## - Generate and apply cond.nonsample filter for condx ("COND_STATUS_CD != 5")
  ## - If ACI, add "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)" 
  ## Check tree data (if tree is not NULL)
  ## - Define necessary tree-level variables (tvars2keep)
  ## - Import tree table and check unique identifier (tuniqueid)
  ## - Check for condid in tree... if no condid, add CONDID=1
  ## - Check if class of tuniqueid matches class of cuniqueid in cond
  ## - Check if all values of tree are in cond and subset rows to match cond
  ## - Check for missing tvars2keep and NA values in tvars2keep
  ## - Add necessary variables to cvars2keep depending on data in tree
  ##   If trees in subplot (TPA_UNADJ > 5 & < 10), add SUBPPROP_UNADJ to cvars2keep
  ##     If no SUBPPROP_UNADJ in cond, add a variable SUBPROP_UNADJ=1 (100%)
  ##   If trees in microplot (TPA_UNADJ > 50), add MICRPROP_UNADJ to cvars2keep 
  ##     If no MICRPROP_UNADJ in cond, add a variable MICRPROP_UNADJ=1 (100%)
  ##   If trees in macroplot (TPA_UNADJ > 0 & < 5), add MACRPROP_UNADJ to cvars2keep 
  ##     If no MACRPROP_UNADJ in cond, add a variable MACRPROP_UNADJ=1 (100%)
  ## Subset variables for pltassgnx, condx, and pltcondx
  ###################################################################################

  ## Set global variables
  CONDPROP_UNADJ=COND_STATUS_CD=CONDID=SUBPPROP_UNADJ=MICRPROP_UNADJ=MACRPROP_UNADJ=
	STATECD=PLOT_STATUS_CD=PSTATUSCD=cndnmlst=pltdomainlst=invyrs=PROP_BASIS=
	ACI.filter=V1=ONEUNIT=plotsampcnt=nfplotsampcnt=condsampcnt=INVYR=
	NF_PLOT_STATUS_CD=NF_COND_STATUS_CD=TPA_UNADJ=methodlst=nonsampplots=
	plotqry=condqry=treeqry <- NULL


  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  pvars2keep <- unique(c(unitvar, unitvar2, pvars2keep))
  cvars2keep <- unique(c(cvars2keep, "CONDPROP_UNADJ"))

  pdoms2keep <- unique(c("STATECD", "UNITCD", "COUNTYCD", "INVYR", 
	"MEASYEAR", "PLOT_STATUS_CD", "PSTATUSCD", "RDDISTCD", "WATERCD", "ELEV", 
	"ELEV_PUBLIC", "ECOSUBCD", "CONGCD", "INTENSITY", "DESIGNCD"))
  pdoms2keep <- pdoms2keep[!pdoms2keep %in% pvars2keep]

  ###################################################################################
  ## Check module, method, adj
  ###################################################################################

  ## Check estimator module 
  ########################################################
  modulelst <- c("GB", "MA", "SA")
  module <- FIESTA::pcheck.varchar(var2check=module, varnm="module", gui=gui, 
		checklst=modulelst, caption="FIESTA module", stopifnull=TRUE)

  ## Check method 
  ########################################################
  if (module %in% c("MA", "SA")) {
    if (module == "MA") {
      if (!"mase" %in% rownames(installed.packages()))
	   stop("MA module requires package mase.")
      methodlst <- c("HT", "PS", "GREG")
      method <- FIESTA::pcheck.varchar(var2check=method, varnm="method", gui=gui, 
		checklst=methodlst, caption="method", multiple=FALSE, stopifnull=TRUE)
      if (method == "PS") strata <- TRUE
    } else if (module == "SA") {
      if (!any(c("JoSAE", "sae") %in% rownames(installed.packages())))
        stop("SA module requires either package JoSAE or sae.")
    }
  }

  ## Check adj
  ########################################################
  adjlst <- c("samp", "plot", "none")
  adj <- FIESTA::pcheck.varchar(var2check=adj, varnm="adj", gui=gui, 
		checklst=adjlst, caption="adj", multiple=FALSE, stopifnull=TRUE)

  ## Check adj
  ########################################################
  if (adj == "samp" && module == "SA")  {
    message("adj='samp' is currently invalid for SA module... adjusting for plot")
    adj <- "plot"
  }
  if (adj == "plot" && module == "GB") 
    message("adj='plot' is not typical for GA modules")


  ###################################################################################
  ## Check logical parameters: unitcombine, strata, ACI
  ###################################################################################


  ## Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
  ###################################################################################
  ACI <- FIESTA::pcheck.logical(ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
  if (ACI) 
    pdoms2keep <- unique(c(pdoms2keep, "NF_PLOT_STATUS_CD"))

  ## Check unitcombine 
  ########################################################
  unitcombine <- FIESTA::pcheck.logical(unitcombine, varnm="unitcombine", 
		title="Combine estimation units?", first="YES", gui=gui, stopifnull=TRUE)

 
  ## Check strata, strvars
  ###################################################################################
  if (module == "GB" || (module == "MA" && method == "PS")) {
    strata <- FIESTA::pcheck.logical(strata, varnm="strata", 
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)
 
    if (strata) {
      if (is.null(strvar)) stop("must include strvar for post-strat estimates")
      if (length(strvar) > 1) stop("invalid strvar... only 1 variable allowed")
      pvars2keep <- unique(c(pvars2keep, strvar))

      if (module == "MA" && method == "PS") {
        if (!strvar %in% predfac) predfac <- strvar
        prednames <- NULL 
      }

      ## Check nonresp
      nonresp <- FIESTA::pcheck.logical(nonresp, varnm="nonresp", 
		title="Post stratify?", first="YES", gui=gui)
      if (nonresp) {
        pvars2keep <- c(pvars2keep, substrvar)
      } else {
        substrvar <- NULL
      }

      ## Check stratcombine 
      ########################################################
      stratcombine <- FIESTA::pcheck.logical(stratcombine, varnm="stratcombine", 
		title="Combine strata?", first="YES", gui=gui, stopifnull=TRUE)

    } else {
      strvar <- NULL
    }
  } else if (module == "MA" && method == "HT") {
    if (!is.null(strvar)) strvar <- NULL
    if (!is.null(prednames)) prednames <- NULL
    if (!is.null(predfac)) predfac <- NULL
  } else {
    strvar <- NULL
    if (is.null(prednames)) 
      stop("prednames is null... must include at least one variable name")
    pvars2keep <- c(pvars2keep, prednames)
  }

  ## Check predfac
  ###################################################################################
  if (module == "SA" || (module == "MA" && method == "GREG"))  {
    if (!is.null(predfac) && !is.character(predfac)) 
      stop("invalid predfac... must be character string")

    notin <- predfac[!predfac %in% prednames]
    if (length(notin) > 0) {
      warning("invalid predfac... not in prednames: ", toString(notin))
      predfac <- predfac[predfac %in% prednames]
      if (length(predfac) == 0) predfac <- NULL
    }
  } 

  ## Check dsn and create queries to get population subset from database
  ###################################################################################
  if (!is.null(dsn) && getext(dsn) == "sqlite") {
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
    tablst <- DBI::dbListTables(dbconn)
    chk <- TRUE
    SCHEMA. <- NULL
  
    ## Filter for population data
    if (!is.null(evalid)) {
      pfromqry <- getpfromqry(evalid, dsn=dsn)
      whereqry <- paste0("where EVALID in(", toString(evalid), ")")
    } else if (measCur) {
      pfromqry <- getpfromqry(varCur="MEASYEAR", Endyr=measEndyr, dsn=dsn)
      whereqry <- ""
    } else if (!is.null(invyrs)) {
      pfromqry <- getpfromqry(invyrs=invyrs, dsn=dsn)
      whereqry <- paste0("where invyrs in(", toString(invyrs), ")")
    }
    cfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"cond c ON (c.PLT_CN = p.CN)")
    tfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
				"tree t ON (t.PLT_CN = p.CN)")

    if (plt %in% tablst) 
      plotqry <- paste("select p.* from", pfromqry, whereqry)
    if (cond %in% tablst) 
      condqry <- paste("select c.* from", cfromqry, whereqry)
    if (pltassgn %in% tablst) 
      pltassgnqry <- paste("select ppsa.* from", pfromqry, whereqry)
    if (tree %in% tablst) 
      treeqry <- paste("select t.* from", tfromqry, whereqry)

  }
 
  ###################################################################################
  ## Import plt and cond table and check unique identifiers
  ###################################################################################
  condx <- pcheck.table(cond, tab_dsn=dsn, tabnm="cond", caption="cond table?", 
		nullcheck=nullcheck, tabqry=condqry, returnsf=FALSE)
  pltx <- pcheck.table(plt, tab_dsn=dsn, tabnm="plt", caption="plot table?", 
		nullcheck=nullcheck, tabqry=plotqry, returnsf=FALSE)
  pltassgnx <- pcheck.table(pltassgn, tab_dsn=dsn, tabnm="pltassgn", 
		caption="plot assignments?", nullcheck=nullcheck, tabqry=pltassgnqry,
		returnsf=FALSE)

  if (is.null(condx) && is.null(pltx) && is.null(pltassgnx)) 
    stop("must include plt or cond table")

  if (!is.null(condx)) {
    cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", gui=gui, 
		checklst=names(condx), caption="Unique identifier of plot", 
		warn=paste(cuniqueid, "not in cond table"), stopifnull=TRUE)
    setkeyv(condx, cuniqueid)

    ## Check for NA values in necessary variables in cond table
    condx.na <- sum(is.na(condx[[cuniqueid]]))
    if (condx.na > 0) stop("NA values in ", cuniqueid)
  }

  ## Define cdoms2keep
  cdoms2keep <- names(condx)

  ###################################################################################
  ## Check and merge plt, pltassgn, cond
  ###################################################################################
  if (!is.null(pltx) || !is.null(pltassgnx)) {
    if (!is.null(pltx)) {
      pltnmlst <- names(plt)
      puniqueid <- pcheck.varchar(var2check=puniqueid, varnm="puniqueid", gui=gui, 
		checklst=names(pltx), caption="UniqueID variable of plot", 
		warn=paste(puniqueid, "not in plt table"), stopifnull=TRUE)
      if (any(duplicated(pltx[[puniqueid]]))) {
        dups <- plt[[puniqueid]][duplicated(plt[[puniqueid]])]
        warning(paste("plt records are not unique in: plt:", toString(dups)))
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
		warn=paste(pltassgnid, "not in pltassgn"), stopifnull=TRUE)
      if (any(duplicated(pltassgnx[[pltassgnid]]))) 
        warning("plot records are not unique in: pltassgn")
      
      ## Check for NA values in necessary variables in plt table
      pltassgnx.na <- sum(is.na(pltassgnx[[pltassgnid]]))
      if (pltassgnx.na > 0) stop("NA values in ", pltassgnid)

      ## Set key
      setkeyv(pltassgnx, pltassgnid)
    }

    ## Merge plot and pltassgn tables
    #########################################################
    if (!is.null(pltx) && !is.null(pltassgnx)) {
      pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", gui=gui, 
		checklst=names(pltx), caption="Joinid variable in plot", 
		warn=paste(pjoinid, "not in plt table"), stopifnull=TRUE)
      setkeyv(pltx, pjoinid)

      pltassgnx <- pltassgnx[, unique(c(pltassgnid, 
		names(pltassgnx)[!names(pltassgnx) %in% names(pltx)])), with=FALSE]
      setkeyv(pltassgnx, pltassgnid)

      ## Check if class of pjoinid in pltx matches class of pltassgnid in pltassgnx
      tabs <- check.matchclass(pltx, pltassgnx, pjoinid, pltassgnid)
      pltx <- tabs$tab1
      pltassgnx <- tabs$tab2

      ## Merge pltx and pltassgnx
      pltx <- merge(pltx, pltassgnx, by.x=pjoinid, by.y=pltassgnid)

    } else if (is.null(pltx)) {
      pltx <- pltassgnx
      puniqueid <- pltassgnid
    }
 
    ##################################################################################
    ## Filter for population data
    ##################################################################################
    if (!is.null(evalid)) {
      if (!"EVALID" %in% names(pltx)) stop("EVALID not in pltx") 
      if (!is.null(evalid)) {
        if (!all(evalid %in% unique(pltx[["EVALID"]]))) {
          evalid.miss <- evalid[which(!evalid %in% unique(pltx[["EVALID"]]))]
          message("evalid not in dataset: ", paste(evalid.miss, collapse=", "))
          if (length(evalid.miss) == length(evalid)) stop("")
          evalid <- evalid[!evalid %in% evalid.miss]
        } 
      }
      pltx <- datFilter(pltx, getfilter("EVALID", evalid, syntax="R"))$xf

    } else if (measCur || !is.null(measEndyr)) {
      pltx <- getPlotCur(pltx, Endyr=measEndyr, varCur="MEASYEAR", 
				Endyr.filter=measEndyr.filter)

    } else if (!is.null(invyrs)) {
      if (!"INVYR" %in% names(pltx)) stop("INVYR not in pltx")
      if (!all(invyrs %in% unique(pltx[["INVYR"]]))) {
        invyrs.miss <- invyrs[which(!invyrs %in% unique(pltx[["INVYR"]]))]
        message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
        if (length(invyrs.miss) == length(invyrs)) stop("")
        invyrs <- invyrs[!invyrs %in% invyrs.miss]
      } 
      pltx <- datFilter(x=pltx, xfilter=paste("INVYR %in% c(", toString(invyrs), ")"))$xf
    }

    ## Merge plot data to cond
    #########################################################
    if (!is.null(condx)) {
      ## Check if class of puniqueid in pltx matches class of puniqueid in condx
      tabs <- check.matchclass(condx, pltx, cuniqueid, puniqueid)
      condx <- tabs$tab1
      pltx <- tabs$tab2

      ## Check for matching unique identifiers of condx and pltx
      condx <- check.matchval(condx, pltx, cuniqueid, puniqueid, tab1txt="cond",
			tab2txt="plt", subsetrows=TRUE)
      
      pltcols <- unique(c(puniqueid, names(pltx)[!names(pltx) %in% names(condx)]))
      pltcondx <- merge(condx, pltx[, pltcols, with=FALSE], all.y=TRUE, 
				by.x=cuniqueid, by.y=puniqueid)
    } else {
      pltcondx <- pltx
      cuniqueid <- puniqueid
    }
  } else {
    pltcondx <- condx
  }

  ###################################################################################
  ###################################################################################
  ## Check plot data
  ###################################################################################
  ###################################################################################

  ######################################################################################
  ## Check for missing plot variables
  ###########################################################################
  pltcondnmlst <- names(pltcondx)
  pvarsmiss <- pvars2keep[which(!pvars2keep %in% pltcondnmlst)]
  if (length(pvarsmiss) > 0) 
    stop("missing variables: ", paste(pvarsmiss, collapse=", "))
  pvars <- pltcondnmlst[which(pltcondnmlst %in% c(pvars2keep, pdoms2keep))]


  ###########################################################################
  ## Check missing pdoms2keep variables in pltcondx
  ###########################################################################
  pltcondnmlst <- names(pltcondx) 
  pmissvars <- pdoms2keep[which(!pdoms2keep %in% pltcondnmlst)]
  if (length(pmissvars) > 0) {
    if ("STATECD" %in% pmissvars) 
      message("STATECD not in dataset.. assuming 1 state in dataset")
    if ("INTENSITY" %in% pmissvars) 
      message("INTENSITY not in dataset.. assuming single intensity plots")
    if ("INVYR" %in% pmissvars) 
      message("INVYR not in dataset.. assuming inventory years do not span more ",
		"than 1 cycle of measurements")
    if (!"DESIGNCD" %in% pltcondnmlst) {
      message("DESIGNCD not in dataset.. assuming DESIGNCD = 1 (annual inventory)")
      designcd <- 1
    } else {
      ##################################################################################
      ## NOTE: If adj='samp', make sure dataset is annual inventory only (DESIGNCD=1). 
      ## Only adjust plots when DESIGNCD=1. Cannot have more than 1 DESIGNCD.
      ##################################################################################
      designcd <- unique(pltcondx[["DESIGNCD"]])
      if (length(designcd) != 1) {
        stop("more than 1 plot design, calculate separate estimates by design")
      } else if (adj == "samp" && designcd != 1) {
        stop("samp adjustment for trees is only for designcd = 1 (annual inventory)") 
      }
    }

    pmissvars <- pmissvars[which(!pmissvars %in% pltcondnmlst)]

    if (any(prednames %in% pmissvars)) {
      prednames[which(!prednames %in% pmissvars)]
      stop("predname not in tables: ", paste(prednames, collapse=", "))
    }
    unitvars <- c(unitvar, unitvar2)
    if (any(unitvars %in% pmissvars)) {
      unitvars[which(!unitvars %in% pmissvars)]
      stop("unitvar not in tables: ", paste(unitvars, collapse=", "))
    }
  }
  #pdoms2keep <- unique(pdoms2keep[which(!pdoms2keep %in% pmissvars)])
  

  ## Get state(s) and inventory year(s) (for titles)
  ############################################################
  states <- NULL
  invyrs <- NULL
  if ("STATECD" %in% names(pltcondx)) {
    stcds <- unique(pltcondx[["STATECD"]])
    states <- FIESTA::pcheck.states(stcds)
    if ("INVYR" %in% names(pltcondx)) {
      invyrtab <- unique(pltcondx[, c("STATECD", "INVYR")])
      invyrs <- as.list(by(invyrtab$INVYR, invyrtab$STATECD, I))
      names(invyrs) <- FIESTA::pcheck.states(names(invyrs))
    }
  } 

  ######################################################################################
  ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
  ######################################################################################
  if (any(c("PLOT_STATUS_CD", "PSTATUSCD") %in% pltcondnmlst)) {
    if ("PSTATUSCD" %in% names(pltcondx)) 
      names(pltcondx)[names(pltcondx) == "PSTATUSCD"] <- "PLOT_STATUS_CD"
    ref_plot_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "PLOT_STATUS_CD", ]
    plotsampcnt <- pltcondx[, list(NBRPLOT=uniqueN(get(cuniqueid))), by=PLOT_STATUS_CD]
    plotsampcnt <- 
	cbind(PLOT_STATUS_NM=ref_plot_status_cd[match(plotsampcnt$PLOT_STATUS_CD, 
	ref_plot_status_cd$VALUE), "MEANING"], plotsampcnt)
    setkey(plotsampcnt, PLOT_STATUS_CD)
  } else {
    if (nonresp) stop("PLOT_STATUS_CD must be in dataset if nonresp=TRUE")
    message("PLOT_STATUS_CD not in dataset.. assuming all plots are at least ", 
			"partially sampled")
  }

  if (ACI) {
    if (any(c("NF_PLOT_STATUS_CD", "PSTATUSNF") %in% pltcondnmlst)) {
      if ("PSTATUSNF" %in% names(pltcondx)) 
        names(pltcondx)[names(pltcondx) == "PSTATUSNF"] <- "NF_PLOT_STATUS_CD"
      ref_nf_plot_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "NF_PLOT_STATUS_CD", ]
      nfplotsampcnt <- pltcondx[, list(NBRPLOT=uniqueN(get(cuniqueid))), by=NF_PLOT_STATUS_CD]
      nfplotsampcnt <- 
	 	cbind(NF_PLOT_STATUS_NM=ref_nf_plot_status_cd[match(nfplotsampcnt$NF_PLOT_STATUS_CD, 
		ref_nf_plot_status_cd$VALUE), "MEANING"], nfplotsampcnt)
      setkey(nfplotsampcnt, NF_PLOT_STATUS_CD)
      nfplotsampcnt <- nfplotsampcnt[!is.na(NF_PLOT_STATUS_CD), ]
    } else {
      message("NF_PLOT_STATUS_CD not in dataset.. assuming all ACI nonforest plots are at least ", 
			"partially sampled")
    }
    if (!is.null(plotsampcnt)) {
      plotsampcnt <- rbindlist(list(plotsampcnt, nfplotsampcnt), use.names=FALSE)
    } else {
      plotsampcnt <- nfplotsampcnt
    }
  }


  ######################################################################################
  ## Check unitvar - if NULL, add unitvar=ONEUNIT to pltcondx
  ######################################################################################
  if (is.null(unitvar)) {
    pltcondx[, ONEUNIT := 1] 
    unitvar <- "ONEUNIT"
    pvars2keep <- c(unitvar, pvars2keep)
    unitvars <- unitvar
  }

  ######################################################################################
  ## Generate table of plots by strata, including nonsampled plots (P2POINTCNT)
  ######################################################################################
  if (strata) {
    P2POINTCNT <- pltcondx[, uniqueN(get(cuniqueid)), by=c(unitvars, strvar)]
    setnames(P2POINTCNT, "V1", "P2POINTCNT")
    
    if ("PLOT_STATUS_CD" %in% pltcondnmlst) {
      ## Generate table of nonsampled plots by strata (if nonresp=TRUE)
      if (nonresp && "PLOT_STATUS_CD" %in% pltcondnmlst) {
        if (!3 %in% unique(pltcondx[["PLOT_STATUS_CD"]]))
          stop("must include PLOT_STATUS_CD = 3 in dataset") 

        ## Create table with number of nonsampled plots by strata, substrata
        nonsampplots <- pltcondx[PLOT_STATUS_CD == 3, uniqueN(get(cuniqueid)), 
					by=c(strvar, substrvar)]
        setkeyv(nonsampplots, c(strvar, substrvar))
      }
    }
  } else {
    P2POINTCNT <- pltcondx[, uniqueN(get(cuniqueid)), unitvars]
    setnames(P2POINTCNT, "V1", "P2POINTCNT")
  }

  #############################################################################
  ## Generate and apply plt.nonsamp.filter 
  #############################################################################
  if ((is.null(plt.nonsamp.filter) || plt.nonsamp.filter == "") && adj != "none") {
    if ("PLOT_STATUS_CD" %in% names(pltcondx)) {
      plt.nonsamp.filter <- "PLOT_STATUS_CD != 3"
      if (sum(pltcondx$PLOT_STATUS_CD == 3) > 0)
        message("removing ", sum(pltcondx$PLOT_STATUS_CD == 3), " nonsampled forest plots")
    }
  }
  ## Apply plt.nonsamp.filter
  if (!is.null(plt.nonsamp.filter) && plt.nonsamp.filter != "NONE") {
    pltcondx <- datFilter(x=pltcondx, xfilter=plt.nonsamp.filter, 
		title.filter="plt.nonsamp.filter", gui=gui)$xf
    if (is.null(pltcondx)) {
      message(paste(plt.nonsamp.filter, "removed all records"))
      return(NULL)
    }
  }

  ## Check for NA values in pvars2keep variables
  pvars.na <- sapply(pvars2keep, function(x, pltcondx){ 
					sum(is.na(pltcondx[, x, with=FALSE])) }, pltcondx)
  if (any(pvars.na > 0))
    stop(paste(pvars.na[pvars.na > 0], "NA values in variable:", 
		paste(names(pvars.na[pvars.na > 0]), collapse=", ")))



  ###################################################################################
  ###################################################################################
  ## Check condition data
  ###################################################################################
  ###################################################################################


  ###################################################################################
  ## Check condition data
  ###################################################################################
  if (is.null(condid) || (!is.null(condid) && !condid %in% pltcondnmlst)) {
    ## If condid = NULL, add a variable CONDID=1 to cond
    if (nrow(pltcondx) == length(unique(pltcondx[[cuniqueid]]))) {
      message("assuming one condition, adding CONDID=1 for 1 condition per plot")
      pltcondx[, CONDID := 1]
      condid <- "CONDID"
    } else { 
      stop("only 1 record for each cuniqueid allowed")
    }
  } else {
    ## Check for NA values in condid
    condid.na <- sum(is.na(pltcondx[[cuniqueid]]))
    if (condid.na > 0) stop("NA values in ", cuniqueid)
  }

  ## Check if 1 plot-condition per record in cond
  ######################################################
  condid.dupid <- pltcondx[duplicated(pltcondx, by=c(cuniqueid, condid))][[cuniqueid]]
  if (length(condid.dupid) > 0) {
    msg <- paste("check cuniqueid/condid... duplicate records")
    if (length(condid.dupid) < 20) print(condid.dupid)
    stop(msg)
  }

  #############################################################################
  ## Check for necessary cond variables in cond table 
  #############################################################################

  ## If CONDPROP_UNADJ not in cond table and only 1 condition per plot, 
  ## 	add CONDPROP_UNADJ and set = 1 (100 percent)
  if (!"CONDPROP_UNADJ" %in% pltcondnmlst) {
    ## If only 1 condition, check CONDPROP_UNADJ
    if (nrow(pltcondx) == length(unique(pltcondx[[cuniqueid]]))) {
      message("CONDPROP_UNADJ not in dataset.. assuming CONDPROP_UNADJ = 1") 
      pltcondx[, CONDPROP_UNADJ := 1]  
    }
  }
  pltcondx$CONDPROP_UNADJ <- check.numeric(pltcondx$CONDPROP_UNADJ)


  ## Check for COND_STATUS_CD and create ACI filter
  #############################################################################
  if (!"COND_STATUS_CD" %in% pltcondnmlst) {
    if (length(unique(pltcondx[[cuniqueid]])) == nrow(pltcondx) && 
		"PLOT_STATUS_CD" %in% pltcondnmlst) {
      message("COND_STATUS_CD not in dataset.. using PLOT_STATUS_CD for COND_STATUS_CD")
      pltcondx[, COND_STATUS_CD := PLOT_STATUS_CD]
      pltcondx[COND_STATUS_CD == 3, COND_STATUS_CD := 5]
    } else {
      message("COND_STATUS_CD not in dataset.. assuming all sampled conditions")
      cvars2keep <- cvars2keep[cvars2keep != "COND_STATUS_CD"]
    }
  }

  #############################################################################
  ## Generate table of sampled/nonsampled conditions from condx
  #############################################################################
  if ("COND_STATUS_CD" %in% pltcondnmlst) {
    condsampcnt <- pltcondx[, list(NBRCOND=.N), by=COND_STATUS_CD]
    ref_cond_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "COND_STATUS_CD", ]
 
    condsampcnt <- 
	cbind(COND_STATUS_NM=ref_cond_status_cd[match(condsampcnt$COND_STATUS_CD, 
	ref_cond_status_cd$VALUE), "MEANING"], condsampcnt)
    setkey(condsampcnt, COND_STATUS_CD)
    
    if (!ACI) ACI.filter <- "COND_STATUS_CD == 1"
  } 
  if (ACI) {
    if ("NF_COND_STATUS_CD" %in% pltcondnmlst) {
      ref_nf_cond_status_cd <- FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "NF_COND_STATUS_CD", ]
      nfcondsampcnt <- pltcondx[, list(NBRCOND=.N), by=NF_COND_STATUS_CD]
      nfcondsampcnt <- 
	 	cbind(NF_COND_STATUS_NM=ref_nf_cond_status_cd[match(nfcondsampcnt$NF_COND_STATUS_CD, 
		ref_nf_cond_status_cd$VALUE), "MEANING"], nfcondsampcnt)
      setkey(nfcondsampcnt, NF_COND_STATUS_CD)
      nfcondsampcnt <- nfcondsampcnt[!is.na(NF_COND_STATUS_CD), ]
      condsampcnt <- rbindlist(list(condsampcnt, nfcondsampcnt), use.names=FALSE)
    } else {
      message("NF_COND_STATUS_CD not in dataset.. assuming all sampled nonforest conditions")
    }
  }

  #############################################################################
  ## Generate and apply cond.nonsamp.filter 
  #############################################################################
  if ((is.null(cond.nonsamp.filter) || cond.nonsamp.filter == "") && adj != "none") {
    if ("COND_STATUS_CD" %in% pltcondnmlst) {
      cond.nonsamp.filter <- "COND_STATUS_CD != 5"
      message("removing ", sum(pltcondx$COND_STATUS_CD == 5, na.rm=TRUE), 
		" nonsampled forest conditions")
    }
    if (ACI && "NF_COND_STATUS_CD" %in% pltcondnmlst) {
      cond.nonsamp.filter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
      message("removing ", sum(is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 5, na.rm=TRUE), 
		" nonsampled nonforest conditions")
      if (!is.null(cond.nonsamp.filter)) 
        cond.nonsamp.filter <- paste(cond.nonsamp.filter, "&", cond.nonsamp.filter.ACI)
    }
  } 
  ## Apply cond.nonsamp.filter
  if (!is.null(cond.nonsamp.filter) && cond.nonsamp.filter != "NONE") {
    pltcondx <- datFilter(x=pltcondx, xfilter=cond.nonsamp.filter, 
		title.filter="cond.nonsamp.filter", gui=gui)$xf
    if (is.null(pltcondx)) {
      message(paste(cond.nonsamp.filter, "removed all records"))
      return(NULL)
    }
  }

  ###################################################################################
  ###################################################################################
  ## Check tree data
  ###################################################################################
  ###################################################################################

  ###################################################################################
  ## Import and check tree data table
  ###################################################################################
  treex <- pcheck.table(tree, tab_dsn=dsn, tabnm="tree", caption="Tree table?", 
		nullcheck=nullcheck, gui=gui, tabqry=treeqry, returnsf=FALSE)
  if (!is.null(treex)) {
    ## Define necessary variable for tree table
    tvars2keep <- "TPA_UNADJ"
    treenmlst <- names(treex)

    ## Check unique identifiers
    tuniqueid <- FIESTA::pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui, 
		checklst=treenmlst, caption="UniqueID variable of plot", 
		warn=paste(tuniqueid, "not in tree"), stopifnull=TRUE)

    ## Check for NA values in necessary variables in tree table
    treex.na <- sum(is.na(treex[[tuniqueid]]))
    if (treex.na > 0) stop("NA values in ", tuniqueid)

    if (tuniqueid %in% pltcondnmlst) {
      idplace <- which(pltcondnmlst %in% tuniqueid)
      if (idplace != 1) { 
	  pltcondnmlst <- c(tuniqueid, pltcondnmlst) 
	  pltcondnmlst <- pltcondnmlst[-(idplace + 1)] 
      }
    } 

    ## Check for condid in tree
    if (!condid %in% names(treex)) {
      if (nrow(treex) == length(unique(treex[[tuniqueid]]))) {
        treex[, CONDID := 1]
      } else { 
        stop("only 1 record for each tuniqueid allowed")
      }
    } else {
      ## Check for NA values in condid
      treex.na <- sum(is.na(treex[, tuniqueid, with=FALSE]))
      if (treex.na > 0) stop("NA values in ", tuniqueid)
    }
    setkeyv(treex, c(tuniqueid, condid))

    ## Check if class of tuniqueid in treex matches class of cuniqueid in condx
    tabs <- FIESTA::check.matchclass(pltcondx, treex, cuniqueid, tuniqueid)
    pltcondx <- tabs$tab1
    treex <- tabs$tab2

    ## Check for missing tvars2keep 
    tmissvars <- tvars2keep[which(!tvars2keep %in% treenmlst)]
    if (length(tmissvars) > 0)
      stop("missing necessary variables from tree: ", paste(tmissvars, collapse=", "))

    ## Check for NA values in tvars2keep variables
    ## TPA_UNADJ=NA, but trees have a DIA 
    ## these are down dead trees that only count in growth and mortality, 
    ## but wouldn't be measured if they hadn't been alive at the previous inventory

    tvars2keep2 <- tvars2keep[tvars2keep != "TPA_UNADJ"]
    if (length(tvars2keep) > 0) {
      tvars.na <- sapply(c(tuniqueid, condid, tvars2keep2), 
		function(x, treex){ sum(is.na(treex[,x, with=FALSE])) }, treex)
      if (any(tvars.na) > 0) 
        stop(tvars.na[tvars.na > 0], " NA values in variable: ", 
		paste(names(tvars.na[tvars.na > 0]), collapse=", "))
    }
 
    ## Add necessary variables to cvars2keep depending on data in tree
    ###################################################################
    if (adj != "NONE") {
      ## Check for condition proportion variables
      propchk <- check.PROP(treex, pltcondx, checkNA=FALSE)
      propvars <- propchk$propvars
      treex <- propchk$treex
      pltcondx <- propchk$condx
      cvars2keep <- unique(c(cvars2keep, propvars))
    }
  }  
 
  ############################################################################
  ## Subset variables for pltassgnx, condx, and pltcondx
  ############################################################################
  pltassgnx <- unique(pltcondx[, c(cuniqueid, pvars2keep), with=FALSE])
  pltassgnid <- cuniqueid
  pltcondx[, (pvars2keep) := NULL]

  condx <- unique(pltcondx[, c(cuniqueid, condid, cvars2keep), with=FALSE])
  pltcondx[, (cvars2keep) := NULL]

  ## Subset pltcondx
  #cdoms2keep <- cdoms2keep[!cdoms2keep %in% c(pvars2keep, cvars2keep)] 
  #pltcondx <- pltcondx[, unique(c(cuniqueid, condid, pdoms2keep, cdoms2keep)), with=FALSE]

 
  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(condx=condx, pltcondx=pltcondx, pltassgnx=pltassgnx, 
	cuniqueid=cuniqueid, condid=condid, pltassgnid=pltassgnid, unitvar=unitvar, 
	unitvar2=unitvar2, unitcombine=unitcombine, prednames=prednames, predfac=predfac,
	adj=adj, strata=strata, strvar=strvar, stratcombine=stratcombine, nonresp=nonresp,
 	P2POINTCNT=P2POINTCNT, plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
	states=states, invyrs=invyrs, ACI.filter=ACI.filter)

  if (!is.null(treex)) {
    ## Check that the values of tuniqueid in treex are all in cuniqueid in condf
    treef <- check.matchval(treex, pltcondx, tuniqueid, cuniqueid, tab1txt="tree", 
		tab2txt="cond", subsetrows=TRUE)
    returnlst$treef <- treef
    returnlst$tuniqueid <- tuniqueid
  }
  if (module == "MA") returnlst$method <- method
  if (ACI) returnlst$nfplotsampcnt <- nfplotsampcnt
  if (nonresp) {
    returnlst$substrvar <- substrvar 
    returnlst$nonsampplots <- nonsampplots
  }
  
  return(returnlst)
}
