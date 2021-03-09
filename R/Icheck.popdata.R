check.popdata <- function(module="GB", method="greg", popType="VOL", 
	tree=NULL, cond, subplot=NULL, subp_cond=NULL, plt=NULL, seed=NULL, 
	vspspp=NULL, pltassgn=NULL, dsn=NULL, tuniqueid="PLT_CN", 
	cuniqueid="PLT_CN", condid="CONDID", areawt="CONDPROP_UNADJ", puniqueid="CN", 
	pltassgnid="CN", pjoinid="CN", evalid=NULL, measCur=FALSE, measEndyr=NULL,
	measEndyr.filter=NULL, invyrs=NULL, intensity=NULL, adj="samp", 
	strata=TRUE, unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", 
	unitcombine=FALSE, strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", 
	nonresp=FALSE, substrvar=NULL, stratcombine=TRUE, prednames=NULL, 
	predfac=NULL, ACI=FALSE, plt.nonsamp.filter=NULL, cond.nonsamp.filter=NULL, 
	vegplt.nonsamp.filter=NULL, nullcheck=FALSE, pvars2keep=NULL, cvars2keep=NULL, 
	ppsanm="pop_plot_stratum_assgn", gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs 
  ## Define necessary plot and condition-level variables: 
  ## - plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD, 
  ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC, 
  ##		ECOSUBCD, CONGCD, INTENSITY, DESIGNCD
  ## - plt (pvars2keep) - unitvars, auxvars
  ## - cond (cvars2keep) - areawt
  ## Check module, method (if MA,SA), adj
  ## - module in("GB", "MA", "SA")
  ## - if (module="MA") method in("HT", "PS", "greg", "gregEN")
  ## - if (module="SA") SApackage <- c("JoSAE", "sae"); method <- c("unit", "area")
  ## Check logical parameters: ACI, unitcombine, strata stratcombine (if strata=TRUE)
  ## - If ACI, add NF_PLOT_STATUS_CD to pvars2keep and NF_COND_STATUS_CD to cvars2keep
  ## - If unitcombine, estimation units are combined if less than 10 plots
  ## - If strata, only 1 auxvar allowed, add to pvars2keep
  ## - If module = SA or MA-greg, add prednames to pvars2keep
  ## - If adj="samp", nonsample adjustment factors calculated at strata level
  ## - If adj="plot", nonsample adjustment factors calculated at plot level
  ## Check predfac, if module = SA, MA-greg
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
  ## - Check for areawt (if not included, add CONDPROP_UNADJ=1 
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
  COND_STATUS_CD=CONDID=CONDPROP_UNADJ=SUBPPROP_UNADJ=MICRPROP_UNADJ=MACRPROP_UNADJ=
	STATECD=PLOT_STATUS_CD=PSTATUSCD=cndnmlst=pltdomainlst=invyrs=PROP_BASIS=
	ACI.filter=V1=ONEUNIT=plotsampcnt=nfplotsampcnt=condsampcnt=INVYR=
	NF_PLOT_STATUS_CD=NF_COND_STATUS_CD=TPA_UNADJ=methodlst=nonresplut=
	plotqry=condqry=treeqry=pfromqry=pltassgnqry=cfromqry=tfromqry=
	vspsppqry=subplotqry=subp_condqry=NF_SUBP_STATUS_CD <- NULL


  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  pvars2keep <- unique(c(unitvar, unitvar2, pvars2keep))
  cvars2keep <- unique(c(cvars2keep, areawt))
  vuniqueid <- "PLT_CN" 

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
	   stop("MA module requires package mase")
      methodlst <- c("HT", "PS", "greg", "gregEN")
      method <- FIESTA::pcheck.varchar(var2check=method, varnm="method", gui=gui, 
		checklst=methodlst, caption="method", multiple=FALSE, stopifnull=TRUE)
      if (any(method == "PS")) strata <- TRUE
    } else if (module == "SA") {
      if (!any(c("JoSAE", "sae") %in% rownames(installed.packages())))
        stop("SA module requires either package JoSAE or sae")
    }
  }

  ## Check popType
  ########################################################
  evalTyplst <- c("ALL", "VOL", "P2VEG")
  popType <- FIESTA::pcheck.varchar(var2check=popType, varnm="popType", gui=gui, 
		checklst=evalTyplst, caption="popType", multiple=TRUE, stopifnull=TRUE)
  if ("P2VEG" %in% popType) {
    pvars2keep <- c(pvars2keep, "P2VEG_SAMPLING_STATUS_CD")
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
    if (is.null(prednames)) {
      stop("prednames is null... must include at least one variable name\n")
    }
    pvars2keep <- unique(c(pvars2keep, prednames))
  }
 
  ## Check predfac
  ###################################################################################
  if (module == "SA" || (module == "MA" && method %in% c("greg", "gregEN")))  {
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
    SCHEMA.=ppsanm=pltassgnqry <- NULL
    whereqry <- ""
    if (!is.null(pltassgn) && is.character(pltassgn) && pltassgn %in% tablst)
      ppsanm <- pltassgn
 
    ## Filter for population data
    if (!is.null(evalid)) {
      pfromqry <- getpfromqry(evalid, dsn=dsn, ppsanm=ppsanm, ppsaid=pltassgnid)
      whereqry <- paste0("where ppsa.EVALID in(", toString(evalid), ")")
      if (!is.null(pltassgn) && is.character(pltassgn) && pltassgn %in% tablst)
        pltassgnqry <- paste("select distinct ppsa.* from", pfromqry, whereqry)
    } else if (measCur) {
      pfromqry <- getpfromqry(varCur="MEASYEAR", Endyr=measEndyr, dsn=dsn, 
		plotnm=plt, ppsanm=ppsanm)
      if (!is.null(pltassgn) && is.character(pltassgn) && pltassgn %in% tablst)
        pltassgnqry <- paste("select ppsa.* from", pfromqry)
    } else if (!is.null(invyrs)) {
      pfromqry <- getpfromqry(invyrs=invyrs, dsn=dsn, plotnm=plt, ppsanm=ppsanm)
      whereqry <- paste0("where invyrs in(", toString(invyrs), ")")
      if (!is.null(pltassgn) && is.character(pltassgn) && pltassgn %in% tablst)
        pltassgnqry <- paste("select ppsa.* from", pfromqry, whereqry)
    } else {
      if (!is.null(ppsanm)) {
        pfromqry <- paste0("plot p INNER JOIN ", ppsanm, 
			" ON(p.", pjoinid, " = ", ppsanm, ".", pltassgnid, ")")
      } else {
        pfromqry <- "plot p"
      }
#      if (!is.null(pltassgn) && is.character(pltassgn) && pltassgn %in% tablst)
#        pltassgnqry <- paste("select * from", ppsanm)
    }

    if (!is.null(plt) && is.character(plt) && plt %in% tablst) 
      plotqry <- paste("select distinct p.* from", pfromqry, whereqry)
    if (!is.null(cond) && is.character(cond) && cond %in% tablst) {
      cfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., cond,
				" c ON (c.PLT_CN = p.CN)")
      condqry <- paste("select distinct c.* from", cfromqry, whereqry)
    }
    if (!is.null(tree) && is.character(tree) && tree %in% tablst) {
      if (!is.null(pfromqry)) {
        tfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., tree,
				" t ON (t.PLT_CN = p.CN)")
      } else {
        tfromqry <- paste(tree, "t")
      }
      treeqry <- paste("select distinct t.* from", tfromqry, whereqry)
    }
    if (!is.null(tree) && is.character(tree) && tree %in% tablst) {
      if (!is.null(pfromqry)) {
        sfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., seed,
				" s ON (s.PLT_CN = p.CN)")
      } else {
        sfromqry <- paste(seed, "s")
      }
      seedqry <- paste("select distinct s.* from", sfromqry, whereqry)
    }
    if (!is.null(vspspp) && is.character(vspspp) && vspspp %in% tablst) {
      if (!is.null(pfromqry)) {
        vspspp.fromqry <- paste0(pfromqry, " JOIN ", SCHEMA., vspspp,
				" vspspp ON (vspspp.PLT_CN = p.CN)")
      } else {
        vspspp.fromqry <- paste(vspspp, "vspspp")
      }
      vspsppqry <- paste("select distinct vspspp.* from", vspspp.fromqry, whereqry)
    }
    if (!is.null(subplot) && is.character(subplot) && subplot %in% tablst) {
      subpfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., subplot,
				" subp ON (subp.PLT_CN = p.CN)")
      subplotqry <- paste("select distinct subp.* from", subpfromqry, whereqry)
    }
    if (!is.null(subp_cond) && is.character(subp_cond) && subp_cond %in% tablst) {
      subpcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., subp_cond,
				" subc ON (subpc.PLT_CN = p.CN)")
      subp_condqry <- paste("select distinct subc.* from", subpcfromqry, whereqry)
    }
  }
 

  ###################################################################################
  ## Import tables
  ###################################################################################
  condx <- pcheck.table(cond, tab_dsn=dsn, tabnm="cond", caption="cond table?", 
		nullcheck=nullcheck, tabqry=condqry, returnsf=FALSE)
  pltx <- pcheck.table(plt, tab_dsn=dsn, tabnm="plt", caption="plot table?", 
		nullcheck=nullcheck, tabqry=plotqry, returnsf=FALSE)
  pltassgnx <- pcheck.table(pltassgn, tab_dsn=dsn, tabnm="pltassgn", 
		caption="plot assignments?", nullcheck=nullcheck, tabqry=pltassgnqry,
		returnsf=FALSE)
  treex <- pcheck.table(tree, tab_dsn=dsn, tabnm="tree", caption="Tree table?", 
		nullcheck=nullcheck, gui=gui, tabqry=treeqry, returnsf=FALSE)
  vspsppx <- pcheck.table(vspspp, tab_dsn=dsn, tabnm="vspspp", 
		caption="Veg Species table?", nullcheck=nullcheck, gui=gui, 
		tabqry=vspsppqry, returnsf=FALSE)
  subplotx <- pcheck.table(subplot, tab_dsn=dsn, tabnm="subplot", caption="subplot table?", 
		nullcheck=nullcheck, tabqry=subplotqry, returnsf=FALSE)
  subp_condx <- pcheck.table(subp_cond, tab_dsn=dsn, tabnm="subp_cond", caption="subp_cond table?", 
		nullcheck=nullcheck, tabqry=subp_condqry, returnsf=FALSE)
  if (is.null(condx) && is.null(pltx) && is.null(pltassgnx)) 
    stop("must include plt or cond table")

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
        dups <- pltx[[puniqueid]][duplicated(pltx[[puniqueid]])]
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
		warn=paste(pjoinid, "not in plt table"))
      if (is.null(pjoinid)) pjoinid <- puniqueid
      setkeyv(pltx, pjoinid)

      pltassgnx <- pltassgnx[, unique(c(pltassgnid, 
		names(pltassgnx)[!names(pltassgnx) %in% names(pltx)])), with=FALSE]
      setkeyv(pltassgnx, pltassgnid)

      ## Check if class of pjoinid in pltx matches class of pltassgnid in pltassgnx
      tabs <- check.matchclass(pltx, pltassgnx, pjoinid, pltassgnid)
      pltx <- tabs$tab1
      pltassgnx <- tabs$tab2
    
      ## Merge pltx and pltassgnx
      pltx <- pltx[pltassgnx]

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

    } else if (!is.null(pltx) && (measCur || !is.null(measEndyr))) {

      pltx <- getPlotCur(pltx, Endyr=measEndyr, varCur="MEASYEAR", 
				Endyr.filter=measEndyr.filter)
      if (!is.null(intensity)) {
        INTENSITY <- pcheck.varchar(var2check="INTENSITY", 
		checklst=names(pltx), warn="INTENSITY variable not in plt")
        if (!all(intensity %in% unique(pltx[[INTENSITY]])))
          stop("invalid intensity")
        intensity.filter <- getfilter(INTENSITY, intensity)
        pltx <- datFilter(pltx, intensity.filter)$xf 
      }      
    } else if (!is.null(invyrs)) {
      if (!"INVYR" %in% names(pltx)) stop("INVYR not in pltx")
      if (!all(invyrs %in% unique(pltx[["INVYR"]]))) {
        invyrs.miss <- invyrs[which(!invyrs %in% unique(pltx[["INVYR"]]))]
        message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
        if (length(invyrs.miss) == length(invyrs)) stop("")
        invyrs <- invyrs[!invyrs %in% invyrs.miss]
      } 
      pltx <- datFilter(x=pltx, xfilter=paste("INVYR %in% c(", toString(invyrs), ")"))$xf

      if (!is.null(intensity)) {
        INTENSITY <- pcheck.varchar(var2check="INTENSITY", 
		checklst=names(pltx), warn="INTENSITY variable not in plt")
        if (!all(intensity %in% unique(pltx[[INTENSITY]])))
          stop("invalid intensity")
        intensity.filter <- getfilter(INTENSITY, intensity)
        pltx <- datFilter(pltx, intensity.filter)$xf 
      }      
    } else {
      if (!is.null(intensity)) {
        INTENSITY <- pcheck.varchar(var2check="INTENSITY", 
		checklst=names(pltx), warn="INTENSITY variable not in plt")
        if (!all(intensity %in% unique(pltx[[INTENSITY]])))
          stop("invalid intensity")
        intensity.filter <- getfilter(INTENSITY, intensity)
        pltx <- datFilter(pltx, intensity.filter)$xf 
      }      
    }

    ## Merge plot data to cond
    #########################################################
    if (!is.null(condx)) {
      cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", gui=gui, 
		checklst=names(condx), caption="Unique identifier of plot", 
		warn=paste(cuniqueid, "not in cond table"), stopifnull=TRUE)
      setkeyv(condx, cuniqueid)

      ## Check for NA values in necessary variables in cond table
      condx.na <- sum(is.na(condx[[cuniqueid]]))
      if (condx.na > 0) stop("NA values in ", cuniqueid)

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
      designcd <- unique(na.omit(pltcondx[["DESIGNCD"]]))
      if (length(designcd) != 1) {
        warning("more than 1 plot design, calculate separate estimates by design")
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
    plotsampcnt <- pltcondx[, list(NBRPLOT=uniqueN(get(cuniqueid)))]
  }

  if (ACI) {
    if (any(c("NF_PLOT_STATUS_CD", "PSTATUSNF") %in% pltcondnmlst)) {
      if ("PSTATUSNF" %in% names(pltcondx)) 
        names(pltcondx)[names(pltcondx) == "PSTATUSNF"] <- "NF_PLOT_STATUS_CD"
      ref_nf_plot_status_cd <- 
		FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "NF_PLOT_STATUS_CD", ]
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
    if (nonresp) {
      ## Generate table of nonsampled plots by strata (if nonresp=TRUE)
      if ("PLOT_STATUS_CD" %in% pltcondnmlst) {
        if (!3 %in% unique(pltcondx[["PLOT_STATUS_CD"]]))
          stop("must include PLOT_STATUS_CD = 3 in dataset") 

        ## Create table with number of nonsampled plots by strata, substrata
        nonresplut <- pltcondx[PLOT_STATUS_CD == 3, uniqueN(get(cuniqueid)), 
					by=c(unitvars, strvar, substrvar)]
        setnames(nonresplut, "V1", "n.nonresp")
        setkeyv(nonresplut, c(unitvars, strvar, substrvar))
      } 
    }
    P2POINTCNT <- pltcondx[, list(P2POINTCNT=uniqueN(get(cuniqueid))), 
		by=c(unitvars, strvar, substrvar)]
    setkeyv(P2POINTCNT, c(unitvars, strvar, substrvar))
    if (nonresp)
      P2POINTCNT <- P2POINTCNT[nonresplut]
      
  } else {
    P2POINTCNT <- pltcondx[, uniqueN(get(cuniqueid)), unitvars]
    setnames(P2POINTCNT, "V1", "P2POINTCNT")
  }

  #############################################################################
  ## Generate and apply plt.nonsamp.filter 
  #############################################################################
  if ((is.null(plt.nonsamp.filter) || plt.nonsamp.filter == "") && adj != "none") {
    if ("PLOT_STATUS_CD" %in% names(pltcondx)) {
      if (sum(pltcondx$PLOT_STATUS_CD == 3, na.rm=TRUE) > 0) {
        message("removing ", sum(pltcondx$PLOT_STATUS_CD == 3), " nonsampled forest plots")
        plt.nonsamp.filter <- "PLOT_STATUS_CD != 3"
      }
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

  ## If areawt not in cond table and only 1 condition per plot, 
  ## 	add areawt and set = 1 (100 percent)
  if (is.null(areawt) || is.na(areawt) || !areawt %in% pltcondnmlst) {
    ## If only 1 condition, check CONDPROP_UNADJ
    if (nrow(pltcondx) == length(unique(pltcondx[[cuniqueid]]))) {
      message("CONDPROP_UNADJ not in dataset.. assuming CONDPROP_UNADJ = 1") 
      pltcondx[, CONDPROP_UNADJ := 1] 
      areawt <- "CONDPROP_UNADJ" 
    } else {
      stop("areawt is invalid...")
    }
  }
  pltcondx[[areawt]] <- check.numeric(pltcondx[[areawt]])

  ## Check for COND_STATUS_CD and create ACI filter
  #############################################################################
  if (!"COND_STATUS_CD" %in% pltcondnmlst) {
    if (length(unique(pltcondx[[cuniqueid]])) == nrow(pltcondx) && 
		"PLOT_STATUS_CD" %in% pltcondnmlst) {
      message("COND_STATUS_CD not in dataset.. using PLOT_STATUS_CD for COND_STATUS_CD")
      pltcondx[["COND_STATUS_CD"]] <- pltcondx[["PLOT_STATUS_CD"]]
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
  } else {
    condsampcnt <- pltcondx[, list(NBRCOND=.N)]
  }

  if (ACI) {
    if ("NF_COND_STATUS_CD" %in% pltcondnmlst) {
      ref_nf_cond_status_cd <- 
		FIESTA::ref_codes[FIESTA::ref_codes$VARIABLE == "NF_COND_STATUS_CD", ]
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
      nonsampn <- sum(pltcondx$COND_STATUS_CD == 5, na.rm=TRUE)
      if (length(nonsampn) > 0) {
        message("removing ", nonsampn, " nonsampled forest conditions")
      }
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
  if (!is.null(treex)) {
    ## Define necessary variable for tree table
    tvars2keep <- {}
    if (adj != "none") tvars2keep <- "TPA_UNADJ"
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
      treex.na <- sum(is.na(treex[, condid, with=FALSE]))
      if (treex.na > 0) stop("NA values in ", condid)
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
    if (adj != "none") {
      ## Check for condition proportion variables
      propchk <- check.PROP(treex, pltcondx, cuniqueid=cuniqueid, checkNA=FALSE)
      propvars <- propchk$propvars
      treex <- propchk$treex
      pltcondx <- propchk$condx
      cvars2keep <- unique(c(cvars2keep, propvars))
    }
  }

  ###################################################################################
  ###################################################################################
  ## Check seedling data
  ###################################################################################
  ###################################################################################
  seedx <- pcheck.table(seed, tab_dsn=dsn, tabnm="seed", caption="Seedling table?", 
		nullcheck=nullcheck, gui=gui, tabqry=seedqry, returnsf=FALSE)
  if (!is.null(seedx)) {
    ## Define necessary variable for tree table
    svars2keep <- {}
    if (adj != "none") svars2keep <- "TPA_UNADJ"
    seednmlst <- names(seedx)

    ## Check unique identifiers
    tuniqueid <- FIESTA::pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui, 
		checklst=treenmlst, caption="UniqueID variable of plot", 
		warn=paste(tuniqueid, "not in tree"), stopifnull=TRUE)

    ## Check for NA values in necessary variables in tree table
    seedx.na <- sum(is.na(seedx[[tuniqueid]]))
    if (seedx.na > 0) stop("NA values in ", tuniqueid)

    if (tuniqueid %in% pltcondnmlst) {
      idplace <- which(pltcondnmlst %in% tuniqueid)
      if (idplace != 1) { 
	  pltcondnmlst <- c(tuniqueid, pltcondnmlst) 
	  pltcondnmlst <- pltcondnmlst[-(idplace + 1)] 
      }
    } 

    ## Check for condid in tree
    if (!condid %in% names(treex)) {
      if (nrow(seedx) == length(unique(seedx[[tuniqueid]]))) {
        seedx[, CONDID := 1]
      } else { 
        stop("only 1 record for each tuniqueid allowed")
      }
    } else {
      ## Check for NA values in condid
      seedx.na <- sum(is.na(seedx[, tuniqueid, with=FALSE]))
      if (seedx.na > 0) stop("NA values in ", tuniqueid)
    }
    setkeyv(seedx, c(tuniqueid, condid))

    ## Check if class of tuniqueid in seedx matches class of cuniqueid in condx
    tabs <- FIESTA::check.matchclass(pltcondx, seedx, cuniqueid, tuniqueid)
    pltcondx <- tabs$tab1
    seedx <- tabs$tab2

    ## Check for missing tvars2keep 
    smissvars <- svars2keep[which(!svars2keep %in% seednmlst)]
    if (length(smissvars) > 0)
      stop("missing necessary variables from seed: ", paste(smissvars, collapse=", "))

    ## Check for NA values in svars2keep variables
    ## TPA_UNADJ=NA, but trees have a DIA 
    ## these are down dead trees that only count in growth and mortality, 
    ## but wouldn't be measured if they hadn't been alive at the previous inventory

    svars2keep2 <- svars2keep[svars2keep != "TPA_UNADJ"]
    if (length(svars2keep) > 0) {
      svars.na <- sapply(c(tuniqueid, condid, svars2keep2), 
		function(x, seedx){ sum(is.na(seedx[,x, with=FALSE])) }, seedx)
      if (any(svars.na) > 0) 
        stop(svars.na[svars.na > 0], " NA values in variable: ", 
		paste(names(svars.na[svars.na > 0]), collapse=", "))
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

  ###################################################################################
  ###################################################################################
  ## Check subplot and sub_cond data
  ###################################################################################
  ###################################################################################
  if ("P2VEG" %in% popType) {
    subpuniqueid <- "PLT_CN"
    subpid <- "SUBP"

    if (!is.null(subplotx)) {
      subpuniqueid <- FIESTA::pcheck.varchar(var2check=subpuniqueid, varnm="subpuniqueid", 
		checklst=names(subplotx), caption="UniqueID variable of subplot", 
		warn=paste(subpuniqueid, "not in subplot"), stopifnull=TRUE)
      ## Check for NA values in subpuniqueid
      subp.na <- sum(is.na(subplotx[[subpuniqueid]]))
      if (subp.na > 0) stop("NA values in ", subpuniqueid)

      subpid <- FIESTA::pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(subplotx), caption="ID of subplot", 
		warn=paste(subpid, "not in subplot"), stopifnull=TRUE)
      ## Check for NA values in subpuniqueid
      subp.na <- sum(is.na(subplotx[[subpid]]))
      if (subp.na > 0) stop("NA values in ", subpid)
      setkeyv(subplotx, c(subpuniqueid, subpid))
    }
    if (!is.null(subp_condx)) {
      subpuniqueid <- FIESTA::pcheck.varchar(var2check=subpuniqueid, varnm="subpuniqueid", 
		checklst=names(subp_condx), caption="UniqueID variable of subp_cond", 
		warn=paste(subpuniqueid, "not in sub_cond"), stopifnull=TRUE)
      ## Check for NA values in necessary variables in subp_cond table
      subpc.na <- sum(is.na(subp_condx[[subpuniqueid]]))
      if (subpc.na > 0) stop("NA values in ", subpuniqueid) 

      subpid <- FIESTA::pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(subp_condx), caption="ID of subplot", 
		warn=paste(subpid, "not in subp_cond"), stopifnull=TRUE)
      ## Check for NA values in subpuniqueid
      subp.na <- sum(is.na(subp_condx[[subpid]]))
      if (subp.na > 0) stop("NA values in ", subpid)      

      ## Check for condid in subp_condx
      if (!condid %in% names(subp_condx)) {
        stop("invalid subp_condx... must include CONDID variable")
      }
      setkeyv(subp_condx, c(subpuniqueid, subpid, condid))

      if (!is.null(subplotx)) {
        ## Check if class of tuniqueid in treex matches class of cuniqueid in condx
        tabs <- FIESTA::check.matchclass(subplotx, subp_condx, 
		matchcol=c(subpuniqueid, subpid))
        subplotx <- tabs$tab1
        subp_condx <- tabs$tab2

        subp_condx <- subplotx[subp_condx]
      }
    }

    #############################################################################
    ## Subset pltassgn and subp_condx to sampled P2VEG
    #############################################################################
    if (!"P2VEG_SAMPLING_STATUS_CD" %in% names(pltassgnx)) {
      message("assuming all plots sample P2VEG")
    }
    pltassgn.P2VEG <- pltassgnx[pltassgnx[["P2VEG_SAMPLING_STATUS_CD"]] == 1,]


    ## Subset subplots to sample P2VEG plots
    subp_condf <- check.matchval(subp_condx, pltassgn.P2VEG, subpuniqueid, pltassgnid, 
		tab1txt="subp_cond", tab2txt="pltassgn.P2VEG", subsetrows=TRUE)
    setkeyv(subp_condf, c(subpuniqueid, subpid, condid))

    #############################################################################
    ## Define and apply subp.nonsamp.filter 
    #############################################################################
    if (!"SUBP_STATUS_CD" %in% names(subp_condx)) {
      message("SUBP_STATUS_CD not in subp_cond... assuming all subplots are sampled")
    } else {
      subp.nonsamp.filter <- "SUBP_STATUS_CD == 1"
      if (ACI && "NF_SUBP_STATUS_CD" %in% names(subp_condx)) {
        subp.nonsamp.filter.ACI <- "(is.na(NF_SUBP_STATUS_CD) | NF_SUBP_STATUS_CD != 3)"
        message("removing ", sum(is.na(NF_SUBP_STATUS_CD) & NF_SUBP_STATUS_CD == 3, na.rm=TRUE), 
		" nonsampled subplot conditions")
        if (!is.null(subp.nonsamp.filter)) 
          subp.nonsamp.filter <- paste(subp.nonsamp.filter, "&", subp.nonsamp.filter.ACI)
      }
    }
    ## Apply subp.nonsamp.filter 
    subp_condx <- datFilter(x=subp_condx, xfilter=subp.nonsamp.filter, 
		title.filter="subp.nonsamp.filter")$xf
    if (is.null(subp_condx)) {
      message(paste(subp.nonsamp.filter, "removed all records"))
      return(NULL)
    }

    #############################################################################
    ## Define and apply p2veg.nonsamp.filter 
    #############################################################################
    if ("P2VEG_SUBP_STATUS_CD" %in% names(subp_condx)) {
      p2veg.nonsamp.filter <- "!is.na(P2VEG_SUBP_STATUS_CD) & P2VEG_SUBP_STATUS_CD != 2"
    
      subp_condx <- datFilter(x=subp_condx, xfilter=p2veg.nonsamp.filter, 
		title.filter="p2veg.nonsamp.filter")$xf
      if (is.null(subp_condx)) {
        message(paste(p2veg.nonsamp.filter, "removed all records"))
        return(NULL)
      }
    }

    #############################################################################
    ## Check veg profile data
    #############################################################################

    if (!is.null(vspsppx)) {
      ## Define necessary variable for tree table
      vspsppnmlst <- names(vspsppx)

      ## Check unique identifiers
      vuniqueid <- FIESTA::pcheck.varchar(var2check=vuniqueid, varnm="vuniqueid", gui=gui, 
		checklst=vspsppnmlst, caption="UniqueID variable of veg spp", 
		warn=paste(vuniqueid, "not in vegspspp"), stopifnull=TRUE)
      cvars2keep <- c(cvars2keep, "SUBPPROP_UNADJ")
    
      ## Check for NA values in necessary variables in tree table
      vspsppx.na <- sum(is.na(vspsppx[[vuniqueid]]))
      if (vspsppx.na > 0) stop("NA values in ", vuniqueid)

      if (vuniqueid %in% pltcondnmlst) {
        idplace <- which(pltcondnmlst %in% vuniqueid)
        if (idplace != 1) { 
	    pltcondnmlst <- c(vuniqueid, pltcondnmlst) 
	    pltcondnmlst <- pltcondnmlst[-(idplace + 1)] 
        }
      } 

      ## Check that the values of tuniqueid in vspsppx are all in cuniqueid in subp_condf
      vspsppf <- check.matchval(vspsppx, subp_condf, c(vuniqueid, subpid), 
		tab1txt="vspspp", tab2txt="subp_cond", subsetrows=TRUE)
      setkeyv(vspsppf, c(subpuniqueid, subpid, condid))

      subpc_vspsppf <- subp_condf[vspsppx]
      subpc_vspsppf2 <- subp_condf[vspsppf]

#vv = vspsppx[!vspsppx$PLT_CN %in% vspsppf$PLT_CN,]
#ss2 = subp_condf[subp_condf$SUBP_STATUS_CD == 2 & subp_condf$P2VEG_SUBP_STATUS_CD == 1,]
    }
  }
 

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
	states=states, invyrs=invyrs, ACI.filter=ACI.filter, areawt=areawt)

  if (!is.null(treex)) {
    ## Check that the values of tuniqueid in treex are all in cuniqueid in pltcondx
    treef <- check.matchval(treex, pltcondx, tuniqueid, cuniqueid, tab1txt="tree", 
		tab2txt="cond", subsetrows=TRUE)
    returnlst$treef <- treef
    returnlst$tuniqueid <- tuniqueid
  }
  if (!is.null(seedx)) {
    ## Check that the values of tuniqueid in seedx are all in cuniqueid in pltcondx
    seedf <- check.matchval(seedx, pltcondx, tuniqueid, cuniqueid, tab1txt="seed", 
		tab2txt="cond", subsetrows=TRUE)
    returnlst$seedf <- seedf
  }
  if ("P2VEG" %in% popType) {
    returnlst$pltassgn.P2VEG <- pltassgn.P2VEG
    if (!is.null(subp_condx)) {
      returnlst$subp_condf <- subp_condf
      returnlst$subpuniqueid <- subpuniqueid
    }
    if (!is.null(vspsppx)) {
      returnlst$vspsppf <- vspsppf
      returnlst$vuniqueid <- vuniqueid
    }
  }
  if (module == "MA") {
    returnlst$method <- method
    if (method %in% c("greg", "gregEN")) {
      returnlst$prednames <- prednames
      returnlst$predfac <- predfac
    }
  }
  if (ACI) {
    returnlst$nfplotsampcnt <- nfplotsampcnt
  }
  if (nonresp) {
    returnlst$substrvar <- substrvar 
    returnlst$nonresplut <- nonresplut
  }
  
  return(returnlst)
}
