check.popdata <- function(module="GB", popType="VOL", tabs, tabIDs, strata=FALSE,
	pltassgn=NULL, pltassgnid="CN", dsn=NULL, pjoinid="CN", condid="CONDID",
	areawt="CONDPROP_UNADJ", evalid=NULL, measCur=FALSE, measEndyr=NULL,
	measEndyr.filter=NULL, invyrs=NULL, intensity=NULL, adj="samp",
	MICRO_BREAKPOINT_DIA=5, MACRO_BREAKPOINT_DIA=NULL, diavar="DIA",
	areawt_micr="MICRPROP_UNADJ", areawt_subp="SUBPPROP_UNADJ", areawt_macr="MACRPROP_UNADJ",
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES",
	areaunits="acres", unit.action="keep", removetext="unitarea",
	stratalut=NULL, strvar="STRATUMCD", nonresp=FALSE, substrvar=NULL,
	stratcombine=TRUE, prednames=NULL, predfac=NULL, ACI=FALSE, nonsamp.pfilter=NULL,
	nonsamp.cfilter=NULL, nonsamp.vfilter.fixed=FALSE, nullcheck=FALSE,
	pvars2keep=NULL, cvars2keep=NULL, ppsanm="pop_plot_stratum_assgn", gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs
  ## Define necessary plot and condition-level variables:
  ## - plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD,
  ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC,
  ##		ECOSUBCD, CONGCD, INTENSITY, DESIGNCD
  ## - plt (pvars2keep) - unitvars, auxvars
  ## - cond (cvars2keep) - areawt
  ## - module in("GB", "MA", "SA")
  ## - if (module="MA") method in("HT", "PS", "greg", "gregEN", "ratio")
  ## - if (module="SA") SApackage <- c("JoSAE", "sae", "hbsae"); method <- c("unit", "area")
  ## Check logical parameters: ACI, strata, stratcombine (if strata=TRUE)
  ## - If ACI, add NF_PLOT_STATUS_CD to pvars2keep and NF_COND_STATUS_CD to cvars2keep
  ## - If unit.action='combine', estimation units are combined if less than 10 plots
  ## - If strata, only 1 auxvar allowed, add to pvars2keep
  ## - If module = SA or MA-greg, add prednames to pvars2keep
  ## - If adj="samp", nonsample adjustment factors calculated at strata level
  ## - If adj="plot", nonsample adjustment factors calculated at plot level
  ## Check unit.action ('keep', 'remove', 'combine').
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
  ## - Generate and apply nonsamp.pfilter (PLOT_STATUS_CD != 3)
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
	STATECD=PLOT_STATUS_CD=PSTATUSCD=cndnmlst=pltdomainlst=PROP_BASIS=
	ACI.filter=V1=plotsampcnt=nfplotsampcnt=condsampcnt=INVYR=
	NF_PLOT_STATUS_CD=NF_COND_STATUS_CD=TPA_UNADJ=methodlst=nonresplut=
	plotqry=condqry=treeqry=pfromqry=pltassgnqry=cfromqry=tfromqry=
	vsubpsppqry=subplotqry=subp_condqry=unitareaqry=stratalutqry=NF_SUBP_STATUS_CD=
	SUBPCOND_PROP=MACRCOND_PROP=tpropvars=vcondsppf=vcondstrf <- NULL

  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  pvars2keep <- unique(c(unitvar, unitvar2, pvars2keep))
  cvars2keep <- unique(c(cvars2keep, areawt, "PROP_BASIS"))
  datindb=unitindb=stratindb <- FALSE

  pdoms2keep <- unique(c("STATECD", "UNITCD", "COUNTYCD", "INVYR",
	"MEASYEAR", "PLOT_STATUS_CD", "PSTATUSCD", "RDDISTCD", "WATERCD", "ELEV",
	"ELEV_PUBLIC", "ECOSUBCD", "CONGCD", "INTENSITY", "DESIGNCD"))
  #pdoms2keep <- pdoms2keep[!pdoms2keep %in% pvars2keep]

  ###################################################################################
  ## Check module, adj
  ###################################################################################

  ## Check estimator module
  ########################################################
  modulelst <- c("GB", "MA", "SA")
  module <- pcheck.varchar(var2check=module, varnm="module", gui=gui,
		checklst=modulelst, caption="FIESTA module", stopifnull=TRUE)

  ## Check popType
  ########################################################
  evalTyplst <- c("ALL", "CURR", "VOL", "LULC", "P2VEG", "GRM")
  popType <- pcheck.varchar(var2check=popType, varnm="popType", gui=gui,
		checklst=evalTyplst, caption="popType", multiple=TRUE, stopifnull=TRUE)

  plt=cond=tree=seed=vsubpstr=vsubpspp=subplot=subp_cond=lulc <- NULL
  ## Get tables from tabs
  for (tabnm in names(tabs)) {
    assign(tabnm, tabs[[tabnm]])
  }
  puniqueid <- tabIDs[["plt"]]
  cuniqueid <- tabIDs[["cond"]]
  tuniqueid <- tabIDs[["tree"]]
  suniqueid <- tabIDs[["seed"]]
  vsubpstrid <- tabIDs[["vsubpstr"]]
  vsubpsppid <- tabIDs[["vsubpspp"]]
  subplotid <- tabIDs[["subplot"]]
  subp_condid <- tabIDs[["subp_cond"]]
  subpuniqueid <- subplotid
  lulcid <- "PLT_CN"


  ## Check adj
  ########################################################
  adjlst <- c("samp", "plot", "none")
  adj <- pcheck.varchar(var2check=adj, varnm="adj", gui=gui,
		checklst=adjlst, caption="adj", multiple=FALSE, stopifnull=TRUE)

  ## Check adj
  ########################################################
  if (adj == "samp" && module == "SA")  {
    message("adj='samp' is currently invalid for SA module... adjusting for plot")
    adj <- "plot"
  }
  if (adj == "plot" && module == "GB") {
    message("adj='plot' is not typical for GA modules")
  }

  ###################################################################################
  ## Check logical parameters: strata, ACI
  ###################################################################################

  ## Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
  ###################################################################################
  ACI <- pcheck.logical(ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
  if (ACI) {
    pdoms2keep <- unique(c(pdoms2keep, "NF_PLOT_STATUS_CD"))
  }

  ## Check unit.action
  ########################################################
  unit.actionlst <- c("keep", "remove", "combine")
  unit.action <- pcheck.varchar(var2check=unit.action, varnm="unit.action", gui=gui,
		checklst=unit.actionlst, caption="unit.action", multiple=FALSE, stopifnull=TRUE)

  ## Check strata, strvars
  ###################################################################################
  strata <- pcheck.logical(strata, varnm="strata",
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)

  if (strata) {
    if (is.null(strvar)) stop("must include strvar for post-strat estimates")
    if (length(strvar) > 1) stop("invalid strvar... only 1 variable allowed")
    pvars2keep <- unique(c(pvars2keep, strvar))

    ## Check nonresp
    nonresp <- pcheck.logical(nonresp, varnm="nonresp",
		title="Post stratify?", first="YES", gui=gui)
    if (nonresp) {
      pvars2keep <- c(pvars2keep, substrvar)
    } else {
      substrvar <- NULL
    }

    ## Check stratcombine
    ########################################################
    stratcombine <- pcheck.logical(stratcombine, varnm="stratcombine",
		title="Combine strata?", first="YES", gui=gui, stopifnull=TRUE)

  } else {
    strvar <- NULL
    #if (is.null(prednames)) {
    #  stop("no prednames included\n")
    #}
    pvars2keep <- unique(c(pvars2keep, prednames))
  }

  ## Check predfac
  ###################################################################################
  if (!is.null(predfac)) {
    if (!is.character(predfac)) {
      stop("invalid predfac... must be character string")
    }
    notin <- predfac[!predfac %in% prednames]
    if (length(notin) > 0) {
      warning("invalid predfac... not in prednames: ", toString(notin))
      predfac <- predfac[predfac %in% prednames]
      if (length(predfac) == 0) predfac <- NULL
    }
  }

  ## Check dsn and create queries to get population subset from database
  ###################################################################################
  if (!is.null(dsn) && getext(dsn) %in% c("sqlite", "db", "db3", "sqlite3", "gpkg")) {
    datindb <- TRUE
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
    tablst <- DBI::dbListTables(dbconn)
    chk <- TRUE
    SCHEMA.=ppsanm=pltassgnqry <- NULL

    ## Filter for population data
    if (!is.null(evalid) && !is.data.frame(pltassgn)) {
      ppsanm <- chkdbtab(tablst, pltassgn, stopifnull=TRUE)
      if (!pltassgnid %in% DBI::dbListFields(dbconn, ppsanm)) {
        stop("pltassgnid is invalid")
      }
      pfromqry <- getpfromqry(evalid, dsn=dsn, ppsanm=ppsanm, ppsaid=pltassgnid)
      whereqry <- paste0("where ppsa.EVALID in(", toString(evalid), ")")
      pltassgnqry <- paste("select distinct ppsa.* from", pfromqry, whereqry)
    } else if (measCur) {
      pfromqry <- getpfromqry(varCur="MEASYEAR", Endyr=measEndyr, dsn=dsn,
		plotnm=plt)
      pltassgnqry <- paste("select p.* from", pfromqry)
    } else if (!is.null(invyrs)) {
      pfromqry <- getpfromqry(invyrs=invyrs, dsn=dsn, plotnm=plt)
      whereqry <- paste0("where invyrs in(", toString(invyrs), ")")
      pltassgnqry <- paste("select p.* from", pfromqry, whereqry)
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
    if (!is.null(vsubpspp) && is.character(vsubpspp) && vsubpspp %in% tablst) {
      if (!is.null(pfromqry)) {
        vsubpspp.fromqry <- paste0(pfromqry, " JOIN ", SCHEMA., vsubpspp,
				" vsubpspp ON (vsubpspp.PLT_CN = p.CN)")
      } else {
        vsubpspp.fromqry <- paste(vsubpspp, "vsubpspp")
      }
      vsubpsppqry <- paste("select distinct vsubpspp.* from", vsubpspp.fromqry, whereqry)
    }
    if (!is.null(vsubpstr) && is.character(vsubpstr) && vsubpstr %in% tablst) {
      if (!is.null(pfromqry)) {
        vsubpstr.fromqry <- paste0(pfromqry, " JOIN ", SCHEMA., vsubpstr,
				" vsubpspp ON (vsubpstr.PLT_CN = p.CN)")
      } else {
        vsubpstr.fromqry <- paste(vsubpspp, "vsubpstr")
      }
      vsubpstrqry <- paste("select distinct vsubpstr.* from", vsubpstr.fromqry, whereqry)
    }
    if (!is.null(subplot) && is.character(subplot) && subplot %in% tablst) {
      subpfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., subplot,
				" subp ON (subp.PLT_CN = p.CN)")
      subplotqry <- paste("select distinct subp.* from", subpfromqry, whereqry)
    }
    if (!is.null(subp_cond) && is.character(subp_cond) && subp_cond %in% tablst) {
      subpcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., subp_cond,
				" subpc ON (subpc.PLT_CN = p.CN)")
      subp_condqry <- paste("select distinct subc.* from", subpcfromqry, whereqry)
    }
    if (!is.null(lulc) && is.character(lulc) && lulc %in% tablst) {
      lulcfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., lulc,
				" ON (lulc.PLT_CN = p.CN)")
      lulcqry <- paste("select distinct lulc.* from", lulcfromqry, whereqry)
    }
    if (is.character(unitarea) && !is.null(chkdbtab(tablst, unitarea))) {
      unitindb <- TRUE
      unitarea_layer <- chkdbtab(tablst, unitarea)
      unitareaqry <- paste("select * from", unitarea_layer)
      if (!is.null(evalid)) {
        unitareaqry <- paste(unitareaqry, "where evalid in(", toString(evalid), ")")
      }

      unitarea <- pcheck.table(unitarea, tab_dsn=dsn, tabnm="unitarea", caption="unitarea?",
		nullcheck=nullcheck, tabqry=unitareaqry, returnsf=FALSE)

    }
 
    if (strata && is.character(stratalut) && !is.null(chkdbtab(tablst, stratalut))) {
      stratindb <- TRUE
      stratalut_layer <- chkdbtab(tablst, stratalut)
      stratalutqry <- paste("select * from", stratalut_layer)
      if (!is.null(evalid)) {
        stratalutqry <- paste(stratalutqry, "where evalid in(", toString(evalid), ")")
      }
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
  if (popType != "LULC" && (is.null(condx) && is.null(pltx) && is.null(pltassgnx))) {
    stop("must include plt or cond table")
  }
  treex <- pcheck.table(tree, tab_dsn=dsn, tabnm="tree", caption="Tree table?",
		nullcheck=nullcheck, gui=gui, tabqry=treeqry, returnsf=FALSE)
  vsubpsppx <- pcheck.table(vsubpspp, tab_dsn=dsn, tabnm="vsubpspp",
		caption="Veg Species table?", nullcheck=nullcheck, gui=gui,
		tabqry=vsubpsppqry, returnsf=FALSE)
  vsubpstrx <- pcheck.table(vsubpstr, tab_dsn=dsn, tabnm="vsubpstr",
		caption="Veg Structure table?", nullcheck=nullcheck, gui=gui,
		tabqry=vsubpsppqry, returnsf=FALSE)
  subplotx <- pcheck.table(subplot, tab_dsn=dsn, tabnm="subplot",
		caption="subplot table?", nullcheck=nullcheck, tabqry=subplotqry,
		returnsf=FALSE)
  subp_condx <- pcheck.table(subp_cond, tab_dsn=dsn, tabnm="subp_cond",
		caption="subp_cond table?", nullcheck=nullcheck, tabqry=subp_condqry,
		returnsf=FALSE)
  lulcx <- pcheck.table(lulc, tab_dsn=dsn, tabnm="lulc", caption="lulc table?",
		nullcheck=nullcheck, tabqry=lulcqry, returnsf=FALSE)
 

  ## Define cdoms2keep
  cdoms2keep <- names(condx)

  if (popType == "P2VEG") {
    if (is.null(vsubpsppx) && is.null(vsubpstrx)) {
      stop("must include vsubpspp and/or vsubpstr tables for popType='P2VEG'")
    }
  }
  if (popType == "GRM") {
    pvars2keep <- c(pvars2keep, "REMPER")
  }

  ###################################################################################
  ## Check and merge plt, pltassgn, cond
  ###################################################################################
  if (!is.null(pltx) || !is.null(pltassgnx)) {
    if (!is.null(pltx)) {
      pltnmlst <- names(pltx)
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

      if (!is.null(evalid) && !is.null(chkdbtab(names(pltassgnx), "EVALID"))) {
        evalidnm <- chkdbtab(names(pltassgnx), "EVALID")
        evalid <- unlist(evalid)
        if (!all(evalid %in% unique(pltassgnx[[evalidnm]]))) {
          evalid.miss <- evalid[which(!evalid %in% unique(pltassgnx[["EVALID"]]))]
          message("evalid not in dataset: ", paste(evalid.miss, collapse=", "))
          if (length(evalid.miss) == length(evalid)) stop("")
          evalid <- evalid[!evalid %in% evalid.miss]
        }
        pltassgnx <- datFilter(pltassgnx, getfilter("EVALID", evalid, syntax="R"))$xf
        if (nrow(pltassgnx) == 0) {
          stop("evalid removed all records")
        }
      }
      if (any(duplicated(pltassgnx[[pltassgnid]]))) {
        warning("plot records are not unique in: pltassgn")
      }
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

      if (pjoinid %in% names(pltassgnx)) {
        pltassgnid <- pjoinid
        setkeyv(pltassgnx, pjoinid)
      }
      pltassgnx <- pltassgnx[, unique(c(pltassgnid,
		names(pltassgnx)[!names(pltassgnx) %in% names(pltx)])), with=FALSE]
      setkeyv(pltassgnx, pltassgnid)

      ## Check if class of pjoinid in pltx matches class of pltassgnid in pltassgnx
      tabchk <- check.matchclass(pltx, pltassgnx, pjoinid, pltassgnid)
      pltx <- tabchk$tab1
      pltassgnx <- tabchk$tab2

#################
      ## Check for matching unique identifiers of pltx with pltassgnx
      ## Subset pltx to pltassgnx ids
      pltx <- check.matchval(pltx, pltassgnx, pjoinid, pltassgnid, tab1txt="plt",
			tab2txt="pltassgn", subsetrows=TRUE)

      ## Merge pltx and pltassgnx (Note: inner join)
      #pltx <- merge(pltassgnx, pltx, by.x=pltassgnid, by.y=pjoinid)
      pltx <- merge(pltx, pltassgnx, by.x=pjoinid, by.y=pltassgnid)
      #puniqueid <- pltassgnid

    } else if (is.null(pltx)) {
      pltx <- pltassgnx
      puniqueid <- pltassgnid
    }
 
    ##################################################################################
    ## Filter for population data
    ##################################################################################
    if (!datindb) {
      if (!is.null(pltx) && (measCur || !is.null(measEndyr))) {

        pltx <- getPlotCur(pltx, Endyr=measEndyr, varCur="MEASYEAR",
				Endyr.filter=measEndyr.filter)
        if (!is.null(intensity)) {
          INTENSITYNM <- pcheck.varchar(var2check="INTENSITY",
		checklst=names(pltx), warn="INTENSITY variable not in plt")
          if (!all(intensity %in% unique(pltx[[INTENSITYNM]])))
            stop("invalid intensity")
          intensity.filter <- getfilter(INTENSITYNM, intensity)
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
          INTENSITYNM <- pcheck.varchar(var2check="INTENSITY",
			checklst=names(pltx), warn="INTENSITY variable not in plt")
          if (!all(intensity %in% unique(pltx[[INTENSITYNM]])))
            stop("invalid intensity")
          intensity.filter <- getfilter(INTENSITYNM, intensity)
          pltx <- datFilter(pltx, intensity.filter)$xf
        }
      } else {
        if (!is.null(intensity)) {
          INTENSITYNM <- pcheck.varchar(var2check="INTENSITY",
			checklst=names(pltx), warn="INTENSITY variable not in plt")
          if (!all(intensity %in% unique(pltx[[INTENSITYNM]]))) {
            stop("invalid intensity")
          }
          intensity.filter <- getfilter(INTENSITYNM, intensity)
          pltx <- datFilter(pltx, intensity.filter)$xf
        }
      }
    }
 
    ## Merge plot data to cond (and lulc to cond)
    #########################################################
    if (!is.null(condx)) {
      cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", gui=gui,
		checklst=names(condx), caption="Unique identifier of plot",
		warn=paste(cuniqueid, "not in cond table"), stopifnull=TRUE)
      setkeyv(condx, cuniqueid)

      ## Check for NA values in necessary variables in cond table
      condx.na <- sum(is.na(condx[[cuniqueid]]))
      if (condx.na > 0) stop("NA values in ", cuniqueid)

      condid <- pcheck.varchar(var2check=condid, varnm="condid", gui=gui,
		checklst=names(condx), caption="Unique identifier of plot",
		warn=paste(condid, "not in cond table"), stopifinvalid=FALSE)
      if (is.null(condid)) {
        if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
          condx[, CONDID := 1]
          condid <- "CONDID"
        } else {
          stop("there is more than 1 record per plot... must include valid CONDID")
        }
      }
      ## Check for NA values in necessary variables in cond table
      condx.na <- sum(is.na(condx[[condid]]))
      if (condx.na > 0) stop("NA values in ", condid)

      ## Check if 1 plot-condition per record in cond
      ######################################################
      condid.dupid <- condx[duplicated(condx, by=c(cuniqueid, condid))][[cuniqueid]]
      if (length(condid.dupid) > 0) {
        msg <- paste("check cuniqueid/condid... duplicate records")
        if (length(condid.dupid) < 20) print(condid.dupid)
        stop(msg)
      }
      setkeyv(condx, c(cuniqueid, condid))

      ## Check and append lulcx if popType='LULC'
      #####################################################
      if (popType == "LULC" && !is.null(lulcx)) {
        if (!cuniqueid %in% names(lulcx)) {
          stop(cuniqueid, " must be in lulc")
        }
        if (!condid %in% names(lulcx)) {
          if (nrow(lulcx) == length(unique(lulcx[[cuniqueid]]))) {
            lulcx[, (condid) := 1]
          }
        }

        ## Check if class of puniqueid in lulcx matches class of cuniqueid in condx
        tabchk <- check.matchclass(lulcx, condx, cuniqueid, cuniqueid)
        lulcx <- tabchk$tab1
        condx <- tabchk$tab2

        ## Check for matching unique identifiers of lulcx and condx
        lulcx <- check.matchval(lulcx, condx, cuniqueid, cuniqueid, tab1txt="lulc",
			tab2txt="cond", subsetrows=TRUE)
        setkeyv(lulcx, c(cuniqueid, condid))

        ## Get columns in condx that are not in lulcx
        condcols <- unique(c(cuniqueid, condid, names(condx)[!names(condx) %in% names(lulcx)]))

        ## Merge lulcx and condx (Note: inner join to use only lulc conditions)
        #condx <- merge(condx[, condcols, with=FALSE], lulcx, by=key(condx), all.x=TRUE)
        condx <- merge(condx[, condcols, with=FALSE], lulcx, by=key(condx))

        ## Add PROP_CHNG (sum(SCCM.SUBPTYP_PROP_CHNG) / 4) to cvars2keep
        cvars2keep <- unique(c(cvars2keep, "PROP_CHNG"))
        areawt <- "PROP_CHNG"
      }

      ## Check if class of puniqueid in pltx matches class of puniqueid in condx
      tabchk <- check.matchclass(condx, pltx, cuniqueid, puniqueid)
      condx <- tabchk$tab1
      pltx <- tabchk$tab2

      ## Check for matching unique identifiers of condx and pltx
      condx <- check.matchval(condx, pltx, cuniqueid, puniqueid,
			tab1txt=paste0("cond-", cuniqueid),
			tab2txt=paste0("plt-", puniqueid), subsetrows=TRUE)

      nrow.before <- nrow(pltx)

       ## Merge cond to plt (Note: inner join to use only plots with sampled conditions)
#      if (keepplots) {
#        condcols <- unique(c(cuniqueid, names(condx)[!names(condx) %in% names(pltx)]))
#        pltcondx <- merge(pltx, condx[, condcols, with=FALSE],
#				by.x=puniqueid, by.y=cuniqueid, all.x=TRUE)
#        if (cuniqueid != puniqueid) {
#          setnames(pltcondx, puniqueid, cuniqueid)
#        }
#      } else {
        pltcols <- unique(c(puniqueid, names(pltx)[!names(pltx) %in% names(condx)]))
#        pltcondx <- merge(condx, pltx[, pltcols, with=FALSE],
#				by.x=cuniqueid, by.y=puniqueid)
        pltcondx <- merge(pltx[, pltcols, with=FALSE], condx,
				by.x=puniqueid, by.y=cuniqueid)

        if ("CN" %in% names(pltcondx) && !"PLT_CN" %in% names(pltcondx)) {
          setnames(pltcondx, "CN", cuniqueid)
        }
        if (!cuniqueid %in% names(pltcondx) && puniqueid %in% names(pltcondx)) {
          setnames(pltcondx, puniqueid, cuniqueid)
        }
        setkeyv(pltcondx, c(cuniqueid, condid))
#      }

      nrow.after <- length(unique(pltcondx[[cuniqueid]]))
      if (nrow.after < nrow.before) {
        message(abs(nrow.after - nrow.before), " plots were removed from population")
      }
    } else if (popType == "LULC" && !is.null(lulcx)) {
      condx <- lulcx
      if ("PROP_CHNG" %in% names(condx)) {
        areawt <- "PROP_CHNG"
        cvars2keep <- unique(c(cvars2keep, "PROP_CHNG"))
      }
      cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", gui=gui,
		checklst=names(condx), caption="Unique identifier of plot",
		warn=paste(cuniqueid, "not in cond table"), stopifnull=TRUE)
      setkeyv(condx, cuniqueid)

      ## Check if class of puniqueid in pltx matches class of puniqueid in condx
      tabchk <- check.matchclass(condx, pltx, cuniqueid, puniqueid)
      condx <- tabchk$tab1
      pltx <- tabchk$tab2

      ## Check for matching unique identifiers of condx and pltx
      condx <- check.matchval(condx, pltx, cuniqueid, puniqueid, tab1txt="cond",
			tab2txt="plt", subsetrows=TRUE)

      nrow.before <- nrow(pltx)

      pltcols <- unique(c(puniqueid, names(pltx)[!names(pltx) %in% names(condx)]))
#      pltcondx <- merge(condx, pltx[, pltcols, with=FALSE],
#				by.x=cuniqueid, by.y=puniqueid)
      pltcondx <- merge(pltx[, pltcols, with=FALSE], condx,
				by.x=puniqueid, by.y=cuniqueid)
      setnames(pltcondx, "CN", "PLT_CN")

      nrow.after <- length(unique(pltcondx[[cuniqueid]]))
      if (nrow.after < nrow.before) {
        message(abs(nrow.after - nrow.before), " plots were removed from population")
      }
    }

    if (is.null(condx)) {
      pltcondx <- pltx
      cuniqueid <- puniqueid
      condid <- pcheck.varchar(var2check=condid, varnm="condid", gui=gui,
		checklst=names(condx), caption="Unique identifier of plot",
		warn=paste(condid, "not in cond table"), stopifnull=FALSE)
      if (is.null(condid)) {
        condx[, CONDID := 1]
      }
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
      } else if (adj == "samp" && !designcd %in% c(1, 501:505, 230:242, 328)) {
        message("samp adjustment for trees is only for annual inventory")
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
    states <- pcheck.states(stcds)
    if ("INVYR" %in% names(pltcondx)) {
      invyrtab <- unique(pltcondx[, c("STATECD", "INVYR")])
      setorderv(invyrtab, c("STATECD", "INVYR"))
      invyrs <- as.list(by(invyrtab$INVYR, invyrtab$STATECD, I))
      names(invyrs) <- pcheck.states(names(invyrs))
    }
  }

  ######################################################################################
  ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
  ######################################################################################
  if (any(c("PLOT_STATUS_CD", "PSTATUSCD") %in% pltcondnmlst)) {
    if ("PSTATUSCD" %in% names(pltcondx))
      names(pltcondx)[names(pltcondx) == "PSTATUSCD"] <- "PLOT_STATUS_CD"
    ref_plot_status_cd <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "PLOT_STATUS_CD", ]
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
		FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "NF_PLOT_STATUS_CD", ]
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

  ## Remove plots that have not been remeasured
  if (popType == "GRM") {
    pltcondx <- pltcondx[!is.na(pltcondx$REMPER), ]
  }


  ######################################################################################
  ## Check unitvar - if NULL, add unitvar=ONEUNIT to pltcondx
  ######################################################################################
  if (is.null(unitvar)) {
    unitvar <- checknm("ONEUNIT", names(pltcondx))
    message("no unitvar specified...  adding a variable named ", unitvar)
    pltcondx[, (unitvar) := 1]
    pvars2keep <- c(unitvar, pvars2keep)
    unitvars <- unitvar
    #areavar <- NULL
  }
 
  ###################################################################################
  ## CHECK unitarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and area by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  vars2keep <- NULL
  if (module == "SA" && "AOI" %in% names(unitarea)) {
    vars2keep <- "AOI"
  }
  removeunits <- ifelse(unit.action == "remove", TRUE, FALSE)
  unitdat <- check.unitarea(unitarea=unitarea, pltx=pltcondx,
	unitvars=c(unitvar, unitvar2), areavar=areavar, areaunits=areaunits,
	removeunits=removeunits, removetext=removetext, gui=gui,
	vars2keep=vars2keep)
  unitarea <- unitdat$unitarea
  areavar <- unitdat$areavar
  areaunits <- unitdat$areaunits

  if (!unitindb && !is.null(evalid)) {
    ecol <- pcheck.varchar("EVALID", checklst=names(unitarea), stopifinvalid=FALSE)
    if (!is.null(ecol)) {
      unitarea <- unitarea[unitarea[[ecol]] %in% evalid,]
      if (nrow(unitarea) == 0) {
        stop("evalid in unitarea does not match evalid")
      }
    }
  }

  ######################################################################################
  ## Strata - Generate table of plots by strata, including nonsampled plots (P2POINTCNT)
  ######################################################################################
  if (strata) {
    stratalut <- pcheck.table(stratalut, tab_dsn=dsn, tabnm="stratalut",
		caption="stratalut table?", nullcheck=nullcheck, tabqry=stratalutqry,
		returnsf=FALSE)
    if (!stratindb && !is.null(evalid)) {
      ecol <- pcheck.varchar("EVALID", checklst=names(stratalut), stopifinvalid=FALSE)
      if (!is.null(ecol)) {
        stratalut <- stratalut[stratalut[[ecol]] %in% evalid,]
        if (nrow(stratalut) == 0) {
          stop("evalid in stratalut does not match evalid")
        }
      }
    }
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
  ## Generate and apply nonsamp.pfilter
  #############################################################################
  if ((is.null(nonsamp.pfilter) || nonsamp.pfilter == "") && adj != "none") {
    if ("PLOT_STATUS_CD" %in% names(pltcondx)) {
      if (sum(pltcondx$PLOT_STATUS_CD == 3, na.rm=TRUE) > 0) {
        message("removing ", sum(pltcondx$PLOT_STATUS_CD == 3), " nonsampled forest plots")
        nonsamp.pfilter <- "PLOT_STATUS_CD != 3"
      }
    }
  }
  ## Apply nonsamp.pfilter
  if (!is.null(nonsamp.pfilter) && nonsamp.pfilter != "NONE") {
    pltcondx <- datFilter(x=pltcondx, xfilter=nonsamp.pfilter,
		title.filter="nonsamp.pfilter", gui=gui)$xf
    if (is.null(pltcondx)) {
      message(paste(nonsamp.pfilter, "removed all records"))
      return(NULL)
    }
  }

  ## Check for NA values in pvars2keep variables
  pvars.na <- sapply(pvars2keep, function(x, pltcondx){
					sum(is.na(pltcondx[, x, with=FALSE])) }, pltcondx)
  if (any(pvars.na > 0)) {
    stop(paste(pvars.na[pvars.na > 0], "NA values in variable:",
		paste(names(pvars.na[pvars.na > 0]), collapse=", ")))
  }

  ###################################################################################
  ###################################################################################
  ## Check condition data
  ###################################################################################
  ###################################################################################

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
    ref_cond_status_cd <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "COND_STATUS_CD", ]

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
		FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "NF_COND_STATUS_CD", ]
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
  ## Generate and apply nonsamp.cfilter
  #############################################################################
  if ((is.null(nonsamp.cfilter) || nonsamp.cfilter == "") && adj != "none") {
    if ("COND_STATUS_CD" %in% pltcondnmlst) {
      nonsamp.cfilter <- "COND_STATUS_CD != 5"
      nonsampn <- sum(pltcondx$COND_STATUS_CD == 5, na.rm=TRUE)
      if (length(nonsampn) > 0) {
        message("removing ", nonsampn, " nonsampled forest conditions")
      }
    }
    if (ACI && "NF_COND_STATUS_CD" %in% pltcondnmlst) {
      nonsamp.cfilter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
      message("removing ", sum(is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 5, na.rm=TRUE),
		" nonsampled nonforest conditions")
      if (!is.null(nonsamp.cfilter)) {
        nonsamp.cfilter <- paste(nonsamp.cfilter, "&", nonsamp.cfilter.ACI)
      }
    }
    if (popType == "LULC" && "PREV_COND_STATUS_CD" %in% names(pltcondx)) {
      nonsamp.cfilter.lulc <- "(is.na(PREV_COND_STATUS_CD) | PREV_COND_STATUS_CD != 5)"
      if (!is.null(nonsamp.cfilter)) {
        nonsamp.cfilter <- paste(nonsamp.cfilter, "&", nonsamp.cfilter.lulc)
      }
    }
  }

  ## Apply nonsamp.cfilter
  if (!is.null(nonsamp.cfilter) && nonsamp.cfilter != "NONE") {
    pltcondx <- datFilter(x=pltcondx, xfilter=nonsamp.cfilter,
		title.filter="nonsamp.cfilter", gui=gui)$xf
    if (is.null(pltcondx)) {
      message(paste(nonsamp.cfilter, "removed all records"))
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
    treenmlst <- names(treex)

    ## Check unique identifiers
    tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui,
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
    tabchk <- check.matchclass(pltcondx, treex, key(pltcondx), key(treex))
    pltcondx <- tabchk$tab1
    treex <- tabchk$tab2

    ## Check for missing tvars2keep
    tmissvars <- tvars2keep[which(!tvars2keep %in% treenmlst)]
    if (length(tmissvars) > 0) {
      stop("missing necessary variables from tree: ", paste(tmissvars, collapse=", "))
    }

    ## Check for NA values in tvars2keep variables
    ## TPA_UNADJ=NA, but trees have a DIA
    ## these are down dead trees that only count in growth and mortality,
    ## but wouldn't be measured if they hadn't been alive at the previous inventory

    if (length(tvars2keep) > 0) {
      tvars.na <- sapply(c(tuniqueid, condid, tvars2keep),
		function(x, treex){ sum(is.na(treex[,x, with=FALSE])) }, treex)
      if (any(tvars.na) > 0) {
        stop(tvars.na[tvars.na > 0], " NA values in variable: ",
		paste(names(tvars.na[tvars.na > 0]), collapse=", "))
      }
    }

    ## Add necessary variables to cvars2keep depending on data in tree
    ###################################################################
    ## If trees with DIA less than MICRO_BREAKPOINT_DIA exist in database
    ##   and there is no areawt_micr defined, the areawt will be used.
    ## If trees with DIA greater than MACRO_BREAKPOINT_DIA exist in database
    ##    and there is no areawt_macr defined, the areawt will be used.
    if (adj != "none") {

      ## Check for condition proportion variables
      propchk <- check.PROP(treex, pltcondx, cuniqueid=cuniqueid, checkNA=FALSE,
		areawt=areawt, diavar=diavar, MICRO_BREAKPOINT_DIA=MICRO_BREAKPOINT_DIA,
		MACRO_BREAKPOINT_DIA=MACRO_BREAKPOINT_DIA,
		areawt_micr=areawt_micr, areawt_subp=areawt_subp, areawt_macr=areawt_macr)
      tpropvars <- propchk$tpropvars
      treex <- propchk$treex
      pltcondx <- propchk$condx
      cvars2keep <- unique(c(cvars2keep, unlist(tpropvars)))
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
    tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", gui=gui,
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
    tabchk <- check.matchclass(pltcondx, seedx, cuniqueid, tuniqueid)
    pltcondx <- tabchk$tab1
    seedx <- tabchk$tab2

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
  pltassgnvars <- c(cuniqueid, pvars2keep)
  if (popType == "P2VEG") {
    if ("P2VEG_SAMPLING_STATUS_CD" %in% names(pltcondx)) {
      pltassgnvars <- c(pltassgnvars, "P2VEG_SAMPLING_STATUS_CD")
    }
    if ("SAMP_METHOD_CD" %in% names(pltcondx)) {
      pltassgnvars <- c(pltassgnvars, "SAMP_METHOD_CD")
    } else {
      message("removing nonresponse from field-visited and remotely-sensed plots")
    }
  }
  pltassgnx <- unique(pltcondx[, pltassgnvars, with=FALSE])
  pltassgnid <- cuniqueid

  if ("STATECD" %in% pvars2keep) {
    pvars2keep <- pvars2keep[pvars2keep != "STATECD"]
  }

  cvars2keep <- cvars2keep[cvars2keep %in% names(pltcondx)]
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
      subpuniqueid <- pcheck.varchar(var2check=subpuniqueid, varnm="subpuniqueid",
		checklst=names(subplotx), caption="UniqueID variable of subplot",
		warn=paste(subpuniqueid, "not in subplot"), stopifnull=TRUE)
      ## Check for NA values in subpuniqueid
      subp.na <- sum(is.na(subplotx[[subpuniqueid]]))
      if (subp.na > 0) stop("NA values in ", subpuniqueid)

      subpid <- pcheck.varchar(var2check=subpid, varnm="subpid",
		checklst=names(subplotx), caption="ID of subplot",
		warn=paste(subpid, "not in subplot"), stopifnull=TRUE)
      ## Check for NA values in subpuniqueid
      subp.na <- sum(is.na(subplotx[[subpid]]))
      if (subp.na > 0) stop("NA values in ", subpid)
      setkeyv(subplotx, c(subpuniqueid, subpid))

      ## Remove nonsampled subplots (SUBP_STATUS_CD = 3)
      if ("SUBP_STATUS_CD" %in% names(subplotx)) {
        subplotx <- subplotx[subplotx$SUBP_STATUS_CD < 3,]
      } else {
        message("SUBP_STATUS_CD not in subplot... assuming all sampled subplots")
      }
    }
    if (!is.null(subp_condx)) {
      subpuniqueid <- pcheck.varchar(var2check=subpuniqueid, varnm="subpuniqueid",
		checklst=names(subp_condx), caption="UniqueID variable of subp_cond",
		warn=paste(subpuniqueid, "not in sub_cond"), stopifnull=TRUE)
      ## Check for NA values in necessary variables in subp_cond table
      subpc.na <- sum(is.na(subp_condx[[subpuniqueid]]))
      if (subpc.na > 0) stop("NA values in ", subpuniqueid)

      subpid <- pcheck.varchar(var2check=subpid, varnm="subpid",
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
        tabchk <- check.matchclass(subplotx, subp_condx, matchcol=c(subpuniqueid, subpid))
        subplotx <- tabchk$tab1
        subp_condx <- tabchk$tab2

        cols <- c(names(subp_condx)[!names(subp_condx) %in% names(subplotx)],
				subpuniqueid, subpid)
        subp_condx <- merge(subplotx, subp_condx[, cols, with=FALSE])
      }
      if (!"SUBPCOND_PROP" %in% names(subp_condx)) {
        stop("must include SUBPCOND_PROP in subp_cond")
      }
      #table(subplot$SUBP_STATUS_CD, subplot$P2VEG_SUBP_STATUS_CD)
    } else {
      stop("must include subp_condx for P2VEG estimation")
    }

    #############################################################################
    ## Subset pltassgn to sampled P2VEG and merge to subp_condx
    #############################################################################
    if (!"P2VEG_SAMPLING_STATUS_CD" %in% names(pltassgnx)) {
      message("assuming all plots sample P2VEG")
      pltassgnx.P2VEG <- pltassgnx
    } else {
      pltassgnx.P2VEG <- pltassgnx[pltassgnx[["P2VEG_SAMPLING_STATUS_CD"]] %in% c(1,2),]
    }

    ## Merge pltassgnx.P2VEG to subp_condx - inner join
    subp_vars <- unique(c("PLT_CN",
			names(subp_condx)[!names(subp_condx) %in% names(pltassgnx.P2VEG)]))
    subp_condf <- merge(pltassgnx.P2VEG, subp_condx[, subp_vars, with=FALSE] , by="PLT_CN")

    #############################################################################
    ## Define and apply subp.nonsamp.filter
    #############################################################################
#    if (!"SUBP_STATUS_CD" %in% names(subp_condf)) {
#      message("SUBP_STATUS_CD not in subp_cond... assuming all subplots are sampled")
#    } else {
#      subp.nonsamp.filter <- "SUBP_STATUS_CD == 1"
#      if (ACI && "NF_SUBP_STATUS_CD" %in% names(subp_condf)) {
#        subp.nonsamp.filter.ACI <- "(is.na(NF_SUBP_STATUS_CD) | NF_SUBP_STATUS_CD != 3)"
#        message("removing ", sum(is.na(NF_SUBP_STATUS_CD) & NF_SUBP_STATUS_CD == 3, na.rm=TRUE),
#		" nonsampled subplot conditions")
#        if (!is.null(subp.nonsamp.filter))
#          subp.nonsamp.filter <- paste(subp.nonsamp.filter, "&", subp.nonsamp.filter.ACI)
#      }
#    }
#    ## Apply subp.nonsamp.filter
#    subp_condf <- datFilter(x=subp_condf, xfilter=subp.nonsamp.filter,
#		title.filter="subp.nonsamp.filter")$xf
#    if (is.null(subp_condf)) {
#      message(paste(subp.nonsamp.filter, "removed all records"))
#      return(NULL)
#    }

    #############################################################################
    ## Define and apply p2veg.nonsamp.filter
    #############################################################################
    if ("P2VEG_SUBP_STATUS_CD" %in% names(subp_condf)) {
      if (nonsamp.vfilter.fixed) {
        p2veg.nonsamp.filter <- "(SAMP_METHOD_CD == 1 & P2VEG_SUBP_STATUS_CD == 1) |
			SAMP_METHOD_CD == 2"
      } else {
        p2veg.nonsamp.filter <- "(SAMP_METHOD_CD == 1 & P2VEG_SUBP_STATUS_CD == 1 |
 			is.na(P2VEG_SUBP_STATUS_CD)) | SAMP_METHOD_CD == 2"
      }

      ## It should be this after database is fixed
      ## So, when SAMP_METHOD_CD == 1 & P2VEG_SUBP_STATUS_CD == 1,
      ##		P2VEG_SUBP_STATUS_CD should equal 2
      subp_condf <- datFilter(x=subp_condf, xfilter=p2veg.nonsamp.filter,
		title.filter="p2veg.nonsamp.filter")$xf

      if (is.null(subp_condf)) {
        message(paste(p2veg.nonsamp.filter, "removed all records"))
        return(NULL)
      }
    }

    #############################################################################
    ## Sum subplot conditions and append to condx table
    #############################################################################
    SUBP_CONDPROP_UNADJ <- subp_condf[, list(SUBP_CONDPROP_UNADJ =
      sum(ifelse(!is.na(MACRCOND_PROP), MACRCOND_PROP, SUBPCOND_PROP), na.rm=TRUE)/4),
			by=c("PLT_CN", "CONDID")]
    setkeyv(SUBP_CONDPROP_UNADJ, c(subpuniqueid, condid))
    setkeyv(condx, c(cuniqueid, condid))

    ## Merge summed subplot condition proportions to condx
    vcondx <- merge(condx, SUBP_CONDPROP_UNADJ)


    #############################################################################
    ## Check veg profile data
    #############################################################################

    if (!is.null(vsubpsppx) && nrow(vsubpsppx) > 0) {
      ## Define necessary variable for tree table
      vsubpsppnmlst <- names(vsubpsppx)

      ## Check unique identifiers
      vsubpsppid <- pcheck.varchar(var2check=vsubpsppid, varnm="vsubpsppid", gui=gui,
		checklst=vsubpsppnmlst, caption="UniqueID variable of veg spp",
		warn=paste(vsubpsppid, "not in vegspspp"), stopifnull=TRUE)
      cvars2keep <- c(cvars2keep, "SUBPCOND_PROP")

      ## Check for NA values in necessary variables in tree table
      vsubpsppx.na <- sum(is.na(vsubpsppx[[vsubpsppid]]))
      if (vsubpsppx.na > 0) stop("NA values in ", vsubpsppid)

      if (vsubpsppid %in% pltcondnmlst) {
        idplace <- which(pltcondnmlst %in% vsubpsppid)
        if (idplace != 1) {
	    pltcondnmlst <- c(vsubpsppid, pltcondnmlst)
	    pltcondnmlst <- pltcondnmlst[-(idplace + 1)]
        }
      }

      ## Check that the values of vsubpsppid in vsubpsppx are all in cuniqueid in subp_condf
      vsubpsppf <- check.matchval(vsubpsppx, vcondx, c(vsubpsppid, condid),
		tab1txt="vsubpspp", tab2txt="subp_cond", subsetrows=TRUE)
      setkeyv(vsubpsppf, c(subpuniqueid, condid))

      ## Summarize vsubpsppf columns and divide by 4 (subplots) by condition
      covpctnm <- findnm("COVER_PCT", names(vsubpsppf))
      vcols <- c("VEG_FLDSPCD", "VEG_SPCD", "GROWTH_HABIT_CD", "LAYER")
      vcols <- vcols[vcols %in% names(vsubpsppf)]
      vcondsppf <- vsubpsppf[, list(COVER_PCT_SUM = sum(get(covpctnm), na.rm=TRUE)/4/100),
		by=c(vsubpsppid, condid, vcols)]
      setkeyv(vcondsppf, c(subpuniqueid, condid))

      ## Merge condition sums to pltcondx
      #vpltcondx <- merge(pltcondx, vcondsppf, all.x=TRUE)
    }
    if (!is.null(vsubpstrx) && nrow(vsubpstrx) > 0) {
      ## Define necessary variable for tree table
      vsubpstrnmlst <- names(vsubpstrx)

      ## Check unique identifiers
      vsubpstrid <- pcheck.varchar(var2check=vsubpstrid, varnm="vsubpstrid", gui=gui,
		checklst=vsubpstrnmlst, caption="UniqueID variable of veg structure",
		warn=paste(vsubpstrid, "not in vegspstr"), stopifnull=TRUE)

      ## Check for NA values in necessary variables in tree table
      vsubpstrx.na <- sum(is.na(vsubpstrx[[vsubpstrid]]))
      if (vsubpstrx.na > 0) stop("NA values in ", vsubpstrid)

      if (vsubpstrid %in% pltcondnmlst) {
        idplace <- which(pltcondnmlst %in% vsubpstrid)
        if (idplace != 1) {
	    pltcondnmlst <- c(vsubpstrid, pltcondnmlst)
	    pltcondnmlst <- pltcondnmlst[-(idplace + 1)]
        }
      }

      ## Check that the values of vsubpstrid in vsubpsppx are all in cuniqueid in subp_condf
      vsubpstrf <- check.matchval(vsubpstrx, vcondx, c(vsubpstrid, condid),
		tab1txt="vsubpstr", tab2txt="subp_cond", subsetrows=TRUE)
      setkeyv(vsubpstrf, c(subpuniqueid, condid))

      ## Summarize vsubpsppf columns and divide by 4 (subplots) by condition
      covpctnm <- findnm("COVER_PCT", names(vsubpstrf))
      vcols <- c("GROWTH_HABIT_CD", "LAYER")
      vcols <- vcols[vcols %in% names(vsubpstrf)]
      vcondstrf <- vsubpstrf[, list(COVER_PCT_SUM = sum(get(covpctnm), na.rm=TRUE)/4/100),
		by=c(vsubpstrid, condid, vcols)]
#      vcondstrf <- vsubpstrf[, list(COVER_PCT_SUM = sum(get(covpctnm), na.rm=TRUE)),
#		by=c(vsubpstrid, condid, vcols)]
      setkeyv(vcondstrf, c(subpuniqueid, condid))
    }

    ## Merge condition sums to pltcondx
    #vpltcondx <- merge(pltcondx, vsubpstrf, all.x=TRUE)
  }

  ## Subset pltcondx
  #cdoms2keep <- cdoms2keep[!cdoms2keep %in% c(pvars2keep, cvars2keep)]
  #pltcondx <- pltcondx[, unique(c(cuniqueid, condid, pdoms2keep, cdoms2keep)), with=FALSE]


  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(condx=condx, pltcondx=pltcondx, pltassgnx=pltassgnx,
	cuniqueid=cuniqueid, condid=condid, pltassgnid=pltassgnid, unitvar=unitvar,
	unitarea=unitarea, unitvar2=unitvar2, areavar=areavar, areaunits=areaunits,
	unit.action=unit.action, prednames=prednames, predfac=predfac, adj=adj,
	strata=strata, strvar=strvar, stratcombine=stratcombine, nonresp=nonresp,
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
    returnlst$pltassgnx <- pltassgnx.P2VEG
    returnlst$condx <- vcondx
    if (!is.null(vcondsppf)) {
      returnlst$vcondsppf <- vcondsppf
      returnlst$vcondsppid <- vsubpstrid
    }
    if (!is.null(vcondstrf)) {
      returnlst$vcondstrf <- vcondstrf
      returnlst$vcondstrid <- vsubpstrid
    }
  }
  if (module %in% c("MA", "SA")) {
    returnlst$prednames <- prednames
    returnlst$predfac <- predfac
  }
  if (ACI) {
    returnlst$nfplotsampcnt <- nfplotsampcnt
  }
  if (strata) {
    returnlst$stratalut <- stratalut
  }
  if (nonresp) {
    returnlst$substrvar <- substrvar
    returnlst$nonresplut <- nonresplut
  }
  if (adj != "none") {
    returnlst$tpropvars <- tpropvars
  }

  return(returnlst)
}
