check.popdataDWM <- function(tabs, tabIDs, pltassgnx, pltassgnid,
	pfromqry, palias, pjoinid, whereqry, adj, ACI, pltx=NULL, puniqueid="CN", 
	dsn=NULL, condid="CONDID", areawt="CONDPROP_UNADJ",
	nonsamp.cfilter=NULL, nullcheck=FALSE, cvars2keep=NULL, 
	dwmvars2keep=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs for DWM estimation
  ## Define necessary plot and condition-level variables:
  ## - cond (cvars2keep) - areawt
  ## - dwm variables (dwmvars2keep)
  ## Import and check cond, cond_dwm_calc, plt, pltassgn tables
  ## Merge cond and pltx
  ## Check condition data
  ## - Check condid (NA values and duplicate records (cuniqueid, condid)
  ## - Check for areawt (if not included, add CONDPROP_UNADJ=1
  ## - Check for COND_STATUS_CD (if not included, add COND_STATUS_CD = PLOT_STATUS_CD with 3=5)
  ## - Generate table of sampled/nonsampled plots and conditions (if COND_STATUS_CD included)
  ## - If ACI, add table of sampled/nonsampled nonforest conditions (if NF_COND_STATUS_CD included)
  ## - IF ACI=FALSE, create ACI.filter="COND_STATUS_CD == 1"
  ## - Generate and apply cond.nonsample filter for condx ("COND_STATUS_CD != 5")
  ## - If ACI, add "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
  ## Check cond_dwm_calc data
  ## - Import cond_dwm_calc table and check unique identifier (duniqueid)
  ## - Check for condid in cond_dwm_calc... if no condid, add CONDID=1
  ## - Check if class of duniqueid matches class of cuniqueid in cond
  ## - Check if all values of cond_dwm_calc are in cond and subset rows to match cond
  ## - Check for missing dwmvars2keep and NA values in dwmvars2keep
  ## Subset variables for pltassgnx, condx, and pltcondx
  ###################################################################################

  ## Set global variables
  COND_STATUS_CD=CONDID=CONDPROP_UNADJ=SUBPPROP_UNADJ=MICRPROP_UNADJ=MACRPROP_UNADJ=
	STATECD=cndnmlst=PROP_BASIS=ACI.filter=condsampcnt=
	NF_COND_STATUS_CD=condqry=cfromqry=dwmqry=cwdvars2keep <- NULL

  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  cvars2keep <- unique(c(cvars2keep, areawt, "PROP_BASIS"))

  cwdvars2keep <- c("CWD_LPA_UNADJ", "CWD_VOLCF_UNADJ", "CWD_DRYBIO_UNADJ",
				"CWD_CARBON_UNADJ")
  fwdvars2keep <- c("FWD_SM_VOLCF_UNADJ", "FWD_SM_DRYBIO_UNADJ", "FWD_SM_CARBON_UNADJ",
				"FWD_MD_VOLCF_UNADJ", "FWD_MD_DRYBIO_UNADJ", "FWD_MD_CARBON_UNADJ",
				"FWD_LG_VOLCF_UNADJ", "FWD_LG_DRYBIO_UNADJ", "FWD_LG_CARBON_UNADJ")
  pilevars2keep <- c("PILE_VOLCF_UNADJ", "PILE_DRYBIO_UNADJ", "PILE_CARBON_UNADJ")
  duffvars2keep <- c("DUFF_VOLCF_UNADJ", "DUFF_DRYBIO_UNADJ", "DUFF_CARBON_UNADJ")
  dwmdoms2keep <- c(cwdvars2keep, fwdvars2keep)
  datindb <- FALSE

  ## Get tables from tabs
  ########################################################## 
  cond=cond_dwm_calc <- NULL
  for (tabnm in names(tabs)) {
    assign(tabnm, tabs[[tabnm]])
  }
  cuniqueid <- tabIDs[["cond"]]
  duniqueid <- tabIDs[["cond_dwm_calc"]]


  ## Check dsn and create queries to get population subset from database
  ###################################################################################
  if (!is.null(dsn) && getext(dsn) %in% c("sqlite", "db", "db3", "sqlite3", "gpkg")) {
    datindb <- TRUE
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE, showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    chk <- TRUE
    SCHEMA. <- NULL
    dbqueries <- list()

    ## Create query for cond
    #########################################
    if (all(!is.null(cond), is.character(cond), cond %in% tablst)) {
      #condvars <-  DBvars.default()$condvarlst

      if (is.null(pfromqry)) {
        cfromqry <- paste0(SCHEMA., cond, " c")
      } else {
        cfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., cond,
				" c ON (c.", cuniqueid, " = ", palias, ".", pjoinid, ")")
      }
#      condqry <- paste("select distinct", toString(paste0("c.", condvars)), 
#				"from", cfromqry, whereqry)
      condqry <- paste("select distinct c.* from", cfromqry, whereqry)
      dbqueries$cond <- condqry   
    }

    ## Create query for dwm
    #########################################
    if (all(!is.null(cond_dwm_calc), is.character(cond_dwm_calc), cond_dwm_calc %in% tablst)) {
      dwmfromqry <- paste0(SCHEMA., cond_dwm_calc)
      dwmqry <- paste("select distinct * from", dwmfromqry, whereqry)
    }
  }
 
  ###################################################################################
  ## Import tables
  ###################################################################################
  if (is.null(cond)) {
    stop("must include cond table")
  }
  condx <- suppressMessages(pcheck.table(cond, tab_dsn=dsn, 
           tabnm="cond", caption="cond table?",
		nullcheck=nullcheck, tabqry=condqry, returnsf=FALSE))
  cond_dwm_calcx <- suppressMessages(pcheck.table(cond_dwm_calc, tab_dsn=dsn, 
           tabnm="cond_dwm_calc", caption="lulc table?", 
           nullcheck=nullcheck, tabqry=dwmqry, returnsf=FALSE))
 
  ## Define cdoms2keep
  cdoms2keep <- names(condx)


  ###############################################################################
  ## Check uniqueids and merge cond with plt
  ###############################################################################
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


  ## Merge pltx to condx
  ###################################################################
  if (!is.null(pltx)) {

    ## Set key
    setkeyv(pltx, puniqueid)

    ## Subset condition columns
    cvars <- unique(c(cuniqueid, names(condx)[!names(condx) %in% names(pltx)])) 
    condx <- condx[, cvars, with=FALSE]


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
    pltcols <- unique(c(puniqueid, names(pltx)[!names(pltx) %in% names(condx)]))
    pltcondx <- tryCatch(merge(pltx[, pltcols, with=FALSE], condx,
				by.x=puniqueid, by.y=cuniqueid),
     	 	error=function(e) {
			return(NULL) })
    if (is.null(pltcondx)) {
      stop("invalid dataset")
    }

    if ("CN" %in% names(pltcondx) && !"PLT_CN" %in% names(pltcondx)) {
      setnames(pltcondx, "CN", cuniqueid)
    }
    if (!cuniqueid %in% names(pltcondx) && puniqueid %in% names(pltcondx)) {
      setnames(pltcondx, puniqueid, cuniqueid)
    }
    setkeyv(pltcondx, c(cuniqueid, condid))

    nrow.after <- length(unique(pltcondx[[cuniqueid]]))
    if (nrow.after < nrow.before) {
      message(abs(nrow.after - nrow.before), " plots were removed from population")
    }
  } else {
    pltcondx <- condx

    ## Check for matching unique identifiers of pltcondx with pltassgnx
    ## Subset pltx to pltassgnx ids
    pltcondx <- check.matchval(pltcondx, pltassgnx, cuniqueid, pltassgnid, 
			tab1txt="cond", tab2txt="pltassgn", subsetrows=TRUE)
  }

  ###################################################################################
  ## Check condition data
  ###################################################################################
  pltcondnmlst <- names(pltcondx)

  ## Check for COND_STATUS_CD and create ACI filter
  #############################################################################
  if (!"COND_STATUS_CD" %in% pltcondnmlst) {
    message("COND_STATUS_CD not in dataset.. assuming all sampled conditions")
    cvars2keep <- cvars2keep[cvars2keep != "COND_STATUS_CD"]
  }

  #############################################################################
  ## Generate table of sampled/nonsampled conditions from condx
  #############################################################################
  if ("COND_STATUS_CD" %in% pltcondnmlst) {
    condsampcnt <- pltcondx[, list(NBRCOND=.N), by=COND_STATUS_CD]
    ref_cond_status_cd <- 
	FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "COND_STATUS_CD", ]

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
        message("For FIA estimation, adjustment factors are calculated to account for plots with partial nonresponse.")
        message("...there are ", nonsampn, " nonsampled forest conditions in the dataset.")
      }
    }
    if (ACI && "NF_COND_STATUS_CD" %in% pltcondnmlst) {
      nonsamp.cfilter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
      message("...there are ", sum(is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 5, na.rm=TRUE),
		" nonsampled nonforest conditions in the dataset.")
      if (!is.null(nonsamp.cfilter)) {
        nonsamp.cfilter <- paste(nonsamp.cfilter, "&", nonsamp.cfilter.ACI)
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
  ## Check area weight 
  ###################################################################################
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



  ###################################################################################
  ###################################################################################
  ## Check cond_dwm_calc
  ###################################################################################
  ###################################################################################
  dwmnmlst <- names(cond_dwm_calcx)
  dwmpropvars <- dwmnmlst[grepl("CONDPROP", dwmnmlst, ignore.case=TRUE)]
  dwmvars2keep <- c(dwmvars2keep, dwmpropvars)

  duniqueid <- pcheck.varchar(var2check=duniqueid, varnm="duniqueid", gui=gui,
		checklst=dwmnmlst, caption="Unique identifier of plot",
		warn=paste(duniqueid, "not in cond table"), stopifnull=TRUE)
  setkeyv(cond_dwm_calcx, duniqueid)

  ## Check for NA values in necessary variables in cond_dwm_calc table
  cond_dwm_calcx.na <- sum(is.na(cond_dwm_calcx[[duniqueid]]))
  if (cond_dwm_calcx.na > 0) stop("NA values in ", duniqueid)

  condid <- pcheck.varchar(var2check=condid, varnm="condid", gui=gui,
		checklst=dwmnmlst, caption="Unique identifier of plot",
		warn=paste(condid, "not in cond table"), stopifinvalid=FALSE)
  if (is.null(condid)) {
    if (nrow(cond_dwm_calcx) == length(unique(cond_dwm_calcx[[duniqueid]]))) {
      cond_dwm_calcx[, CONDID := 1]
      condid <- "CONDID"
    } else {
      stop("there is more than 1 record per plot... must include valid CONDID")
    }
  }
  ## Check for NA values in necessary variables in cond table
  cond_dwm_calcx.na <- sum(is.na(cond_dwm_calcx[[condid]]))
  if (cond_dwm_calcx.na > 0) stop("NA values in ", condid)

  ## Check if 1 plot-condition per record in cond
  ######################################################
  condid.dupid <- condx[duplicated(cond_dwm_calcx, by=c(duniqueid, condid))][[duniqueid]]

  if (length(condid.dupid) > 0) {
    msg <- paste("check cuniqueid/condid... duplicate records")
    if (length(condid.dupid) < 20) print(condid.dupid)
    stop(msg)
  }
  setkeyv(cond_dwm_calcx, c(duniqueid, condid))


  ## Check if class of duniqueid in cond_dwm_calcx matches class of duniqueid in condx
  tabchk <- check.matchclass(condx, cond_dwm_calcx, cuniqueid, duniqueid)
  condx <- tabchk$tab1
  cond_dwm_calcx <- tabchk$tab2

  ## Check for matching unique identifiers of condx and pltx
  condx <- check.matchval(condx, cond_dwm_calcx, cuniqueid, duniqueid,
			tab1txt=paste0("cond-", cuniqueid),
			tab2txt=paste0("cond_dwm_calc-", duniqueid), subsetrows=TRUE)


  ## Check for missing dwmvars2keep
  ######################################################
  dwmmissvars <- dwmvars2keep[which(!dwmvars2keep %in% dwmnmlst)]
  if (length(dwmmissvars) > 0) {
    if (length(dwmmissvars) == length(dwmvars2keep)) {
      stop("missing all necessary variables from cond_dwm_calc: ", 
			paste(dwmmissvars, collapse=", "))
    } else {
      message("missing necessary variables from cond_dwm_calc: ", 
			paste(dwmmissvars, collapse=", "))
      dwmvars2keep <- dwmvars2keep[!dwmvars2keep %in% dwmmissvars]
    } 
  }

  ## Check for NA values in dwmvars2keep variables
  dwmvars.na <- sapply(c(duniqueid, condid, dwmvars2keep),
		function(x, cond_dwm_calcx){ sum(is.na(cond_dwm_calcx[,x, with=FALSE])) }, 
			cond_dwm_calcx)
  if (any(dwmvars.na) > 0) {
    stop(dwmvars.na[dwmvars.na > 0], " NA values in variable: ",
		paste(names(dwmvars.na[dwmvars.na > 0]), collapse=", "))
  }


  ########################################################################
  ## Separate and merge tables for estimation
  ########################################################################
#  if ("STATECD" %in% pvars2keep) {
#    pvars2keep <- pvars2keep[pvars2keep != "STATECD"]
#  }
  cvars2keep <- cvars2keep[cvars2keep %in% names(pltcondx)]
  condx <- unique(pltcondx[, c(cuniqueid, condid, cvars2keep), with=FALSE])
  pltcondx[, (cvars2keep) := NULL]


  ## Merge condx to cond_dwm_calcx to get variables for summed condition proportions
  condx <- merge(condx, cond_dwm_calcx[, c(duniqueid, condid, dwmvars2keep), with=FALSE])


  ## Merge condx to cond_dwm_calcx to get variables for summed condition proportions
  pltcondx <- merge(pltcondx, cond_dwm_calcx[, c(duniqueid, condid, dwmdoms2keep), with=FALSE])


  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, 
	condid=condid, condsampcnt=as.data.frame(condsampcnt),
	ACI.filter=ACI.filter, areawt=areawt, dwmpropvars=dwmpropvars)


  return(returnlst)
}
