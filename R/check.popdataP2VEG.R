check.popdataP2VEG <- function(tabs, tabIDs, pltassgnx, pltassgnid,
	pfromqry, palias, pjoinid, whereqry, adj, ACI, 
	pltx = NULL, puniqueid = "CN", dsn = NULL, dbconn = NULL, 
	condid = "CONDID", areawt = "CONDPROP_UNADJ",
	nonsamp.cfilter = NULL, nullcheck = FALSE, pvars2keep = NULL, 
	cvars2keep = NULL, gui = FALSE){

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
  ## Check P2VEG data
  ## - Import SUBPLOT, SUBP_COND tables and check unique identifiers (for adjfactors)
  ## - Import P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE tables and check unique identifiers
  ## - Check for condid in cond_dwm_calc... if no condid, add CONDID=1
  ## - Check if class of duniqueid matches class of cuniqueid in cond
  ## - Check if all values of cond_dwm_calc are in cond and subset rows to match cond
  ## - Check for missing dwmvars2keep and NA values in dwmvars2keep
  ## Subset variables for pltassgnx, condx, and pltcondx
  ###################################################################################

  ## Set global variables
  COND_STATUS_CD=CONDID=CONDPROP_UNADJ=SUBPPROP_UNADJ=MICRPROP_UNADJ=MACRPROP_UNADJ=
	STATECD=cndnmlst=PROP_BASIS=ACI.filter=condsampcnt=P2VEG_SAMPLING_STATUS_CD=
	NF_COND_STATUS_CD=condqry=cfromqry=vsubpsppqry=subplotqry=subp_condqry=
	vcondsppf=vcondstrf=MACRCOND_PROP=SUBPCOND_PROP <- NULL

  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  cvars2keep <- unique(c(cvars2keep, areawt, "PROP_BASIS"))
  subpvars2keep <- c("SUBP_STATUS_CD", "P2VEG_SUBP_STATUS_CD", 
			"MICRCOND_PROP", "SUBPCOND_PROP", "MACRCOND_PROP")
  datindb <- FALSE
  nonsamp.vfilter.fixed <- TRUE

checktabs <- function(tabs, names) {
  ## DESCRIPTION: check name in tabs list
  
  for (name in names) {
    chk <- findnm(name, names(tabs), returnNULL = TRUE)
	if (!is.null(chk)) {
	  return(chk)
    }
  }
  return(NULL)
}

  ## Get tables from tabs
  ##########################################################  
  condnm=vsubpstrnm=vsubpsppnm=subplotnm=subp_condnm <- NULL

  ## Check name of COND table
  pltnmchk <- checktabs(tabs, c("plt", "plot"))
  if (is.null(pltnmchk)) {
    message("plot data needed for estimates")
	return(NULL)
  }
  if (is.character(tabs[[pltnmchk]])) {  
    pltnm <- tabs[[pltnmchk]]
  } else {
    pltnm <- "plt"
  }

  ## Check name of COND table
  condnmchk <- checktabs(tabs, "cond")
  if (is.null(condnmchk)) {
    message("cond data needed for estimates")
	return(NULL)
  }
  if (is.character(tabs[[condnmchk]])) {  
    condnm <- tabs[[condnmchk]]
  } else {
    condnm <- "cond"
  }
  assign(condnm, tabs[[condnmchk]])
  cuniqueid <- tabIDs[[condnmchk]]

  ## Check name of P2VEG_SUBP_STRUCTURE table
  vsubpstrchk <- checktabs(tabs, c("p2veg_subp_structure", "vsubpstr"))
  if (is.character(tabs[[vsubpstrchk]])) {  
    vsubpstrnm <- tabs[[vsubpstrchk]]
  } else {
    vsubpstrnm <- "vsubpstr"
  }
  assign(vsubpstrnm, tabs[[vsubpstrchk]])
  vsubpstrid <- tabIDs[[vsubpstrchk]]

  ## Check name of P2VEG_SUBPLOT_SPP table
  vsubpsppchk <- checktabs(tabs, c("p2veg_subplot_spp", "vsubpspp"))
  if (!is.null(vsubpsppchk)) {
    if (is.character(tabs[[vsubpsppchk]])) {  
      vsubpsppnm <- tabs[[vsubpsppchk]]
    } else {
      vsubpsppnm <- "vsubpspp"
    }
    assign(vsubpsppnm, tabs[[vsubpsppchk]])
    vsubpsppid <- tabIDs[[vsubpsppchk]]
  }
  
  ## Check name of SUBPLOT table
  subplotchk <- checktabs(tabs, "subplot")
  if (is.character(tabs[[subplotchk]])) {  
    subplotnm <- tabs[[subplotchk]]
  } else {
    subplotnm <- "subplot"
  }
  assign(subplotnm, tabs[[subplotchk]])
  subplotid <- tabIDs[[subplotchk]]

  ## Check name of SUBP_COND table
  subp_condchk <- checktabs(tabs, c("subp_cond", "subpcond"))
  if (is.character(tabs[[subp_condchk]])) {  
    subp_condnm <- tabs[[subp_condchk]]
  } else {
    subp_condnm <- "subp_cond"
  }
  assign(subp_condnm, tabs[[subp_condchk]])
  subp_condid <- tabIDs[[subp_condchk]]


  SCHEMA. <- NULL
  dbqueries <- list()

  ## Check palias
  if (is.null(palias)) {
    palias <- "p"
  }

  ###################################################################################
  ## Database queries
  ###################################################################################
  if (!is.null(dbconn) || 
	(!is.null(dsn) && getext(dsn) %in% c("sqlite", "db", "db3", "sqlite3", "gpkg"))) {

    datindb <- TRUE
    if (is.null(dbconn)) {
      dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE, showlist=FALSE)
    }
    tablst <- DBI::dbListTables(dbconn)
    chk <- TRUE
    dbname <- dsn

    ## Check plt in database
    if (!is.null(pltnm) && is.character(pltnm)) {    
	  pltnm <- findnm(pltnm, tablst, returnNULL = TRUE)
	  if (is.null(pltnm)) {
        message("need PLOT table in database")
	    return(NULL)
      } else {
	    pltflds <- DBI::dbListFields(dbconn, pltnm)
      }
	}
    ## Check cond in database
    condnm <- findnm(condnm, tablst, returnNULL = TRUE)
	if (is.null(condnm)) {
      message("need COND table in database")
	  return(NULL)
    } else {
	  condflds <- DBI::dbListFields(dbconn, condnm)
    }
    ## Check subplot in database
    subplotnm <- findnm(subplotnm, tablst, returnNULL = TRUE)
	if (is.null(subplotnm)) {
      message("need SUBPLOT table in database")
	  return(NULL)
    }
    ## Check subp_cond in database
    subp_condnm <- findnm(subp_condnm, tablst, returnNULL = TRUE)
	if (is.null(subp_condnm)) {
      message("need SUBP_COND table in database")
	  return(NULL)
    }
    ## Check P2VEG_SUBP_STRUCTURE in database
    vsubpstrnm <- findnm(vsubpstrnm, tablst, returnNULL = TRUE)
	if (is.null(vsubpstrnm)) {
      message("need P2VEG_SUBP_STRUCTURE table in database")
	  return(NULL)
    }
    ## Check P2VEG_SUBPLOT_SPP in database
    vsubpsppnm <- findnm(vsubpsppnm, tablst, returnNULL = TRUE)
	
  } else {

    ## Get remeasured plot/condition data
	if (!is.null(condnm)) {
      assign(condnm, pcheck.table(get(condnm), tab_dsn=dsn, 
           tabnm="cond", caption="Remeasured condition data?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
	  condflds <- names(get(condnm))
	}

    ## Get remeasured plot data
    if (!is.null(pltx)) {
	  pltnm <- "pltx"
	  pltflds <- names(pltx)
	  
      if (!pjoinid %in% pltflds) {
	    if (puniqueid %in% pltflds) {
		  pjoinid <- puniqueid
		}
	  }  
    } 

    ## Get subplot data for generating estimates
    assign(subplotnm, pcheck.table(get(subplotnm), tab_dsn=dsn, 
           tabnm="subplot", caption="subplot table?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
		   
    ## Get subp_cond data for generating estimates
    assign(subp_condnm, pcheck.table(get(subp_condnm), tab_dsn=dsn, 
           tabnm="subp_cond", caption="subp_cond table?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
		   
    ## Get vsubpstr data for generating estimates
    assign(vsubpstrnm, pcheck.table(get(vsubpstrnm), tab_dsn=dsn, 
           tabnm="vsubpstr", caption="vsubpstr table?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))

    ## Get vsubpspp data for generating estimates
	if (!is.null(vsubpsppnm)) {
      assign(vsubpsppnm, pcheck.table(get(vsubpsppnm), tab_dsn=dsn, 
           tabnm="vsubpspp", caption="vsubpspp table?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
    }
  }  

  ## Build pfromqry
  if (is.null(pfromqry) && !is.null(pltnm)) {
    pfromqry <- paste0(SCHEMA., pltnm, " ", palias)
  }
  
  ## Build from query for cond
  if (!is.null(condnm)) {
    if (is.null(pfromqry)) {
      cfromqry <- paste0(SCHEMA., condnm, " c")
    } else {
      cfromqry <- paste0(pfromqry, 
	               "\nJOIN ", SCHEMA., condnm, 
				          " c ON (c.", cuniqueid, " = ", palias, ".", pjoinid, ")")
    }
    condqry <- paste("SELECT c.* \nFROM", cfromqry, whereqry)
    dbqueries$cond <- condqry
  }	

  ## Build from query for subplot
  subpfromqry <- paste0(pfromqry, 
                   "\nJOIN ", SCHEMA., subplotnm,
				         " subp ON (subp.PLT_CN = ", palias, ".", pjoinid, ")")
  subplotqry <- paste("SELECT subp.* \nFROM ", subpfromqry, whereqry)
  dbqueries$subplot <- subplotqry
    
  ## Build query for subp_cond
  subpcfromqry <- paste0(pfromqry, 
                    "\nJOIN ", SCHEMA., subp_condnm,
				        " subpc ON (subpc.PLT_CN = ", palias, ".", pjoinid, ")")
  subp_condqry <- paste("SELECT subpc.* \nFROM", subpcfromqry, whereqry)
  dbqueries$subp_cond <- subp_condqry

  ## Build query for vsubpstr
  if (!is.null(pfromqry)) {
    vsubpstr.fromqry <- paste0(pfromqry, 
	          "\nJOIN ", SCHEMA., vsubpstrnm,
				" vsubpstr ON (vsubpstr.PLT_CN = ", palias, ".", pjoinid, ")")
  } else {
    vsubpstr.fromqry <- paste(vsubpsppnm, "vsubpstr")
  }
  vsubpstrqry <- paste("SELECT vsubpstr.* \nFROM", vsubpstr.fromqry, whereqry)
  dbqueries$vsubpstr <- vsubpstrqry

  ## Build query for vsubpspp
  if (!is.null(vsubpsppnm)) {
    if (!is.null(pfromqry)) {
      vsubpspp.fromqry <- paste0(pfromqry, 
	           "\nJOIN ", SCHEMA., vsubpsppnm,
				" vsubpspp ON (vsubpspp.PLT_CN = ", palias, ".", pjoinid, ")")
    } else {
      vsubpspp.fromqry <- paste(vsubpsppnm, "vsubpspp")
    }
    vsubpsppqry <- paste("SELECT vsubpspp.* \nFROM", vsubpspp.fromqry, whereqry)
    dbqueries$vsubpspp <- vsubpsppqry
  }

  ###################################################################################
  ## Import tables
  ###################################################################################
  if (is.null(dbconn)) {
    condx <- data.table(sqldf::sqldf(condqry, connection = NULL))
    subplotx <- data.table(sqldf::sqldf(subplotqry, connection = NULL))
    subp_condx <- data.table(sqldf::sqldf(subp_condqry, connection = NULL))
    if (!is.null(vsubpsppnm)) {
      vsubpsppx <- data.table(sqldf::sqldf(vsubpsppqry, connection = NULL))
    }
    vsubpstrx <- data.table(sqldf::sqldf(vsubpstrqry, connection = NULL))
  } else {
    ###################################################################################
    ## Import tables
    ###################################################################################
    condx <- data.table(DBI::dbGetQuery(dbconn, condqry))
    subplotx <- data.table(DBI::dbGetQuery(dbconn, subplotqry))
    subp_condx <- data.table(DBI::dbGetQuery(dbconn, subp_condqry))
    if (!is.null(vsubpsppnm)) {
      vsubpsppx <- data.table(DBI::dbGetQuery(dbconn, vsubpsppqry))
    }
    vsubpstrx <- data.table(DBI::dbGetQuery(dbconn, vsubpstrqry))
  }

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

  # Set key
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

  ###################################################################################
  ## Check condition data
  ###################################################################################
  pltcondnmlst <- names(pltcondx)
  
    ## Check for pvars2keep
  #############################################################################
  if (!all(pvars2keep %in% pltcondnmlst)) {
    pvars2keep <- pvars2keep[!pvars2keep %in% pltcondnmlst] 
    message("variables not in dataset: ", toString(pvars2keep))
  }

  ## Check for COND_STATUS_CD and create ACI filter
  #############################################################################
  if (!"COND_STATUS_CD" %in% pltcondnmlst) {
    stop("COND_STATUS_CD must be included in dataset...")
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
    subpvars2keep <- c(subpvars2keep, "NF_SUBP_STATUS_CD", "NF_SUBP_NONSAMPLE_REASN_CD")
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


  ########################################################################
  ## Separate tables for estimation
  ########################################################################
  cvars2keep <- cvars2keep[cvars2keep %in% names(pltcondx)]
  condx <- unique(pltcondx[, c(cuniqueid, condid, cvars2keep), with=FALSE])
  pltcondx[, (cvars2keep) := NULL]
 

  ###################################################################################
  ###################################################################################
  ## Check P2VEG, subplot and sub_cond data
  ###################################################################################
  ###################################################################################
  pltassgnvars <- cuniqueid


  ## Subset variables for pltassgnx, condx, and pltcondx
  ############################################################################
  if ("SAMP_METHOD_CD" %in% names(pltcondx)) {
    pltassgnvars <- c(pltassgnvars, "SAMP_METHOD_CD")
  } else {
    message("removing nonresponse from field-visited and remotely-sensed plots")
  }

  ## Subset pltassgn to sampled P2VEG and merge to subp_condx
  #############################################################################
  pltassgnx <- merge(pltassgnx, 
		unique(pltcondx[P2VEG_SAMPLING_STATUS_CD < 3, pltassgnvars, with=FALSE]),
		by.x=key(pltassgnx), by.y=cuniqueid)
  if (key(pltassgnx) != cuniqueid) {
    setnames(pltassgnx, key(pltassgnx), cuniqueid)
  }
  pltassgnid <- cuniqueid
  setkeyv(pltassgnx, pltassgnid)

  ## Define subplot ids
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

  ## Subset subp_condx table
  subpvars2keep <- subpvars2keep[subpvars2keep %in% names(subp_condx)]
  subp_condx <- subp_condx[, c(subpuniqueid, subpid, condid, subpvars2keep), with=FALSE]

  ## Merge pltassgnx to subp_condx - inner join
  ##########################################################
  subp_condf <- merge(pltassgnx, subp_condx)

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
      sum(ifelse((!is.na(MACRCOND_PROP) & MACRCOND_PROP > 0),
                 MACRCOND_PROP, SUBPCOND_PROP), na.rm=TRUE)/4),
                       by=c("PLT_CN", "CONDID")]
  setkeyv(SUBP_CONDPROP_UNADJ, c(subpuniqueid, condid))
  setkeyv(condx, c(cuniqueid, condid))

  ## Merge summed subplot condition proportions to condx
  vcondx <- merge(condx, SUBP_CONDPROP_UNADJ)

  #############################################################################
  ## Check veg profile data (P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE)
  #############################################################################
  if (!is.null(vsubpsppnm) && nrow(vsubpsppx) > 0) {
    ## Define necessary variable for tree table
    vsubpsppnmlst <- names(vsubpsppx)

    ## Check unique identifiers
    vsubpsppid <- pcheck.varchar(var2check=vsubpsppid, varnm="vsubpsppid", gui=gui,
		checklst=vsubpsppnmlst, caption="UniqueID variable of veg spp",
		warn=paste(vsubpsppid, "not in vegspspp"), stopifnull=TRUE)
    cvars2keep <- c(cvars2keep, "SUBPCOND_PROP")

    ## Check for NA values in necessary variables in tree table;
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
  
  if (!is.null(vsubpstrnm) && nrow(vsubpstrx) > 0) {
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

  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, 
	condid=condid, condsampcnt=as.data.frame(condsampcnt),
	ACI.filter=ACI.filter, areawt=areawt)


  returnlst$pltassgnx <- pltassgnx
  returnlst$pltassgnid <- pltassgnid
  returnlst$condx <- condx
  returnlst$vcondx <- vcondx
  returnlst$areawt <- "CONDPROP_UNADJ"
  returnlst$vareawt <- "SUBP_CONDPROP_UNADJ"

  if (!is.null(vcondsppf)) {
    #returnlst$vcondsppf <- merge(pltcondx, vcondsppf, all.x=TRUE)
    returnlst$vcondsppf <- vcondsppf
    returnlst$vcondsppid <- vsubpstrid
  }
  if (!is.null(vcondstrf)) {
    #returnlst$vcondstrf <- merge(pltcondx, vcondstrf, all.x=TRUE)
    returnlst$vcondstrf <- vcondstrf
    returnlst$vcondstrid <- vsubpstrid
  }
    
  return(returnlst)
}
