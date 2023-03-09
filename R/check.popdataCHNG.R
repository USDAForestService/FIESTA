check.popdataCHNG <- function(tabs, tabIDs, pltassgnx, pltassgnid,
	pfromqry, palias, pjoinid, whereqry, adj, ACI, pltx=NULL, puniqueid="CN", 
	dsn=NULL, dbconn=NULL, condid="CONDID", areawt="CONDPROP_UNADJ", 
	MICRO_BREAKPOINT_DIA=5, MACRO_BREAKPOINT_DIA=NULL, diavar="DIA",
	areawt_micr="MICRPROP_UNADJ", areawt_subp="SUBPPROP_UNADJ", areawt_macr="MACRPROP_UNADJ",
	nonsamp.cfilter=NULL, nullcheck=FALSE, cvars2keep=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: Checks data inputs for CHNG popType
  ## Define necessary plot and condition-level variables:
  ## - cond (cvars2keep) - areawt
  ## Import and check cond, plt, pltassgn tables
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
	ACI.filter=V1=condsampcnt=NF_COND_STATUS_CD=TPA_UNADJ=
	condqry=treeqry=cfromqry=tfromqry=SUBPCOND_PROP=MACRCOND_PROP=tpropvars=
	plot_pplotx=sccmx=dbqueries=chgwhereqry=nfplotsampcnt <- NULL


  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  cvars2keep <- unique(c(cvars2keep, areawt, "PROP_BASIS"))
  pdoms2keep <- c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT_STATUS_CD", 
			"MEASYEAR", "RDISTCD", "WATERCD", "ECOSUBCD", "CONGCD")
 
  sccm=lulc=grm=treex=seedx <- NULL
  ## Get tables from tabs
  for (tabnm in names(tabs)) {
    assign(tabnm, tabs[[tabnm]])
  }
  puniqueid <- tabIDs[["plt"]]
  cuniqueid <- tabIDs[["cond"]]
  pltuid <- tabIDs[["pltu"]]
  conduid <- tabIDs[["condu"]]
  sccmid <- tabIDs[["sccm"]]
  lulcid <- "PLT_CN"
  popType = "CHNG"

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
    SCHEMA.<- NULL
    dbqueries <- list()

    ## Create query for remeasured plots
    ##########################################################################
    if (!all(!is.null(plt), is.character(plt), plt %in% tablst)) {    
      stop("need PLOT table in database")
    }

    ## Get default variables for plot
    pltvars <- DBvars.default()$pltvarlst

    ## Get from statement for plot change query
    pchgfromqry <- paste0(pfromqry, 
		" JOIN ", SCHEMA., plt, " pplot ON(pplot.", puniqueid, " = ", palias, ".PREV_PLT_CN)")

    ## Get where statement for plot change query
    pchgwhere <- paste0(palias, ".REMPER > 0") 
    if (is.null(whereqry)) {
      pchgwhereqry <- paste("WHERE", pchgwhere)
    } else {
      pchgwhereqry <- paste(whereqry, "and", pchgwhere)
    }

    ## Build query for plot change
    pltuqry <- paste("select distinct", 
				toString(paste0(palias, ".", pltvars)), 
					"from", pchgfromqry, pchgwhereqry,
			  "UNION select distinct", 
				toString(paste0("pplot.", pltvars)), 
					"from", pchgfromqry, pchgwhereqry)
    dbqueries$pltu <- pltuqry

    ## Create query for remeasured conditions
    ##########################################################################
    if (!all(!is.null(cond), is.character(cond), cond %in% tablst)) { 
      stop("need COND table in database")
    } 

    ## Get default variables for cond
    condvars <-  DBvars.default()$condvarlst

    ## Get from statement for cond change query
    cchgfromqry <- paste0(pfromqry, 
		" JOIN ", SCHEMA., cond, 
			" c ON (c.", cuniqueid, " = ", palias, ".", pjoinid, ")",
      	" JOIN ", SCHEMA., cond, 
			" pcond ON (pcond.", cuniqueid, " = ", palias, ".PREV_PLT_CN)")

    ## Build query for cond change
    conduqry <- paste("select distinct", 
				toString(paste0("c.", condvars)), 
					"from", cchgfromqry, pchgwhereqry,
			  "UNION select distinct", 
				toString(paste0("pcond.", condvars)), 
					"from", cchgfromqry, pchgwhereqry)
    dbqueries$condu <- conduqry


    ##########################################################################
    ## Get SUBP_COND_CHNG_MTRX queries for proportion of change (areawt)
    ##########################################################################
    if (!all(!is.null(sccm), is.character(sccm), sccm %in% tablst)) {
      stop("need SUBP_COND_CHNG_MTRX table in database")
    }

    ## This is used for calculation of adjustment factors and estimates
    chgwhere <- "c.CONDPROP_UNADJ IS NOT NULL 
			 		and COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0    
                    		and COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0"
    if (is.null(pchgwhereqry)) {
      chgwhereqry <- paste("WHERE", chgwhere)
    } else {
      chgwhereqry <- paste(pchgwhereqry, "and", chgwhere)
    }

    ## This query is used for estimates
    sccmqry <- paste0("SELECT sccm.* FROM ", pfromqry,  
                	" JOIN ", SCHEMA., sccm, 
				" sccm ON (sccm.", sccmid, " = p.", puniqueid, ") ", whereqry) 

    ## This query is used for calculation of adjustment factors
    sccm_condqry <- paste0(
			"SELECT c.PLT_CN, 
				pcond.PLT_CN PREV_PLT_CN, 
				pcond.CONDID PREVCOND, c.CONDID,
               		pcond.COND_STATUS_CD PREV_COND_STATUS_CD, c.COND_STATUS_CD,
               		SUM(sccm.SUBPTYP_PROP_CHNG * 
                     		(CASE WHEN ((sccm.SUBPTYP = 3 and c.PROP_BASIS = 'MACR') or
									(sccm.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP')) 
									THEN 1 ELSE 0 end)/4) AS CONDPROP_UNADJ,
               		SUM(sccm.SUBPTYP_PROP_CHNG * 
                     		(CASE WHEN sccm.SUBPTYP = 1 THEN 1 ELSE 0 end)/4) AS SUBPPROP_UNADJ,
               		SUM(sccm.SUBPTYP_PROP_CHNG * 
                     		(CASE WHEN sccm.SUBPTYP = 2 THEN 1 ELSE 0 end)/4) AS MICRPROP_UNADJ,
               		SUM(sccm.SUBPTYP_PROP_CHNG * 
                     		(CASE WHEN sccm.SUBPTYP = 3 THEN 1 ELSE 0 end)/4) AS MACRPROP_UNADJ
                  FROM ", cchgfromqry,  
               			" JOIN ", SCHEMA., sccm, " sccm ON (sccm.", sccmid, " = c.", cuniqueid, 
                           	" and sccm.prev_plt_cn = pcond.", cuniqueid,
                         	" and sccm.", condid, " = c.", condid, 
                          	" and sccm.prevcond = pcond.", condid, ") ", 
                  chgwhereqry, 
                  " GROUP BY sccm.plt_cn, p.prev_plt_cn, sccm.condid")
    dbqueries$sccm_cond <- sccm_condqry

    ##########################################################################
    ## Get remeasured tree and seed data queries for GRM
    ##########################################################################
    if (popType ==  "GRM") {
      if (all(!is.null(tree), is.character(tree), tree %in% tablst)) {
        tfromqry <- paste0(cfromqry, " JOIN ", SCHEMA., tree,
				" t ON(t.", tuniqueid, " = c.", cuniqueid, " and t.", 
						condid, " = c.", condid, " and t.prevcond = pcond.", condid, ")
				LEFT JOIN ", SCHEMA., tree, " ptree ON(ptree.cn = t.prev_tre_cn)")
        grmfromqry <- paste0(tfromqry, " LEFT JOIN ", SCHEMA., 
						"tree_grm_component grm on(grm.tre_cn = t.cn)")
        grmqry <- paste("select distinct grm.* from", grmfromqry, whereqry)
      } else if (!is.null(pfromqry)) {
        tfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., tree,
				" t ON (t.PLT_CN = ", palias, ".", pjoinid, ")")
      } else {
        tfromqry <- paste(tree, "t")
      }
      treeqry <- paste("select distinct t.* from", tfromqry, whereqry)
      dbqueries$tree <- treeqry
    }
    if (all(!is.null(seed), is.character(seed), seed %in% tablst)) {
      if (!is.null(pfromqry)) {
        sfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., seed,
				" s ON (s.PLT_CN = ", palias, ".", pjoinid, ")")
      } else {
        sfromqry <- paste(seed, "s")
      }
      seedqry <- paste("select distinct s.* from", sfromqry, whereqry)
      dbqueries$seed <- seedqry
    }
  }
 
  ###################################################################################
  ## Import tables
  ###################################################################################

  ## Get remeasured plot/condition data
  condx <- suppressMessages(pcheck.table(cond, tab_dsn=dsn, 
           tabnm="cond", caption="Remeasured condition data?", 
           nullcheck=nullcheck, gui=gui, tabqry=conduqry, returnsf=FALSE))

  ## Get remeasured plot data
  if (!is.null(plt)) {
    pltx <- suppressMessages(pcheck.table(plt, tab_dsn=dsn, 
           tabnm="plt", caption="Remeasured plot data?", 
           nullcheck=nullcheck, gui=gui, tabqry=pltuqry, returnsf=FALSE))
  } 

  ## Get subplot matrix data for generating estimates
  sccmx <- suppressMessages(pcheck.table(sccm, tab_dsn=dsn, 
           tabnm="sccm", caption="sccm table?", 
           nullcheck=nullcheck, gui=gui, tabqry=sccmqry, returnsf=FALSE))
  setkeyv(sccmx, c(cuniqueid, condid))

  ## Get subplot matrix condition-level data for generating adjustment factors,
  ## and with area weight variable (SUBPTYP_PROP_CHNG).
  sccm_condx <- suppressMessages(pcheck.table(sccm, tab_dsn=dsn, 
           tabnm="sccm", caption="sccm table?", 
           nullcheck=nullcheck, gui=gui, tabqry=sccm_condqry, returnsf=FALSE))
  setkeyv(sccm_condx, c(cuniqueid, condid))
  
  if (popType == "GRM") {
    treex <- suppressMessages(pcheck.table(tree, tab_dsn=dsn, 
           tabnm="tree", caption="Tree table?",
		nullcheck=nullcheck, gui=gui, tabqry=treeqry, returnsf=FALSE))

    seedx <- suppressMessages(pcheck.table(seed, tab_dsn=dsn, 
           tabnm="seed", caption="Seedling table?",
		nullcheck=nullcheck, gui=gui, tabqry=seedqry, returnsf=FALSE))

    grmx <- suppressMessages(pcheck.table(grm, tab_dsn=dsn, 
           tabnm="grm", caption="tree_grm_component table?", 
           nullcheck=nullcheck, gui=gui, tabqry=grmqry, returnsf=FALSE))
  }
  if (popType == "LULC") {

    lulcqry <- 
		"SELECT distinct c.PLT_CN, c.CONDID, 
			pcond.COND_STATUS_CD PREV_COND_STATUS_CD, c.COND_STATUS_CD, 
			pcond.LAND_COVER_CLASS_CD PREV_LAND_COVER_CLASS_CD, c.LAND_COVER_CLASS_CD, 
			pcond.PRESNFCD PREV_PRESNFCD, c.PRESNFCD,
			case when pcond.PRESNFCD is null 
				then pcond.COND_STATUS_CD 
				else pcond.PRESNFCD end as PREV_LANDUSECD,
			case when c.PRESNFCD is null 
				then c.COND_STATUS_CD 
				else c.PRESNFCD end as LANDUSECD, chg.*
		FROM pltx p
                JOIN cond_pcondx c ON (c.PLT_CN = p.CN) 
                JOIN cond_pcondx pcond ON (pcond.PLT_CN = p.PREV_PLT_CN) 
                JOIN sccm_condx chg ON(chg.PLT_CN = c.PLT_CN and chg.CONDID = c.CONDID)
           WHERE COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0 
				AND COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0" 

    lulcx <- sqldf::sqldf(lulcqry)
  }
 
  ## Define cdoms2keep
  cdoms2keep <- names(condx)


  ###################################################################################
  ## Define pltcondx
  ###################################################################################
  ## Set as a placeholder for plot-level data checks
  #pltcondx <- pltdat$pltx
  #cuniqueid <- pltdat$puniqueid
  #setkeyv(pltcondx, cuniqueid)


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


  ## Merge pltx with condx
  ###########################################################
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
  condx <- sccm_condx
  #areawt <- "SUBPTYP_PROP_CHNG"
  condx[[areawt]] <- check.numeric(condx[[areawt]])


  ###################################################################################
  ###################################################################################
  ## Check tree data
  ###################################################################################
  ###################################################################################
  if (popType == "GRM" && !is.null(treex)) {
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
      message("CONDID not in tree table... appending CONDID = 1")
      treex[, CONDID := 1]
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
  if (popType == "GRM" && !is.null(seedx)) {
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
 
 
  ## Set up list of variables to return
  ######################################################################################
  returnlst <- list(condx=condx, pltcondx=pltcondx, sccmx=sccmx, cuniqueid=cuniqueid, 
	condid=condid, condsampcnt=as.data.frame(condsampcnt),
	ACI.filter=ACI.filter, areawt=areawt)

  if (popType == "GRM" && !is.null(treex)) {
    ## Check that the values of tuniqueid in treex are all in cuniqueid in pltcondx
    treef <- check.matchval(treex, pltcondx, tuniqueid, cuniqueid, tab1txt="tree",
		tab2txt="cond", subsetrows=TRUE)
    returnlst$treef <- treef
    returnlst$tuniqueid <- tuniqueid
  }
  if (popType == "GRM" && !is.null(seedx)) {
    ## Check that the values of tuniqueid in seedx are all in cuniqueid in pltcondx
    seedf <- check.matchval(seedx, pltcondx, tuniqueid, cuniqueid, tab1txt="seed",
		tab2txt="cond", subsetrows=TRUE)
    returnlst$seedf <- seedf
  }

  if (popType == "LULC") {
    returnlst$lulcx <- lulcx
  }

    
  if (ACI) {
    returnlst$nfplotsampcnt <- nfplotsampcnt
  }
  if (adj != "none") {
    returnlst$tpropvars <- tpropvars
  }

  return(returnlst)
}
