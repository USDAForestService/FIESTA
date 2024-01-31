check.popdataCHNG <- function(tabs, tabIDs, popType = popType, 
     pltassgnx, pltassgnid, pfromqry, palias, pjoinid, whereqry, adj, ACI, 
     pltx = NULL, puniqueid = "CN", pltvars = NULL, dsn = NULL, dbconn = NULL, 
     condid = "CONDID", areawt = "CONDPROP_UNADJ",
     MICRO_BREAKPOINT_DIA = 5, MACRO_BREAKPOINT_DIA = NULL, diavar = "DIA",
     areawt_micr = "MICRPROP_UNADJ", areawt_subp = "SUBPPROP_UNADJ", 
     areawt_macr = "MACRPROP_UNADJ",
     nonsamp.cfilter = NULL, nullcheck = FALSE, pvars2keep = NULL, 
	 cvars2keep = NULL, defaultVars = TRUE, gui = FALSE){

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
	plot_pplotx=sccmx=dbqueries=chgwhereqry=nfplotsampcnt=dbname=drv <- NULL


  ###################################################################################
  ## Define necessary plot and condition level variables
  ###################################################################################
  cvars2keep <- unique(c(cvars2keep, areawt, "PROP_BASIS"))
  pdoms2keep <- c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT_STATUS_CD", 
			"MEASYEAR", "RDISTCD", "WATERCD", "ECOSUBCD", "CONGCD")
  datindb <- FALSE
 
  sccm=lulc=grm=treex=seedx=condnm <- NULL


  ## Check name of PLOT table
  pltnmchk <- findnm("plotu", names(tabs), returnNULL = TRUE)
  if (is.null(pltnmchk)) {
    pltnmchk <- findnm("pltu", names(tabs), returnNULL = TRUE)
  }
  if (is.null(pltnmchk)) {
    pltnmchk <- findnm("plot", names(tabs), returnNULL = TRUE)
  }
  if (is.null(pltnmchk)) {
    pltnmchk <- findnm("plt", names(tabs), returnNULL = TRUE)
  } 
 
  if (is.null(pltnmchk)) {
    message("plot data needed for CHNG estimates")
	return(NULL)
  }

  if (is.character(tabs[[pltnmchk]])) {  
    pltnm <- tabs[[pltnmchk]]
  } else {
    pltnm <- "pltu"
  }
  assign(pltnm, tabs[[pltnmchk]])
  puniqueid <- tabIDs[[pltnmchk]]
 
  ## Check name of COND table
  condnmchk <- findnm("condu", names(tabs), returnNULL = TRUE)
  if (is.null(condnmchk)) {
    condnmchk <- findnm("cond", names(tabs), returnNULL = TRUE)
  }
  if (is.null(condnmchk)) {
    condnmchk <- findnm("cond", names(tabs), returnNULL = TRUE)
  } 
  if (is.character(tabs[[condnmchk]])) {  
    condnm <- tabs[[condnmchk]]
  } else {
    condnm <- "condu"
  }
  assign(condnm, tabs[[condnmchk]])
  cuniqueid <- tabIDs[[condnmchk]]


  ## Check name of SUBP_COND_CHNG_MTRX table
  sccmnmchk <- findnm("subp_cond_chng_mtrx", names(tabs), returnNULL = TRUE)
  if (is.null(sccmnmchk)) {
    sccmnmchk <- findnm("sccm", names(tabs), returnNULL = TRUE)
  } 
  if (is.character(tabs[[sccmnmchk]])) {  
    sccmnm <- tabs[[sccmnmchk]]
  } else {
    sccmnm <- "sccm"
  }
  assign(sccmnm, tabs[[sccmnmchk]])
  sccmid <- tabIDs[[sccmnmchk]]
  lulcid <- "PLT_CN"

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
    drv <- "SQLite"

    ## Check plt in database
    if (!all(!is.null(pltnm), is.character(pltnm), pltnm %in% tablst)) {    
      message("need PLOT table in database")
	  return(NULL)
    } else {
	  pltflds <- DBI::dbListFields(dbconn, pltnm)
    }
    ## Check cond in database
    if (!all(!is.null(condnm), is.character(condnm), condnm %in% tablst)) { 
      message("need COND table in database")
	  return(NULL)
    } else {
	  condflds <- DBI::dbListFields(dbconn, condnm)
    } 
    ## Check sccm in database
    if (!all(!is.null(sccmnm), is.character(sccmnm), sccmnm %in% tablst)) {
      message("need SUBP_COND_CHNG_MTRX table in database")
	  return(NULL)
    }
  } else {

    ## Get remeasured plot/condition data
	if (!is.null(condnm)) {
      assign(condnm, pcheck.table(get(condnm), tab_dsn=dsn, 
           tabnm="cond", caption="Remeasured condition data?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
	  condflds <- names(get(condnm))
	}

    ## Get remeasured plot data
    if (!is.null(pltnm)) {
      assign(pltnm, pcheck.table(get(pltnm), tab_dsn=dsn, 
           tabnm="plt", caption="Remeasured plot data?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
	  pltflds <- names(get(pltnm))
    } 

    ## Get subplot matrix data for generating estimates
    assign(sccmnm, pcheck.table(get(sccmnm), tab_dsn=dsn, 
           tabnm="sccm", caption="sccm table?", 
           nullcheck=nullcheck, gui=gui, returnsf=FALSE))
  }  

  if (is.null(pfromqry)) {
    pfromqry <- paste0(SCHEMA., pltnm, " ", palias)
  }

  ## Get from statement for plot change query
  if (is.null(pfromqry)) {
    pchgfromqry <- paste0(SCHEMA., pltnm, 
		" pplot ON(pplot.", puniqueid, " = ", palias, ".PREV_PLT_CN)")
  } else {
    pchgfromqry <- paste0(pfromqry, 
	          "\nJOIN ", SCHEMA., pltnm, 
                " pplot ON(pplot.", puniqueid, " = ", palias, ".PREV_PLT_CN)")
  }

  ## Get default variables for plot
  if (is.null(pltvars)) {
    pltvars <- "*"
  }

  ## Get where statement for plot change query
  rempernm <- findnm("REMPER", pltflds)
  if (!is.null(rempernm)) {
    remper.qry <- paste0(palias, ".", rempernm, " > 0") 
  
    if (is.null(whereqry)) {
      pchgwhereqry <- paste0("\nWHERE ", remper.qry)
    } else {
      pchgwhereqry <- paste0(whereqry, "\n  AND ", remper.qry)
    }
  } else {
    pchgwhereqry <- whereqry
  }
  
  ## Build query for plot change
  pltuqry <- paste0("SELECT ", 
				toString(paste0(palias, ".", pltvars)), 
					"\nFROM ", pchgfromqry, pchgwhereqry,
			  "\nUNION \nSELECT ", 
				toString(paste0("pplot.", pltvars)), 
					"\nFROM ", pchgfromqry, pchgwhereqry)
  dbqueries$plt <- pltuqry

  if (defaultVars) {
    ## Get default variables for cond
    condvars <-  DBvars.default()$condvarlst
  } else {
    condvars <- condflds
  }

  ## Get from statement for cond change query
  cchgfromqry <- paste0(pfromqry, 
		"\nJOIN ", SCHEMA., condnm, 
			" c ON (c.", cuniqueid, " = ", palias, ".", pjoinid, ")",
      	"\nJOIN ", SCHEMA., condnm, 
			" pcond ON (pcond.", cuniqueid, " = ", palias, ".PREV_PLT_CN)")

  ## Build query for cond change
  conduqry <- paste0("SELECT ", 
				toString(paste0("c.", condvars)), 
			  "\nFROM ", cchgfromqry, pchgwhereqry,
			  "\nUNION \nSELECT ", 
				      toString(paste0("pcond.", condvars)), 
			  "\nFROM ", cchgfromqry, pchgwhereqry)
  dbqueries$condu <- conduqry


  ##########################################################################
  ## Get SUBP_COND_CHNG_MTRX queries for proportion of change (areawt)
  ##########################################################################

  ## This is used for calculation of adjustment factors and estimates
  chgwhere <- paste0("c.CONDPROP_UNADJ IS NOT NULL",
                "\n      AND ((sccm.SUBPTYP = 3 AND c.PROP_BASIS = 'MACR')",
				"\n          OR (sccm.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP'))",
			 	"\n      AND COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0",  
                "\n      AND COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0")
  if (is.null(pchgwhereqry)) {
    chgwhereqry <- paste("WHERE", chgwhere)
  } else {
    chgwhereqry <- paste0(pchgwhereqry, "\n  AND ", chgwhere)
  }

  ## This query is used for estimates
  sccmqry <- paste0("SELECT distinct sccm.* \nFROM ", pfromqry,  
                	"\nJOIN ", SCHEMA., sccmnm, 
				    " sccm ON (sccm.", sccmid, " = p.", puniqueid, ") ", 
					whereqry) 

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
                  (CASE WHEN sccm.SUBPTYP = 3 THEN 1 ELSE 0 end)/4) AS MACRPROP_UNADJ ",
           "\nFROM ", cchgfromqry,  
               	  "\nJOIN ", SCHEMA., sccmnm, " sccm ON (sccm.", sccmid, " = c.", cuniqueid, 
                   "\n     AND sccm.prev_plt_cn = pcond.", cuniqueid,
                   "\n     AND sccm.", condid, " = c.", condid, 
                   "\n     AND sccm.prevcond = pcond.", condid, ") ", 
           chgwhereqry, 
           "\nGROUP BY sccm.plt_cn, p.prev_plt_cn, sccm.condid")
  dbqueries$sccm_cond <- sccm_condqry


  ## Import tables
  #########################################################################
  if (datindb) {
    COND <- tryCatch(
	          data.table(DBI::dbGetQuery(dbconn, conduqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
    PLOT <- tryCatch(
	          data.table(DBI::dbGetQuery(dbconn, pltuqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
    sccmx <- tryCatch(
	          data.table(DBI::dbGetQuery(dbconn, sccmqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
    sccm_condx <- tryCatch(
	          data.table(DBI::dbGetQuery(dbconn, sccm_condqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
  } else {
    COND <- tryCatch(
	          data.table(sqldf::sqldf(conduqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
    PLOT <- tryCatch(
	          data.table(sqldf::sqldf(pltuqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
    sccmx <- tryCatch(
	          data.table(sqldf::sqldf(sccmqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
    sccm_condx <- tryCatch(
	          data.table(sqldf::sqldf(sccm_condqry)),
				  error=function(e) {
				  warning(e)
  			      return(NULL)}
                  )
  }

  if (is.null(COND) || nrow(COND) == 0) {
    message("invalid ", condnm)
	message(conduqry)
    return(NULL)
  }
  if (is.null(PLOT) || nrow(PLOT) == 0) {
    message("invalid ", pltnm)
	message(pltuqry)
    return(NULL)
  }
  if (is.null(sccmx) || nrow(sccmx) == 0) {
    message("invalid ", sccmnm)
	message(sccmqry)
    return(NULL)
  }
  if (is.null(sccm_condx) || nrow(sccm_condx) == 0) {
    message("invalid ", sccmnm)
	message(sccm_condqry)
    return(NULL)
  }

  ## Import tables
  #########################################################################
#  condx <- data.table(sqldf::sqldf(conduqry, dbname=dsn, drv=drv))
#  if (!is.null(pltnm)) {
#    pltx <- data.table(sqldf::sqldf(pltuqry, dbname=dsn, drv=drv))
#  }   
#  sccmx <- data.table(sqldf::sqldf(sccmqry, dbname=dsn, drv=drv))


  ##########################################################################
  ## Get remeasured tree and seed data queries for GRM
  ## And TREE_GRM_BEGIN and TREE_GRM_MIDPT queries
  ##########################################################################
  if (popType ==  "GRM") {
    if (all(!is.null(tree), is.character(tree), tree %in% tablst)) {
      tfromqry <- paste0(cchgfromqry, " JOIN ", SCHEMA., tree,
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

    if (all(!is.null(begin), is.character(begin), begin %in% tablst)) {
      if (!is.null(pfromqry)) {
        bfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., begin,
			" b ON (b.PLT_CN = ", palias, ".", pjoinid, ")")
      } else {
        bfromqry <- paste(begin, "b")
      }
      beginqry <- paste("select distinct b.* from", bfromqry, whereqry)
      dbqueries$begin <- beginqry
    }

    if (all(!is.null(midpt), is.character(midpt), midpt %in% tablst)) {
      if (!is.null(pfromqry)) {
        mfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., midpt,
			" m ON (m.PLT_CN = ", palias, ".", pjoinid, ")")
      } else {
        mfromqry <- paste(begin, "m")
      }
      midptqry <- paste("select distinct m.* from", mfromqry, whereqry)
      dbqueries$midpt <- midptqry
    }
   
    ## Import tables
    #########################################################################
    treex <- suppressMessages(pcheck.table(tree, tab_dsn=dsn, 
           tabnm="tree", caption="Tree table?",
		nullcheck=nullcheck, gui=gui, tabqry=treeqry, returnsf=FALSE))

    seedx <- suppressMessages(pcheck.table(seed, tab_dsn=dsn, 
           tabnm="seed", caption="Seedling table?",
		nullcheck=nullcheck, gui=gui, tabqry=seedqry, returnsf=FALSE))

    grmx <- suppressMessages(pcheck.table(grm, tab_dsn=dsn, 
           tabnm="grm", caption="tree_grm_component table?", 
           nullcheck=nullcheck, gui=gui, tabqry=grmqry, returnsf=FALSE))

    beginx <- suppressMessages(pcheck.table(begin, tab_dsn=dsn, 
           tabnm="begin", caption="tree_grm_begin table?", 
           nullcheck=nullcheck, gui=gui, tabqry=beginqry, returnsf=FALSE))

    midptx <- suppressMessages(pcheck.table(midpt, tab_dsn=dsn, 
           tabnm="midpt", caption="tree_grm_midpt table?", 
           nullcheck=nullcheck, gui=gui, tabqry=midptqry, returnsf=FALSE))

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
  #cdoms2keep <- names(condx)


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
		checklst=condflds, caption="Unique identifier of plot",
		warn=paste(cuniqueid, "not in cond table"), stopifnull=TRUE)
  #setkeyv(condx, cuniqueid)

  ## Check for NA values in necessary variables in cond table
#  condx.na <- sum(is.na(condx[[cuniqueid]]))
#  if (condx.na > 0) stop("NA values in ", cuniqueid)

  condid <- pcheck.varchar(var2check=condid, varnm="condid", gui=gui,
		checklst=condflds, caption="Unique identifier of plot",
		warn=paste(condid, "not in cond table"), stopifinvalid=FALSE)
  # if (is.null(condid)) {
    # if (nrow(condx) == length(unique(condx[[cuniqueid]]))) {
      # condx[, CONDID := 1]
      # condid <- "CONDID"
    # } else {
      # stop("there is more than 1 record per plot... must include valid CONDID")
    # }
  # }
  # Check for NA values in necessary variables in cond table
  # condx.na <- sum(is.na(condx[[condid]]))
  # if (condx.na > 0) stop("NA values in ", condid)

  # Check if 1 plot-condition per record in cond
  #####################################################
  # condid.dupid <- condx[duplicated(condx, by=c(cuniqueid, condid))][[cuniqueid]]

  # if (length(condid.dupid) > 0) {
    # msg <- paste("check cuniqueid/condid... duplicate records")
    # if (length(condid.dupid) < 20) print(condid.dupid)
    # stop(msg)
  # }
  # setkeyv(condx, c(cuniqueid, condid))

  ## Merge pltx with condx
  ###########################################################
  if (!is.null(PLOT)){

    ## Set key
    setkeyv(PLOT, puniqueid)

    ## Subset condition columns
    cvars <- unique(c(cuniqueid, names(COND)[!names(COND) %in% names(PLOT)])) 
    COND <- COND[, cvars, with=FALSE]

    ## Check if class of puniqueid in pltx matches class of puniqueid in condx
    tabchk <- check.matchclass(COND, PLOT, cuniqueid, puniqueid)
    COND <- tabchk$tab1
    PLOT <- tabchk$tab2

    ## Check for matching unique identifiers of condx and pltx
    COND <- check.matchval(COND, PLOT, cuniqueid, puniqueid,
			tab1txt=paste0("cond-", cuniqueid),
			tab2txt=paste0("plt-", puniqueid), subsetrows=TRUE)

    nrow.before <- nrow(PLOT)

    ## Merge cond to plt (Note: inner join to use only plots with sampled conditions)
    pltcols <- unique(c(puniqueid, names(PLOT)[!names(PLOT) %in% names(COND)]))
    pltcondx <- tryCatch(merge(PLOT[, pltcols, with=FALSE], COND,
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
    pltcondx <- COND

    ## Check for matching unique identifiers of pltcondx with pltassgnx
    ## Subset pltx to pltassgnx ids
    pltcondx <- check.matchval(pltcondx, pltassgnx, cuniqueid, pltassgnid, 
			tab1txt="cond", tab2txt="pltassgn", subsetrows=TRUE)
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
#  condx <- sccm_condx
#  #areawt <- "SUBPTYP_PROP_CHNG"
#  condx[[areawt]] <- check.numeric(condx[[areawt]])

  cvars2keep <- c(cvars2keep, areawt_subp, areawt_macr)
  sccm_condx[[areawt]] <- check.numeric(sccm_condx[[areawt]])


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
    if (!condid %in% names(seedx)) {
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
  returnlst <- list(sccm_condx=sccm_condx, pltcondx=pltcondx, sccmx=sccmx, 
      cuniqueid=cuniqueid, condid=condid, condsampcnt=as.data.frame(condsampcnt),
      ACI.filter=ACI.filter, areawt=areawt)

  if (popType == "GRM") {
    if (!is.null(treex)) {
      ## Check that the values of tuniqueid in treex are all in cuniqueid in pltcondx
      returnlst$treef <- check.matchval(treex, pltcondx, tuniqueid, cuniqueid, 
            tab1txt="tree", tab2txt="cond", subsetrows=TRUE)
      returnlst$tuniqueid <- tuniqueid
      rm(treex)
      # gc()
    }
    if (!is.null(seedx)) {
      ## Check that the values of tuniqueid in seedx are all in cuniqueid in pltcondx
      returnlst$seedf <- check.matchval(seedx, pltcondx, tuniqueid, cuniqueid, 
            tab1txt="seed", tab2txt="cond", subsetrows=TRUE)
      rm(seedx)
      # gc()
    }
    if (!is.null(grmx)) {
      ## Check that the values of tuniqueid in grmx are all in cuniqueid in pltcondx
      returnlst$grmf <- check.matchval(grmx, pltcondx, tuniqueid, cuniqueid, 
            tab1txt="grm", tab2txt="cond", subsetrows=TRUE)
      rm(grmx)
      # gc()
    }
    if (!is.null(beginx)) {
      ## Check that the values of tuniqueid in beginx are all in cuniqueid in pltcondx
      returnlst$beginf <- check.matchval(beginx, pltcondx, tuniqueid, cuniqueid, 
            tab1txt="begin", tab2txt="cond", subsetrows=TRUE)
      rm(beginx)
      # gc()
    }
    if (!is.null(midptx)) {
      ## Check that the values of tuniqueid in midptx are all in cuniqueid in pltcondx
      returnlst$midptf <- check.matchval(midptx, pltcondx, tuniqueid, cuniqueid, 
            tab1txt="midpt", tab2txt="cond", subsetrows=TRUE)
      rm(midptx)
      # gc()
    }
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
