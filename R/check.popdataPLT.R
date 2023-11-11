check.popdataPLT <- function(dsn, tabs, tabIDs, pltassgn, pltassgnid, 
	pjoinid, module, popType, popevalid, adj, popFilter, 
	nonsamp.pfilter=NULL, unitarea=NULL, areavar, unitvar, unitvar2=NULL, 
	areaunits, unit.action="keep", removetext="unitarea", strata=FALSE, 
	stratalut=NULL, strvar=NULL, stratcombine=TRUE, pivot=FALSE, nonresp=FALSE, 
	prednames=NULL, predfac=NULL, pvars2keep=NULL, pdoms2keep=NULL, 
	nullcheck=FALSE, defaultVars=TRUE, gui=FALSE) {

  ###################################################################################
  ## DESCRIPTION: Checks plot data inputs
  ## - Set plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD,
  ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC,
  ##		ECOSUBCD, CONGCD, INTENSITY, DESIGNCD
  ## Check logical parameters: ACI, strata, stratcombine (if strata=TRUE)
  ## - If ACI, add NF_PLOT_STATUS_CD to pvars2keep and NF_COND_STATUS_CD to cvars2keep
  ## - If unit.action='combine', estimation units are combined if less than 10 plots
  ## - If strata, only 1 auxvar allowed, add to pvars2keep
  ## - If module = SA or MA-greg, add prednames to pvars2keep
  ## - If adj="samp", nonsample adjustment factors calculated at strata level
  ## - If adj="plot", nonsample adjustment factors calculated at plot level
  ## Check unit.action ('keep', 'remove', 'combine').
  ## Check predfac, if module = SA, MA-greg
  ## Import and check plt and pltassgn tables
  ## Check corresponding unique identifiers (puniqueid, pltassgnid)
  ## Merge plt, pltassgn tables to check necessary variables
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
  ## Subset variables for pltx and pltassgnx
  ###################################################################################

  ## Set global variables
  STATECD=PLOT_STATUS_CD=PSTATUSCD=plotsampcnt=nfplotsampcnt=INVYR=
	NF_PLOT_STATUS_CD=NF_COND_STATUS_CD=TPA_UNADJ=methodlst=nonresplut=
	plotqry=pfromqry=pltassgnqry=unitareaqry=stratalutqry=whereqry=palias=
	P2POINTCNT=plt=dbconn_pltassgn=popwhereqry <- NULL


  ###################################################################################
  ## Define necessary plot variables
  ###################################################################################
  datindb=unitindb=stratindb <- FALSE
  dbconn=pstratvars=plotqry <- NULL
  unitvars <- unique(c(unitvar, unitvar2))
  pltassgnvars <- unique(c(pltassgnid, unitvars)) 
   
  ## Define plt variables
  #########################################################################
  pvars2keep <- unique(pvars2keep) 
  pdoms <- c("STATECD", "UNITCD", "COUNTYCD", "INVYR", "PLOT_STATUS_CD", 
	  "PLOT_NONSAMPLE_REASN_CD", "PSTATUSCD", "INTENSITY", "MEASYEAR", "RDDISTCD", 
	  "WATERCD", "ELEV", "ELEV_PUBLIC", "ECOSUBCD", "CONGCD", "DESIGNCD", "EMAP_HEX")
  
  
  ## Get tables from tabs
  ########################################################
  for (tabnm in names(tabs)) {
    assign(tabnm, tabs[[tabnm]])
  }
  puniqueid <- tabIDs[["plt"]]
 
  ###################################################################################
  ## Check parameters
  ###################################################################################

  ## Check adj
  ########################################################
  adjlst <- c("samp", "plot", "none")
  adj <- pcheck.varchar(var2check=adj, varnm="adj", gui=gui,
		checklst=adjlst, caption="adj", multiple=FALSE, stopifnull=TRUE)
  if (adj == "plot" && module == "GB") {
    message("adj='plot' is not typical for GA modules")
  }
  if (adj != "none") {
    pvars2keep <- c(pvars2keep, "MACRO_BREAKPOINT_DIA")
  }

  ## Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
  ###################################################################################
  ACI <- pcheck.logical(popFilter$ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
  if (ACI) {
    pvars2keep <- c(pvars2keep, "NF_SAMPLING_STATUS_CD", "NF_PLOT_STATUS_CD")
  }

  ## Check unit.action
  ########################################################
  unit.actionlst <- c("keep", "remove", "combine")
  unit.action <- pcheck.varchar(var2check=unit.action, varnm="unit.action", gui=gui,
		checklst=unit.actionlst, caption="unit.action", multiple=FALSE, stopifnull=TRUE)


  ## Set additional pvars2keep
  #####################################################################################
  if (popType %in% c("GRM", "CHNG", "LULC")) {
    pvars2keep <- unique(c(pvars2keep, c("PREV_PLT_CN", "REMPER")))
  } else if (popType == "P2VEG") {
    pvars2keep <- c(pvars2keep, "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
	"SAMP_METHOD_CD")
  } else if (popType == "INV") {
    pvars2keep <- c(pvars2keep, "INVASIVE_SAMPLING_STATUS_CD", "INVASIVE_SPECIMEN_RULE_CD")
  }  

  ## Check strata, strvars
  ###################################################################################
  strata <- pcheck.logical(strata, varnm="strata",
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check strata parameters
  ########################################################
  if (strata) {
    ## pivot
    pivot <- pcheck.logical(pivot, varnm="pivot",
		title="Pivot stratalut?", first="NO", gui=gui)
    ## Check nonresp
    nonresp <- pcheck.logical(nonresp, varnm="nonresp",
		title="Post stratify?", first="YES", gui=gui)
    ## Check stratcombine
    stratcombine <- pcheck.logical(stratcombine, varnm="stratcombine",
		title="Combine strata?", first="YES", gui=gui, stopifnull=TRUE)

    ## Check strvar
    if (is.null(strvar)) stop("must include strvar for post-strat estimates")
    if (length(strvar) > 1) stop("invalid strvar... only 1 variable allowed")
    pltassgnvars <- unique(c(pltassgnvars, strvar))

    if (nonresp) {
      pstratvars <- unique(c(pstratvars, c("PLOT_STATUS_CD", "SAMP_METHOD_CD")))
    } 
  } else {
    strvar <- NULL
    pltassgnvars <- unique(c(pltassgnvars, prednames))
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

  ###################################################################################
  ## Database queries
  ###################################################################################
  evalid <- popFilter$evalid
  invyrs <- popFilter$invyrs
  if (!is.null(dsn) && getext(dsn) %in% c("sqlite", "db", "db3", "sqlite3", "gpkg")) {
    datindb <- TRUE
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE, showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    ppsanm=pltassgnqry <- NULL
	
	## Check name of plt
	if (!is.null(plt)) {
	  if (plt == "plot" && "plt" %in% tablst) {
		plt <- "plt"
	  } else if (plt == "plt" && "plot" %in% tablst) {
		plt <- "plot"
	  }
	  pltflds <- DBI::dbListFields(dbconn, plt)
	}
	## Check name of pltassgn
	if (!is.null(pltassgn)) {
	  if (!is.data.frame(pltassgn) && is.character(pltassgn)) {
	    dbconn_pltassgn <- dbconn
	    ppsanm <- chkdbtab(tablst, pltassgn, stopifnull=TRUE)
		ppsaflds <- DBI::dbListFields(dbconn_pltassgn, ppsanm)
	  }
	  pltassgnid <- findnm(pltassgnid, ppsaflds, returnNULL = TRUE)
	  if (is.null(pltassgnid)) {
        stop("pltassgnid is invalid")
      }
	}
	
    if (!is.null(evalid)) {	
      ## Filter for population data
	  evalidnm <- chkdbtab(ppsaflds, "EVALID", stopifnull=TRUE)
	  if (is.data.frame(pltassgn)) {
	    evalidvals <- sort(unique(pltassgn[[evalidnm]]))
	  } else {
        evalidvals <- DBI::dbGetQuery(dbconn_pltassgn, 
			paste("select distinct", evalidnm, "from", ppsanm))[[1]]
      }
      if (any(!evalid %in% evalidvals)) {
        stop("evalids are missing: ", toString(evalid[!evalid %in% evalidvals]))
      } 
      whereqry <- paste0("where ", evalidnm, " in(", toString(evalid), ")")
      popwhereqry <- paste0("where ", evalidnm, " in(", toString(popevalid), ")")
      pltassgnqry <- paste("select * from", ppsanm, popwhereqry)
	  
      if (is.null(plt)) {
        palias <- "ppsa"
        pfromqry <- paste(ppsanm, "ppsa")
        pjoinid <- pltassgnid
      } else {
        palias <- "p"
        pfromqry <- getpfromqry(evalid=evalid, popevalid, ppsanm=ppsanm, 
				ppsaid=pltassgnid, pjoinid=pjoinid, plotnm=plt, dbconn=dbconn)
	  }
    } else if (!is.null(popFilter$measCur) && popFilter$measCur) {
      palias <- "p"
      pfromqry <- getpfromqry(varCur="MEASYEAR", Endyr=popFilter$measEndyr, 
		               dsn=dsn, plotnm=plt, dbconn=dbconn)
	  if (!is.null(ppsanm)) {
	    ppsaalias <- "ppsa"
        pltassgnqry <- paste0("select ", ppsaalias, ".* from ", pfromqry,
			          " \nJOIN ", ppsanm, " ", ppsaalias, " ON(", 
					  palias, ".", puniqueid, " = ", ppsaalias, ".", pltassgnid, 
					  ") \n", whereqry)

	  }
    } else if (!is.null(invyrs)) {
	  if (is.null(plt)) {
	    invyrnm <- chkdbtab(ppsaflds, "INVYR", stopifnull=TRUE)
		whereqry <- paste0("where ", invyrnm, " in(", toString(invyrs), ")")
		pltassgnqry <- paste0("select ", ppsaalias, ".* from ",  
	                  ppsanm, " \n", whereqry)
	  } else {
	    palias <- "p"
        pfromqry <- getpfromqry(invyrs=invyrs, plotCur=FALSE, dsn=dsn, 
							  plotnm=plt, dbconn=dbconn)
							  
	    if (!is.null(ppsanm)) {	      
		  ppsaalias <- "ppsa"
	      invyrnmplt <- findnm("INVYR", pltflds, returnNULL=TRUE)
	      invyrnmppsa <- findnm("INVYR", ppsaflds, returnNULL=TRUE)
	      if (is.null(invyrnmplt) && is.null(invyrnmppsa)) {
		    stop("INVYR variable not in tables")
		  }
	      if (!is.null(invyrnmplt) && !is.null(invyrnmppsa)) {
		    if (!is.null(invyrnmplt)) {
	          invyrnm <- paste0(palias, ".", invyrnmplt)
		    } else {
		      invyrnm <- paste0(ppsaalias, ".", invyrnmppsa)
		    }
		  } else {
		    if (!is.null(invyrnmplt)) {
		      invyrnm <- invyrnmplt
		    } else {
		      invyrnm <- invyrnmppsa
		    }
		  }
		  whereqry <- paste0("where ", invyrnm, " in(", toString(invyrs), ")")
		  pltassgnqry <- paste0("select ", ppsaalias, ".* from ", pfromqry, 
	                  " \nJOIN ", ppsanm, " ", ppsaalias, " ON(", 
					  palias, ".", puniqueid, " = ", ppsaalias, ".", pltassgnid, 
					  ") \n", whereqry)
	    } 
      }		
    } else {
      whereqry <- NULL
      if (is.null(plt)) {
        palias <- "ppsa"
        pfromqry <- paste(ppsanm, "ppsa")
        pjoinid <- pltassgnid
      } else {
        palias <- "p"
        pfromqry <- paste0(plt, " p INNER JOIN ", ppsanm,
			" ON(p.", pjoinid, " = ", ppsanm, ".", pltassgnid, ")")
	  }
      pltassgnqry <- paste("select * from", ppsanm)	  
    }
 
    if (!is.null(pfromqry)) {
      pvars <- paste0(palias, ".*")
      ppvars <- paste0("pplot", ".*")
      plotqry <- paste("select distinct", palias, ".* from", pfromqry)
      if (popType == "CHNG") {
        plot1qry <- paste0("select distinct ", pvars, " from ", pfromqry, 
        		" JOIN ", plt, " pplot ON (pplot", ".", puniqueid, " = ", 
				palias, ".PREV_PLT_CN) ", whereqry)
        plot2qry <- paste0("select distinct ", ppvars, " from ", pfromqry, 
        		" JOIN ", plt, " pplot ON (pplot", ".", puniqueid, " = ", 
				palias, ".PREV_PLT_CN) ", whereqry)
        plotqry <- paste(plot1qry, "UNION", plot2qry)
      } else {
        plotqry <- paste("select distinct", pvars, "from", pfromqry, whereqry)
      }
      #dbqueries$plot <- plotqry
    }

    if (is.character(unitarea) && !is.null(chkdbtab(tablst, unitarea))) {
      unitindb <- TRUE
      unitarea_layer <- chkdbtab(tablst, unitarea)
	  unitareaflds <- DBI::dbListFields(dbconn, unitarea_layer)
      unitareaqry <- paste("select * from", unitarea_layer)
	  evalidnm <- findnm("EVALID", unitareaflds, returnNULL = TRUE) 
      if (!is.null(evalid) && !is.null(evalidnm)) {
        unitareaqry <- paste(unitareaqry, "where", evalidnm, "in(", toString(popevalid), ")")
      }
      unitarea <- pcheck.table(unitarea, conn=dbconn,
           tabnm="unitarea", caption="unitarea?",
		    nullcheck=nullcheck, tabqry=unitareaqry, returnsf=FALSE)
    } 
	
    if (strata && is.character(stratalut) && !is.null(chkdbtab(tablst, stratalut))) {
      stratindb <- TRUE
      stratalut_layer <- chkdbtab(tablst, stratalut)
	  stratalutflds <- DBI::dbListFields(dbconn, stratalut_layer)
      stratalutqry <- paste("select * from", stratalut_layer)
	  evalidnm <- findnm("EVALID", unitareaflds, returnNULL = TRUE) 
      if (!is.null(evalid) && !is.null(evalidnm)) {
        stratalutqry <- paste(stratalutqry, "where", evalidnm, "in(", toString(popevalid), ")")
      }
	  stratalut <- pcheck.table(stratalut, conn=dbconn,
          tabnm="stratalut", caption="stratalut?",
		  nullcheck=nullcheck, tabqry=stratalutqry, returnsf=FALSE)
    }
  }
 
  ###################################################################################
  ## Import tables
  ###################################################################################
  pltx <- pcheck.table(plt, conn=dbconn, 
                       tabnm="plt", caption="plot table?",
		               nullcheck=nullcheck, tabqry=plotqry, returnsf=FALSE)
  pltassgnx <- pcheck.table(pltassgn, conn=dbconn_pltassgn,
                            tabnm="pltassgn", caption="plot assignments?", 
                            nullcheck=nullcheck, tabqry=pltassgnqry, returnsf=FALSE)


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
      if (pltx.na > 0) {
        if (length(pltx.na) == 1) {
          message("1 NA value in ", puniqueid)
        } else {
          message(pltx.na, " NA values in ", puniqueid)
        }
        pltx <- pltx[!is.na(pltx[[puniqueid]]), ]
      }
      ## Set key
      setkeyv(pltx, puniqueid)
    }

    if (!is.null(pltassgnx)) {
      if (!datindb && !is.null(evalid) && !is.null(chkdbtab(names(pltassgnx), "EVALID"))) {
        evalidnm <- chkdbtab(names(pltassgnx), "EVALID")
        popevalid <- unlist(popevalid)
        if (!all(evalid %in% unique(pltassgnx[[evalidnm]]))) {
          evalid.miss <- evalid[which(!evalid %in% unique(pltassgnx[["EVALID"]]))]
          message("evalid not in dataset: ", paste(evalid.miss, collapse=", "))
          if (length(evalid.miss) == length(evalid)) stop("")
          evalid <- evalid[!evalid %in% evalid.miss]
        }
        pltassgnx <- datFilter(pltassgnx, getfilter("EVALID", popevalid, syntax="R"))$xf
        if (nrow(pltassgnx) == 0) {
          stop("evalid removed all records")
		  return(NULL)
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

      pltassgnx <- pltassgnx[, unique(c(pltassgnid,
		names(pltassgnx)[!names(pltassgnx) %in% names(pltx)])), with=FALSE]
      setkeyv(pltassgnx, pltassgnid)

      ## Check if class of pjoinid in pltx matches class of pltassgnid in pltassgnx
      tabchk <- check.matchclass(pltx, pltassgnx, pjoinid, pltassgnid)
      pltx <- tabchk$tab1
      pltassgnx <- tabchk$tab2

      #if (pjoinid %in% names(pltassgnx)) {
      #  pltassgnid <- pjoinid
      #  setkeyv(pltassgnx, pjoinid)
      #}

      ## Check for matching unique identifiers of pltx with pltassgnx
      ## Subset pltx to pltassgnx ids
      pltx <- check.matchval(pltx, pltassgnx, pjoinid, pltassgnid, tab1txt="plt",
			tab2txt="pltassgn", subsetrows=TRUE)
      pltxvars <- names(pltx)[!names(pltx) %in% names(pltassgnx)]

      ## Merge pltx and pltassgnx (Note: inner join)
      pltx <- merge(pltassgnx, pltx[, unique(c(pjoinid, pltxvars)), with=FALSE], 
				by.x=pltassgnid, by.y=pjoinid)
      #pltx <- merge(pltx, pltassgnx, by.x=pjoinid, by.y=pltassgnid)
      puniqueid <- pltassgnid

    } else if (is.null(pltx)) {
      pltx <- pltassgnx
      puniqueid <- pltassgnid
      if (is.null(pjoinid)) pjoinid <- pltassgnid
    }
  }

  ## Check for duplicate plots
  locvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
  if (all(locvars %in% names(pltx)) && 
		any(pltx[, duplicated(.SD), .SDcols=locvars]) && 
			(!popType %in% c("GRM", "CHNG"))) {
    warning("duplicated plot locations exist... invalid for estimation")
	return(NULL)
  }

  ##################################################################################
  ## Check filter(s) for population data
  ##################################################################################
  if (!datindb) {
    if (!is.null(pltx) && (!is.null(popFilter$measCur) && popFilter$measCur || !is.null(popFilter$measEndyr))) {
      pltx <- getPlotCur(pltx, Endyr=popFilter$measEndyr, varCur="MEASYEAR",
				Endyr.filter=popFilter$measEndyr.filter)
    } else if (!is.null(invyrs)) {
      if (!"INVYR" %in% names(pltx)) stop("INVYR not in pltx")
      if (!all(invyrs %in% unique(pltx[["INVYR"]]))) {
        invyrs.miss <- invyrs[which(!invyrs %in% unique(pltx[["INVYR"]]))]
        message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
        if (length(invyrs.miss) == length(invyrs)) stop("")
        invyrs <- invyrs[!invyrs %in% invyrs.miss]
      }
      invyrs.filter <- getfilter(invyrsnm, invyrs)
      pltx <- datFilter(pltx, invyrs.filter)$xf
    }
  }	
  
  ## Subset popFilter - intensity
  if (!is.null(popFilter$intensity)) {
    intensity <- popFilter$intensity
    intensitynm <- pcheck.varchar(var2check="INTENSITY",
	       checklst=names(pltx), warn="INTENSITY variable not in plt")
    intensitymiss <- intensity[!all(intensity %in% unique(pltx[[intensitynm]]))]
    if (length(intensitymiss) > 0) {
      warning("invalid intensity: ", toString(intensitymiss))
	  return(NULL)
    }
    intensity.filter <- getfilter(intensitynm, intensity)
    pltx <- datFilter(pltx, intensity.filter)$xf
	if (nrow(pltx) == 0) {
	  warning("intensity pop filter returned 0 records")
	  return(NULL)
	}
  }
 
  ## Subset popFilter - invyrs (if additional to evalid)
  if (!is.null(evalid) && !is.null(invyrs)) {
    invyrsnm <- pcheck.varchar(var2check="INVYR",
	       checklst=names(pltx), warn="INVYR variable not in plt")
    invyrsmiss <- invyrs[!all(invyrs %in% unique(pltx[[invyrsnm]]))]
    if (length(invyrsmiss) > 0) {
      warning("invalid invyrs: ", toString(invyrsmiss))
	  return(NULL)
    }
    invyrs.filter <- getfilter(invyrsnm, invyrs)
    pltx <- datFilter(pltx, invyrs.filter)$xf
	if (nrow(pltx) == 0) {
	  warning("invry pop filter returned 0 records")
	  return(NULL)
	}
  }

  ## Subset popFilter - AOIonly
  if (!is.null(popFilter$AOIonly) && popFilter$AOIonly) {
    AOInm <- pcheck.varchar(var2check="AOI",
	       checklst=names(pltx), warn="AOI variable not in plt")
    AOI.filter <- getfilter(AOInm, 1)
    pltx <- datFilter(pltx, paste(AOInm, "== 1"))$xf
	if (nrow(pltx) == 0) {
	  warning("AOI pop filter returned 0 records")
	  return(NULL)
	}
  }

  ######################################################################################
  ## Check for missing plot variables
  ######################################################################################
  pltnmlst <- names(pltx)
  pltvars <- unique(c(pltassgnvars, pstratvars, pvars2keep))
  pvarsmiss <- pltvars[which(!pltvars %in% pltnmlst)]
#  if (length(pvarsmiss) > 0) {
#    stop("missing variables: ", paste(pvarsmiss, collapse=", "))
#  }
  if (defaultVars) {
    pdoms2keep <- pdoms
  } else {
    pdoms2keep <- pltnmlst
  }
  
  ## Check missing pdoms2keep variables in pltx
  ###########################################################################
  pmissvars <- pdoms2keep[which(!pdoms2keep %in% pltnmlst)]
  if (length(pmissvars) > 0) {
    if ("STATECD" %in% pmissvars)
      message("STATECD not in dataset.. assuming 1 state in dataset")
    if ("INTENSITY" %in% pmissvars)
      message("INTENSITY not in dataset.. assuming single intensity plots")
    if ("INVYR" %in% pmissvars)
      message("INVYR not in dataset.. assuming inventory years do not span more ",
		"than 1 cycle of measurements")
    if (!"DESIGNCD" %in% pltnmlst) {
      message("DESIGNCD not in dataset.. assuming DESIGNCD = 1 (annual inventory)")
      designcd <- 1
    } else {
      ##################################################################################
      ## NOTE: If adj='samp', make sure dataset is annual inventory only (DESIGNCD=1).
      ## Only adjust plots when DESIGNCD=1. Cannot have more than 1 DESIGNCD.
      ##################################################################################
      designcd <- unique(na.omit(pltx[["DESIGNCD"]]))
      if (length(designcd) > 1) {
        if (any(!designcd %in% c(1, 501:505, 230:242, 311:323, 328))) {
          if (adj == "samp") {
            message("designcds include: ", toString(sort(designcd)))
            message("samp adjustment for trees is only for annual inventory designs... see FIA database manual")
          } else {
            warning("more than 1 plot design, calculate separate estimates by design")
          }
        }
      }
    }
    pmissvars <- pmissvars[which(!pmissvars %in% pltnmlst)]

    if (any(prednames %in% pmissvars)) {
      prednames[which(!prednames %in% pmissvars)]
      warning("predname not in tables: ", paste(prednames, collapse=", "))
	  return(NULL)
    }
    if (any(unitvars %in% pmissvars)) {
      unitvars[which(!unitvars %in% pmissvars)]
      warning("unitvar not in tables: ", paste(unitvars, collapse=", "))
	  return(NULL)
    }
  }
  #pdoms2keep <- unique(pdoms2keep[which(!pdoms2keep %in% pmissvars)])

  ## Get state(s) and inventory year(s) (for titles)
  ############################################################
  states <- NULL
  invyrs <- NULL
  if ("STATECD" %in% names(pltx)) {
    stcds <- unique(pltx[["STATECD"]])
    states <- pcheck.states(stcds)
    if ("INVYR" %in% names(pltx)) {
      invyrtab <- unique(pltx[, c("STATECD", "INVYR")])
      setorderv(invyrtab, c("STATECD", "INVYR"))
      invyrs <- as.list(by(invyrtab$INVYR, invyrtab$STATECD, I))
      names(invyrs) <- pcheck.states(names(invyrs))
    }
  }
 
  ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
  ######################################################################################
  if (any(c("PLOT_STATUS_CD", "PSTATUSCD") %in% pltnmlst)) {
    if ("PSTATUSCD" %in% names(pltx))
      names(pltx)[names(pltx) == "PSTATUSCD"] <- "PLOT_STATUS_CD"
    ref_plot_status_cd <- ref_codes[ref_codes$VARIABLE == "PLOT_STATUS_CD", ]
    plotsampcnt <- pltx[, list(NBRPLOT=uniqueN(get(puniqueid))), by="PLOT_STATUS_CD"]
    plotsampcnt <-
	cbind(PLOT_STATUS_NM=ref_plot_status_cd[match(plotsampcnt$PLOT_STATUS_CD,
	ref_plot_status_cd$VALUE), "MEANING"], plotsampcnt)
    setkey(plotsampcnt, PLOT_STATUS_CD)
  } else {
    if (nonresp) stop("PLOT_STATUS_CD must be in dataset if nonresp=TRUE")
    message("PLOT_STATUS_CD not in dataset.. assuming all plots are at least ",
			"partially sampled")
    plotsampcnt <- pltx[, list(NBRPLOT=uniqueN(get(puniqueid)))]
  }
 
  if (ACI) {
    if (any(c("NF_PLOT_STATUS_CD", "PSTATUSNF") %in% pltnmlst)) {
      if ("PSTATUSNF" %in% names(pltx))
        names(pltx)[names(pltx) == "PSTATUSNF"] <- "NF_PLOT_STATUS_CD"
      ref_nf_plot_status_cd <- ref_codes[ref_codes$VARIABLE == "NF_PLOT_STATUS_CD", ]
      nfplotsampcnt <- pltx[, list(NBRPLOT=uniqueN(get(puniqueid))), by=NF_PLOT_STATUS_CD]
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

  ## Remove plots that have no remeasurement data
  ######################################################################################
  if (popType %in% c("GRM", "CHNG", "LULC") && "REMPER" %in% names(pltx)) {
    ## Remove plots that have no remeasurement data
    pltx <- pltx[!is.na(pltx$REMPER), ]
  }


  ######################################################################################
  ## Check estimation unit and strata
  ######################################################################################
 
  ## Check unitvar - if NULL, add unitvar=ONEUNIT to pltx
  ######################################################################################
  if (is.null(unitvar)) {
    unitvar <- checknm("ONEUNIT", names(pltx))
    message("no unitvar specified...  adding a variable named ", unitvar)
    pltx[, (unitvar) := 1]
    unitvars <- unitvar
    #areavar <- NULL
    pltassgnvars <- unique(c(unitvars, pltassgnvars))
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
  unitdat <- check.unitarea(unitarea=unitarea, pltx=pltx,
	unitvars=c(unitvar, unitvar2), areavar=areavar, areaunits=areaunits,
	removeunits=removeunits, removetext=removetext, gui=gui,
	vars2keep=vars2keep)
  unitarea <- unitdat$unitarea
  areavar <- unitdat$areavar
  areaunits <- unitdat$areaunits

  if (!unitindb && !is.null(evalid)) {
    ecol <- pcheck.varchar("EVALID", checklst=names(unitarea), stopifinvalid=FALSE)
    if (!is.null(ecol)) {
      unitarea <- unitarea[unitarea[[ecol]] %in% popevalid,]
      if (nrow(unitarea) == 0) {
        warning("evalid in unitarea does not match popevalid")
		return(NULL)
      }
    }
  }
 
  ######################################################################################
  ## Strata - Generate table of plots by strata, including nonsampled plots (P2POINTCNT)
  ######################################################################################
  if (strata) {
    stratalut <- pcheck.table(stratalut, tabnm="stratalut", 
		caption="stratalut table?", nullcheck=nullcheck, returnsf=FALSE)
    if (!stratindb && !is.null(evalid)) {
      ecol <- pcheck.varchar("EVALID", checklst=names(stratalut), stopifinvalid=FALSE)
      if (!is.null(ecol)) {
        stratalut <- stratalut[stratalut[[ecol]] %in% popevalid,]
        if (nrow(stratalut) == 0) {
          warning("evalid in stratalut does not match evalid")
		  return(NULL)
        }
      }
    }
 
    if (pivot) {
      strwtvar <- "strwt"
      unitvars <- unitvars[unitvars %in% names(stratalut)]
      stratalut <- strat.pivot(stratalut, unitvars=unitvars, 
                              strvar, strwtvar=strwtvar)
    }

    ## Create table of number of plots by estimation unit and strata
    P2POINTCNT <- pltx[, list(P2POINTCNT=uniqueN(get(puniqueid))),
		by=c(unitvars, strvar)]
    setkeyv(P2POINTCNT, c(unitvars, strvar))
 
    ## If nonresp, get Response Homogeneity Groups for WestFest
    #####################################################################
    if (nonresp) {
      RHGdat <- getRHG(pltx=pltx, puniqueid=puniqueid, unitvars=unitvars, 
				strvar=strvar)  
      pltx <- RHGdat$pltx
      RHGlut <- RHGdat$RHGlut
      P2POINTCNT <- RHGdat$P2POINTCNT
      nonresplut <- RHGdat$nonresplut  

      pltassgnvars <- unique(c(pltassgnvars, "RHG"))    
    } else {
      ## Create table of number of plots by estimation unit and strata
      P2POINTCNT <- pltx[, list(P2POINTCNT=uniqueN(get(puniqueid))),
		by=c(unitvars, strvar)]
      setkeyv(P2POINTCNT, c(unitvars, strvar))
    }
  } else {
    strvar <- NULL
    pltassgnvars <- unique(c(pltassgnvars, prednames))
  }
   
  #############################################################################
  ## Generate and apply nonsamp.pfilter
  #############################################################################
  if ((is.null(nonsamp.pfilter) || nonsamp.pfilter == "") && adj != "none") {
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (sum(pltx$PLOT_STATUS_CD == 3, na.rm=TRUE) > 0) {
        message("removing ", sum(pltx$PLOT_STATUS_CD == 3), " nonsampled forest plots")
        nonsamp.pfilter <- "PLOT_STATUS_CD != 3"
      }
    }
  }
  ## Apply nonsamp.pfilter
  if (!is.null(nonsamp.pfilter) && nonsamp.pfilter != "NONE") {
    pltx <- datFilter(x=pltx, xfilter=nonsamp.pfilter,
		title.filter="nonsamp.pfilter", gui=gui)$xf
    if (is.null(pltx)) {
      message(paste(nonsamp.pfilter, "removed all records"))
      return(NULL)
    }
  }

  #############################################################################
  ## Split tables
  #############################################################################
  ## Subset columns for pltassgn table
  pltassgnx <- data.table(pltx[, pltassgnvars, with=FALSE])
  setkeyv(pltassgnx, pltassgnid)

  ## Subset columns for pltassgn table
  pdoms2keep <- pdoms2keep[pdoms2keep %in% names(pltx)]
  pvars2keep <- pvars2keep[pvars2keep %in% names(pltx)]
  pltx <- data.table(pltx[, unique(c(puniqueid, pdoms2keep, pvars2keep)), with=FALSE])
  setkeyv(pltx, puniqueid)


  returnlst <- list(pltassgnx=pltassgnx, pltassgnid=pltassgnid, pltx=pltx,
        pfromqry=pfromqry, whereqry=whereqry, popwhereqry=popwhereqry, 
		puniqueid=puniqueid, pjoinid=pjoinid, popevalid=popevalid, palias=palias, 
		unitvar=unitvar, unitarea=unitarea, unitvar2=unitvar2, areavar=areavar, 
		areaunits=areaunits, unit.action=unit.action, ACI=ACI, 
 		P2POINTCNT=as.data.frame(P2POINTCNT), 
		plotsampcnt=as.data.frame(plotsampcnt), pdoms2keep=pdoms2keep, 
		states=states, invyrs=lapply(invyrs,I), dbconn=dbconn)

  if (module == "GB") {
    returnlst$strata <- strata
    if (strata) {
      returnlst$stratcombine <- stratcombine 
      returnlst$stratalut <- stratalut
      returnlst$strvar <- strvar
      returnlst$nonresp <- nonresp
    }
    if (nonresp) {
      returnlst$RHGlut <- RHGlut
      returnlst$nonresplut <- nonresplut
    }  
  } 
  if (module %in% c("MA", "SA")) {
    returnlst$prednames <- prednames
    returnlst$predfac <- predfac
  }
  if (ACI) {
    returnlst$nfplotsampcnt <- nfplotsampcnt
  }

  return(returnlst)
}
