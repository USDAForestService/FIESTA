check.popdataPLT <- 
  function(dsn, 
           datsource, schema = NULL, 
           tabs, tabIDs, 
           pltassgn, pltassgnid, pjoinid, 
           module, popType, popevalid, adj, 
           popFilter, nonsamp.pfilter = NULL, 
           unitarea = NULL, areavar, unitvar, unitvar2 = NULL, 
           areaunits, unit.action = "keep", removetext = "unitarea", 
           strata = FALSE, stratalut = NULL, auxlut = NULL, 
           strvar = NULL, stratcombine = TRUE, pivot = FALSE, 
           nonresp = FALSE, strwtvar = "strwt", 
           prednames = NULL, predfac = NULL, 
           pvars2keep = NULL, pdoms2keep = NULL, 
           nullcheck = FALSE, gui = FALSE, dsn_driver = "SQLite", 
           unitlevels = NULL, defaultVars = FALSE, 
           projectid = NULL, dbconn = NULL) {

  ###################################################################################
  ## DESCRIPTION: Checks plot data inputs
  ## - Set plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD,
  ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC,
  ##		ECOSUBCD, CONGCD, INTENSITY, DESIGNCD
  ## Check logical parameters: ACI, strata, stratcombine (if strata=TRUE)
  ## - If ACI, add NF_PLOT_STATUS_CD to pvars2keep 
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
  ##################################################################################
  plotsampcnt=nonresplut=pfromqry=pltassgnqry=unitareaqry=auxlutqry=
  pwhereqry=pltx=pltassgnx=popwhereqry=pstratvars <- NULL

  datindb=pltaindb=unitindb=stratindb=subcycle99 <- FALSE
  unitvars <- unique(c(unitvar2, unitvar))
  pltassgnvars <- unique(c(projectid, pltassgnid, unitvars)) 
  returndata <- FALSE
  SCHEMA. <- ""
  
  
  ###################################################################################
  ## 1. Define a set of plot-level variables that are necessary to keep for estimation. 
  ###################################################################################
  pvars2keep <- unique(c(pvars2keep, c("STATECD", "UNITCD", "COUNTYCD", "PLOT",
               "PLOT_STATUS_CD", "PLOT_NONSAMPLE_REASN_CD", "PSTATUSCD", "INTENSITY"))) 
  
  ## Set additional pvars2keep depending on popType
  if (popType %in% c("GRM", "CHNG", "LULC")) {
    pvars2keep <- unique(c(pvars2keep, c("PREV_PLT_CN", "REMPER")))
  } else if (popType == "P2VEG") {
    pvars2keep <- c(pvars2keep, "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
                    "SAMP_METHOD_CD")
  } else if (popType == "INV") {
    pvars2keep <- c(pvars2keep, "INVASIVE_SAMPLING_STATUS_CD", "INVASIVE_SPECIMEN_RULE_CD")
  }  
  
  ###################################################################################
  ## 2. Define a set of plot-level domain variables used for estimation. 
  ###################################################################################
  pdoms <- c("INVYR", "MEASYEAR", "RDDISTCD", "WATERCD", "ELEV", 
	           "ELEV_PUBLIC", "ECOSUBCD", "CONGCD", "DESIGNCD", "EMAP_HEX")

  ##############################################################################
  ## 3. Check database connection (dbconn) or dsn and define SCHEMA.
  ##############################################################################
  if (!is.null(dbconn)) {
    if (!DBI::dbIsValid(dbconn)) {
      stop("dbconn is invalid")
    }
    dbinfo <- DBI::dbGetInfo(dbconn)
    SCHEMA. <- ifelse (is.null(schema), "", paste0(schema, "."))
    dbtablst <- DBI::dbListTables(dbconn)
  } else if (!is.null(dsn)) {
    if (datsource == "sqlite") {
      dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
    } else {
      message("not supported")
    }
    SCHEMA. <- ifelse (is.null(schema), "", paste0(schema, "."))
  }
 
  ##############################################################################
  ## 4. Check plot and pltassgn tables.
  ##############################################################################
  pltassgnx=pltassgnnm=pfromqry <- NULL
  
  ## 4.1. Check plot table.
  ###########################################################
  if (popType %in% c("CHNG", "GRM")) {
    tabnames <- c("pltu", "plotu", "plt", "plot")
  } else {
    tabnames <- "plt"
  }
 
  plotlst <- popTabchk(tabnames, tabtext = "plt", 
                       tabs, tabIDs, dbtablst, dbconn) 
  plotnm <- plotlst$tabnm
  puniqueid <- plotlst$tabid
  pltflds <- plotlst$tabflds
  pltx <- plotlst$tabx
  plt <- plotlst$tab

  if (is.null(pltx) && !is.null(plotnm)) {
    datindb <- TRUE
    pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
  }	

  ## 4.2. Check pltassgn table.
  ############################################################# 
  
  ## Check if pltassgn table is a data.frame object
  pltassgnx <- pcheck.table(pltassgn, tabnm="pltassgn", 
                            caption="pltassgn table?", returnsf=FALSE)
  if (!is.null(pltassgnx)) {
    pltaindb <- FALSE
    pltassgnnm <- "pltassgnx"
    pltassgnflds <- names(pltassgnx)
  }	   
  
  ## Check if pltassgn is in database
  if (is.null(pltassgnx) && !is.null(pltassgn)) {
    pltassgnnm <- findnm(pltassgn, dbtablst, returnNULL = TRUE)
    if (is.null(pltassgn)) {
      message("pltassgn does not exist in database")
      if (!is.null(popevalid)) {
        return(NULL)
      }
    } else {
      pltaindb <- TRUE
      pltassgnflds <- DBI::dbListFields(dbconn, pltassgnnm)
    }
  }

  ## If pltassgn is a data.frame, import the plot table as data.frame R object.
  if (!is.null(pltassgnx) && is.data.frame(pltassgn) && !is.data.frame(pltx)) {	  
    if (!is.null(plotnm)) {
      pltx <- pcheck.table(plotnm, conn = dbconn, 
                           tabnm = plotnm, returnsf = FALSE)
      plotnm <- "pltx"
      pltflds <- names(pltx)
    }
  }
  
  ## 4.3. Check unique identifier of plot.
  ##################################################################################
  if (!is.null(plotnm)) {
    puniqueid <- pcheck.varchar(var2check = puniqueid, varnm = "puniqueid", gui=gui,
          checklst = pltflds, caption = "UniqueID variable of plot",
          warn = paste(puniqueid, "not in plt table"), stopifnull = TRUE)
  }
  
  ## 4.4. Check pjoinid and pltassgnid
  ##################################################################################
  ## Note: If pjoinid is NULL, set to pltassgnid
  if (!is.null(plotnm) && !is.null(pltassgnnm)) {
    pjoinidchk <- unlist(sapply(pjoinid, findnm, pltflds, returnNULL=TRUE))
    pltassgnchk <- unlist(sapply(pltassgnid, findnm, pltassgnflds, returnNULL=TRUE))
    if (is.null(pltassgnchk)) {
      pltassgnid <- pjoinid
      pltassgnchk <- unlist(sapply(pltassgnid, findnm, pltassgnflds, returnNULL=TRUE))
      if (is.null(pltassgnchk)) {
        stop("pltassgnid is invalid")
      }
    }
    if (length(pltassgnchk) < length(pltassgnid)) {
      pltassgnidmiss <- pltassgnid[!pltassgnid %in% pltassgnchk]
      message("pltassgnid variables are missing from dataset: ", toString(pltassgnidmiss))
      stop()
    } 
    if (is.null(pjoinidchk)) {
      pjoinid <- puniqueid
      pjoinidchk <- unlist(sapply(pjoinid, findnm, pltflds, returnNULL=TRUE))
      if (is.null(pjoinidchk)) {
        stop("invalid pjoinid")
      }
    } else if (length(pjoinidchk) != length(pltassgnid)) {
      message("pjoinid must be same number of variables as pltassgnid")
      stop()
    } else {
      pjoinid <- pjoinidchk
    }
  }

  ## 4.5. Build pfromqry using information above.
  ##################################################################################
  pwhereqry <- NULL
  pltassgn. <- "plta."
  plt. <- "p."
  
  ## Build from qry for all popfilters (epopfromqry)
  if (is.null(pfromqry)) {
    if (is.null(plotnm)) {
      pfromqry <- paste0("\nFROM ", SCHEMA., pltassgnnm, " plta")
      pflds <- pltassgnflds
      pltassgnvars <- pltassgnid	
    } else {
      pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
      pflds <- pltflds
      pltassgnvars <- puniqueid
    }
  }
  if (!is.null(plotnm) && !is.null(pltassgnnm)) {
    pjoinqry <- getjoinqry(pjoinid, pltassgnid, plt., pltassgn.)
    pltfromqry <- paste0("\n FROM ", SCHEMA., pltassgnnm, " plta ", 
                         "\n JOIN ", SCHEMA., plotnm, " p ", pjoinqry)
    pflds <- c(pltassgnflds, pltflds[!pltflds %in% pltassgnflds])
    pltassgnvars <- pltassgnid	
  }	else {
    pltfromqry <- pfromqry
  }
  
  ## Get default variables for plot
  if (defaultVars) {
    #pdoms2keep <- DBvars.default()$pltvarlst
    pdoms2keep <- pdoms
    pdoms2keep <- pdoms2keep[pdoms2keep %in% pflds]
  } else {
    pdoms2keep <- pflds
  }
  
  ## 4.6. Define select variables (selectpvars).
  ##################################################################################
  pltidvars <- {}
  ## Add projectid and popType to selectqry
  if (!is.null(projectid)) {
    selectidvars <- c(paste0("'", projectid, "' AS PROJECTID"), 
                      paste0("'", popType, "' AS POP_TYP"))
    projidvars <- c("PROJECTID", "POP_TYP")
  } else {
    selectidvars <- paste0("'", popType, "' AS POP_TYP")
    projidvars <- "POP_TYP"
  }
  ## Append uniqueid(s)
  selectpvars <- c(selectidvars, paste0(plt., puniqueid, " AS PLT_CN"))
  pltidsid <- "PLT_CN"
  pltidvars <- c(projidvars, "PLT_CN")
  
  ## Append pjoinid
  if (!identical(as.vector(pjoinid), as.vector(puniqueid))) {
    selectpvars <- c(selectpvars, paste0(plt., pjoinid))
    pltidvars <- c(projidvars, pjoinid)
  } 
  
  
  ###################################################################################
  ## 5. Check input parameters
  ###################################################################################

  ## 5.1. Check adj
  ##########################################################
  adjlst <- c("samp", "plot", "none")
  adj <- pcheck.varchar(var2check=adj, varnm="adj", gui=gui,
		checklst=adjlst, caption="adj", multiple=FALSE, stopifnull=TRUE)
  if (adj == "plot" && module == "GB") {
    message("adj='plot' is not typical for GA modules")
  }
  if (adj != "none") {
    pvars2keep <- c(pvars2keep, "MACRO_BREAKPOINT_DIA")
  }

  ## 5.2. Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
  ###################################################################################
  ACI <- pcheck.logical(popFilter$ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
  if (ACI) {
    pvars2keep <- c(pvars2keep, "NF_SAMPLING_STATUS_CD", "NF_PLOT_STATUS_CD")
  }

  ## 5.3. Check defaultVars
  ###################################################################################
  defaultVars <- pcheck.logical(defaultVars, varnm="defaultVars", 
                       title="Default variables?", first="NO", gui=gui)

  ## 5.4. Check unit.action
  ###################################################################################
  unit.actionlst <- c("keep", "remove", "combine")
  unit.action <- pcheck.varchar(var2check=unit.action, varnm="unit.action", gui=gui,
		checklst=unit.actionlst, caption="unit.action", multiple=FALSE, stopifnull=TRUE)


  ## 5.5. Check strata and other strata parameters.
  ###################################################################################
  strata <- pcheck.logical(strata, varnm="strata",
		title="Post stratify?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check strata parameters
  ########################################################
  if (strata) {
    ## 5.5.1. Check pivot
    pivot <- pcheck.logical(pivot, varnm="pivot",
		              title="Pivot stratalut?", first="NO", gui=gui)
    ## 5.5.2. Check nonresp
    nonresp <- pcheck.logical(nonresp, varnm="nonresp",
		               title="Post stratify?", first="YES", gui=gui)
    if (nonresp) {
      pstratvars <- unique(c(pstratvars, c("PLOT_STATUS_CD", "SAMP_METHOD_CD")))
      pltassgnvars <- unique(c(pltassgnvars, prednames))
    } 

    ## 5.5.3. Check stratcombine
    stratcombine <- pcheck.logical(stratcombine, varnm="stratcombine",
		               title="Combine strata?", first="YES", gui=gui, stopifnull=TRUE)
  }
  
  ## 5.6 Check predfac.
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

  ##################################################################################
  ## 6. Check estimation unit(s) and auxiliary information in plt/pltassgn.
  ##################################################################################
  
  ## 6.1. Check unitvars (unitvar, unitvar2)
  #######################################################################
  if (!is.null(unitvars)) {
    unitvarchk <- unlist(sapply(unitvars, findnm, pflds, returnNULL=TRUE))
    if (is.null(unitvarchk)) {
      message("unitvars must be included in dataset")
      return(NULL)
    } else {
      unitvars <- unitvarchk
    }	
  } else {
    unitvar <- checknm("ONEUNIT", pflds)
    message("no unitvar specified...  adding a variable named ", unitvar)
    unitvar=unitvars <- "ONEUNIT"
    nbrunits <- 1
    punit.vals <- 1
  }
  pltassgnvars <- unique(c(pltassgnvars, unitvars))
  adjbyvars <- unitvars
  unitvarsa. <- ifelse(all(unitvars %in% pltassgnflds), pltassgn., plt.)
  if (any(unitvars %in% pltidvars)) {
    unitvars2 <- unitvars[!unitvars %in% pltidvars]
    if (length(unitvars2) > 0) {
      selectpvars <- c(selectpvars, paste0(unitvarsa., unitvars2))
      pltidvars <- c(pltidvars, unitvars2)
    }
  } else {
    selectpvars <- c(selectpvars, paste0(unitvarsa., unitvars))
    pltidvars <- c(pltidvars, unitvars)
  }

  ## 6.2. Check strata variable (strvar) in plot/pltassgn.
  #######################################################################
  if (strata) {
    if (is.null(strvar)) {
      stop("must include strvar for post-strat estimates")
    } else {
      if (length(strvar) > 1) {
        stop("invalid strvar... only 1 variable allowed")
      }
      strvar <- findnm(strvar, pflds, returnNULL=TRUE)
      if (is.null(strvar)) {
        message("strata=TRUE, strvar must be included in dataset")
        return(NULL)
      }	
    }
    pltassgnvars <- unique(c(pltassgnvars, strvar))
    if (adj == "samp") {
      adjbyvars <- c(adjbyvars, strvar)
      strvara. <- ifelse(all(strvar %in% pltassgnflds), pltassgn., plt.)	
      
      if (any(strvar %in% pltidvars)) {
        strvar2 <- strvar[!strvar %in% pltidvars]
        if (length(strvar2) > 0) {
          selectpvars <- c(selectpvars, paste0(strvara., strvar2))
        }
      } else {
        selectpvars <- c(selectpvars, paste0(strvara., strvar))
      }
    }
  } else {
    strvar <- NULL
  }
  
  ## 6.3. Check prednames in plot/pltassgn.
  #######################################################################
  if (!is.null(prednames)) {
    prednameschk <- unlist(sapply(prednames, findnm, pflds, returnNULL=TRUE))
    if (is.null(prednameschk)) {
      message("no prednames are not found in dataset")
      return(NULL)
    } else if (length(prednameschk) < length(prednames)) {
      message("prednames are missing from dataset: ", 
              toString(prednames[!prednames %in% prednameschk]))
      return(NULL)
    } else {
      prednames <- prednameschk
    }
    pltassgnvars <- unique(c(pltassgnvars, prednames))
  }

  
  ## 7. Check pvars2keep in plot/pltassgn.
  #######################################################################
  if (!is.null(pvars2keep)) {
    pvars2keepchk <- unlist(sapply(pvars2keep, findnm, pflds, returnNULL = TRUE))
	  if (length(pvars2keepchk) < length(pvars2keep)) {
	    pvars2keepmiss <- pvars2keep[!pvars2keep %in% pvars2keepchk]
	    pvars2keepmiss <- pvars2keepmiss[pvars2keepmiss != "PSTATUSCD"]
	    if (length(pvars2keepmiss) > 0) {
        message("variables are missing from dataset: ", toString(pvars2keepmiss))
	    } else {
	      pvars2keep <- pvars2keepchk
	    }
      #return(NULL)
    } else {
      pvars2keep <- pvars2keepchk
    }	  
  }

  pvars <- unique(c(pvars2keep, pdoms))
  pvars <- pvars[pvars %in% pflds]
  pltvars <- pvars[pvars %in% pltflds]
  pltavars <- pvars[!pvars %in% c(adjbyvars, pltvars) & pvars %in% pltassgnflds]
  if (length(pvars) > 0) {
    pltvars <- paste0(plt., pvars)
    #selectpvars <- unique(c(selectpvars, pltvars))
  }
  if (length(pltavars)) {
    pltavars <- paste0(pltassgn., pltavars)
    #selectpvars <- unique(c(selectpvars, pltvars))
  }

  ##################################################################################
  ## 8. Check popFilters and create pltidsqry
  ##################################################################################
  popFilterqry <- 
    getpopFilterqry(popType = popType, 
                    popFilter = popFilter, 
                    pltfromqry = pltfromqry,
                    plotnm = plotnm,
                    pltassgnnm = pltassgnnm,
                    pflds = pflds,
                    pltassgnflds = pltassgnflds, 
                    puniqueid = puniqueid, 
                    pltassgn. = pltassgn., 
                    plt. = plt.,
                    selectpvars = selectpvars,
                    dbconn = dbconn,
                    schema = schema,
                    datsource = datsource,
                    dbTabs = dbTables(plot_layer = tabs$plt),
                    datindb = datindb,
                    pltaindb = pltaindb,
                    pltassgnx = pltassgnx,
                    pltx = pltx,
                    chkvalues = FALSE,
                    projectid = projectid) 
  pltidsqry <- popFilterqry$pltidsqry
  pwhereqry <- popFilterqry$pwhereqry
  pfromqry <- popFilterqry$pfromqry
  pltselectqry <- popFilterqry$pltselectqry
  pltfromqry <- popFilterqry$pltfromqry
  states <- popFilterqry$states
  invyrs <- popFilterqry$invyrs
  nonsamp.pfilter <- popFilterqry$nonsamp.pfilter
  popevalid <- popFilterqry$popevalid
  ppsanm <- popFilterqry$ppsanm
  POP_PLOT_STRATUM_ASSGN <- popFilterqry$POP_PLOT_STRATUM_ASSGN

  
  ## 9. Check PLOT_STATUS_CD and generate table with number of plots
  ########################################################################
  pstatusvars <- c("PLOT_STATUS_CD", "PSTATUSCD")
  pstatuschk <- unlist(sapply(pstatusvars, findnm, pflds, returnNULL=TRUE))
  if (!is.null(pstatuschk)) {
    ref_plot_status_cd <- ref_codes[ref_codes$VARIABLE == "PLOT_STATUS_CD", ]
    if (length(pstatuschk) > 1) {
      pstatuscdnm <- pstatuschk[1]
    } else {
      pstatuscdnm <- pstatuschk
    }  
    
    ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
    plotsampcntqry <- paste0(
      "SELECT p.", pstatuscdnm, ", COUNT(*) AS NBRPLOTS", 
      pltfromqry, 
      pwhereqry, 
      "\nGROUP BY p.", pstatuscdnm,
      "\nORDER BY p.", pstatuscdnm)
    
    if (pltaindb) {      
      plotsampcnt <- tryCatch(
        DBI::dbGetQuery(dbconn, plotsampcntqry),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    } else {
      plotsampcnt <- tryCatch(
        sqldf::sqldf(plotsampcntqry, connection = NULL),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }
    
    if (is.null(plotsampcnt)) {
      message("invalid plotsampcnt query")
      message(plotsampcntqry)
    }
    
    ## create data frame of plot counts
    plotsampcnt <-
      cbind(PLOT_STATUS_NM = ref_plot_status_cd[match(plotsampcnt$PLOT_STATUS_CD,
                    ref_plot_status_cd$VALUE), "MEANING"], plotsampcnt)
  }						  
 

  ## If ACI, check NF_PLOT_STATUS_CD and generate table with number of plots
  ##########################################################################
  if (popFilter$ACI) {
    nfpstatusvars <- c("NF_PLOT_STATUS_CD", "PSTATUSNF")
    nfpstatuschk <- unlist(sapply(nfpstatusvars, findnm, pflds, returnNULL=TRUE))
    if (!is.null(nfpstatuschk)) {
      ref_nf_plot_status_cd <- ref_codes[ref_codes$VARIABLE == "NF_PLOT_STATUS_CD", ]
      if (length(nfpstatuschk) > 1) {
        nfpstatuscdnm <- nfpstatuschk[1]
      } else {
        nfpstatuscdnm <- nfpstatuschk
      }  
      
      ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
      nfplotsampcntqry <- paste0(
        "SELECT p.", nfpstatuscdnm, ", COUNT(*) AS NBRPLOTS", 
        pltfromqry, 
        pwhereqry,
        "\nGROUP BY p.", nfpstatuscdnm,
        "\nORDER BY p.", nfpstatuscdnm)
      
      if (pltaindb) {      
        nfplotsampcnt <- tryCatch(
          DBI::dbGetQuery(dbconn, nfplotsampcntqry),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      } else {
        nfplotsampcnt <- tryCatch(
          sqldf::sqldf(nfplotsampcntqry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(plotsampcnt)) {
        message("invalid plotsampcnt query")
        message(plotsampcntqry)
      }
      
      ## create data frame of nonforest plot counts
      nfplotsampcnt <- nfplotsampcnt[!is.na(nfplotsampcnt$NF_PLOT_STATUS_CD), ]
      if (nrow(nfplotsampcnt) > 0) {
        nfplotsampcnt <-
          cbind(NF_PLOT_STATUS_NM = ref_nf_plot_status_cd[match(nfplotsampcnt$NF_PLOT_STATUS_CD,
                  ref_nf_plot_status_cd$VALUE), "MEANING"], nfplotsampcnt)
        ## Append to plotsampcnt
        if (!is.null(plotsampcnt)) {
          plotsampcnt <- rbindlist(list(plotsampcnt, nfplotsampcnt), use.names=FALSE)
        } else {
          plotsampcnt <- nfplotsampcnt
        }
        
      }        
    }
  }
  

  ##################################################################################
  ## 10. Get estimation unit(s) (unitvars) values
  ##################################################################################
  if (!is.null(unitvars)) {
    ## Check unitvars in plot data
    unitvarsa. <- ifelse(all(unitvars %in% pltflds), plt., pltassgn.)	  	
	  unitvarqry <- paste0(
	      "SELECT DISTINCT ", toString(paste0(unitvarsa., unitvars)), 
          pltfromqry,
          pwhereqry,
          "\nORDER BY ", toString(paste0(unitvarsa., unitvars)))
    if (pltaindb) {      
      unitvartab <- tryCatch(
               data.table(DBI::dbGetQuery(dbconn, unitvarqry)),
                      error = function(e) {
                      message(e,"\n")
                      return(NULL) })
      if (is.null(unitvartab) || nrow(unitvartab) == 0) {
        message(unitvar, " is invalid...")
        message(unitvarqry)
        stop()
      }
    } else {
      unitvartab <- tryCatch(
                data.table(sqldf::sqldf(unitvarqry, connection = NULL)),
                             error = function(e) {
                               message(e,"\n")
                               return(NULL) })
      if (is.null(unitvartab) || nrow(unitvartab) == 0) {
        message(unitvar, " is invalid...")
        message(unitvarqry)
        stop()
      }
    }
    if (nrow(unitvartab) > 0) {
	    punit.vals <- sort(do.call(paste, unitvartab[, unitvars, with=FALSE]))
	    nbrunits <- length(punit.vals)
    } else {
	    message("unitvars not in dataset...")
	    message(unitvarqry)
	  }
  }

  ######################################################################################
  ## 11. Check unitarea 
  ######################################################################################
  vars2keep=unitareax <- NULL
  if (module == "SA" && "AOI" %in% names(unitarea)) {
    vars2keep <- "AOI"
  }
  
  ## 11.1. Check if unitarea is a data.frame object
  ###################################################################
  if (is.null(unitarea)) {
    message("unitarea is missing... include with population data if total estimates are desired")
  } else {
    if (nbrunits == 1 && !is.data.frame(unitarea)) {
      if (is.vector(unitarea) && length(unitarea) == 1 && is.null(chkdbtab(dbtablst, unitarea))) {
	      if (is.numeric(unitarea)) {
	        unitarea <- data.table(ONEUNIT = 1, unitarea)
	      } else if (sum(grepl(",", unitarea)) > 0) {
	        unitarea <- data.table(ONEUNIT = 1, as.numeric(gsub(",", "", unitarea)))
	      }
	      unitvar=unitvars <- "ONEUNIT"
		    if (is.null(areavar)) areavar <- "AREA_USED"
	      names(unitarea) <- c(unitvar, areavar)
	    }
	  } else if (is.data.frame(unitarea)) {	
	    ## Check unitarea
      unitareax <- pcheck.table(unitarea, tabnm="unitarea", 
		                   caption="unitarea table?", returnsf=FALSE)
	    if (nbrunits == 1 && unitvar %in% names(unitarea)) {
	      unitarea[[unitvar]] <- 1
	    }
	  } 

    ## 11.2. Check evalid and filter 
    ###################################################################
    if (is.null(unitareax) && !is.null(chkdbtab(dbtablst, unitarea))) {
      unitindb <- TRUE
      unitarea_layer <- chkdbtab(dbtablst, unitarea)
	    unitareaflds <- DBI::dbListFields(dbconn, unitarea_layer)
      unitareaqry <- paste0("SELECT * \nFROM ", unitarea_layer)
	
	    if (!is.null(popevalid)) {	
	      uevalidnm <- findnm("EVALID", unitareaflds, returnNULL = TRUE) 
		
		    ## Check popevalid values in database	  
	      uevalidqry <- paste0(
		                 "SELECT DISTINCT ", uevalidnm, 
	                   "\nFROM ", SCHEMA., unitarea_layer,
	                   "\nORDER BY ", uevalidnm)
        if (!is.data.frame(unitarea)) {      
          uevalidvals <- DBI::dbGetQuery(dbconn, uevalidqry)[[1]]
	      } else {
	        uevalidvals <- sqldf::sqldf(uevalidqry, connection = NULL)[[1]]
	      }
        uevalidmiss <- popevalid[!popevalid %in% uevalidvals]
          
        if (any(!popevalid %in% uevalidvals)) {
	        message("evalids are missing in unitarea: ", 
		                   toString(popevalid[!popevalid %in% uevalidvals]))
          return(NULL)
        }
		
        if (!is.null(popevalid) && !is.null(uevalidnm)) {
           unitareaqry <- paste0(unitareaqry, 
		                     "\nWHERE ", uevalidnm, " IN (", toString(popevalid), ")")
        }
	    } else {
	      unitareaqry <- NULL
	    }
      unitareax <- pcheck.table(unitarea, conn=dbconn,
              tabnm="unitarea", caption="unitarea?", 
              nullcheck=nullcheck, tabqry=unitareaqry, returnsf=FALSE)
			  
	    if (is.null(unitareax)) {
	      message("invalid unitareax")
	      return(NULL)
	    } 
	  
	    unitareax <- unique(unitareax[, c(unitvars, areavar), with=FALSE])	  
	    if (any(duplicated(unitareax[, unitvars, with=FALSE]))) {
	      message("unitarea is invalid... multiple unitvars exist")
		    return(NULL)
	    }
    }
	
    ## 11.3. Check areavar 
    ###################################################################
    areavar <- pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui,
		      checklst=names(unitareax), caption="Area variable?", stopifnull=TRUE)
    
    ## Check areaunits
    areaunits <- pcheck.varchar(var2check=areaunits, varnm="areaunits",
                   gui=gui, checklst=c("acres", "hectares"), caption="Area units?",
                   stopifnull=TRUE)
    
		   
    ## Check if areavar column is numeric
    if (!is.numeric(unitareax[[areavar]])) {
      if(sum(grepl(",", unitareax[[areavar]])) > 0)
        unitareax[[areavar]] <- as.numeric(gsub(",", "", unitareax[[areavar]]))
        if (!is.na(unitareax[[areavar]])) {
          stop("invalid areavar in unitarea.. must be a number")
        }
      }

      ## Check for NA values in areavar
      if (any(is.na(unitareax[[areavar]]))) {
        navals <- unitareax[is.na(get(areavar)), ]
        message("there are NA values in area.. removing from table")
        print(navals)
        unitareax <- unitareax[!is.na(get(areavar)), ]
      }
		   
	    ## Check unitvars
	    if (!all(unitvars %in% names(unitareax))) {
        unitvarchk2 <- unlist(sapply(unitvars, findnm, names(unitareax), returnNULL=TRUE))
	      if (!is.null(unitvarchk2)) {
	        setnames(unitareax, unitvarchk2, names(unitvarchk2))
	      }
 	    }
	    if (!all(unitvars %in% names(unitareax))) {
	      message("unitvars are not in unitareax: ", 
	                    toString(unitvars[!unitvars %in% names(unitareax)]))
	      return(NULL)
	    }	 
	
      ## 11.4. Check if all values in unitarea are in pltassgn
      ###############################################################################
      ## Check unit.vals with punit.vals
	    unit.vals <- sort(do.call(paste, unitareax[, unitvars, with=FALSE]))
	
	    if (any(is.na(match(unit.vals, punit.vals)))) {
	      unit.miss <- unit.vals[is.na(match(unit.vals, punit.vals))]
	      message("unit in unitarea is not in plot data: ", toString(unit.miss))
 	  
	    if (unit.action == "remove") {
        unitareax[, `:=`(MATCH, do.call(paste, .SD)), .SDcols = unitvars]
        unitareax <- unitareax[!MATCH %in% unit.miss, ]
        unitareax[, `:=`(MATCH, NULL)]
      }
		
	  } else if (any(is.na(match(punit.vals, unit.vals)))) {
	    punit.miss <- unit.vals[is.na(match(punit.vals, unit.vals))]
	    message("unit in plot data is not in unitarea: ", toString(punit.miss))
	    return(NULL)
	  }  

	  ## 11.5. Sum areavar by unitvars to aggregate any duplicate rows
	  ###############################################################################
    unitareax <- unitareax[, sum(.SD, na.rm=TRUE), by=c(unitvars, vars2keep), .SDcols=areavar]
    setnames(unitareax, "V1", areavar)
  }	

  ######################################################################################
  ## 12. Check auxiliary data in auxlut.
  ######################################################################################
  strunitvars <- unitvars
  
  auxvars <- unique(c(strvar, prednames))
  auxtabnm <- ifelse(strata, "stratalut", "auxlut")
  if (!is.null(auxvars)) {
    if (is.null(auxlut)) {
      if (strata) {
        message("strata=TRUE and stratalut is missing... ")
	    } else {
	      message("prednames != NULL and auxlut is missing... ")
	    }
	    return(NULL)
    }	
	  ## 12.1. Check if auxlut is a data.frame R object
    auxlutx <- pcheck.table(auxlut, tabnm = auxtabnm, 
		             caption = paste(auxtabnm, " table?"), returnsf=FALSE)

    if (is.null(auxlutx) && !is.null(chkdbtab(dbtablst, auxlut))) {
      auxindb <- TRUE
      auxlut_layer <- chkdbtab(dbtablst, auxlut)
	    auxlutflds <- DBI::dbListFields(dbconn, auxlut_layer)
      auxlutqry <- paste0("SELECT * \nFROM ", SCHEMA., auxlut_layer)
	  
      ## 12.2. Check evalid and filter 
      ###################################################################
      if (!is.null(popevalid)) {
	      evalidnm <- findnm("EVALID", auxlutflds, returnNULL = TRUE) 
        if (!is.null(popevalid) && !is.null(evalidnm)) {
          auxlutqry <- paste0(
		          auxlutqry, 
		          "\nWHERE ", evalidnm, " IN (", toString(popevalid), ")")
        }
	      auxlutx <- pcheck.table(auxlut, conn = dbconn,
                      tabnm = auxtabnm, caption = paste(auxtabnm, " table?"),
		                  tabqry = auxlutqry, returnsf = FALSE)
	      if (is.null(auxlutx) || nrow(auxlutx) == 0) {
		      ## Check popevalid values in database	  
	        sevalidqry <- paste0("SELECT DISTINCT ", evalidnm, 
	                             "\nFROM ", SCHEMA., auxlut_layer)
          if (!is.data.frame(auxlutx)) {      
            sevalidvals <- DBI::dbGetQuery(dbconn, sevalidqry)[[1]]
	        } else {
	          sevalidvals <- sqldf::sqldf(sevalidqry, connection = NULL)[[1]]
	        }
          sevalidmiss <- popevalid[!popevalid %in% sevalidvals]
          if (any(!popevalid %in% sevalidvals)) {
	          message("evalids are missing in stratalut: ", 
	                            toString(popevalid[!popevalid %in% sevalidvals]))
            return(NULL)
          }
	      }
	    } 
	  }

    if (strata) {
      
      ## 12.3. Pivot auxlut table based on strwtvar
      ###################################################################
      if (pivot) {
        stratalutx <- strat.pivot(auxlutx, unitvars = unitvars, 
                                  strvar = strvar, strwtvar = strwtvar)
      } else {			
        stratalutx <- auxlutx
      }

      ## 12.4. Check if strvar is in auxlut, if strata=TRUE
      ###################################################################
      strvar <- pcheck.varchar(var2check = strvar, varnm = "strvar", gui=gui,
		     checklst = names(stratalutx), caption = "strata variable",
		     warn=paste(strvar, "not in strata table"), stopifnull=TRUE)

 	    ## 12.5. Check unitlevels for collapsing
	    if (!is.null(unitlevels)) {
	      unitvals <- unique(stratalutx[[unitvar]])
	      if (length(unitlevels) != length(unitvals)) {
		      misslevels <- unitvals[!unitvals %in% unitlevels]
	        message("unitlevels does not match unitvals... missing: ", toString(misslevels))
		      return(NULL)
        } else if (any(is.na(match(unitlevels, unitvals)))) {
	        difflevels <- unitvals[is.na(match(unitlevels, unitvals))]
	        message("unitlevels does not match unitvals... missing: ", toString(difflevels))
		      return(NULL)
        }		
      } 	

      ## 12.6. Create a table of number of plots by strata and estimation unit
      ###################################################################
      strunitvars <- unique(c(unitvars, strvar))
	    strunitvarsqry <- paste0(pltassgn., strunitvars)
	    P2POINTCNTqry <- paste0(
	         "SELECT ", toString(strunitvarsqry), ", COUNT(*) AS NBRPLOTS", 
	          pltfromqry,
			      pwhereqry,
			      "\nGROUP BY ", toString(strunitvarsqry),
			      "\nORDER BY ", toString(strunitvarsqry))
	    if (pltaindb) {      
        P2POINTCNT <- data.table(DBI::dbGetQuery(dbconn, P2POINTCNTqry))
      } else {
	      P2POINTCNT <- data.table(sqldf::sqldf(P2POINTCNTqry, connection = NULL))
      }
      setkeyv(P2POINTCNT, strunitvars)
 
      ## 12.7. If nonresp, get Response Homogeneity Groups for WestFest
      #####################################################################
      if (nonresp) {
        nonrespvars <- c("PLOT_STATUS_CD", "SAMP_METHOD_CD")
        nonrespchk <- unlist(sapply(nonrespvars, findnm, pflds, returnNULL=TRUE))
        if (is.null(unitvarchk)) {
	        message("the following vars must be included in dataset: ", toString(nonrespvars))
          return(NULL)
	      }
        pltnrqry <- paste0(
		         "SELECT ", toString(strunitvarsqry), ", ",
		          toString(paste0("p.", c(puniqueid, nonrespvars))),
              pltfromqry,
              pwhereqry,
              "\nORDER BY ", toString(strunitvarsqry), ", ",
		                    toString(paste0("p.", c(puniqueid, nonrespvars))))						   
	      if (pltaindb) {      
          pltnr <- data.table(DBI::dbGetQuery(dbconn, pltnrqry))
        } else {
	        pltnr <- data.table(sqldf::sqldf(pltnrqry, connection = NULL))
        }
	
        RHGdat <- getRHG(pltx = pltnr, puniqueid = puniqueid, 
		                 unitvars = unitvars, strvar = strvar)  
        pltnr <- RHGdat$pltx
        RHGlut <- RHGdat$RHGlut
        P2POINTCNT <- RHGdat$P2POINTCNT
        nonresplut <- RHGdat$nonresplut  

        pltassgnvars <- unique(c(pltassgnvars, "RHG"))  
      } 	
    }
  }
   
  ######################################################################################
  ## 13. Query and import pltassgnx
  ######################################################################################
  ppsaselectvars <- {}
  pltassgnvars <- pltassgnvars[pltassgnvars %in% pltassgnflds]
  if (length(pltassgnvars) > 0) {
    pltassgnselectvars <- paste0(pltassgn., pltassgnvars)
  }

  ## Build select for pltassgnx
  pltassgn_selectqry <- paste0("SELECT ",
                              toString(pltassgnselectvars))

  ## Build query for pltassgnx
  pltassgnxqry <- paste0(pltassgn_selectqry, 
                         pltfromqry,
                         pwhereqry)
  if (pltaindb) {      
    pltassgnx <- data.table(DBI::dbGetQuery(dbconn, pltassgnxqry))
  } else {
	  pltassgnx <- data.table(sqldf::sqldf(pltassgnxqry, connection = NULL))
  }
  setkeyv(pltassgnx, pltassgnid)
  
  
  
  ######################################################################################
  ## 14. Get plot counts by estimation unit
  ######################################################################################

  ## Build select for plot counts
  pltcnt_grpbyvars <- toString(paste0(pltassgn., unitvars))
  pltcnt_selectqry <- paste0(
      "\nSELECT ", pltcnt_grpbyvars, ", COUNT(*) NBRPLOTS")
  if (!is.null(pstatuscdnm)) {    
    pltcnt_selectqry <- paste0(pltcnt_selectqry, ", ",
      "\n  SUM(CASE WHEN p.PLOT_STATUS_CD == 1 THEN 1 ELSE 0 END) AS FOREST,",
      "\n  SUM(CASE WHEN p.PLOT_STATUS_CD == 2 THEN 1 ELSE 0 END) AS NONFOREST")
  }  
    

  ## Build query for plot counts
  plotunitcntqry <- paste0(pltcnt_selectqry, 
                        pltfromqry,
                        pwhereqry,
                        "\nGROUP BY ", pltcnt_grpbyvars,
                        "\nORDER BY ", pltcnt_grpbyvars)
  if (pltaindb) {      
    plotunitcnt <- DBI::dbGetQuery(dbconn, plotunitcntqry)
  } else {
    plotunitcnt <- sqldf::sqldf(plotunitcntqry, connection = NULL)
  }
  

  #############################################################################
  ## 13. Return data
  #############################################################################
  returnlst <- list(pltassgnx=pltassgnx, pltassgnid=pltassgnid,
        pltidsqry=pltidsqry, pwhereqry=pwhereqry, pltassgn.=pltassgn.,
        pltselectqry=pltselectqry, pltfromqry=pltfromqry,
        pltx=pltx, plotlst=plotlst, pltidsid = pltidsid,
        projidvars=projidvars, pltidvars=as.vector(pltidvars),
        pdoms2keep=as.vector(unique(c(pvars2keep, pdoms2keep))),
		    puniqueid=puniqueid, pjoinid=pjoinid, adjbyvars=adjbyvars,
		    unitvar=unitvar, unitarea=unitareax, unitvar2=unitvar2, areavar=areavar, 
		    areaunits=areaunits, unit.action=unit.action, ACI=ACI, 
 		    P2POINTCNT=as.data.frame(P2POINTCNT), unitlevels=unitlevels, 
		    plotsampcnt=as.data.frame(plotsampcnt), plotunitcnt = plotunitcnt,
		    nonsamp.pfilter=nonsamp.pfilter, POP_PLOT_STRATUM_ASSGN=POP_PLOT_STRATUM_ASSGN,
		    states=states, invyrs=invyrs, pltaindb=pltaindb, datindb=datindb, 
		    dbconn=dbconn, SCHEMA.=SCHEMA.)

  if (module == "GB") {
    returnlst$strata <- strata
    if (strata) {
      returnlst$stratcombine <- stratcombine 
      returnlst$stratalut <- stratalutx
      returnlst$strvar <- strvar
      returnlst$nonresp <- nonresp
    }
    if (nonresp) {
      returnlst$RHGlut <- RHGlut
      returnlst$nonresplut <- nonresplut
    }  
  } 
  if (module %in% c("MA", "SA")) {
    returnlst$auxlut <- auxlutx
    returnlst$prednames <- prednames
    returnlst$predfac <- predfac
  }
  if (ACI) {
    returnlst$nfplotsampcnt <- nfplotsampcnt
  }

  return(returnlst)
}
