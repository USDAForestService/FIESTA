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
           projectid = NULL, dbconn = NULL,
           database_opts = NULL,
           dsnreadonly = TRUE) {
    
    ###################################################################################
    ## DESCRIPTION: Checks plot data inputs
    ## - Set plt domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD,
    ##		INVYR, MEASYEAR, PLOT_STATUS_CD, RDDISTCD, WATERCD, ELEV, ELEV_PUBLIC,
    ##		ECOSUBCD, CONGCD, INTENSITY, DESIGNCD
    ## Check logical parameters: ACI, strata, stratcombine (if strata=TRUE)
    ## - If ACI, add NF_PLOT_STATUS_CD to pltvars2keep 
    ## - If unit.action='combine', estimation units are combined if less than 10 plots
    ## - If strata, only 1 auxvar allowed, add to pltvars2keep
    ## - If module = SA or MA-greg, add prednames to pltvars2keep
    ## - If adj="samp", nonsample adjustment factors calculated at strata level
    ## - If adj="plot", nonsample adjustment factors calculated at plot level
    ## Check unit.action ('keep', 'remove', 'combine').
    ## Check predfac, if module = SA, MA-greg
    ## Import and check plt and pltassgn tables
    ## Check corresponding unique identifiers (puniqueid, pltassgnid)
    ## Merge plt, pltassgn tables to check necessary variables
    ## Check plot data
    ## - Check for missing pltvars2keep variables
    ## - Check for missing pdoms2keep variables (STATE, INTENSITY, INVYR, DESIGNCD)
    ## - Get state(s) and inventory year(s) (for titles)
    ## - Generate table of sampled/nonsampled plots (if PLOT_STATUS_CD included)
    ## - If ACI, add table of sampled/nonsampled nonforest plots (if NF_PLOT_STATUS_CD included)
    ## - Generate table of plots by strata, including nonsampled plots (P2POINTCNT)
    ## - If nonresp, generate table of nonsampled plots by strata, substrvar
    ## - Generate and apply nonsamp.pfilter (PLOT_STATUS_CD != 3)
    ## - Check for NA values in pltvars2keep variables
    ## - If unitvar = NULL, add unitvar=ONEUNIT
    ## Subset variables for pltx and pltassgnx
    ###################################################################################
    
    ## Set global variables
    ##################################################################################
    plotsampcnt=nonresplut=pfromqry=pltassgnqry=unitareaqry=auxlutqry=MATCH=
      pwhereqry=pltx=pltassgnx=popwhereqry=pstratvars=getdataWITHqry=getdataCNs <- NULL
    
    datindb=pltaindb=unitindb=stratindb=subcycle99 <- FALSE
    unitvars <- unique(c(unitvar2, unitvar))
    pltassgnvars <- unique(c(projectid, pltassgnid, unitvars)) 
    SCHEMA. <- ""
    P2POINTCNT=POP_PLOT_STRATUM_ASSGN=PLOT=pstatuscdnm <- NULL
    
   
    ###################################################################################
    ## 1. Define a set of plot-level variables that are necessary to keep for estimation. 
    ###################################################################################
    
    ## Variables to keep in plot table (Note: pvars2keep are variables to keep in pltassgnx)
    pltvars2keep <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "PLOT_STATUS_CD",
                      "PLOT_NONSAMPLE_REASN_CD", "PSTATUSCD", "INTENSITY",
                      "SUBCYCLE")
    
    ## Set additional pltvars2keep depending on popType
    if (popType %in% c("GRM", "CHNG", "LULC")) {
      pltvars2keep <- unique(c(pltvars2keep, c("PREV_PLT_CN", "REMPER")))
    } else if (popType == "P2VEG") {
      pltvars2keep <- c(pltvars2keep, "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
                      "SAMP_METHOD_CD")
    } else if (popType == "INV") {
      pltvars2keep <- c(pltvars2keep, "INVASIVE_SAMPLING_STATUS_CD", "INVASIVE_SPECIMEN_RULE_CD")
    }  
    
    ###################################################################################
    ## 2. Define a set of plot-level domain variables used for estimation. 
    ###################################################################################
    pltdoms <- c("INVYR", "MEASYEAR", "RDDISTCD", "WATERCD", "ELEV", 
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
    } else {
      datsourcelst <- c("sqlite", "postgres", "obj")
      datsource <- pcheck.varchar(var2check = datsource, varnm = "datsource", gui=gui,
                       checklst = datsourcelst, caption = "Data source",
                       warn = paste(datsource, "is invalid"), stopifinvalid = FALSE,
                       stopifnull = FALSE)
      if (is.null(dsn)) {
        datsource <- "obj"
        dbtablst <- NULL
      } else if (datsource == "sqlite") {
        dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE, showlist=FALSE)
        dbtablst <- DBI::dbListTables(dbconn)
      } else if (datsource == "postgres") {
        dbconn <- DBtestPostgreSQL(dbname = database_opts$dbname,
                                   host = database_opts$host,
                                   port = database_opts$port,
                                   user = database_opts$user,
                                   password = database_opts$password,
                                   dbconnopen=TRUE, showlist=FALSE)
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
                         tabs, tabIDs, dbtablst, dbconn = dbconn) 
    plotnm <- plotlst$tabnm
    puniqueid <- plotlst$tabid
    pltflds <- plotlst$tabflds
    pflds <- pltflds
    pltx <- plotlst$tabx
    plt <- plotlst$tab
    

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
      if (is.null(pltassgnnm)) {
        message("pltassgn does not exist in database")
        if (!is.null(popevalid)) {
          return(NULL)
        }
      } else {
        pltaindb <- TRUE
        pltassgnflds <- DBI::dbListFields(dbconn, pltassgnnm)
      }
    }

    ## 4.3. Check unique identifier of plot and pltassgn and pjoinid
    ##################################################################################
    ## Define alias prefixes for pltassgn and plt
    pltassgn. <- "plta."
    plt. <- "p."

    if (!is.null(plotnm)) {
      puniqueid <- 
        pcheck.varchar(var2check = puniqueid, varnm = "puniqueid", gui=gui,
                       checklst = pltflds, caption = "UniqueID variable of plot",
                       warn = paste(puniqueid, "not in plt table"), stopifnull = TRUE)
      
      ## Build from query for plot
      ## If pltx is NULL and plotnm is not null, the assumption is that all plot data are a database
      if (is.null(pltx)) {
        datindb <- TRUE
        pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
      }
      
      ## Check pjoinid and pltassgnid
      if (!is.null(pltassgnnm)) {
        pltassgnchk <- unlist(sapply(pltassgnid, findnm, pltassgnflds, returnNULL=TRUE))
        if (is.null(pltassgnchk)) {
          if (pltassgnid == "CN" && !is.null(findnm("PLT_CN", pltassgnflds, returnNULL=TRUE))) {
            pltassgnid <- findnm("PLT_CN", pltassgnflds)
          } else if (pltassgnid == "PLT_CN" && !is.null(findnm("CN", pltassgnflds, returnNULL=TRUE))) {
            pltassgnid <- findnm("CN", pltassgnflds)
          } else {
            stop("pltassgnid is not in pltassgn")
          }
        } else {
          pltassgnid <- pltassgnchk
        }
      }
      if (!is.null(pjoinid)) {
        pjoinidchk <- unlist(sapply(pjoinid, findnm, pltflds, returnNULL=TRUE))
        if (is.null(pjoinidchk)) {
          stop("invalid pjoinid... must be in plot table")
        } else {
          pjoinid <- pjoinidchk
        }
      } else {
        pjoinid <- puniqueid
      }
      if (length(pjoinid) != length(pltassgnid)) {
        pltassgnchk <- unlist(sapply(pjoinidchk, findnm, pltassgnflds, returnNULL=TRUE))
        if (is.null(pltassgnchk)) {
          message("pjoinid must be same number of variables as pltassgnid")
          stop()
        } else {
          pltassgnid <- pltassgnchk
        }
      }
      pltassgnvars <- pltassgnid
      
      ## Check pvars2keep
      if (!is.null(pvars2keep) && length(pvars2keep) > 0) {
        pvars2keep <- pvars2keep[pvars2keep %in% pltassgnflds]
        if (length(pvars2keep) > 0) {
          pltassgnvars <- c(pltassgnvars, pvars2keep)
        }
      }
    } else {
      
      ## Build pfromqry using pltassgn table
      pfromqry <- paste0("\nFROM ", SCHEMA., pltassgnnm, " p")
      
      ## Check pvars2keep
      if (!is.null(pvars2keep) && length(pvars2keep) > 0) {
        pvars2keep <- pvars2keep[!pvars2keep %in% pltassgnflds]
        if (length(pvars2keep) > 0) {
          pltassgnvars <- c(pltassgnvars, pvars2keep)
        }
      }
    }

    ## 4.4. Define select variables (selectpvars)
    ##################################################################################
    pltidvars=selectpvars <- {}
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
    if (!is.null(plotnm)) {
      selectpvars <- c(selectidvars, paste0(plt., puniqueid))
      pltidsid <- pjoinid
     
      ## Append pjoinid
      if (!identical(as.vector(pjoinid), as.vector(puniqueid))) {
        selectpvars <- c(selectpvars, paste0(plt., pjoinid))
        pltidvars <- c(projidvars, pjoinid)
      } 
      
    } else {
      pltidsid <- pltassgnid
      selectpvars <- c(selectidvars, paste0(pltassgn., pltidsid))
    }
    pltidvars <- c(projidvars, pltidsid)
    

    ###################################################################################
    ## 5. Check input parameters
    ###################################################################################
    
    ## 5.1. Check adj
    ##########################################################
    adjlst <- c("samp", "plot", "none")
    adj <- pcheck.varchar(var2check=adj, varnm="adj", gui=gui,
                          checklst=adjlst, caption="adj", multiple=FALSE, stopifnull=TRUE)
    if (adj == "plot" && module == "GB") {
      message("adj='plot' is not typical for GB modules")
    }
    if (adj != "none") {
      pltvars2keep <- c(pltvars2keep, "MACRO_BREAKPOINT_DIA")
    }
    
    ## 5.2. Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
    ###################################################################################
    ACI <- pcheck.logical(popFilter$ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
    if (ACI) {
      pltvars2keep <- c(pltvars2keep, "NF_SAMPLING_STATUS_CD", "NF_PLOT_STATUS_CD")
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
    ## 6. Check estimation unit(s) and auxiliary information in pltassgn.
    ##################################################################################
    
    ## 6.1. Check unitvars (unitvar, unitvar2)
    #######################################################################
    if (!is.null(unitvars)) {
      unitvarchk <- unlist(sapply(unitvars, findnm, pltassgnflds, returnNULL=TRUE))
      if (is.null(unitvarchk)) {
        message("unitvars must be included in dataset")
        return(NULL)
      } else {
        unitvars <- unitvarchk
      }	
      if (any(unitvar == "ONEUNIT")) {
        nbrunits <- 1
        punit.vals <- 1
      }
    } else {
      # if (module != "GB") {
      #   stop("must include unitvar")
      # }
      unitvar <- checknm("ONEUNIT", pflds)
      message("no unitvar specified...  adding a variable named ", unitvar)
      unitvar=unitvars <- "ONEUNIT"
      nbrunits <- 1
      punit.vals <- 1
      pltassgnx$ONEUNIT <- 1
      pltassgnflds <- c(pltassgnflds, "ONEUNIT")
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
        strvar <- findnm(strvar, pltassgnflds, returnNULL=TRUE)
        if (is.null(strvar)) {
          message("strata=TRUE, strvar must be included in dataset")
          return(NULL)
        }	
      }
      pltassgnvars <- unique(c(pltassgnvars, strvar))
      if (adj == "samp") {
        adjbyvars <- c(adjbyvars, strvar)
        strvara. <- ifelse(all(strvar %in% pltassgnflds), pltassgn., plt.)
        
        if (!is.null(strvar)) {
          selectpvars <- c(selectpvars, paste0(strvara., strvar))
        }
      }
    } else {
      
      if (is.null(auxlut)) {
        if (unitvar == "ONEUNIT") {
          auxlut <- data.frame(ONEUNIT = 1)
        } else {
          auxlut <- unique(pltassgnx[, c(unitvar2, unitvar), with=FALSE])
          names(auxlut) <- c(unitvar2, unitvar)
        }
      }

      if (module == "GB") {
        strvar <- "ONESTRAT"
        strata <- TRUE
      
        auxlut$ONESTRAT <- 1
        auxlut$strwt <- 1
        pltassgnx$ONESTRAT <- 1
        pltassgnvars <- c(pltassgnvars, "ONESTRAT")
        pltassgnflds <- c(pltassgnflds, "ONESTRAT")
        selectpvars <- c(selectpvars, "ONESTRAT")
      }
    }
   
    ## 6.3. Check prednames in plot/pltassgn.
    #######################################################################
    if (!is.null(prednames)) {
      prednameschk <- unlist(sapply(prednames, findnm, pltassgnflds, returnNULL=TRUE))
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
    
   
    ## 7. Check pltvars2keep in pltassgn and pltdoms2keep in plt
    #######################################################################
    if (!is.null(pltvars2keep)) {
      if (!is.null(pltvars2keep)) {
        pvars2keepchk <- unlist(sapply(pltvars2keep, findnm, pflds, returnNULL = TRUE))
        if (length(pvars2keepchk) < length(pltvars2keep)) {
          pvars2keepmiss <- pltvars2keep[!pltvars2keep %in% pvars2keepchk]
          pvars2keepmiss <- pvars2keepmiss[pvars2keepmiss != "PSTATUSCD"]
          if (length(pvars2keepmiss) > 0) {
            message("variables are missing from dataset: ", toString(pvars2keepmiss))
          } else {
            pltvars2keep <- pvars2keepchk
          }
          #return(NULL)
        } else {
          pltvars2keep <- pvars2keepchk
        }
      }
    }
    
    ## Get default variables for plot
    if (defaultVars) {
      #pdoms2keep <- DBvars.default()$pltvarlst
      pdoms2keep <- pltdoms
      pdoms2keep <- pdoms2keep[pdoms2keep %in% pltflds]
    } else {
      pdoms2keep <- pltflds
    }
    
    pvars <- unique(c(pltvars2keep, pltdoms))
    pvars <- pvars[pvars %in% pltflds]
    

    ## 8. Write pltassgnx to database 
    ##################################################################################
    if (datindb && !pltaindb && !is.null(dbconn)) {
      if (dsnreadonly) {
        message("consider writing pltassgn to database or including dsnreadonly=TRUE, for more efficient queries...")
      } else {
        index <- NULL
        pltassgnnm <- checknm("pltassgn", dbtablst)
        if (!identical(pltassgnid, pjoinid)) index <- pjoinid
        write2sqlite(pltassgnx[, pltassgnvars, with=FALSE], 
                     dbconn = dbconn, out_name = pltassgnnm, index = index)
        pltaindb <- TRUE
      }
    }

    ##################################################################################
    ## 9. Check popFilters and create pltidsqry
    ##################################################################################
    if (!is.null(pltx))  {
      dbTabs <- dbTables(plot_layer = pltx)
    } else {
      dbTabs <- dbTables(plot_layer = tabs$plt)
    }

    popFilterqry <- 
      getpopFilterqry(popType = popType, 
                      popFilter = popFilter, 
                      plotnm = plotnm,
                      pltassgnnm = pltassgnnm,
                      pltassgnflds = pltassgnflds, 
                      pltflds = pltflds,
                      puniqueid = puniqueid, 
                      pjoinid = pjoinid,
                      pltassgnid = pltassgnid,
                      plt. = plt.,
                      pltassgnvars = pltassgnvars,
                      selectpvars = selectpvars,
                      dbconn = dbconn,
                      schema = schema,
                      datsource = datsource,
                      dbTabs = dbTabs,
                      datindb = datindb,
                      pltaindb = pltaindb,
                      pltassgnx = pltassgnx,
                      pltx = pltx,
                      chkvalues = FALSE,
                      projectid = projectid) 
    if (is.null(popFilterqry)) {
      message("error getting pop filters...")
      return(NULL)
    }
    pltidsqry <- popFilterqry$pltidsqry
    pwhereqry <- popFilterqry$pwhereqry
    pltselectqry <- popFilterqry$pltselectqry
    pltassgnvars <- popFilterqry$pltassgnvars
    #pfromqry <- popFilterqry$pfromqry
    pltafromqry <- popFilterqry$pltafromqry
    states <- popFilterqry$states
    invyrs <- popFilterqry$invyrs
    nonsamp.pfilter <- popFilterqry$nonsamp.pfilter
    iseval <- popFilterqry$iseval
    if (iseval) {
      popevalid <- popFilterqry$popevalid
      ppsanm <- popFilterqry$ppsanm
      POP_PLOT_STRATUM_ASSGN <- popFilterqry$POP_PLOT_STRATUM_ASSGN
      PLOT <- popFilterqry$PLOT
    } 

    ## 10. Build WITH queries and extract plt
    ##################################################################################
    
    ## Build WITH query for returning data tables in database if pltassgn is not in database
    ## Note: if pltassgn is in database, getdatWITHqry = pltidsqry
    if (!pltaindb && datindb && !is.null(plotnm)) {
      getdataWITHqry <- paste0("WITH",
                               "\npltids AS",
                               "\n(SELECT p.", puniqueid, " AS PLT_CN",
                               pfromqry,
                               pwhereqry, ")")
      ## Get plot data (pltx)
      if (!is.null(PLOT)) {
        pltx <- PLOT
      } else {
        ## Build query to query plot data
        pltqry <- paste0("SELECT ", toString(paste0("p.", c(pjoinid, pvars))),
                         pfromqry,
                         pwhereqry)
        
        ## Get plot data from database (to join to pltassgn and filter popFilters)
        pltx <- DBI::dbGetQuery(dbconn, pltqry)
      }
      
      ## Subset pltx to pltassgnx
      pltx <- merge(pltx, pltassgnx[,pltassgnid, with=FALSE], by.x = pjoinid, by.y=pltassgnid)
      
      ## Substitute plot name for pltafromqry and pltidsqry
      pltafromqry <- sub(plotnm, "pltx", pltafromqry, plotnm)
      pltidsqry <- sub(plotnm, "pltx", pltidsqry, plotnm)
      
      plotnm <- "pltx"
      pltflds <- pvars
    } else {
      
      getdataWITHqry <- paste0("WITH",
                               "\npltids AS",
                               "\n(", pltidsqry, ")")
    }
    
    ## 11. Check PLOT_STATUS_CD and generate table with number of plots
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
      pstatuscda. <- ifelse(pstatuscdnm %in% pltflds, plt., pltassgn.)
      plotsampcntqry <- paste0(
        "SELECT ", pstatuscda., pstatuscdnm, ", COUNT(*) AS NBRPLOTS", 
        pltafromqry, 
        pwhereqry, 
        "\nGROUP BY ", pstatuscda., pstatuscdnm,
        "\nORDER BY ", pstatuscda., pstatuscdnm)
      
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
        nfpstatuscda. <- ifelse(nfpstatuscdnm %in% pltassgnflds, pltassgn., plt.)
        nfplotsampcntqry <- paste0(
          "SELECT p.", nfpstatuscdnm, ", COUNT(*) AS NBRPLOTS", 
          pltafromqry, 
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
    ## 12. Get estimation unit(s) (unitvars) values
    ##################################################################################
    chkvalues <- TRUE
    if (!is.null(unitvars) && !all(unitvars == "ONEUNIT")) {

      if (chkvalues) {
        
        ## Check unitvars in plot data
        unitvarsa. <- ifelse(all(unitvars %in% pltassgnflds), pltassgn., plt.)
        unitvarqry <- paste0(
          "SELECT DISTINCT ", toString(paste0(unitvarsa., unitvars)), 
          pltafromqry, 
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
    }

    ######################################################################################
    ## 13. Check unitarea 
    ######################################################################################
    vars2keep=unitareax <- NULL
    if (module == "SA" && "AOI" %in% names(unitarea)) {
      vars2keep <- "AOI"
    }
    
    ## 13.1. Check if unitarea is a data.frame object
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
      
      ## 13.2. Check evalid and filter 
      ###################################################################
      if (is.null(unitareax) && !is.null(chkdbtab(dbtablst, unitarea))) {

        unitindb <- TRUE
        unitarea_layer <- chkdbtab(dbtablst, unitarea)
        unitareaflds <- DBI::dbListFields(dbconn, unitarea_layer)
        unitareajoinqry <- getjoinqry("EVALID", alias1="unitarea.", alias2="plta.")
#        unitareaqry <- paste0("SELECT ", toString(paste0("unitarea.", unitareaflds)),
#                              pltafromqry,
#                              "\n JOIN ", unitarea_layer, " unitarea ", unitareajoinqry,
#                              pwhereqry)
        unitareaqry <- paste0("SELECT *", 
                              "\nFROM ", unitarea_layer)
       
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
                                  "\nWHERE ", uevalidnm, " IN (", toString(popevalid), ")",
                                  "\nORDER BY ", toString(unitvars))
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
        
        if (any(unitvars == "ONEUNIT") && !("ONEUNIT" %in% names(unitareax))) {
          unitareax$ONEUNIT <- 1
          unitareax <- unitareax[ , sum(get(areavar), na.rm = TRUE), by = unitvars]
          setnames(unitareax, "V1", areavar)
        }
        
        unitareax <- unique(unitareax[, c(unitvars, areavar), with=FALSE])	  
        if (any(duplicated(unitareax[, unitvars, with=FALSE]))) {
          message("unitarea is invalid... multiple unitvars exist")
          return(NULL)
        }
      }

      ## 13.3. Check areavar 
      ###################################################################
      areavar <- pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui,
                                checklst=names(unitareax), caption="Area variable?", stopifnull=TRUE)
      
      ## Check areaunits
      areaunits <- pcheck.varchar(var2check=areaunits, varnm="areaunits",
                                  gui=gui, checklst=c("acres", "hectares"), caption="Area units?",
                                  stopifnull=TRUE)
      
      
      ## Aggregate area to ONEUNIT
      if (any(unitvars == "ONEUNIT")) {
        unitareax$ONEUNIT <- 1
      }
      
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
      
      ## 13.4. Check if all values in unitarea are in pltassgn
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
      
      ## 13.5. Sum areavar by unitvars to aggregate any duplicate rows
      ###############################################################################
      unitareax <- unitareax[, sum(.SD, na.rm=TRUE), by=c(unitvars, vars2keep), .SDcols=areavar]
      setnames(unitareax, "V1", areavar)
    }	

    ######################################################################################
    ## 14. Check auxiliary data in auxlut.
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
      ## 14.1. Check if auxlut is a data.frame R object
      auxlutx <- pcheck.table(auxlut, tabnm = auxtabnm, 
                              caption = paste(auxtabnm, " table?"), returnsf=FALSE)
      
      if (is.null(auxlutx) && !is.null(chkdbtab(dbtablst, auxlut))) {
        auxindb <- TRUE
        auxlut_layer <- chkdbtab(dbtablst, auxlut)
        auxlutflds <- DBI::dbListFields(dbconn, auxlut_layer)
        auxlutqry <- paste0("SELECT * \nFROM ", SCHEMA., auxlut_layer)

        
        ## 14.2. Check evalid and filter 
        ###################################################################
        if (!is.null(popevalid)) {
          evalidnm <- findnm("EVALID", auxlutflds, returnNULL = TRUE) 
          if (!is.null(popevalid) && !is.null(evalidnm)) {
            auxlutqry <- paste0(
              auxlutqry, 
              "\nWHERE ", evalidnm, " IN (", toString(popevalid), ")",
              "\nORDER BY ", toString(unitvars, strvar))
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
        } else {
          auxlutx <- pcheck.table(auxlut, conn = dbconn,
                                  tabnm = auxtabnm, caption = paste(auxtabnm, " table?"),
                                  tabqry = auxlutqry, returnsf = FALSE)
          
        }
      }

      ## Aggregate area to ONEUNIT
      if (any(unitvars == "ONEUNIT")) {
        if (is.null(auxlutx)) {
          auxlutx <- data.frame(ONEUNIT = 1)
        } else {
          auxlutx$ONEUNIT <- 1
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
       
        ## 14.4. Check if strvar is in auxlut, if strata=TRUE
        ###################################################################
        strvar <- pcheck.varchar(var2check = strvar, varnm = "strvar", gui=gui,
                                 checklst = names(stratalutx), caption = "strata variable",
                                 warn=paste(strvar, "not in strata table"), stopifnull=TRUE)

        ## 14.5. Check unitlevels for collapsing
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
        
        ## 14.6. Create a table of number of plots by strata and estimation unit
        ###################################################################
        strunitvars <- unique(c(unitvars, strvar))
        if (any(unitvars == "ONEUNIT")) {
          strunitvarsqry <- toString(paste0("1 AS ONEUNIT, ", pltassgn., strvar)) 
          strunitvarsby <- c("ONEUNIT", paste0(pltassgn., strvar))
        } else {
          strunitvarsqry <- toString(paste0(pltassgn., strunitvars))
          strunitvarsby <- paste0(pltassgn., strunitvars)
        }
        P2POINTCNTqry <- paste0(
          "SELECT ", strunitvarsqry, ", COUNT(*) AS NBRPLOTS", 
          pltafromqry, 
          pwhereqry,
          "\nGROUP BY ", toString(strunitvarsby),
          "\nORDER BY ", toString(strunitvarsby))
        if (pltaindb) {      
          P2POINTCNT <- data.table(DBI::dbGetQuery(dbconn, P2POINTCNTqry))
        } else {
          P2POINTCNT <- data.table(sqldf::sqldf(P2POINTCNTqry, connection = NULL))
        }
        setkeyv(P2POINTCNT, strunitvars)
        
        ## 14.7. If nonresp, get Response Homogeneity Groups for WestFest
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
            pltafromqry, 
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
    ## 16. Get plot counts by estimation unit
    ######################################################################################
    
    ## Build select for plot counts
    if (any(unitvars == "ONEUNIT")) {
      pltcnt_grpbyvars <- "ONEUNIT"
      pltcnt_selectqry <- paste0(
        "\nSELECT 1 AS ONEUNIT, COUNT(*) NBRPLOTS")
    } else {
      pltcnt_grpbyvars <- paste0(pltassgn., unitvars)
      pltcnt_selectqry <- paste0(
        "\nSELECT ", toString(pltcnt_grpbyvars), ", COUNT(*) NBRPLOTS")
    }
    
    
    if (!is.null(pstatuscdnm)) {    
      pltcnt_selectqry <- paste0(pltcnt_selectqry, ", ",
                                 "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD == 1 THEN 1 ELSE 0 END) AS FOREST,",
                                 "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD == 2 THEN 1 ELSE 0 END) AS NONFOREST")
    }  
    
    ## Build query for plot counts
    plotunitcntqry <- paste0(pltcnt_selectqry, 
                             pltafromqry, 
                             pwhereqry,
                             "\nGROUP BY ", toString(pltcnt_grpbyvars),
                             "\nORDER BY ", toString(pltcnt_grpbyvars))
    if (pltaindb) {      
      plotunitcnt <- DBI::dbGetQuery(dbconn, plotunitcntqry)
    } else {
      plotunitcnt <- sqldf::sqldf(plotunitcntqry, connection = NULL)
    }
    
    
    ######################################################################################
    ## 15. Query pltassgnx using popfilters and import pltassgnx
    ######################################################################################
    ppsaselectvars <- {}
    pltassgnvars <- pltassgnvars[pltassgnvars %in% pltassgnflds]
    if (length(pltassgnvars) > 0) {
      pltassgnselectvars <- paste0(pltassgn., sQuote(pltassgnvars, FALSE))
    }

    ## Build select for pltassgnx
    pltassgn_selectqry <- paste0("SELECT ",
                                 toString(pltassgnselectvars))
    
    ## Build query for pltassgnx
    pltassgnxqry <- paste0(pltassgn_selectqry, 
                           pltafromqry, 
                           pwhereqry) 

    if (pltaindb) {      
      pltassgnx <- tryCatch(
        DBI::dbGetQuery(dbconn, pltassgnxqry),
        error=function(e) {
          message(e,"\n")
          return(NULL)})
    } else {
      pltassgnx <- tryCatch(
        sqldf::sqldf(pltassgnxqry, connection = NULL),
        error=function(e) {
          message(e,"\n")
          return(NULL)})
    }
    if (is.null(pltassgnx)) {
      message("invalid pltassgn query...")
      message(pltassgnxqry)
      return(NULL)
    } else {
      pltassgnx <- setDT(pltassgnx)
    }
    setkeyv(pltassgnx, pltassgnid)

    ## Subset pltx to plots in pltassgn
    ######################################################################################
    if (!pltaindb) {
      getdataCNs <- pltx[[puniqueid]]
      
      ## Change plot information in plotlst   
      plotlst$tabflds <- pltflds
      plotlst$tabx <- pltx
    }
    
    ## check pdoms2keep  
    pdoms2keep <- unique(c(pltvars2keep, pdoms2keep))
    pdoms2keep <- pdoms2keep[pdoms2keep %in% pltflds]
    
    
    #############################################################################
    ## 18. Build WITH query defining pltids in population
    #############################################################################
    pltidsWITHqry <- paste0(
      "WITH",
      "\npltids AS",
      "\n(", pltidsqry, ")")
    pltidsa. <- "pltids."
    

    #############################################################################
    ## 19. Return data
    #############################################################################
    returnlst <- list(pltassgnx=pltassgnx, pltassgnid=pltassgnid,
                      pltidsWITHqry=pltidsWITHqry, pwhereqry=pwhereqry, pltassgn.=pltassgn.,
                      pltselectqry=pltselectqry, pltafromqry=pltafromqry, 
                      pltx=pltx, plotlst=plotlst, pltidsid = pltidsid,
                      projidvars=projidvars, pltidvars=as.vector(pltidvars),
                      pdoms2keep=pdoms2keep,
                      puniqueid=puniqueid, pjoinid=pjoinid, adjbyvars=adjbyvars,
                      unitvar=unitvar, unitarea=unitareax, unitvar2=unitvar2, areavar=areavar, 
                      areaunits=areaunits, unit.action=unit.action, ACI=ACI, 
                      P2POINTCNT=as.data.frame(P2POINTCNT), unitlevels=unitlevels, 
                      plotsampcnt=as.data.frame(plotsampcnt), plotunitcnt = plotunitcnt,
                      nonsamp.pfilter=nonsamp.pfilter, POP_PLOT_STRATUM_ASSGN=POP_PLOT_STRATUM_ASSGN,
                      states=states, invyrs=invyrs, pltaindb=pltaindb, datindb=datindb, 
                      getdataWITHqry = getdataWITHqry, getdataCNs = getdataCNs, 
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