check.popdataPLT <-
  function(dsn,
           datsource, schema = NULL,
           tabs, tabIDs,
           pltassgn, pltassgnid, pjoinid,
           module, popType, popevalid, adj,
           popFilter, nonsamp.pfilter = NULL,
           unitarea = NULL, areavar, unitvar, unitvar2 = NULL,
           areaunits, unit.action = "keep",
           strata = FALSE, stratalut = NULL, auxlut = NULL,
           strvar = NULL, stratcombine = TRUE, pivot = FALSE,
           strwtvar = "strwt",
           expnwt = FALSE,
           prednames = NULL, predfac = NULL,
           pvars2keep = NULL, pdoms2keep = NULL,
           unitlevels = NULL, defaultVars = FALSE,
           projectid = NULL, 
           dbconn = NULL,
           database_opts = NULL) {

    ###################################################################################
    ## DESCRIPTION: Checks plot data inputs
    ## - Set plot domains to add to cond (pdoms2keep) - STATECD, UNITCD, COUNTYCD,
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
    ## expnwt - if TRUE, extracts plot-level extractions from pop_stratum and
    ##           pop_plot_stratum_assgn tables in database.
    ###################################################################################

    ## Set global variables
    ###############################################################################
    plotsampcnt=nonresplut=pfromqry=pltassgnqry=unitareaqry=auxlutqry=MATCH=
      pwhereqry=pltx=pltassgnx=pstratvars=getdataWITHqry=getdataCNs=
      P2POINTCNT=POP_PLOT_STRATUM_ASSGN=PLOT=pstatuscdnm=expnwts <- NULL

    datindb=pltaindb=stratindb=subcycle99=gui=nullcheck=nonresp <- FALSE
    unitvars <- unique(c(unitvar2, unitvar))
    pltassgnvars <- unique(c(projectid, pltassgnid, unitvars))
    removetext <- "unitarea"
    dsnreadonly <- TRUE
    
  

    ###################################################################################
    ## 1. Define a set of plot-level variables that are necessary to keep for estimation.
    ###################################################################################

    ## Variables to keep in plot table (Note: pvars2keep are variables to keep in pltassgnx)
    pltvars2keep <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "PLOT_STATUS_CD",
                      "PLOT_NONSAMPLE_REASN_CD", "PSTATUSCD", "INTENSITY",
                      "SUBCYCLE")

    ## Include additional pltvars2keep depending on popType
    if (popType %in% c("GRM", "CHNG", "LULC")) {
      pltvars2keep <- unique(c(pltvars2keep, c("PREV_PLT_CN", "REMPER")))
      if (popType == "GRM") {
        grow_typ_cd = mort_typ_cd <- NULL
        pltvars2keep <- unique(c(pltvars2keep, c("GROW_TYP_CD", "MORT_TYP_CD")))
      }
    } else if (popType == "P2VEG") {
      pltvars2keep <- c(pltvars2keep, "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
                      "SAMP_METHOD_CD")
    } else if (popType == "INV") {
      pltvars2keep <- c(pltvars2keep, "INVASIVE_SAMPLING_STATUS_CD", "INVASIVE_SPECIMEN_RULE_CD")
    }

    ###############################################################################
    ## 2. Define a set of plot-level domain variables used for estimation.
    ###############################################################################
    pltdoms <- c("INVYR", "MEASYEAR", "MEASMON", "RDDISTCD", "WATERCD",
                 "ELEV", "ELEV_PUBLIC", "ECOSUBCD", "CONGCD", "DESIGNCD",
                 "P2PANEL", "SUBPANEL",
                 "HUC", "EMAP_HEX", "ALP_ADFORCD", "FVS_VARIANT", "FVS_LOC_CD",
                 "FVS_REGION", "FVS_FOREST", "FVS_DISTRICT", "ROADLESSCD",
                 "NBRCND", "NBRCNDFOR", "CCLIVEPLT")


    ##############################################################################
    ## 3. Check database information
    ##############################################################################
    dbinfo <- pcheck.datsource(dbconn, 
                               datsource = datsource, 
                               dsn = dsn, 
                               database_opts = database_opts)
    if (!is.null(dbinfo)) {
      dbconn <- dbinfo$dbconn
      datsource <- dbinfo$datsource
      dbtablst <- dbinfo$dbtablst
      indb <- dbinfo$indb
      schema <- dbinfo$schema
      SCHEMA. <- dbinfo$SCHEMA.
    }

    ###############################################################################
    ## 4. Check plot-level tables (pltassgn and plot).
    ###############################################################################
    pltassgnx=pltassgnnm=pfromqry <- NULL

    ## 4.1. Check plot table.
    ###########################################################
    if (popType %in% c("CHNG", "GRM")) {
      tabnames <- c("pltu", "plotu", "plot", "plt")
    } else {
      tabnames <- c("plt", "plot")
    }
    plotlst <- popTabchk(tabnames, tabtext = "plt",
                         tabs, tabIDs, dbtablst, dbconn = dbconn, schema = schema)
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
        pltassgnflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = pltassgnnm, upper = TRUE)
      }
    }

    ## 4.3. Check unique identifiers of plot and pltassgn and pjoinid, and build pfromqry
    ###############################################################################
    
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
      if (popType %in% c("CHNG", "GRM")) {
        prev_plt_cnnm <- findnm("PREV_PLT_CN", pltflds, returnNULL = TRUE)
        if (is.null(prev_plt_cnnm)) {
          stop("must include PREV_PLT_CN in plt table")
        }  
      }
          
      if (is.null(pltx)) {
        datindb <- TRUE
        pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
        
        if (popType %in% c("CHNG", "GRM")) {
          pchgjoinqry <- getjoinqry(puniqueid, prev_plt_cnnm, "pplot.", "p.")
          pfromqry <- paste0(pfromqry,
                             "\nJOIN ", SCHEMA., plotnm, " pplot ", pchgjoinqry)
        }
              
      } else {
        
        ## Check if remeasured plots are in pltx
        if (popType %in% c("CHNG", "GRM")) {
          if (!any(pltx[[prev_plt_cnnm]] %in% pltx[[puniqueid]])) {
            stop("must include remeasured plots in pltx")
          }
        }
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

    ## 4.4. Define initial SELECT variables for query (selectpvars) 
    ###############################################################################
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


    ###############################################################################
    ## 5. Check input parameters
    ###############################################################################

    ## Check adj
    adjlst <- c("samp", "plot", "none")
    adj <- pcheck.varchar(var2check = adj, varnm = "adj", gui = gui,
                          checklst = adjlst, caption = "adj", multiple = FALSE, 
                          stopifnull = TRUE)
    if (adj == "plot" && module == "GB") {
      message("adj='plot' is not typical for GB modules")
    }
    if (adj != "none") {
      pltvars2keep <- c(pltvars2keep, "MACRO_BREAKPOINT_DIA")
    }

    ## Check ACI (if ACI=FALSE, need to filter COND_STATUS_CD == 1)
    ACI <- pcheck.logical(popFilter$ACI, varnm="ACI", title="ACI?", first="NO", gui=gui)
    if (ACI) {
      pltvars2keep <- c(pltvars2keep, "NF_SAMPLING_STATUS_CD", "NF_PLOT_STATUS_CD")
    }

    ## Check defaultVars
    defaultVars <- pcheck.logical(defaultVars, varnm="defaultVars",
                                  title="Default variables?", first="NO", gui=gui)

    ## heck unit.action
    unit.actionlst <- c("keep", "remove", "combine")
    unit.action <- pcheck.varchar(var2check = unit.action, varnm = "unit.action", 
                                  checklst = unit.actionlst, 
                                  caption = "unit.action", 
                                  multiple = FALSE, stopifnull = TRUE, gui = gui)



    ## Check strata parameters (if module = 'GB')
    ########################################################
    if (module == "GB") {
      
      ## 5.5. Check strata and other strata parameters.
      strata <- pcheck.logical(strata, varnm = "strata",
                               title = "Post stratify?", first = "YES", 
                               stopifnull = TRUE, gui = gui)
      
      if (strata) {
      
        ## Check pivot
        pivot <- pcheck.logical(pivot, varnm="pivot",
                                title="Pivot stratalut?", first="NO", gui=gui)
        ## Check nonresp
        nonresp <- pcheck.logical(nonresp, varnm="nonresp",
                                  title="Post stratify?", first="YES", gui=gui)
        if (nonresp) {
          pstratvars <- unique(c(pstratvars, c("PLOT_STATUS_CD", "SAMP_METHOD_CD")))
        }
        
        ## Check stratcombine
        stratcombine <- pcheck.logical(stratcombine, 
                                       varnm = "stratcombine",
                                       title = "Combine strata?", 
                                       first = "YES", 
                                       stopifnull = TRUE, gui=gui)
      }
    }
    
    
    ###############################################################################
    ## 6. Check Estimation Unit(s) and auxiliary information in plt/pltassgn.
    ###############################################################################

    ## 6.1. Check unitvars (unitvar, unitvar2)
    #######################################################################
    # Check unitvars defined in the unitvar and unitvar2 (from unit_opts) parameters,
    # to make sure the variable(s) defining Estimation Unit(s) are in either the
    # plt or pltassgn table (pflds).
    # Note: If two unitvars are desired, unitvar2 must be the larger unit
    # (e.g., unitvar = ’ESTN_UNIT’, unitvar2 = ’STATECD’).
    # Note: If unitvar is NULL, a dummy variable will be set as ONEUNIT=1, with
    # unitvar = ‘ONEUNIT’.
    # Add unitvars to selectpvars and pltassgnvars.
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

    
    ## 6.2. Check unitvar in auxlut
    #######################################################################
    if (is.null(auxlut)) {
      if (unitvar == "ONEUNIT") {
        auxlut <- data.frame(ONEUNIT = 1)
      } else {
        auxlut <- unique(pltassgnx[, c(unitvar2, unitvar), with=FALSE])
        names(auxlut) <- c(unitvar2, unitvar)
      }
    } else {
      if (any(grepl("ONEUNIT", unitvars))) {
        unittest <- unitvars[any(grepl("ONEUNIT", unitvars))]
        if (length(unittest) > 1) {
          stop("more than one ONEUNIT variable")
        }
        if (!unittest %in% names(auxlut)) {
          auxlut[, (unittest) := 1]
        }
      }
    }
    
    
    ## 6.2. Check strata variable (strvar) in plot/pltassgn.
    #######################################################################
    # If using post-stratification (strata=TRUE), check to make sure the variable
    # defining the strata is either in the plt or pltassgn table (pflds).
    # Note: only 1 strata variable is allowed.
    # Add strvar to selectpvars and pltassgnvars.
    #######################################################################
    if (module == "GB") {
      
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
        strvar <- "ONESTRAT"
        
        ## if auxlut is a data.frame, add a variable named ONESTRAT with value = 1
        if (is.data.frame(auxlut)) {
          auxlut$ONESTRAT <- 1
          strvar <- "ONESTRAT"
        }
        
        if (is.data.frame(pltassgnx)) {
          
          ## if pltassgnx is a data.frame, add a variable named ONESTRAT with value = 1
          ## and add ONESTRAT to selectpvars
          pltassgnx$ONESTRAT <- 1
          pltassgnvars <- c(pltassgnvars, "ONESTRAT")
          pltassgnflds <- c(pltassgnflds, "ONESTRAT")
          
          selectpvars <- c(selectpvars, paste0(pltassgn., "ONESTRAT"))
        } else {
          
          ## if pltassgnx is not a data.frame, add 1 AS ONESTRAT to selectpvars query 
          selectpvars <- c(selectpvars, paste0("1 AS ONESTRAT"))
        }
        
        strata <- TRUE
      }
    } else {

      ## Check predfac (if module != 'GB')
      #######################################################################
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
      
    
      ## 6.3. Check prednames in plot/pltassgn.
      #######################################################################
      # If using model-assisted or small area modules, check to make sure all 
      # variable in prednames are either in the plt or pltassgn table (pflds). 
      # Add prednames to pltassgnvars.
      #######################################################################
      if (!is.null(prednames)) {
        prednameschk <- findnms(prednames, pltassgnflds)
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
    }
    
    
    ###############################################################################
    ## 7. Check and define all plot variables used for estimation (pvars)
    ###############################################################################
    
    ## 7.1. Check to make sure all pltvars2keep are in pltassgn or plt (pflds).
    if (!is.null(pltvars2keep)) {
      if (!is.null(pltvars2keep)) {
        pvars2keepchk <- findnms(pltvars2keep, pflds)
        if (length(pvars2keepchk) < length(pltvars2keep)) {
          pvars2keepmiss <- pltvars2keep[!pltvars2keep %in% pvars2keepchk]
          pvars2keepmiss <- pvars2keepmiss[pvars2keepmiss != "PSTATUSCD"]
          if (length(pvars2keepmiss) > 0) {
            message("variables not included in dataset: ", toString(pvars2keepmiss))
          } else {
            pltvars2keep <- pvars2keepchk
          }
          #return(NULL)
        } else {
          pltvars2keep <- pvars2keepchk
        }
      }
    }

    ## 7.2 Check to make sure all pdoms2keep are in plt (pltflds).
    if (defaultVars) {
      #pdoms2keep <- DBvars.default()$pltvarlst
      pdoms2keep <- pltdoms
      pdoms2keep <- pdoms2keep[pdoms2keep %in% pltflds]
    } else {
      pdoms2keep <- pltflds
    }

    ## 7.3 Set default variables for plot (pvars).
    pvars <- unique(c(pltvars2keep, pltdoms))
    pvars <- pvars[pvars %in% pltflds]


    ## 8. Write pltassgnx to database
    #######################################################################
    if (datindb && !pltaindb && !is.null(dbconn)) {
      if (dsnreadonly) {
        #message("database is readonly")
        message("")
        #message("consider writing pltassgn to database or including dsnreadonly=TRUE, for more efficient queries...")
      } else {
        index <- NULL
        pltassgnnm <- checknm("pltassgn", dbtablst)
        if (!identical(pltassgnid, pjoinid)) index <- pjoinid
        write2sqlite(pltassgnx[, pltassgnvars, with=FALSE],
                     dbconn = dbconn, out_name = pltassgnnm, index = index)
        pltaindb <- TRUE
      }
    }

    # Note: At this point, the following are objects defined for plot-level data (plot / pltassgn).
    # -	pltassgnx – R data.table object, if pltassgn is an R object.
    # -	pltaindb – Logical. If TRUE, pltassgn is in database.
    # -	pltassgnnm – String. Name of pltassgn table (in database or R object).
    # -	pltassgnflds – String vector. Field names in pltassgn.
    # -	pltx – R data.table object, if plt is an R object.
    # -	datindb – Logical. If TRUE, plt (assuming all data input tables) are in database.
    # -	plotnm – String. Name of plot table (in database or R object).
    # -	pltflds – String vector. Field names in plt.
    # -	pflds – String vector. Field names in both pltassgn and plt.
    # - pltassgnvars – String vector. Plot assignment variables (e.g., Estimation Unit, Strata).
    # -	pltdoms – String vector. Set of plot-level domain variables used for estimation.


    ###############################################################################
    ## 9.	Check popFilters and create a sql WITH query to identify plots in the
    ## given population (pwithqry).
    ## The main objective of this function is to generate a query to use as a 
    ## sql WITH query (pwithqry) that identifies the plot CNs that will be used 
    ## for estimation. The pwithqry uses the FROM statement (pfromqry) previously 
    ## defined and adds a WHERE statement with filters defined in the popFilters 
    ## parameter (e.g., evalid, evalCur, measCur, INTENSITY). If evalid or evalCur 
    ## is defined in popFilters, the POP_PLOT_STRATUM_ASSGN table is appended to 
    ## the pfromqry to filter for EVALID. This function also checks the nonsamp.pfilter 
    ## and includes it in the WHERE statement.
    ###############################################################################
    if (!is.null(pltx))  {
      dbTabs <- dbTables(plot_layer = pltx)
    } else {
      dbTabs <- dbTables(plot_layer = plotnm)
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
    pltidsqry <- popFilterqry$pltidsqry           ## query defining plots in population
    pwhereqry <- popFilterqry$pwhereqry           ## WHERE statement in pltidsqry
    pltselectqry <- popFilterqry$pltselectqry     ## SELECT statement in pltidsqry
    pltassgnvars <- popFilterqry$pltassgnvars     ## variables in plot assignment table
    pfromqry <- popFilterqry$pfromqry             ## FROM statement, if pltaindb = FALSE
    pltafromqry <- popFilterqry$pltafromqry       ## FROM statement, including plot assignment table
    states <- popFilterqry$states                 ## states in population data
    invyrs <- popFilterqry$invyrs                 ## inventory years in population data
    nonsamp.pfilter <- popFilterqry$nonsamp.pfilter   ## filter for nonsampled plots
    iseval <- popFilterqry$iseval                     ## if using an FIA evaluation
    if (iseval) {
      popevalid <- popFilterqry$popevalid  ## the population data EVALID based on popType
      ppsanm <- popFilterqry$ppsanm        ## the name of the pop_plot_stratum_assgn (ppsa) table
      POP_PLOT_STRATUM_ASSGN <- popFilterqry$POP_PLOT_STRATUM_ASSGN  ## dataframe, if returndata = TRUE
      PLOT <- popFilterqry$PLOT  ## dataframe, if returndata = TRUE
    }

    ###############################################################################
    ## 10. Build WITH query for returning data tables in a database (getdataWITHqry).
    ##  If pltassgn is not in the database (pltaindb = FALSE) and all other tables 
    ##  are in the database, a new query is built using the FROM and WHERE statements 
    ##  returned from getpopFilterqry. Here, we also extract the plot table 
    ##  from the database (pltx) and join to the pltassgn table so that all plot 
    ##  data are in R memory.
    ##  If pltassgn is in the database along with all other tables, the pltidsqry
    ##  returned from getpopFilterqr is used.
    ###############################################################################

    ## Build WITH query for returning data tables in database if pltassgn is not in database
    ## Note: if pltassgn is in database, getdataWITHqry = pltidsqry
    if (!pltaindb && datindb && !is.null(plotnm)) {
   
      ## Get plot data (pltx)
      if (!is.null(PLOT)) {
        pltx <- PLOT
      } else {
        pselectqry <- paste0("SELECT ", toString(paste0("p.", c(pjoinid, pvars))))
        
        ## Build query to query plot data
        pltqry <- paste0(pselectqry,
                         pfromqry,
                         pwhereqry)
        
        if (popType %in% c("CHNG", "GRM")) {
          pchngselectqry <- paste0("\nSELECT ", toString(paste0("pplot.", c(pjoinid, pvars))))
          
          pltqry <- paste0(pltqry,
                           "\nUNION",
                           pchngselectqry,
                           pfromqry,
                           pwhereqry)
        }

        ## Get plot data from database (to join to pltassgn and filter popFilters)
        pltx <- DBI::dbGetQuery(dbconn, pltqry)
      }

      ## Subset pltx to pltassgnx
      if (popType %in% c("CHNG", "GRM")) {
        if (length(pjoinid) == 1) {
          pltx1 <- merge(pltx, pltassgnx[,pltassgnid, with=FALSE], by.x=pjoinid, by.y=pltassgnid)
          pltx2 <- pltx[pltx[[puniqueid]] %in% pltx1[[prev_plt_cnnm]],]
        } else {
          pltx <- merge(pltx, pltassgnx[,pltassgnid, with=FALSE], by.x=pjoinid, by.y=pltassgnid)
        }
      } else {
        pltx <- merge(pltx, pltassgnx[,pltassgnid, with=FALSE], by.x=pjoinid, by.y=pltassgnid)
      }

      ## Substitute plot name for pltafromqry and pltidsqry
      pltafromqry <- sub(plotnm, "pltx", pltafromqry)
      pltidsqry <- sub(plotnm, "pltx", pltidsqry)

      plotnm <- "pltx"
      pltflds <- pvars
      
      ## Build getdataWITH query to subset other data 
      if (popType %in% c("CHNG", "GRM")) {
        getdataWITHqry <- paste0("WITH",
                                 "\npltids AS",
                                 "\n(SELECT p.", puniqueid,
                                 pfromqry,
                                 pwhereqry, 
                                 "\nUNION",
                                 "\nSELECT pplot.", puniqueid,
                                 pfromqry, ")") 
          
      } else {
        getdataWITHqry <- paste0("WITH",
                                 "\npltids AS",
                                 "\n(SELECT p.", puniqueid,
                                 pfromqry,
                                 pwhereqry, ")")
      }
    } else {
      getdataWITHqry <- paste0("WITH",
                               "\npltids AS",
                               "\n(", pltidsqry, ")")
    }

    
    ###############################################################################
    ## 11. Check PLOT_STATUS_CD and NF_PLOT_STATUS_CD.
    ###############################################################################
    
    ## 11.1 Check PLOT_STATUS_CD and generate table with number of plots
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
      } else {
        names(plotsampcnt) <- toupper(names(plotsampcnt))
      }

      ## create data frame of plot counts
      plotsampcnt <-
        cbind(PLOT_STATUS_NM = ref_plot_status_cd[match(plotsampcnt$PLOT_STATUS_CD,
                                                        ref_plot_status_cd$VALUE), "MEANING"], plotsampcnt)
    }

    ## 11.2. If ACI, check NF_PLOT_STATUS_CD and generate table with number of plots
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
        if (is.null(nfplotsampcnt)) {
          message("invalid plotsampcnt query")
          message(nfplotsampcntqry)
        } else {
          names(nfplotsampcnt) <- toupper(names(nfplotsampcnt))
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
    
 
    ## 12. Check grm_types if popType == 'GRM'
    ########################################################################
    ## Note: GROW_TYP_CD and MORT_TYP_CD are variables in the FIADB plot table
    ## that indicated how the volume growth/mortality is estimated. 
    
    ## GROW_TYP_CD
    ## 1:Current annual - an estimate of the amount of volume that was added to a 
    ##    tree in the year before the tree was sampled, based on the measured 
    ##    diameter increment recorded when the tree was sampled or on a modeled
    ##    diameter for the previous year.
    ## 2:Periodic annual - an estimate of the average annual change in volume
    ##    occurring between two measurements, usually current and previous inventory,
    ##    where the same plot is evaluated twice (the increase in volume between 
    ##    inventories divided by the number of years between each inventor (REMPER)).
    ## MORT_TYP_CD
    ## 1:Current annual - an estimate of the volume of trees dying in the year before
    ##    the plot was measured, based on the year of death or on a modeled estimate.
    ## 2:Periodic annual - an estimate of the average annual volume of trees dying
    ##    between two measurements, usually the current inventory and previous inventory,
    ##    where the same plot is evaluated twice (the loss of volume between inventories
    ##    divided by the number of years between each inventory (REMPER)).
    
    if (popType == "GRM") {
      grmvars <- c("GROW_TYP_CD", "MORT_TYP_CD")
      grmvarschk <- unlist(sapply(grmvars, findnm, pflds, returnNULL=TRUE))
      
      if (!is.null(grmvarschk)) {
        
        ## Build query for determining grow_typ_cd and mort_typ_cd
        grmtypeqry <- paste0(
          "SELECT DISTINCT ", toString(paste0("p.", grmvarschk)),
          pltafromqry,
          pwhereqry)
        
        if (pltaindb) {
          grmtypedf <- tryCatch(
            DBI::dbGetQuery(dbconn, grmtypeqry),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        } else {
          grmtypedf <- tryCatch(
            sqldf::sqldf(grmtypeqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        
        if (is.null(grmtypedf)) {
          message("invalid grmtypr query")
          message(grmtypeqry)
        } else {
          names(grmtypedf) <- toupper(names(grmtypedf))
        }
        
        if ("GROW_TYP_CD" %in% names(grmtypedf)) {
          grow_typ_cd <- grmtypedf[["GROW_TYP_CD"]][!is.na(grmtypedf[["GROW_TYP_CD"]])]
        }
        if ("MORT_TYP_CD" %in% names(grmtypedf)) {
          mort_typ_cd <- grmtypedf[["MORT_TYP_CD"]][!is.na(grmtypedf[["MORT_TYP_CD"]])]
        }
      }
    }

    
    ###############################################################################
    ## 13. Get estimation unit(s) (unitvars) values in plot data
    ###############################################################################
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
            DBI::dbGetQuery(dbconn, unitvarqry),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        } else {
          unitvartab <- tryCatch(
            sqldf::sqldf(unitvarqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        
        if (is.null(unitvartab) || nrow(unitvartab) == 0) {
          message("unitvar check", " is invalid...")
          message(unitvarqry)
          stop()
        } else {
          unitvartab <- setDT(unitvartab)
          names(unitvartab) <- toupper(names(unitvartab))
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

    
    ###############################################################################
    ## 14. Check unitarea
    ###############################################################################
    vars2keep=unitareax <- NULL
    if (module == "SA" && "AOI" %in% names(unitarea)) {
      vars2keep <- "AOI"
    }

    ## 14.1. Check if unitarea is a data.frame object 
    ###################################################################
    # Check to make sure the unitvars in the unitarea table coincide with the unitvars.
    # The unitarea is defined by the unitarea parameter and can be an R object data.frame,
    # a full path character string to a *.csv file, or a table in a database, defined by
    # the dsn parameter. The unitarea can also be a number if there is only one
    # Estimation Unit in the population.
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

      ## 14.2. Check for EVALID and filter if needed
      ###################################################################
      # If unitarea is a character and is in the database (i.e., POP_ESTN_UNIT)
      # build a query to extract from the database based on popevalid.
      if (is.null(unitareax) && !is.null(chkdbtab(dbtablst, unitarea))) {

        unitarea_layer <- chkdbtab(dbtablst, unitarea)
        unitareaflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = unitarea_layer, upper = TRUE)
        unitareajoinqry <- getjoinqry("EVALID", alias1="unitarea.", alias2="plta.")
        unitareaqry <- paste0("SELECT *",
                              "\nFROM ", SCHEMA., unitarea_layer)

        if (!is.null(popevalid)) {
          uevalidnm <- findnm("EVALID", unitareaflds, returnNULL = TRUE)
          
          ## Check popevalid values in database
          uevalidqry <- paste0(
            "SELECT DISTINCT ", uevalidnm,
            "\nFROM ", SCHEMA., unitarea_layer,
            "\nORDER BY ", uevalidnm)
          
          if (!is.data.frame(unitarea)) {
            uevalidvals <- tryCatch(
              DBI::dbGetQuery(dbconn, uevalidqry)[[1]],
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {
            uevalidvals <- tryCatch(
              sqldf::sqldf(uevalidqry, connection = NULL)[[1]],
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(uevalidvals)) {
            message("there was an error when retrieving estimation unit values")
            message(uevalidqry)
            stop()
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
        ## Get unitareax from database based on query
        unitareax <- pcheck.table(unitarea, conn = dbconn, schema = schema,
                                  tabnm = "unitarea", caption = "unitarea?",
                                  nullcheck = nullcheck, tabqry = unitareaqry, 
                                  returnsf = FALSE)

        if (is.null(unitareax)) {
          message("invalid unitareax")
          return(NULL)
        } else {
          names(unitareax) <- toupper(names(unitareax))
        }

        ## If only 1 estimation unit, add a column named ONEUNIT and aggregate acres
        if (any(unitvars == "ONEUNIT") && !("ONEUNIT" %in% names(unitareax))) {
          unitareax$ONEUNIT <- 1
          unitareax <- unitareax[ , sum(get(areavar), na.rm = TRUE), by = unitvars]
          setnames(unitareax, "V1", areavar)
        }

        ## check for duplicate estimation unit values
        unitareax <- unique(unitareax[, c(unitvars, areavar), with=FALSE])
        if (any(duplicated(unitareax[, unitvars, with=FALSE]))) {
          message("unitarea is invalid... multiple unitvars exist")
          return(NULL)
        }

      } else if (!is.null(unitareax) && is.data.frame(unitareax)) {
        
        ## If unitarea is a data.frame, also check for EVALID and 
        ## use popevalid to query. 
        unitareaflds <- names(unitareax)
        
        if (!is.null(popevalid)) {
          evalidnm <- findnm("EVALID", unitareaflds, returnNULL = TRUE)
          
          if (!is.null(evalidnm)) {
            unitareaqry <- paste0(
              "SELECT *",
              "\nFROM unitareax",
              "\nWHERE ", evalidnm, " IN (", toString(popevalid), ")")
            
            unitareax <- tryCatch(
              sqldf::sqldf(unitareaqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
            
            if (is.null(unitareax)) {
              stop("Evalid is invalid in unitarea table: ", toString(popevalid))
            } else {
              unitareax <- setDT(unitareax)
            }
          }
        }
      }
      

      ## 14.3. Check areavar
      ###################################################################
      areavar <- pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui,
                                checklst=names(unitareax), 
                                caption="Area variable?", stopifnull=TRUE)

      ## Check areaunits
      areaunits <- pcheck.varchar(var2check=areaunits, varnm="areaunits",
                                  gui=gui, checklst=c("acres", "hectares"), 
                                  caption="Area units?", stopifnull=TRUE)


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

      ## 14.4. Check if all values in unitarea are in pltassgn
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

      ## 14.5. Sum areavar by unitvars to aggregate any duplicate rows
      ###############################################################################
      unitareax <- unitareax[, sum(.SD, na.rm=TRUE), by=c(unitvars, vars2keep), .SDcols=areavar]
      setnames(unitareax, "V1", areavar)
    }


    ###############################################################################
    ## 15. Check auxiliary data in auxlut.
    ###############################################################################
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
      
      ## 15.1. Check if auxlut is a data.frame R object
      ###################################################################
      # The auxlut is defined by the auxlut parameter and can be an R object data.frame,
      # a full path character string to a *.csv file, or a table in a database, defined by
      # the dsn parameter.
      auxlutx <- pcheck.table(auxlut, tabnm = auxtabnm,
                              caption = paste(auxtabnm, " table?"), returnsf=FALSE)

      if (is.null(auxlutx) && !is.null(chkdbtab(dbtablst, auxlut))) {
        auxindb <- TRUE
        auxlut_layer <- chkdbtab(dbtablst, auxlut)
        auxlutflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = auxlut_layer, upper = TRUE)
        auxlutqry <- paste0("SELECT * \nFROM ", SCHEMA., auxlut_layer)


        ## 15.2. Check for EVALID and filter if needed
        ###################################################################
        # If unitarea is a character and is in the database (i.e., POP_ESTN_UNIT)
        # build a query to extract from the database based on popevalid.
        if (!is.null(popevalid)) {
          evalidnm <- findnm("EVALID", auxlutflds, returnNULL = TRUE)
          if (!is.null(popevalid) && !is.null(evalidnm)) {
            auxlutqry <- paste0(
              auxlutqry,
              "\nWHERE ", evalidnm, " IN (", toString(popevalid), ")",
              "\nORDER BY ", toString(unitvars, strvar))
          }
          auxlutx <- pcheck.table(auxlut, conn = dbconn, schema = schema,
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
          } else {
            names(auxlutx) <- toupper(names(auxlutx))
          }
        } else {
          
          auxlutx <- pcheck.table(auxlut, conn = dbconn,
                                  tabnm = auxtabnm, caption = paste(auxtabnm, " table?"),
                                  tabqry = auxlutqry, returnsf = FALSE)
          if (is.null(auxlutx) || nrow(auxlutx) == 0) {
            stop("invalid auxlut")
            if (!is.null(auxlutqry)) {
              message(auxlutqry)
            }
          } else {
            names(auxlutx) <- toupper(names(auxlutx))
          }
        }
      } else if (!is.null(auxlutx) && is.data.frame(auxlutx)) {
        
        
        ## If auxlut is a data.frame, also check for EVALID and 
        ## use popevalid to query. 
        auxlutflds <- names(auxlutx)
        
        if (!is.null(popevalid)) {
          evalidnm <- findnm("EVALID", auxlutflds, returnNULL = TRUE)
          
          if (!is.null(evalidnm)) {
            auxlutqry <- paste0(
              "SELECT *",
              "\nFROM auxlutx",
              "\nWHERE ", evalidnm, " IN (", toString(popevalid), ")")
            
            auxlutx <- tryCatch(
              sqldf::sqldf(auxlutqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
            
            if (is.null(auxlutx)) {
              stop("Evalid is invalid in auxiliary table: ", toString(popevalid))
            } else {
              auxlutx <- setDT(auxlutx)
            }
          }
        }
      }

      # ## Aggregate area to ONEUNIT
      # if (any(unitvars == "ONEUNIT")) {
      #   if (is.null(auxlutx)) {
      #     auxlutx <- data.frame(ONEUNIT = 1)
      #   } else {
      #     auxlutx$ONEUNIT <- 1
      #   }
      # }
      # 
      # ## If no strata, add a column named ONESTRAT with strwt = 1
      # stratvarlst <- names(auxlutx)
      # if (!strata) {
      #   auxlutx$ONESTRAT <- 1
      #   strvar <- "ONESTRAT"
      #   stratvarlst <- c(stratvarlst, "ONESTRAT")
      # }
      
      
      ## 15.3. Check unitlevels for collapsing
      ###################################################################
      ## Collapsing is based on unit.action. If unit.action = 'combine', 
      ## the estimation units are combined based on the order in auxlut. 
      if (!is.null(unitlevels)) {
        unitvals <- unique(auxlutx[[unitvar]])
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
      

      ## 15.4. Pivot auxlut table based on strwtvar (from wide to long)
      ###################################################################
      if (strata) {
        if (pivot) {
          stratalutx <- strat.pivot(auxlutx, unitvars = unitvars,
                                    strvar = strvar, strwtvar = strwtvar)
        } else {
          stratalutx <- auxlutx
        }
        
        ## 15.4. Check if strvar is in auxlut, if strata=TRUE
        ###################################################################
        strvar <- pcheck.varchar(var2check = strvar, varnm = "strvar", gui=gui,
                                 checklst = names(auxlutx), caption = "strata variable",
                                 warn=paste(strvar, "not in strata table"), stopifnull=TRUE)
      }
      
      
      ## 15.6. Create a table of number of plots by strata and estimation unit
      ###################################################################
      strunitvars <- unique(c(unitvars, strvar))
      
      if (any(unitvars == "ONEUNIT")) {
        strunitvarsqry <- paste0("1 AS ONEUNIT")
        strunitvarsby <- "ONEUNIT"
      } else {
        strunitvarsqry <- paste0(toString(paste0(pltassgn., unitvars)))
        strunitvarsby <- paste0(pltassgn., unitvars)
      }
        
      if (strata) {
        if (strvar == "ONESTRAT") {
          strunitvarsqry <- paste0(strunitvarsqry, ", 1 AS ONESTRAT")
        } else {  
          strunitvarsqry <- paste0(strunitvarsqry, ", ", toString(paste0(pltassgn., strvar)))
          strunitvarsby <- c(strunitvarsby, paste0(pltassgn., strvar))
        }
      }
      
      P2POINTCNTqry <- paste0(
        "SELECT ", strunitvarsqry, ", COUNT(*) AS NBRPLOTS",
        pltafromqry,
        pwhereqry,
        "\nGROUP BY ", toString(strunitvarsby),
        "\nORDER BY ", toString(strunitvarsby))
            
      if (pltaindb) {
        P2POINTCNT <- tryCatch(
          DBI::dbGetQuery(dbconn, P2POINTCNTqry),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      } else {
        P2POINTCNT <- tryCatch(
          sqldf::sqldf(P2POINTCNTqry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(P2POINTCNT)) {
        message("invalid query for P2 point counts")
        message(P2POINTCNTqry) 
      } else {
        names(P2POINTCNT) <- toupper(names(P2POINTCNT))
        P2POINTCNT <- setDT(P2POINTCNT)
        setkeyv(P2POINTCNT, strunitvars)
      }

      
      ## 15.7. If nonresp, get Response Homogeneity Groups for WestFest
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
          pltnr <- tryCatch(
            DBI::dbGetQuery(dbconn, pltnrqry),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        } else {
          pltnr <- tryCatch(
            sqldf::sqldf(pltnrqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(pltnr)) {
          message("invalid query for nonresponse plot counts")
          message(pltnrqry) 
        } else {
          names(pltnr) <- toupper(names(pltnr))
          pltnr <- setDT(pltnr)
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

    
    ###############################################################################
    ## 16. Get plot counts by estimation unit (plotunitcnt)
    ###############################################################################
    
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
      pltcnt_selectqry <- paste0(
        pltcnt_selectqry, ", ",
          "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD = 1 THEN 1 ELSE 0 END) AS FOREST,",
          "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD = 2 THEN 1 ELSE 0 END) AS NONFOREST")
    }

    ## Build query for plot counts
    plotunitcntqry <- paste0(
      pltcnt_selectqry,
      pltafromqry,
      pwhereqry,
      "\nGROUP BY ", toString(pltcnt_grpbyvars),
      "\nORDER BY ", toString(pltcnt_grpbyvars))
    
    if (pltaindb) {
      plotunitcnt <- tryCatch(
        DBI::dbGetQuery(dbconn, plotunitcntqry),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    } else {
      plotunitcnt <- tryCatch(
        sqldf::sqldf(plotunitcntqry, connection = NULL),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(plotunitcnt)) {
      message("invalid query for estimation unit plot counts")
      message(plotunitcntqry) 
    } else {
      names(plotunitcnt) <- toupper(names(plotunitcnt))
      plotunitcnt <- setDT(plotunitcnt)
    }
    

    ###############################################################################
    ## 17. Get plot-level expansion factors from database
    ###############################################################################
    
    if (expnwt) {
      ppsanm <- findnm("pop_plot_stratum_assgn", dbtablst, returnNULL = TRUE)
      pop_stratumnm <- findnm("pop_stratum", dbtablst, returnNULL = TRUE)

      if (!is.null(ppsanm) && !is.null(pop_stratumnm)) {
        expnwtqry <- paste0(
          " SELECT plta.plt_cn, plta.statecd, plta.unitcd, plta.countycd, plta.plot, expns",
          "\n FROM ", SCHEMA., ppsanm, " plta",
          "\n JOIN ", SCHEMA., pop_stratumnm, " pop ON(pop.CN = plta.STRATUM_CN)",
          pwhereqry)

        if (pltaindb) {
          expnwts <- tryCatch(
            DBI::dbGetQuery(dbconn, expnwtqry),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        } else {
          expnwts <- tryCatch(
            sqldf::sqldf(expnwtqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(expnwts)) {
          message("invalid query for expansion factors")
          message(expnwtqry) 
        } else {
          names(expnwts) <- toupper(names(expnwts))
          #expnwts <- setDT(expnwts)
        }
      }
    }

    ###############################################################################
    ## 18. Build query for pltassgnx and extract from database if pltaindb = TRUE. 
    ###############################################################################
    ppsaselectvars <- {}
    pltassgnvars <- pltassgnvars[pltassgnvars %in% pltassgnflds]
    pltassgnselectvars <- paste0(pltassgn., pltassgnvars)
    

    ## 18.1. Build query for pltassgnx
    ###################################################################
    pltassgn_selectqry <- paste0("SELECT ",
                                 toString(pltassgnselectvars))
    if (pltaindb && strvar == "ONESTRAT") {
      pltassgn_selectqry <- paste0(pltassgn_selectqry, ", 1 AS ONESTRAT")
    }

    pltassgnxqry <- paste0(pltassgn_selectqry,
                           pltafromqry,
                           pwhereqry)

    ## 18.2. Extract pltassgnx if pltassgnx is not in database
    ###################################################################
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
      #names(pltassgnx) <- toupper(names(pltassgnx))
      pltassgnx <- setDT(pltassgnx)
      setkeyv(pltassgnx, pltassgnid)
    }
    

    ## 18.3. Get unique identifiers of plots in pltassgnx (getdataCNs)
    ###############################################################################
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
    ## 19. Build WITH query defining pltids in population
    #############################################################################
    pltidsWITHqry <- paste0(
      "WITH",
      "\npltids AS",
      "\n(", pltidsqry, ")")
    pltidsa. <- "pltids."


    #############################################################################
    ## 20. Return data
    #############################################################################
    returnlst <- list(
      pltassgnx = pltassgnx,  ## includes unique plot identifier and auxiliary plot assignments
      pltassgnid = pltassgnid, ## name of unique identifier in pltassgnx
      pltidsWITHqry = pltidsWITHqry, ## sql WITH query identifying plots in population
      pwhereqry = pwhereqry, ## WHERE statement in pltidsWITHqry
      pltassgn. = pltassgn., ## alias prefix for pltassgnx in pltidsWITHqry
      pltselectqry = pltselectqry, ## SELECT statement in pltidsqry
      pltafromqry = pltafromqry, ## FROM statement, including plot assignment table
      plotlst = plotlst, ## list of plot information, including name, id, fields, and data.frame, if returndata=TRUE
      pltidsid = pltidsid,  ## name of unique identifier in pltidsWITHqry
      projidvars = projidvars, ## select variables in pltidsqry, defining project
      pltidvars = as.vector(pltidvars),  ## select variables in pltidsqry
      pdoms2keep = pdoms2keep,  ## plot domain variables to keep
      puniqueid = puniqueid, ## name of unique indentifier in plot table
      pjoinid = pjoinid, ## name of unique identifier to join plot table with plot assignment table
      adjbyvars = adjbyvars,  ## group by variables for adjusting nonsample plots
      unitarea = unitareax, ## data.frame defining area by estimation unit
      unitvar = unitvar,  ## variable defining the estimation unit
      unitvar2 = unitvar2,  ## variable defining a second (broader level) estimation unit
      areavar = areavar, ## variable in unitarea data.frame defining area of estimation unit
      areaunits = areaunits,  ## units of area in unitarea data.frame
      unit.action = unit.action, ## what to do with estimation units that have no data
      unitlevels = unitlevels, ## if factor levels are defined in stratalut
      ACI = ACI, ## if all condition inventory data are included
      P2POINTCNT = as.data.frame(P2POINTCNT),  ## data.frame with number of plots by estimation unit and stratum
      plotsampcnt = as.data.frame(plotsampcnt), ## data.frame with number of plots by PLOT_STATUS_CD
      plotunitcnt = plotunitcnt,  ## plot counts by estimation unit(s)
      nonsamp.pfilter = nonsamp.pfilter, ## filter to remove nonsampled plots
      POP_PLOT_STRATUM_ASSGN = POP_PLOT_STRATUM_ASSGN, ## if using FIA evaluation, name of the ppsa table, or dataframe, if returndata=TRUE
      states = states, ## a vector of state names included in population dataset
      invyrs = invyrs, ## a list of inventory years, by state, included in population dataset
      pltaindb = pltaindb, ## logical, if plot assignment table is in database (or is a data.frame)
      datindb = datindb, ## logical, if data for estimation are in database (or included as data.frames)
      getdataWITHqry = getdataWITHqry, ## WITH query to use to get data tables (if returndata or savedata)
      getdataCNs = getdataCNs, ## a vector of CN values to subset plots to within population
      dbconn = dbconn, ## an open database connection, if included
      datsource = datsource,  ## input datasource
      popevalid = popevalid  ## FIA Evaluation Identifier
      )

    if (!is.null(dbconn)) {
      returnlst$schema <- schema ## schema in database where tables reside
      returnlst$dbtablst <- dbtablst ## list of tables in database
    }

    if (module == "GB") {
      returnlst$strata <- TRUE  ## if estimation includes post-stratification
      returnlst$stratalut <- stratalutx  ## data.frame with strata pixel counts
      returnlst$strvar <- strvar  ## name of variable defining strata in auxlut
      if (strata) {
        returnlst$stratcombine <- stratcombine ## what to do with stratum if not enough plots in stratum class
        returnlst$nonresp <- nonresp  ## logical, if nonresp strata methods are used (RHG)
      }
      if (nonresp) {
        returnlst$RHGlut <- RHGlut  ## Response homogeneity groups (RHG) look up table
        returnlst$nonresplut <- nonresplut
      }
    }
    if (module %in% c("MA", "SA")) {
      returnlst$auxlut <- auxlutx  ## table with summarized auxiliary data
      returnlst$prednames <- prednames  ## name of predictors used in model-assisted / small area estimators
      returnlst$predfac <- predfac  ## name of categorical predictors
    }
    if (ACI) {
      returnlst$nfplotsampcnt <- nfplotsampcnt  ## data.frame with number of plots by NF_PLOT_STATUS_CD
    }
    
    if (popType == "GRM") {
      returnlst$grow_typ_cd <- grow_typ_cd
      returnlst$mort_typ_cd <- mort_typ_cd
    }

    if (!is.null(expnwt)) {
      returnlst$expnwts <- expnwts
    }
   

    return(returnlst)
  }
