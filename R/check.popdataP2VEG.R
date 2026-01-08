check.popdataP2VEG <- 
  function(tabs, tabIDs, popType, 
           datindb, pltaindb, popdatindb,
           pltidsWITHqry,
           pltidsid, projidvars = NULL,
           pltidvars,
           pdoms2keep = NULL, 
           pltidsadjindb = FALSE, 
           defaultVars = TRUE,
           pltassgnid, 
           pltassgnx, 
           POP_PLOT_STRATUM_ASSGN, 
           adj, ACI, plotlst, 
           condid = "CONDID", 
           areawt = "CONDPROP_UNADJ", areawt2 = NULL,
           MICRO_BREAKPOINT_DIA = 5, 
           MACRO_BREAKPOINT_DIA = NULL, 
           unitvars = NULL, 
           strunitvars = NULL, 
           nonsamp.cfilter = NULL, 
           cvars2keep = NULL, 
           dbconn = NULL, schema = NULL, 
           datsource = NULL, dbtablst = NULL,
           getdataWITHqry = NULL,
           getdataCNs = NULL,
           returndata = FALSE, 
           savedata = FALSE, 
           outlst = NULL,
           gui = FALSE){

  ##############################################################################
  ## DESCRIPTION: Checks data inputs for AREA/VOL estimation
  ## 1. Define variables necessary for estimation:
  ## - cvars2keep = 'PROP_BASIS'
  ## 2. Check if data are in a database (datindb) and if dbconn is valid.
  ## 3. Get table names used in estimation from tabs.
  ## - PLOT; COND; SUBPLOT; SUBP_COND; P2VEG_SUBP_STRUCTURE; P2VEG_SUBPLOT_SPP
  ## - TREE (if popType = 'VOL'); SEEDLING (if popType = 'VOL')
  ## 4. Check for variables in tables and define SELECT variable in cond, plot
  ##    cond - (cuniqueid, condid, cvars2keep)
  ##    area weight variables - (SUBPPROP_UNADJ, MACRPROP_UNADJ, MICRPROP_UNADJ)
  ## 5. Extract tables used to calculate adjustment factors from database if
  ##    pltassgn is not in database
  ## 6. Build query for adjustment factors.
  ## 7. Build queries for PLOT/COND (pltcondx)
  ## 8. Build CASE statement for adding adjustment factors to SELECT
  ## 9. Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE
  ## 10. Build and run queries for other necessary tables (if returndata/savedata = TRUE)
  ## 11. Check COND_STATUS_CD and generate table with number of conditions
  ## 12. Build FROM statement for estimation queries
  ## 13. Return data objects
  ###################################################################################
    
  ## Set global variables
  vsubpsppx=vadjfac=ADJ_FACTOR_P2VEG_SUBP=SCHEMA. <- NULL
  dbqueries=dbqueriesWITH <- list()
  propvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
  subpid <- "SUBP"
  addfortypgrp <- TRUE
  returnadj <- TRUE
  #if (!returndata && !savedata) returndata <- TRUE
  
  ## Get variables from outlst
  if (savedata) {
    append_layer <- outlst$append_layer
    poptablst <- {}
  } else {
    poptablst <- dbtablst
  }

  ###################################################################################
  ## 1. Define variables necessary for estimation
  ###################################################################################
  cvars2keep <- unique(c(cvars2keep, "PROP_BASIS", "COND_NONSAMPLE_REASN_CD"))
  
  
  ##############################################################################
  ## 2. Check if data are in a database (datindb) and if dbconn is valid
  ##############################################################################
  if (datindb) {
    if (is.null(dbconn) || !DBI::dbIsValid(dbconn)) {
      message("the database connection is invalid")
      stop()
    }
    if (!is.null(schema)) {
      SCHEMA. <- paste0(schema, ".")
    }
  }
  
  
  ##############################################################################
  ## 3. Get table names used in estimation from tabs
  ##############################################################################
  
  ## plot table
  plotnm <- plotlst$tabnm
  puniqueid <- plotlst$tabid
  pltx <- plotlst$tabx
  pltflds <- plotlst$tabflds
  pltxnm <- plotnm
  if (!is.null(pltx)) pltxnm <- "pltx"
  
  ## cond table
  condlst <- popTabchk("cond", tabtext = "cond", 
                       tabs, tabIDs, 
                       dbtablst = dbtablst, 
                       dbconn = dbconn, schema = schema, 
                       datindb = datindb) 
  condnm <- condlst$tabnm
  condflds <- condlst$tabflds
  cuniqueid <- condlst$tabid
  condx <- condlst$tabx
  condxnm <- condnm
  if (!is.null(condx)) condxnm <- "condx"
  
  ## define aliases
  plota. <- "p."
  conda. <- "c."
  pltidsa. <- "pltids."
  
  ## subplot table
  subplotlst <- popTabchk(c("subplot"), tabtext = "subplot",
                          tabs, tabIDs, 
                          dbtablst = dbtablst, 
                          dbconn = dbconn, schema = schema, 
                          datindb = datindb)
  subplotnm <- subplotlst$tabnm
  subplotflds <- subplotlst$tabflds
  subplotid <- subplotlst$tabid
  subplotx <- subplotlst$tabx
  subpa. <- "subp."
  if (is.null(subplotnm)) {
    stop("must include subplot for P2VEG estimates")
  }
  
  ## subp_cond table
  subp_condlst <- popTabchk(c("subp_cond", "subpcond"), 
                            tabtext = "subp_cond",
                            tabs, tabIDs, 
                            dbtablst = dbtablst, 
                            dbconn = dbconn, schema = schema, 
                            datindb = datindb)
  subp_condnm <- subp_condlst$tabnm
  subp_condflds <- subp_condlst$tabflds
  subp_condid <- subp_condlst$tabid
  subp_condx <- subp_condlst$tabx
  subpca. <- "subpc."
  if (is.null(subplotnm)) {
    stop("must include subp_cond for P2VEG estimates")
  }
  
  ## p2veg_subp_structure table
  vsubpstrlst <- popTabchk(c("p2veg_subp_structure", "vsubpstr"), 
                           tabtext = "p2veg_subp_structure",
                           tabs, tabIDs, 
                           dbtablst = dbtablst, 
                           dbconn = dbconn, schema = schema, 
                           datindb = datindb)
  vsubpstrnm <- vsubpstrlst$tabnm
  vsubpstrflds <- vsubpstrlst$tabflds
  vsubpstrid <- vsubpstrlst$tabid
  p2veg_subp_structurex <- vsubpstrlst$tabx
  if (is.null(vsubpstrnm)) {
    stop("must include p2veg_subp_structure for P2VEG estimates")
  } else {
    vsubpstra. <- "vsubpstr."
  }
  
  ## p2veg_subplot_spp table
  vsubpspplst <- popTabchk(c("p2veg_subplot_spp", "vsubpspp"), 
                           tabtext = "p2veg_subplot_spp",
                           tabs, tabIDs, 
                           dbtablst = dbtablst, 
                           dbconn = dbconn, schema = schema, 
                           datindb = datindb)
  vsubpsppnm <- vsubpspplst$tabnm
  vsubpsppflds <- vsubpspplst$tabflds
  vsubpsppid <- vsubpspplst$tabid
  p2veg_subplot_sppx <- vsubpspplst$tabx
  if (!is.null(vsubpsppnm)) {
    vsubpsppa. <- "vsubpspp."
  }
  
  
  ##############################################################################
  ## 4. Check for variables in tables and define SELECT variable for cond and plot
  ##############################################################################

  ## 4.1. Check unique identifiers and necessary variables to keep
  ##############################################################################
  
  ## Check cuniqueid in cond
  cuniqueid <- pcheck.varchar(var2check = cuniqueid, varnm="cuniqueid", gui=gui,
                              checklst = condflds, caption="Unique identifier of plot in cond",
                              warn = paste(cuniqueid, "not in cond"), stopifnull = TRUE)
  
  ## Check condid in cond
  condid <- pcheck.varchar(var2check = condid, varnm="condid", gui=gui,
                           checklst = condflds, caption="Unique identifier of conditions in cond",
                           warn = paste(condid, "not in cond"), stopifnull = TRUE)
  
  ## Check cvars2keep in cond
  if (!is.null(cvars2keep)) {
    cvars2keepchk <- unlist(sapply(cvars2keep, findnm, 
                                   condflds, returnNULL = TRUE))
    if (length(cvars2keep) < length(cvars2keep)) {
      message("variables are missing from dataset: ", 
              toString(cvars2keep[!cvars2keep %in% cvars2keepchk]))
      return(NULL)
    } else {
      cvars2keep <- cvars2keepchk
    }	  
  }
  
  ## Check subpid in subplot
  subpid <- pcheck.varchar(var2check = subpid, varnm="subpid", gui=gui,
                           checklst = subplotflds, caption="Unique identifier of subplots",
                           warn = paste(subpid, "not in subplot"), stopifnull = TRUE)
  
  
  ## 4.2. Check condition proportion variables for calculating area weights
  ##############################################################################
  propvars <- check.PROPvars(condflds,
                             propvars = propvars)
  if (!areawt %in% condflds) {
    stop("areawt not in dataset: ", areawt)
  }
  
  
  ## 4.3. Define SELECT variables for cond and plot
  ##############################################################################
  
  ## Define SELECT variables for cond
  if (defaultVars) {
    condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
  } else {
    condvars <- condflds
  }
  
  ## Define SELECT variables for plot
  if (!is.null(plotnm)) {
    if (defaultVars) {
      pvars <- pdoms2keep
    } else {
      pvars <- pltflds
    }
  } else {
    pvars <- NULL
  }
  
  ## Define SELECT variables for subplot
  if (defaultVars) {
    subpvars <-  subplotflds[subplotflds %in% DBvars.default(issubp = TRUE)$subpvarlst]
  } else {
    subpvars <- "*"
  }
  
  ## Define SELECT variables for subp_cond
  if (defaultVars) {
    subpcvars <-  subp_condflds[subp_condflds %in% DBvars.default(issubp = TRUE)$subpcvarlst]
  } else {
    subpcvars <- "*"
  }
  
  ## Define SELECT variables for p2veg_subp_structure
  if (!is.null(vsubpstrnm)) {
    if (defaultVars) {
      vsubpstrvars <-  vsubpstrflds[vsubpstrflds %in% DBvars.default(isveg = TRUE)$vsubpstrvarlst]
    } else {
      vsubpstrvars <- vsubpstrflds
    }
  }
  
  ## Define SELECT variables for p2veg_subplot_spp
  if (!is.null(vsubpsppnm)) {
    if (defaultVars) {
      vsubpsppvars <-  vsubpsppflds[vsubpsppflds %in% DBvars.default(isveg = TRUE)$vsubpsppvarlst]
    } else {
      vsubpsppvars <- vsubpsppflds
    }
  }
  
  
  ##############################################################################
  ## 5. Extract tables used to calculate adjustment factors from database 
  ##    if pltassgn is not in database.
  ## Note: If pltassgn is not in database but all other tables are in database,
  ##       import the cond table to memory and subset for calculating
  ##       adjustment factors.
  ##############################################################################
  if (datindb && !pltaindb) {
    
    ## 5.1. Extract cond table
    ###################################################################
    
    ## Build cond query
    if (!is.null(getdataWITHqry) && !is.null(getdataCNs)) {
      
      ## Build cond FROM query
      condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
      condfromqry <- paste0("\n JOIN ", SCHEMA., condxnm, " c ", condjoinqry)
      
      ## Build cond SELECT query
      condselectqry <- toString(paste0(conda., condvars))
      
      ## Build final cond query, including getdataWITHqry
      condqry <- paste0(getdataWITHqry,
                        "\n-------------------------------------------",
                        "\n SELECT ", condselectqry,
                        "\n FROM pltids",
                        condfromqry) 
      dbqueries$COND <- condqry
      
      ## Run final cond query, including pltidsqry
      if (datindb) {
        condx <- tryCatch(
          DBI::dbGetQuery(dbconn, condqry),
          error=function(e) {
            message(e,"\n")
            return(NULL)})
      } else {
        condx <- tryCatch(
          sqldf::sqldf(condqry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(condx) || nrow(condx) == 0) {
        message("invalid cond query...")
        message(condqry)
        return(NULL)
      } else {
        names(condx) <- toupper(names(condx))
      }
      
      ## Return and/or save cond data
      condkey <- c(cuniqueid, condid)
      setkeyv(setDT(condx), condkey)
      
      ## Subset condx to plots in pltassgn using getdataCNs
      if (!is.null(getdataCNs)) {
        condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
      }
      
    } else {
      assign(condnm, DBI::dbReadTable(dbconn, condnm))
    }
    
    ## Save data
    if (savedata) {
      message("saving cond table...")
      outlst$out_layer <- "cond"
      if (!append_layer) index.unique.cond <- condkey
      datExportData(condx,
                    savedata_opts = outlst)
      if (!returndata) {
        poptablst <- c(poptablst, "cond")
        condnm <- "cond"
      } else {
        condnm=condxnm <- "condx"
      }
    } else {
      condnm=condxnm <- "condx"
    }
    
    
    ## 5.2. Extract subplot table
    ###################################################################
    if (!is.null(getdataWITHqry) && !is.null(getdataCNs)) {
      
      ## Check variables
      subp <- findnm("SUBP", subplotflds, returnNULL = TRUE)
      keyvars <- c(subp)
      if (any(sapply(keyvars, is.null))) {
        keymiss <- keyvars[sapply(keyvars, is.null)]
        stop("missing key variables in subplot data: ", toString(keymiss))
      }
      
      ## Build subplot FROM query
      subpjoinqry <- getjoinqry(subplotid, pltidsid, subpa., pltidsa.)
      subpfromqry <- paste0(
        "\nJOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry)
      
      ## Build subplot SELECT query
      subpselectqry <- toString(paste0(subpa., subpvars))
      
      ## Build final subplot query, including pltidsqry
      subplotqry <- paste0(getdataWITHqry,
                           "\n-------------------------------------------",
                           "\n SELECT ", subpselectqry,
                           "\n FROM pltids",
                           subpfromqry) 
      dbqueries$subplot <- subplotqry
      
      ## Run final subplot query, including pltidsqry
      if (datindb) {
        subplotx <- tryCatch(
          DBI::dbGetQuery(dbconn, subplotqry),
          error=function(e) {
            message(e,"\n")
            return(NULL)})
      } else {
        subplotx <- tryCatch(
          sqldf::sqldf(subplotqry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(subplotx) || nrow(subplotx) == 0) {
        message("invalid subplot query...")
        message(subplotqry)
        return(NULL)
      } else {
        names(subplotx) <- toupper(names(subplotx))
      }
      
      ## Set key on data.table
      subplotkey <- c(subplotid, subp)
      setkeyv(setDT(subplotx), subplotid)
      
      ## Subset subplot to plots in pltassgn using getdataCNs
      if (!is.null(getdataCNs)) {
        subplotx <- subplotx[subplotx[[subplotid]] %in% getdataCNs,]
      }
      
      ## Save data
      if (savedata) {
        message("saving subplot table...")
        outlst$out_layer <- "subplot"
        if (!append_layer) index.unique.subplot <- subplotkey
        datExportData(subplotx,
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "subplot")
          subplotnm <- "subplot"
        } else {
          subplotnm=subplotxnm <- "subplotx"
        }
      } else {
        subplotnm=subplotxnm <- "subplotx"
      }
      
      
      
      ## 5.2. Extract subp_cond table
      ###################################################################
      subpca. <- "subpc."
      
      ## Check variables
      subpcondid <- findnm("CONDID", subp_condflds, returnNULL = TRUE)
      subp <- findnm("SUBP", subp_condflds, returnNULL = TRUE)
      keyvars <- c(subpcondid, subp)
      if (any(sapply(keyvars, is.null))) {
        keymiss <- keyvars[sapply(keyvars, is.null)]
        stop("missing key variables in subp_cond data: ", toString(keymiss))
      }
      
      ## Build subp_cond FROM query
      subpcjoinqry <- getjoinqry(subp_condid, pltidsid, subpca., pltidsa.)
      subpcfromqry <- paste0(
        "\nFROM pltids",
        "\nJOIN ", SCHEMA., subp_condnm, " subpc ", subpcjoinqry)
      
      
      ## Build subp_cond SELECT query
      subpcselectqry <- toString(paste0(subpca., subpcvars))
      
      ## Build final subp_cond query, including pltidsqry
      subp_condqry <- paste0(getdataWITHqry,
                             "\n-------------------------------------------",
                             "\n SELECT ", subpcselectqry,
                             subpcfromqry) 
      dbqueries$subp_cond <- subp_condqry
      
      ## Run final subp_cond query, including pltidsqry
      if (datindb) {
        subp_condx <- tryCatch(
          DBI::dbGetQuery(dbconn, subp_condqry),
          error=function(e) {
            message(e,"\n")
            return(NULL)})
      } else {
        subp_condx <- tryCatch(
          sqldf::sqldf(subp_condqry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(subp_condx) || nrow(subp_condx) == 0) {
        message("invalid subp_cond query...")
        message(subp_condqry)
        return(NULL)
      } else {
        names(subp_condx) <- toupper(names(subp_condx))
      }
      
      ## Return and/or save subp_cond data
      subp_condkey <- c(subp_condid, subpcondid, subp)
      setkeyv(setDT(subp_condx), subp_condid)
      
      ## Subset subplot to plots in pltassgn using getdataCNs
      if (!is.null(getdataCNs)) {
        subp_condx <- subp_condx[subp_condx[[subp_condid]] %in% getdataCNs,]
      }
      
      ## Save data
      if (savedata) {
        message("saving subp_cond table...")
        outlst$out_layer <- "subp_cond"
        if (!append_layer) index.unique.subp_cond <- subp_condkey
        datExportData(subp_condx,
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "subp_cond")
          subp_condnm <- "subp_cond"
        } else {
          subp_condnm=subp_condxnm <- "subp_condx"
        }
      } else {
        subp_condnm=subp_condxnm <- "subp_condx"
      }
      
      
    } else {
      
      assign(subplotnm, DBI::dbReadTable(dbconn, subplotnm))
      assign(subp_condnm, DBI::dbReadTable(dbconn, subp_condnm))
    }
  }  
  
  ## 5.3. Set output data formats for population data
  ###################################################################
  
  ## Set popdatindb if returning data or saving to a database
  if (returndata) {
    popdatindb <- FALSE
    pop_dsn = popconn <- NULL
    pop_datsource <- "obj"
  }
  ## If pop data are not in a database, but is saved to sqlite database
  if (popdatindb) {
    if (savedata && outlst$out_fmt == "sqlite") {
      pop_dsn <- file.path(outlst$outfolder, outlst$out_dsn)
      popconn <- DBtestSQLite(pop_dsn, dbconnopen = TRUE, showlist = FALSE)
      pop_datsource <- "sqlite"
      pop_schema <- NULL
    } else {
      popconn <- dbconn
      pop_datsource <- datsource
      pop_dsn <- NULL
      pop_schema <- schema
    }
  }
  
  
  ##############################################################################
  ## 6. Build query for adjustment factors
  ##############################################################################
  
  ## 6.1. Build query for adjustment factors (ADJqry) based on popType
  ###################################################################
  
  ## Build FROM statement
  pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
  cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
  pcfromqry <- paste0(
    "\n FROM pltids",
    "\n JOIN ", SCHEMA., pltxnm, " p ", pjoinqry,
    "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry)
  
  ## Add FROM statement for subplot and subp_cond		 
  P2VEGjoinqry <- getjoinqry(subplotid, puniqueid, subpa., plota.)
  P2VEGfromqry <- paste0(
    pcfromqry,                    
    "\n JOIN ", SCHEMA., subplotnm, " subp ", P2VEGjoinqry,
    "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subplotid, " = ", conda., cuniqueid, 
    " AND ", subpca., condid, " = ", conda., condid, 
    " AND ", subpca., subpid, " = ", subpa., subpid, ")")
  
  
  ## Build WHERE statement for P2VEG subplots (i.e., excluding nonresponse)
  adjwhereqry <- NULL
  if (adj != "none") {
    adjwhereqry=P2VEGwhereqry <- getADJwherePLOT(condflds, conda.="c.")
    
    ## Subplot filters
    adjwhereqry <- getADJwhereSUBP(subplotflds, adjwhereqry=P2VEGwhereqry)
    
    ## P2 vegetation filters
    P2VEGwhereqry <- getADJwhereP2VEG(subplotflds, pltflds, adjwhereqry=adjwhereqry)
  }  
 
  ## Next, build query for summarizing subplot sampled proportions
  sumpropqry <- sumpropP2VEGqry(fromqry = P2VEGfromqry, 
                                whereqry = P2VEGwhereqry,
                                ACI = ACI,
                                selectvars = NULL,
                                SCHEMA. = SCHEMA.)
  #message(sumpropqry)
  
  ## Build FROM statement for summarized subplot proportions
  adjfromqry <- paste0(
    "\n FROM pltids",
    "\n LEFT OUTER JOIN subpcprop c ON (", pltidsa., pltidsid, " = c.", subplotid, ")")
  othervars <- c(propvars['SUBP'],propvars['MACR'],propvars['MICR'])

  ## Build final query using getADJqry()
  ADJqry <- 
    getADJqry(popType = popType,
              adj = adj,
              propvars = propvars['COND'],
              adjfromqry = adjfromqry,
              pwhereqry = NULL,
              pltidsid = pltidsid,
              pltassgnid = pltassgnid,
              strunitvars = unique(c(projidvars, strunitvars)),
              pltidsa. = pltidsa.,
              propqry = NULL)

  ## Build final query for adjustment factors, including pltids WITH query
  adjfactors.qry <- paste0(
    pltidsWITHqry, ", ",
    "\n----- sum sampled subplot proportions",
    "\nsubpcprop AS ",
    "\n(", sumpropqry, ")",
    "\n-------------------------------------------",
    "\n", ADJqry)
  #message(adjfactors.qry)
  
  
  ## 6.2. If returnadj = TRUE, return adjustment factors  
  ###################################################################
  if (adj != "samp") returnadj <- FALSE
  if (returnadj) {
    ## Run query to calculate adjustment factors
    if (pltaindb) {
      adjfactors <- tryCatch(
        DBI::dbGetQuery(dbconn, adjfactors.qry),
        error=function(e) {
          message("invalid adjustment query...")
          message(e,"\n")
          return(NULL)})
    } else {
      adjfactors <- tryCatch(
        sqldf::sqldf(adjfactors.qry, connection = NULL),
        error = function(e) {
          message("invalid adjustment query...")
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(adjfactors) || nrow(adjfactors) == 0) {
      message(adjfactors.qry)
      return(NULL)
    } else {
      names(adjfactors) <- toupper(names(adjfactors))
    }
    if (adj == "samp") {
      setkeyv(setDT(adjfactors), strunitvars)
    } else {
      setkeyv(setDT(adjfactors), pltidsid)
    }
  }  
  dbqueries$adjfactors <- adjfactors.qry
    

  ## 6.3 Build query for plot-level adjustment factors
  ###################################################################
  adja. <- "adj."
  adjvars <- "ADJ_FACTOR_P2VEG_SUBP"
  pltidsadjSELECT.qry <- paste0(
    "SELECT ", toString(c(paste0(pltidsa., pltidsid), adjvars)))
  areawt <- "P2VEGPROP_UNADJ"
  
  ## if adj='samp', append query for plot-level adjustment factors 
  if (adj == "samp") {
    
    ## First, build WITH query for adjustment factors
    adjfactorsWITHqry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, "),",
      "\n----- calculate strata-level adjustment factors",
      "\nadjfactors AS ",
      "\n(", ADJqry, ")")
    #message(adjfactorsP2VEGWITHqry)
    
    ## Next, build pltidsadjFROM.qry
    adjjoinqry <- getjoinqry(strunitvars, strunitvars, adja., pltidsa.)
    pltidsadjFROM.qry <- paste0(
      "\nFROM pltids",
      "\nJOIN adjfactors adj ", adjjoinqry)
    
    ## Build pltidsadj.qry
    pltidsadj.qry <- paste0(
      adjfactorsWITHqry,
      "\n-------------------------------------------",
      paste0("\n", pltidsadjSELECT.qry,
             pltidsadjFROM.qry))
    ## message(pltidsadj.qry)
    
    ## Build WITH query for plot-level adjustment factors
    pltidsadjWITHqry <- paste0(
      adjfactorsWITHqry, ",",
      "\n----- get plot-level adjustment factors",
      "\npltidsadj AS ",
      "\n(", pltidsadjSELECT.qry,
      pltidsadjFROM.qry, ")")
    
  } else {
    
    ## Build pltidsadj.qry
    pltidsadj.qry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, ")",
      "\n-------------------------------------------",
      "\n", ADJqry)
    #message(pltidsadj.qry)
    
    ## Build WITH query for plot-level adjustment factors
    pltidsadjWITHqry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, "),",
      "\n----- calculate plot-level adjustment factors",
      "\n(", ADJqry, ")")
    #message(pltidsadjWITHqry)
  }  
  dbqueriesWITH$pltidsWITH <- pltidsWITHqry   
  dbqueriesWITH$pltidsadjWITH <- pltidsadjWITHqry   
  
  
  ## 6.4. Run query to identify plotids, including adjustment factors
  ###################################################################
  if (returndata || savedata) {
    if (pltaindb) {
      pltidsadj <- tryCatch(
        DBI::dbGetQuery(dbconn, pltidsadj.qry),
        error=function(e) {
          message(e,"\n")
          return(NULL)})
    } else {
      pltidsadj <- tryCatch(
        sqldf::sqldf(pltidsadj.qry, connection = NULL),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(pltidsadj) || nrow(pltidsadj) == 0) {
      message("invalid adjustment query...")
      message(pltidsadj.qry)
      return(NULL)
    } else {
      names(pltidsadj) <- toupper(names(pltidsadj))
    }
    setkeyv(setDT(pltidsadj), pltidsid)
    
    ## Save data
    if (savedata) {
      message("saving pltidsadj table...")
      outlst$out_layer <- "pltidsadj"
      if (!append_layer) index.unique.pltidsadj <- pltidsid
      datExportData(pltidsadj,
                    savedata_opts = outlst)
      if (!returndata) {
        poptablst <- c(poptablst, "pltidsadj")
      }
    }
  }
  dbqueries$pltidsadj <- pltidsadj.qry

  
  ##############################################################################
  ## 7. Build queries for PLOT/COND (pltcondx)
  ##############################################################################

  ## 7.1. Build FROM and SELECT statements
  ###############################################################
  if (!is.null(plotnm)) {
    ## Build FROM query for pltcondx query
    plota. <- "p."
    
    pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
    cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
    pcfromqry <- paste0(
      "\n FROM pltids",
      "\n JOIN ", SCHEMA., pltxnm, " p ", pjoinqry,
      "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry)
    subpcjoinqry <- getjoinqry(c(subplotid, condid), c(cuniqueid, condid), subpca., conda.)
    P2VEGpcfromqry <- paste0(pcfromqry,
                        "\n JOIN subpcprop subpc ", subpcjoinqry)
    
    ## Build SELECT query for pltcondx query
    pselectqry <- toString(paste0(plota., pvars))

  } else {
    pvars <- NULL
    
    cjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
    pcfromqry <- paste0(
      "\n FROM pltids",
      "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry)
    subpcjoinqry <- getjoinqry(c(subplotid, condid), c(cuniqueid, condid), subpca., conda.)
    P2VEGpcfromqry <- paste0(pcfromqry,
                        "\n JOIN subpcprop subpc ", subpcjoinqry)
  }  
  
  ## Build SELECT query for pltcondx query
  condvars <- unique(c(condvars, cvars2keep))[!unique(c(condvars, cvars2keep)) %in% pvars]
  cselectqry <- toString(paste0(conda., unique(c(condvars, cvars2keep))))
  
  
  ## 7.2. Add FORTYPGRP to SELECT query
  ###############################################################
  if (addfortypgrp) {
    ref_fortypgrp <- ref_codes[ref_codes$VARIABLE == "FORTYPCD", c("VALUE", "GROUPCD")]
    ftypqry <- classqry(classcol = "c.FORTYPCD",
                        fromval = ref_fortypgrp$VALUE,
                        toval = ref_fortypgrp$GROUPCD,
                        classnm = "FORTYPGRPCD")
    cselectqry <- paste0(cselectqry, ", ",
                         "\n ", ftypqry)
    condvars <- c(condvars, "FORTYPGRPCD")
  }
  
  ## Define P2VEG summed condprop_unadj to use for peracre area estimates
  P2VEGselectqry <- paste0(subpca., "CONDPROP_UNADJ AS ", areawt)
  
  
  ## 7.3. Build query for pltcondx
  ###############################################################
  pltcondflds <- unique(c(condvars, pvars))
  
  if (is.null(pvars)) {
    ## Build query for pltcondx
    pltcondx.qry <- paste0("SELECT ", cselectqry, ", 1 AS TOTAL",
                           pcfromqry)
    
    ## Build query for pltcondxP2VEG
    pltcondxP2VEG.qry <- paste0("SELECT ", cselectqry, ", 1 AS TOTAL,",
                           "\n", P2VEGselectqry,
                           P2VEGpcfromqry)
    
  } else {  
    ## Build query for pltcondx
    pltcondx.qry <- paste0("SELECT ", cselectqry, ", ",
                           "\n", pselectqry, ", 1 AS TOTAL",
                           pcfromqry)
    ## Build query for pltcondxP2VEG
    pltcondxP2VEG.qry <- paste0("SELECT ", cselectqry, ", ",
                           "\n", pselectqry, ", 1 AS TOTAL,",
                           "\n", P2VEGselectqry,
                           P2VEGpcfromqry)
  }
  dbqueries$pltcondx <- pltcondx.qry
  dbqueries$pltcondxP2VEG <- pltcondxP2VEG.qry
 
  
  ## 7.4. Build WITH queries for pltcondx
  ###############################################################
  
  ## Build WITH query for pltcondx, including pltids WITH query
  pltcondxWITHqry <- paste0(pltidsWITHqry, ", ",
                            "\n----- sum sampled subplot proportions",
                            "\nsubpcprop AS ",
                            "\n(", sumpropqry, "),",
                            "\n----- pltcondx",
                            "\npltcondx AS",
                            "\n(", pltcondxP2VEG.qry, ")")
  dbqueriesWITH$pltcondxWITH <- pltcondxWITHqry
  
  ## Build WITH query for pltcondx, including pltidsadj WITH query
  pltcondxadjWITHqry <- paste0(pltidsadjWITHqry, ", ",
                               "\npltcondx AS",
                               "\n(", pltcondxP2VEG.qry, ")")
  dbqueriesWITH$pltcondxadjWITH <- pltcondxadjWITHqry
  
  
  ## 7.5. If returndata or savedata, run query for pltcondx
  ##################################################################
  if (returndata || savedata) {
    pltcondindb <- FALSE
    
    pltcondxqry <- paste0(pltidsWITHqry, ", ",
                          "\n----- sum sampled subplot proportions",
                          "\nsubpcprop AS ",
                          "\n(", sumpropqry, ")",
                          "\n-------------------------------------------",
                          "\n", pltcondxP2VEG.qry)
    if (pltaindb) {
      pltcondx <- tryCatch(
        DBI::dbGetQuery(dbconn, pltcondxqry),
        error=function(e) {
          warning(e)
          return(NULL)})
    } else {
      pltcondx <- tryCatch(
        sqldf::sqldf(pltcondxqry, connection = NULL),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(pltcondx) || nrow(pltcondx) == 0) {
      message("invalid pltcondx query...")
      message(pltcondxqry)
      return(NULL)
    } else {
      names(pltcondx) <- toupper(names(pltcondx))
    }
    
    ## Set key on data.table
    pltcondkey <- c(cuniqueid, condid)
    setkeyv(setDT(pltcondx), pltcondkey)
    
    ## Save data
    if (savedata) {
      message("saving pltcondx...")
      outlst$out_layer <- "pltcondx"
      if (!append_layer) index.unique.pltcondx <- pltcondkey
      datExportData(pltcondx,
                    savedata_opts = outlst)
    }
  }
  
  
  ##############################################################################
  ## 8. Build CASE statement for adding adjustment factors to SELECT
  ##############################################################################
  adjcase <- "ADJ_FACTOR_P2VEG_SUBP"
  

  ##############################################################################
  ## 9. Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE
  ##############################################################################  
  returnlst <- list(popdatindb = popdatindb,   ## logical. if TRUE, population data are in database
                    pltcondflds = pltcondflds, ## vector of field names in pltcondx
                    pltflds = pltflds,
                    condflds = condflds,
                    cuniqueid = cuniqueid,     ## unique identifier of plots in pltcondx
                    condid = condid,           ## unique identifier of conditions
                    areawt = areawt,           ## variable names used to calcuate area
                    adjcase = adjcase,         ## CASE statement for summarizing area weights in estimates
                    adjvarlst = adjvars,       ## named vector of adjustment factor variable names
                    pjoinid = pltidsid)        ## joinid for joining pltids WITH query to other tables
  
  if (returndata || savedata) {
    returnlst$pltcondx <- pltcondx    ## data frame of plot/condition variables
    returnlst$pltidsadj <- pltidsadj  ## data frame of plot-level adjustment factors
  } else {
    returnlst$pltcondx <- "pltcondx"
    returnlst$pltidsadj <- "pltidsadj"
  }
  if (returnadj) {
    returnlst$adjfactors <- adjfactors   ## data frame with adjustment factors
  }
  

  # ##############################################################################
  # ## 10. Build and run queries for other necessary tables (if returndata = TRUE) 
  # ##############################################################################  
  if (returndata || savedata) {
  #   message("returning data needed for estimation...")
  #   
  #   if (savedata) {
  #     ## 7.1 Save plot data (pltx / PLOT)
  #     ##################################################################
  #     if (is.null(pltx)) {
  #       ## Build plot FROM query
  #       plotjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
  #       plotfromqry <- paste0("\n JOIN ", SCHEMA., plotnm, " p ", plotjoinqry)
  #       
  #       ## Build plot SELECT query
  #       pselectqry <- toString(paste0(plota., c(puniqueid, pdoms2keep)))
  #       
  #       ## Build final plot query, including pltidsqry
  #       pltqry <- paste0(getdataWITHqry,
  #                        "\n-------------------------------------------",
  #                        "\n SELECT ", pselectqry,
  #                        "\n FROM pltids",
  #                        plotfromqry)
  #       dbqueries$PLOT <- pltqry
  #       
  #       ## Run final plot query, including pltidsqry
  #       if (datindb) {
  #         pltx <- tryCatch(
  #           DBI::dbGetQuery(dbconn, pltqry),
  #           error=function(e) {
  #             message(e,"\n")
  #             return(NULL)})
  #       } else {
  #         pltx <- tryCatch(
  #           sqldf::sqldf(pltqry, connection = NULL),
  #           error = function(e) {
  #             message(e,"\n")
  #             return(NULL) })
  #       }
  #       if (is.null(pltx) || nrow(pltx) == 0) {
  #         message("invalid plot query...")
  #         message(pltqry)
  #         return(NULL)
  #       }
  #     }
  #     
  #     ## Return and/or save plot data
  #     setkeyv(setDT(pltx), puniqueid)
  #     if (!is.null(getdataCNs)) {
  #       pltx <- pltx[pltx[[puniqueid]] %in% getdataCNs,]
  #     }
  #     
  #     ## Add to returnlst
  #     if (returndata) {
  #       returnlst$puniqueid <- puniqueid
  #     }
  #     ## Save data
  #     if (savedata) {
  #       message("saving PLOT...")
  #       outlst$out_layer <- "PLOT"
  #       if (!append_layer) index.unique.plot <- puniqueid
  #       datExportData(pltx,
  #                     savedata_opts = outlst)
  #     }
  #     
  #     
  #     ## 7.2 Save cond data (condx / COND)
  #     ##################################################################
  #     
  #     if (is.null(condx)) {
  #       
  #       ## Build cond FROM query
  #       condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
  #       condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
  #       
  #       ## Build cond SELECT query
  #       if (defaultVars) {
  #         condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
  #       } else {
  #         condvars <- "*"
  #       }
  #       condselectqry <- toString(paste0(conda., condvars))
  #       
  #       ## Build final cond query, including pltidsqry
  #       condqry <- paste0(getdataWITHqry,
  #                         "\n-------------------------------------------",
  #                         "\n SELECT ", condselectqry,
  #                         "\n FROM pltids",
  #                         condfromqry)
  #       dbqueries$COND <- condqry
  #       
  #       ## Run final cond query, including pltidsqry
  #       if (datindb) {
  #         condx <- tryCatch(
  #           DBI::dbGetQuery(dbconn, condqry),
  #           error=function(e) {
  #             message(e,"\n")
  #             return(NULL)})
  #       } else {
  #         condx <- tryCatch(
  #           sqldf::sqldf(condqry, connection = NULL),
  #           error = function(e) {
  #             message(e,"\n")
  #             return(NULL) })
  #       }
  #       if (is.null(condx) || nrow(condx) == 0) {
  #         message("invalid cond query...")
  #         message(condqry)
  #         return(NULL)
  #       }
  #       
  #       ## Return and/or save cond data
  #       condkey <- c(cuniqueid, condid)
  #       setkeyv(setDT(condx), condkey)
  #       if (!is.null(getdataCNs)) {
  #         condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
  #       }
  #     }
  #     
  #     ## Add to returnlst
  #     if (returndata) {
  #       returnlst$cuniqueid <- cuniqueid
  #       returnlst$condx <- condx
  #     }
  #     ## Save data
  #     if (savedata) {
  #       message("saving COND...")
  #       outlst$out_layer <- "COND"
  #       if (!append_layer) index.unique.cond <- condkey
  #       datExportData(condx,
  #                     savedata_opts = outlst)
  #     }
  #     rm(condx)
  #   
  # 
  #     ## 7.3. Save subplot data (subplotx / SUBPLOT)
  #     ##################################################################
  #     if (is.null(subplotx)) {
  #       subpa. <- "subp."
  #       
  #       ## Check variables
  #       subp <- findnm("SUBP", subplotflds, returnNULL = TRUE)
  #       keyvars <- c(subp)
  #       if (any(sapply(keyvars, is.null))) {
  #         keymiss <- keyvars[sapply(keyvars, is.null)]
  #         stop("missing key variables in subplot data: ", toString(keymiss))
  #       }
  #       
  #       ## Build subplot FROM query
  #       subpjoinqry <- getjoinqry(subplotid, pltidsid, subpa., pltidsa.)
  #       subpfromqry <- paste0(
  #         "\nJOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry)
  #       
  #       ## Build subplot SELECT query
  #       if (defaultVars) {
  #         subpvars <-  subplotflds[subplotflds %in% DBvars.default(issubp = TRUE)$subpvarlst]
  #       } else {
  #         subpvars <- "*"
  #       }
  #       subpselectqry <- toString(paste0(subpa., subpvars))
  #       
  #       ## Build final subplot query, including pltidsqry
  #       subplotqry <- paste0(getdataWITHqry,
  #                            "\n-------------------------------------------",
  #                            "\n SELECT ", subpselectqry,
  #                            "\nFROM pltids", 
  #                            subpfromqry) 
  #       dbqueries$subplot <- subplotqry
  #       
  #       ## Run final subplot query, including pltidsqry
  #       if (datindb) {
  #         subplotx <- tryCatch(
  #           DBI::dbGetQuery(dbconn, subplotqry),
  #           error=function(e) {
  #             message(e,"\n")
  #             return(NULL)})
  #       } else {
  #         subplotx <- tryCatch(
  #           sqldf::sqldf(subplotqry, connection = NULL),
  #           error = function(e) {
  #             message(e,"\n")
  #             return(NULL) })
  #       }
  #       if (is.null(subplotx) || nrow(subplotx) == 0) {
  #         message("invalid subplot query...")
  #         message(subplotqry)
  #         return(NULL)
  #       }
  #       
  #       ## Set key on data.table
  #       subplotkey <- c(subplotid, subp)
  #       setkeyv(setDT(subplotx), subplotid)
  #       if (!is.null(getdataCNs)) {
  #         subplotx <- subplotx[subplotx[[subplotid]] %in% getdataCNs,]
  #       }
  #     }
  #     
  #     ## Add to returnlst 
  #     if (returndata) {
  #       returnlst$subplotx <- subplotx
  #       returnlst$subplotid <- subplotid
  #     }
  #     ## Save data
  #     if (savedata) {
  #       message("saving SUBPLOT...")
  #       outlst$out_layer <- "SUBPLOT"
  #       if (!append_layer) index.unique.subplot <- subplotkey
  #       datExportData(subplotx, 
  #                     savedata_opts = outlst)
  #     }
  #     #rm(subplotx)  
  #     
  #     ## 7.4 Save subp_cond data (subp_condx / SUBP_COND)
  #     ##################################################################
  #     if (is.null(subp_condx)) {
  #       subpca. <- "subpc."
  #       
  #       ## Check variables
  #       subpcondid <- findnm("CONDID", subp_condflds, returnNULL = TRUE)
  #       subp <- findnm("SUBP", subp_condflds, returnNULL = TRUE)
  #       keyvars <- c(subpcondid, subp)
  #       if (any(sapply(keyvars, is.null))) {
  #         keymiss <- keyvars[sapply(keyvars, is.null)]
  #         stop("missing key variables in subp_cond data: ", toString(keymiss))
  #       }
  #       
  #       ## Build subp_cond FROM query
  #       subpcjoinqry <- getjoinqry(subp_condid, pltidsid, subpca., pltidsa.)
  #       subpcfromqry <- paste0(
  #         "\nJOIN ", SCHEMA., subp_condnm, " subpc ", subpcjoinqry)
  #       
  #       
  #       ## Build subp_cond SELECT query
  #       if (defaultVars) {
  #         subpcvars <-  subp_condflds[subp_condflds %in% DBvars.default(issubp = TRUE)$subpcvarlst]
  #       } else {
  #         subpcvars <- "*"
  #       }
  #       subpcselectqry <- toString(paste0(subpca., subpcvars))
  #       
  #       ## Build final subp_cond query, including pltidsqry
  #       subp_condqry <- paste0(getdataWITHqry,
  #                              "\n-------------------------------------------",
  #                              "\n SELECT ", subpcselectqry,
  #                              "\nFROM pltids",
  #                              subpcfromqry) 
  #       dbqueries$subp_cond <- subp_condqry
  #       
  #       ## Run final subp_cond query, including pltidsqry
  #       if (datindb) {
  #         subp_condx <- tryCatch(
  #           DBI::dbGetQuery(dbconn, subp_condqry),
  #           error=function(e) {
  #             message(e,"\n")
  #             return(NULL)})
  #       } else {
  #         subp_condx <- tryCatch(
  #           sqldf::sqldf(subp_condqry, connection = NULL),
  #           error = function(e) {
  #             message(e,"\n")
  #             return(NULL) })
  #       }
  #       if (is.null(subp_condx) || nrow(subp_condx) == 0) {
  #         message("invalid subp_cond query...")
  #         message(subp_condqry)
  #         return(NULL)
  #       }
  #       
  #       ## Return and/or save subp_cond data
  #       subp_condkey <- c(subp_condid, subpcondid, subp)
  #       setkeyv(setDT(subp_condx), subp_condid)
  #       if (!is.null(getdataCNs)) {
  #         subp_condx <- subp_condx[subp_condx[[subp_condid]] %in% getdataCNs,]
  #       }
  #     }
  #     
  #     ## Add to returnlst 
  #     if (returndata) {
  #       returnlst$subp_condx <- subp_condx
  #       returnlst$subp_condid <- subp_condid
  #     }
  #     ## Save data
  #     if (savedata) {
  #       message("saving SUBP_COND...")
  #       outlst$out_layer <- "SUBP_COND"
  #       if (!append_layer) index.unique.subp_cond <- subp_condkey
  #       datExportData(subp_condx, 
  #                     savedata_opts = outlst)
  #     }
  #     #rm(subp_condx)
  #     
  #     
    ## 10.1 Save p2veg_subp_structure (vsubstrx / P2VEG_SUBP_STRUCTRUE)
    ##################################################################
    vsubstra. <- "vsubstr."
    
    ## Check variables
    vsubpstr_condid <- findnm("CONDID", vsubpstrflds, returnNULL = TRUE)
    subp <- findnm("SUBP", vsubpstrflds, returnNULL = TRUE)
    keyvars <- c(vsubpstr_condid, subp)
    if (any(sapply(keyvars, is.null))) {
      keymiss <- keyvars[sapply(keyvars, is.null)]
      stop("missing key variables in p2veg_subp_structure data: ", toString(keymiss))
    }
    
    ## Build subp_cond FROM query
    vsubpstrjoinqry <- getjoinqry(vsubpstrid, pltidsid, vsubstra., pltidsa.)
    vsubpstrfromqry <- paste0(
      "\nJOIN ", SCHEMA., vsubpstrnm, " vsubstr ", vsubpstrjoinqry)
    
    ## Build subplot SELECT query
    vsubpstrselectqry <- toString(paste0(vsubstra., vsubpstrvars))
    
    ## Build final p2veg_subp_structure query, including pltidsqry
    vsubpstr.qry <- paste0(getdataWITHqry,
                           "\n-------------------------------------------",
                           "\n SELECT ", vsubpstrselectqry,
                           "\nFROM pltids",
                           vsubpstrfromqry)
    dbqueries$p2veg_subp_structure <- vsubpstr.qry
    
    
    ## Run final p2veg_subp_structure query, including pltidsqry
    if (datindb) {
      
      ## check for index
      idxchk <- checkidx(dbconn, vsubpstrnm, schema = schema, datsource = datsource)
      if (nrow(idxchk) == 0) {
        message("no index exists on ", vsubpstrnm, " query may be slow")
      }
      
      p2veg_subp_structurex <- tryCatch(
        DBI::dbGetQuery(dbconn, vsubpstr.qry),
        error=function(e) {
          message(e,"\n")
          return(NULL)})
    } else {
      p2veg_subp_structurex <- tryCatch(
        sqldf::sqldf(vsubpstr.qry, connection = NULL),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(p2veg_subp_structurex) || nrow(p2veg_subp_structurex) == 0) {
      message("invalid p2veg_subp_structure query...")
      message(vsubpstr.qry)
      return(NULL)
    } else {
      names(p2veg_subp_structurex) <- toupper(names(p2veg_subp_structurex))
    }
    
    ## Return and/or save vsubpstr data
    vsubpstrkey <- c(vsubpstrid, vsubpstr_condid)
    setkeyv(setDT(p2veg_subp_structurex), vsubpstrid)
    if (!is.null(getdataCNs)) {
      p2veg_subp_structurex <- p2veg_subp_structurex[p2veg_subp_structurex[[vsubpstrid]] %in% getdataCNs,]
    }
    
    ## Append adjustment factors to p2veg_subp_structurex data
    p2veg_subp_structurex <- p2veg_subp_structurex[pltidsadj, vadjfac := ADJ_FACTOR_P2VEG_SUBP]
    vsubpstrvars <- c(vsubpstrvars, "vadjfac")
    

    ## Add to returnlst
    if (returndata) {
      returnlst$p2veg_subp_structurex <- p2veg_subp_structurex
      returnlst$vuniqueid <- vsubpstrid
      returnlst$vsubpstrflds <- vsubpstrvars
    }
    if (savedata) {
      message("saving P2VEG_SUBP_STRUCTURE...")
      outlst$out_layer <- "p2veg_subp_structure"
      if (!append_layer) index.unique.p2veg_subp_structure <- vsubpstrkey
      datExportData(p2veg_subp_structurex,
                    savedata_opts = outlst)
      if (!returndata) {
        poptablst <- c(poptablst, "p2veg_subp_structure")
        vsubpstrnm <- "p2veg_subp_structure"
      }
    }
    
    # ## 10.2 Save p2veg_subplot_spp (vsubsppx / P2VEG_SUBPLOT_SPP)
    # ##################################################################
    if (is.null(vsubpsppx) && !is.null(vsubpsppnm)) {
      
      vsubstra. <- "vsubpspp."
      
      ## Check variables
      vsubpspp_condid <- findnm("CONDID", vsubpsppflds, returnNULL = TRUE)
      subp <- findnm("SUBP", vsubpsppflds, returnNULL = TRUE)
      keyvars <- c(vsubpspp_condid, subp)
      if (any(sapply(keyvars, is.null))) {
        keymiss <- keyvars[sapply(keyvars, is.null)]
        stop("missing key variables in p2veg_subp_structure data: ", toString(keymiss))
      }
      
      ## Build subp_cond FROM query
      vsubpsppjoinqry <- getjoinqry(vsubpsppid, pltidsid, vsubpsppa., pltidsa.)
      vsubpsppfromqry <- paste0(
        "\nJOIN ", SCHEMA., vsubpsppnm, " vsubpspp ", vsubpsppjoinqry)
      
      ## Build subplot SELECT query
      vsubpsppselectqry <- toString(paste0(vsubpsppa., vsubpsppvars))
      
      ## Build final p2veg_subplot_spp query, including pltidsqry
      vsubpspp.qry <- paste0(getdataWITHqry,
                             "\n-------------------------------------------",
                             "\n SELECT ", vsubpsppselectqry,
                             "\nFROM pltids",
                             vsubpsppfromqry)
      dbqueries$p2veg_subplot_spp <- vsubpspp.qry
      
      
      ## Run final p2veg_subplot_spp query, including pltidsqry
      if (datindb) {
        ## check for index
        idxchk <- checkidx(dbconn, vsubpsppnm, schema = schema, datsource = datsource)
        if (nrow(idxchk) == 0) {
          message("no index exists on ", vsubpsppnm, " query may be slow")
        }
        
        p2veg_subplot_sppx <- tryCatch(
          DBI::dbGetQuery(dbconn, vsubpspp.qry),
          error=function(e) {
            message(e,"\n")
            return(NULL)})
      } else {
        p2veg_subplot_sppx <- tryCatch(
          sqldf::sqldf(vsubpspp.qry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(p2veg_subplot_sppx) || nrow(p2veg_subplot_sppx) == 0) {
        message("invalid p2veg_subplot_spp query...")
        message(vsubpspp.qry)
        return(NULL)
      } else {
        names(p2veg_subplot_sppx) <- toupper(names(p2veg_subplot_sppx))
      }
      
      ## Return and/or save vsubpspp data
      vsubpsppkey <- c(vsubpsppid, vsubpspp_condid)
      setkeyv(setDT(p2veg_subplot_sppx), vsubpsppid)
      if (!is.null(getdataCNs)) {
        p2veg_subplot_sppx <- p2veg_subplot_sppx[p2veg_subplot_sppx[[vsubpsppid]] %in% getdataCNs,]
      }
      
      ## Append adjustment factors to p2veg_subplot_sppx data
      p2veg_subplot_sppx <- p2veg_subplot_sppx[pltidsadj, vadjfac := ADJ_FACTOR_P2VEG_SUBP]
      vsubpsppflds <- c(vsubpsppflds, "vadjfac")
      
      
      ## Add to returnlst
      if (returndata) {
        returnlst$p2veg_subplot_sppx <- p2veg_subplot_sppx
        returnlst$vuniqueid <- vsubpsppid
        returnlst$vsubpsppflds <- vsubpsppvars
      }
      if (savedata) {
        message("saving P2VEG_SUBPLOT_SPP...")
        outlst$out_layer <- "p2veg_subplot_spp"
        if (!append_layer) index.unique.p2veg_subplot_spp <- vsubpsppkey
        datExportData(p2veg_subplot_sppx,
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "p2veg_subplot_spp")
          vsubpsppnm <- "p2veg_subplot_spp"
        }
      }
    }
  }   

  ##############################################################################
  ## 11. Check COND_STATUS_CD and generate table with number of conditions
  ##############################################################################

  ## condfromqry
  condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
  condfromqry <- paste0("\nJOIN ", SCHEMA., condnm, " c ", condjoinqry)
  
  ## 11.1. Sampled conditions
  ##########################################################################
  condsampcnt <- NULL
  cstatuschk <- findnm("COND_STATUS_CD", pltcondflds, returnNULL=TRUE)
  
  if (is.null(cstatuschk)) {
    message("COND_STATUS_CD not in dataset.. assuming all conditions are sampled")
  } else {
    ref_cond_status_cd <- ref_codes[ref_codes$VARIABLE == "COND_STATUS_CD", ]
    if (length(cstatuschk) > 1) {
      cstatuscdnm <- cstatuschk[1]
    } else {
      cstatuscdnm <- cstatuschk
    }  
    
    ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
    if (returndata || savedata) {
      condsampcnt <- pltcondx[, .N, by=cstatuscdnm]
      setnames(condsampcnt, "N", "NBRCONDS")
    } else {
      condsampcntqry <- paste0("\nSELECT ", cstatuscdnm, ", COUNT(*) AS NBRCONDS",
                               "\nFROM pltids",
                               condfromqry,
                               "\nGROUP BY ", cstatuscdnm,
                               "\nORDER BY ", cstatuscdnm)
      condsampcntqry <- paste0(getdataWITHqry,
                               condsampcntqry)
      
      condsampcnt <- tryCatch(
        DBI::dbGetQuery(dbconn, condsampcntqry),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
      
      if (is.null(condsampcnt)) {
        message("invalid condsampcnt query")
        message(condsampcntqry)
      } else {
        names(condsampcnt) <- toupper(names(condsampcnt))
      }
    }
    if (!is.null(condsampcnt)) {
      condsampcnt <-
        cbind(COND_STATUS_NM = ref_cond_status_cd[match(condsampcnt$COND_STATUS_CD,
                                                        ref_cond_status_cd$VALUE), "MEANING"], condsampcnt)
      
      nbrnonsampled <- condsampcnt$NBRCONDS[condsampcnt$COND_STATUS_CD == 5]
      if (length(nbrnonsampled) > 0) {
        message("there are ", nbrnonsampled, " nonsampled conditions")
      }
    }
  } 
  
  ## 11.2. Sampled nonforest conditions
  ##########################################################################
  
  ## If ACI, check NF_PLOT_STATUS_CD and generate table with number of plots
  ##########################################################################
  if (ACI) {
    nfcondsampcnt <- NULL
    nfcstatuschk <- findnm("NF_COND_STATUS_CD", pltcondflds, returnNULL=TRUE)
    if (is.null(nfcstatuschk)) {
      message("NF_COND_STATUS_CD not in dataset.. assuming all ACI nonforest conditions")
    } else {  
      ref_nf_cond_status_cd <- ref_codes[ref_codes$VARIABLE == "NF_COND_STATUS_CD", ]
      if (length(nfcstatuschk) > 1) {
        nfcstatuscdnm <- nfcstatuschk[1]
      } else {
        nfcstatuscdnm <- nfcstatuschk
      }  
      
      if (returndata || savedata) {
        nfcondsampcnt <- pltcondx[, .N, by=nfcstatuscdnm]
        setnames(nfcondsampcnt, "N", "NBRCONDS")
      } else {
        
        ## Generate table of sampled/nonsampled conditions (if ACI, nonforest status included)
        nfcondsampcntqry <- paste0("\nSELECT c.", nfcstatuscdnm, ", COUNT(*) AS NBRCONDS",
                                   "\nFROM pltids",
                                   condfromqry,
                                   "\nGROUP BY ", nfcstatuscdnm,
                                   "\nORDER BY ", nfcstatuscdnm)
        nfcondsampcntqry <- paste0(getdataWITHqry,
                                   nfcondsampcntqry)
        
        nfcondsampcnt <- tryCatch(
          DBI::dbGetQuery(dbconn, nfcondsampcntqry),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
        
        if (is.null(nfcondsampcnt)) {
          message("invalid nfcondsampcnt query")
          message(nfcondsampcntqry)
        } else {
          names(nfcondsampcnt) <- toupper(names(nfcondsampcnt))
        }
      }
      if (!is.null(nfcondsampcnt)) {
        nfcondsampcnt <- nfcondsampcnt[!is.na(nfcondsampcnt$NF_COND_STATUS_CD), ]
        if (nrow(nfcondsampcnt) > 0) {
          nfcondsampcnt <-
            cbind(NF_COND_STATUS_NM = ref_cond_status_cd[match(nfcondsampcnt$NF_COND_STATUS_CD,
                                                               ref_cond_status_cd$VALUE), "MEANING"], nfcondsampcnt)
          ## Append to condsampcnt
          if (!is.null(condsampcnt)) {
            condsampcnt <- rbindlist(list(condsampcnt, nfcondsampcnt), use.names=FALSE)
          } else {
            condsampcnt <- nfcondsampcnt
          }
          
          ## Create nonsamp.cfilter
          if (!is.null(nfcstatuscdnm) && (is.null(nonsamp.cfilter) || nonsamp.cfilter == "")) {
            nfnonsamp.cfilter <- paste("c.", "NF_COND_STATUS_CD <> 5")
          }
          if (!is.null(nonsamp.cfilter)) {
            nonsamp.cfilter <- paste0(nonsamp.cfilter, " AND ", nfnonsamp.cfilter)
          } else {
            nonsamp.cfilter <- nfnonsamp.cfilter
          }  
          nbrnfnonsampled <- nfcondsampcnt$NBRCONDS[nfcondsampcnt$NF_COND_STATUS_CD == 5]
          if (length(nbrnfnonsampled) > 0) {
            message("there are ", nbrnfnonsampled, " nonsampled nonforest conditions")
          }
        }
      }        
    }
  }
  
  ## Add condsampcnt to returnlst 
  if (!is.null(condsampcnt)) {
    returnlst$condsampcnt <- as.data.frame(condsampcnt)
  }

  ## 12. Build FROM statement for estimation queries
  ######################################################################################
  if (datindb) {
    estpcfromqry <- paste0(
      "\n FROM ", SCHEMA., plotnm, " p",
      "\n JOIN ", SCHEMA., condnm, " c ON (", conda., cuniqueid, " = ", plota., puniqueid, ")")
  } else {
    estpcfromqry <- paste0(
      "\n FROM pltcondf cond")
  }

  ## Add from statement for subp_cond_chng_matrx
  if (!is.null(vsubpsppnm) && !is.null(vsubpsppx)) {
    estfromqry <- paste0(estpcfromqry,  
      "\n JOIN ", SCHEMA., vsubpsppnm, " vsubpspp ON(", vsubpsppa., vsubpsppid, " = c.", cuniqueid)
  }
  estfromqry <- paste0(estpcfromqry,  
      "\n JOIN ", SCHEMA., vsubpstrnm, " vsubpstr ON(", vsubpstra., vsubpstrid, " = c.", cuniqueid) 
  

  ## 13. Return data objects
  ######################################################################################
  returnlst$pop_datsource <- pop_datsource
  if (popdatindb || savedata) {
    returnlst$popdbinfo <- list(popconn = popconn, 
                                pop_dsn = pop_dsn,
                                pop_schema = pop_schema,
                                pop_datsource = pop_datsource,
                                poptablst = poptablst)
  }

  if (popdatindb) {
    
    if (!is.null(vsubpstrnm)) {
      returnlst$p2veg_subp_structurex <- vsubpstrnm
      returnlst$vuniqueid <- vsubpstrid
      returnlst$vsubpstrflds <- vsubpstrvars
    }
    if (!is.null(vsubpsppnm)) {
      returnlst$p2veg_subplot_sppx <- vsubpsppnm
      returnlst$vuniqueid <- vsubpstrid
      returnlst$vsubsppflds <- vsubpsppvars
    }
  }
  
  returnlst$dbqueries <- dbqueries
  returnlst$dbqueriesWITH <- dbqueriesWITH
  returnlst$estfromqry <- estfromqry

  return(returnlst)
}
