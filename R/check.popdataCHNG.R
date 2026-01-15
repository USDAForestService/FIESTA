check.popdataCHNG <- 
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
           pwhereqry = NULL, 
           condid = "CONDID", 
           areawt = "SUBPTYP_PROP_CHNG", areawt2 = NULL,
           MICRO_BREAKPOINT_DIA = 5, 
           MACRO_BREAKPOINT_DIA = NULL, 
           unitvars = NULL, 
           strunitvars = NULL, 
           nonsamp.cfilter = NULL, 
           cvars2keep = NULL, 
           isseed = FALSE,
           dbconn = NULL, schema = NULL, 
           datsource = NULL, dbtablst = NULL,
           getdataWITHqry = NULL,
           getdataCNs = NULL,
           returndata = FALSE, 
           savedata = FALSE, 
           outlst = NULL,
           gui = FALSE){
    
    ##############################################################################
    ## DESCRIPTION: Checks data inputs for CHNG estimation
    ## 1. Define variables necessary for estimation:
    ## - cvars2keep = c('PROP_BASIS', 'COND_NONSAMPLE_REASN_CD')
    ## 2. Check if data are in a database (datindb) and if dbconn is valid.
    ## 3.	Get table names used in estimation from tabs.
    ## - PLOT; COND; SUBP_COND_CHNG_MTRX
    ## - TREE (if popType = 'GRM'); SEEDLING (if popType = 'GRM')
    ## - TREE_GRM_COMPONENT, TREE_GRM_BEGIN, TREE_GRM_MIDPT (if popType = 'GRM')
    ## 4. Check for variables in tables and define SELECT variable in cond, plot
    ##    cond - (cuniqueid, condid, cvars2keep)
    ##    subp_cond_chng_mtrx - PREVCOND
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
    grmx=beginx=midptx=SUBPTYP_PROP_ADJ=SUBPTYP_PROP_CHNG=ADJ_FACTOR_COND=SCHEMA. <- NULL
    dbqueries=dbqueriesWITH <- list()
    cpropvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ")
    tpropvars <- list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
    diavar <- "DIA"
    popdatindb <- datindb
    addfortypgrp <- TRUE
    returnadj <- TRUE

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
    ## 3. Get tables used in estimation from tabs
    ##############################################################################
    
    ## plot table
    plotnm <- plotlst$tabnm
    puniqueid <- plotlst$tabid
    pltx <- plotlst$tabx
    pltflds <- plotlst$tabflds
    pltxnm <- plotnm
    if (!is.null(pltx)) pltxnm <- "pltx"
    
    ## cond table
    condlst <- popTabchk(c("condu", "cond"), tabtext = "cond", 
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

    ## subp_cond_chng_mtrx table
    sccmlst <- popTabchk(c("subp_cond_chng_mtrx", "sccm"), 
                         tabtext = "sccmx",
                         tabs, tabIDs, 
                         dbtablst = dbtablst, 
                         dbconn = dbconn, schema = schema, 
                         datindb = datindb)
    sccmnm <- sccmlst$tabnm
    sccmflds <- sccmlst$tabflds
    sccmid <- sccmlst$tabid
    sccmx <- sccmlst$tabx
    sccmxnm <- sccmnm
    if (!is.null(sccmx)) sccmxnm <- "sccmx"
    
    if (is.null(condxnm)) {
      stop("must include cond for CHNG estimates")
    }
    if (is.null(sccmxnm)) {
      stop("must include subp_cond_chng_mtrx for CHNG estimates")
    }
    
    ## define aliases
    plota. <- "p."
    conda. <- "c."
    pltidsa. <- "pltids."
    sccma. <- "sccm."
    
    
    if (popType == "GRM") {
      
      ## tree table
      treelst <- popTabchk(c("tree"), tabtext = "tree",
                           tabs, tabIDs, 
                           dbtablst = dbtablst, 
                           dbconn = dbconn, schema = schema, 
                           datindb = datindb)
      treenm <- treelst$tabnm
      treeflds <- treelst$tabflds
      tuniqueid <- treelst$tabid
      treex <- treelst$tabx
      
      if (is.null(treenm)) {
        stop("must include tree for estimation")
      }
      
      ## seedling table
      seedlst <- popTabchk(c("seed", "seedling"), tabtext = "seed",
                           tabs, tabIDs, 
                           dbtablst = dbtablst, 
                           dbconn = dbconn, schema = schema, 
                           datindb = datindb)
      seednm <- seedlst$tabnm
      seedflds <- seedlst$tabflds
      suniqueid <- seedlst$tabid
      seedx <- seedlst$tabx
      
      
      ## tree_grm_component table
      grmlst <- popTabchk(c("grm", "tree_grm_component"), tabtext = "tree_grm_component",
                          tabs, tabIDs, 
                          dbtablst = dbtablst, 
                          dbconn = dbconn, schema = schema, 
                          datindb = datindb)
      grmnm <- grmlst$tabnm
      grmflds <- grmlst$tabflds
      grmid <- grmlst$tabid
      grmx <- grmlst$tabx
      
      if (is.null(grmnm)) {
        stop("must include tree_grm_component for estimation")
      }
      
      ## tree_grm_begin table
      beginlst <- popTabchk(c("begin", "tree_grm_begin"), tabtext = "tree_grm_begin",
                            tabs, tabIDs, 
                            dbtablst = dbtablst, 
                            dbconn = dbconn, schema = schema, 
                            datindb = datindb)
      beginnm <- beginlst$tabnm
      beginflds <- beginlst$tabflds
      beginid <- beginlst$tabid
      beginx <- beginlst$tabx
      
      if (is.null(beginnm)) {
        stop("must include tree_grm_begin for estimation")
      }
      
      ## tree_grm_midpt table
      midptlst <- popTabchk(c("midpt", "tree_grm_midpt"), tabtext = "tree_grm_midpt",
                            tabs, tabIDs, 
                            dbtablst = dbtablst, 
                            dbconn = dbconn, schema = schema, 
                            datindb = datindb)
      midptnm <- midptlst$tabnm
      midptflds <- midptlst$tabflds
      midptid <- midptlst$tabid
      midptx <- midptlst$tabx
      
      if (is.null(midptnm)) {
        stop("must include tree_grm_midpt for estimation")
      }
    }  
    
    
    ##############################################################################
    ## 4. Check for variables in tables and define SELECT variables
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
    
    
    ## Check prev_plt_cn in plot
    prevpltcnnm <- findnm("PREV_PLT_CN", pltflds, returnNULL = TRUE)
    if (is.null(prevpltcnnm)) {
      message("need PREV_PLT_CN in plot table for CHNG estimates")
      stop()
    }
    
    ## Check prevcond in subp_cond_chng_mtrx 
    prevcondnm <- findnm("PREVCOND", sccmflds, returnNULL = TRUE)
    if (is.null(prevcondnm)) {
      message("need PREVCOND in subp_cond_chng_mtrx table for CHNG estimates")
    }
    
    
    ## 4.2. Check condition proportion variables for calculating area weights
    ##############################################################################
    cpropvars <- check.PROPvars(condflds,
                                propvars = unlist(cpropvars))
    areawt <- findnm(areawt, sccmflds, returnNULL = TRUE)
    if (is.null(areawt)) {
      stop("areawt not in dataset: ", areawt)
    }
    propvars <- cpropvars
    
    if (popType == "GRM") {
      tpropvars <- check.PROPvars(condflds, treeflds = treeflds,
                                  propvars = unlist(tpropvars),
                                  MICRO_BREAKPOINT_DIA = MICRO_BREAKPOINT_DIA,
                                  MACRO_BREAKPOINT_DIA = MACRO_BREAKPOINT_DIA)
      cvars2keep <- unique(c(cvars2keep, tpropvars))
      propvars <- c(cpropvars, tpropvars)
      propvars <- propvars[!duplicated(propvars)]
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
    
    ## Define SELECT variables for subp_cond_chng_mtrx
    sccmvars <- sccmflds
    

    if (popType == "GRM") {
      
      ## Define SELECT variables for tree
      if (!is.null(treenm)) {
        if (defaultVars) {
          treevars <- treeflds[treeflds %in% c(DBvars.default(istree=TRUE)$treevarlst,
                                               DBvars.default(istree=TRUE)$tsumvarlst)]
        } else {
          treevars <- treeflds
        }
      }  
      ## Define SELECT variables for tree_grm_component
      # if (defaultVars) {
      #   grmvars <- grmflds[grmflds %in% DBvars.default(isgrm=TRUE)$grmvarlst]
      # } else {
        grmvars <- grmflds
      # }
      
      ## Define SELECT variables for tree_grm_begin
      beginvars <- beginflds
      
      ## Define SELECT variables for tree_grm_midpt
      midptvars <- midptflds 
      
      
      ## Define SELECT variables for seedling
      if (!is.null(seednm)) {
        if (defaultVars) {
          seedvars <- seedflds[seedflds %in% c(DBvars.default(isseed=TRUE)$seedvarlst,
                                               DBvars.default(isseed=TRUE)$ssumvarlst)]
        } else {
          seedvars <- seedflds
        }
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
     
        
      ## Build cond query
      if (!is.null(getdataWITHqry) && !is.null(getdataCNs)) {
        
        ## 5.2. Extract subp_cond_chng_mtrx table (sccm)
        ###################################################################
        
        ## Build sccm FROM query
        sccmjoinqry <- getjoinqry(sccmid, pltidsid, sccma., pltidsa.)
        sccmfromqry <- paste0("\n JOIN ", SCHEMA., sccmnm, " sccm ", sccmjoinqry)
        
        ## Build sccm SELECT query
        sccmselectqry <- toString(paste0(sccma., sccmvars))
        
        ## Build final sccm query, including getdataWITHqry
        sccmqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", sccmselectqry,
                          "\n FROM pltids",
                          sccmfromqry) 
        dbqueries$SCCM <- sccmqry
        
        ## Run final sccm query, including pltidsqry
        if (datindb) {
          sccmx <- tryCatch(
            DBI::dbGetQuery(dbconn, sccmqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
        } else {
          sccmx <- tryCatch(
            sqldf::sqldf(sccmqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(sccmx) || nrow(sccmx) == 0) {
          message("invalid cond query...")
          message(sccmqry)
          return(NULL)
        } else {
          names(sccmx) <- toupper(names(sccmx))
        }
        
        ## Set key on data.table
        sccmkey <- c("PLT_CN", "CONDID", "PREV_PLT_CN", "PREVCOND")
        setkeyv(setDT(sccmx), sccmkey)
        
        ## Subset sccmx to plots in pltassgn using getdataCNs
        if (!is.null(getdataCNs)) {
          sccmx <- sccmx[sccmx[[sccmid]] %in% getdataCNs,]
        }
        
      } else {
        assign(sccmnm, DBI::dbReadTable(dbconn, sccmnm))
      }
      
      ## Save data
      if (savedata) {
        message("saving subp_cond_chng_mtrx...")
        outlst$out_layer <- "subp_cond_chng_mtrx"
        if (!append_layer) index.unique.sccm <- sccmkey
        datExportData(sccmx, 
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "subp_cond_chng_mtrx")
          sccmnm <- "subp_cond_chng_mtrx"
        } else {
          sccmnm=sccmxnm <- "sccmx"
        }
      } else {
        sccmnm=sccmxnm <- "sccmx"
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
        popconn <- DBtestSQLite(pop_dsn, dbconnopen = TRUE)
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
    pplota. <- "pplot."
    pconda. <- "pcond."

    ## Build FROM statement
    pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
    pfromqry <- paste0("\n FROM pltids",
                       "\n JOIN ", SCHEMA., pltxnm, " p ", pjoinqry)

    ## Add FROM statement for remeasured plots
    pplotjoinqry <- getjoinqry(puniqueid, prevpltcnnm, pplota., plota.)
    cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
    pcondjoinqry <- getjoinqry(cuniqueid, prevpltcnnm, pconda., plota.)
    pcfromqry <- paste0(
      pfromqry,
      #"\n JOIN ", SCHEMA., plotnm, " pplot ", pplotjoinqry,
      "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry,
      "\n JOIN ", SCHEMA., condxnm, " pcond ", pcondjoinqry)

    ## Add FROM statement for subp_cond_chng_matrx
    sccmjoinqry <- getjoinqry(c(sccmid, prevpltcnnm), c(pltidsid, prevpltcnnm), sccma., plota.)
    sccmjoinqry <- paste0(
      sccmjoinqry,
      "\n          AND ", sccma., condid, " = ", conda., condid, 
      " AND ", sccma., prevcondnm, " = ", pconda., condid)

    sccmfromqry <- paste0(pcfromqry,  
                      "\n JOIN ", SCHEMA., sccmxnm, " sccm ", sccmjoinqry) 
    
    
    ## Build WHERE statement for subplots (i.e., excluding nonresponse)
    adjwhereqry <- NULL
    if (adj != "none") {
      adjwhereqry <- getADJwherePLOT(condflds, conda.="c.")
      
      ## Change filters
      CHNGwhereqry <- getADJwhereCHNG(condflds, sccmflds, adjwhereqry=adjwhereqry)
    }  

    ## Next, build query for summarizing subplot sampled proportions
    sumpropqry <- sumpropCHNGqry(fromqry = sccmfromqry, 
                                 whereqry = CHNGwhereqry,
                                 ACI = ACI,
                                 selectvars = NULL,
                                 SCHEMA. = SCHEMA.)
    #message(sumpropqry) 
    
    ## If returndata or savedata, return subpcprop  
    ###################################################################
    if (returndata || savedata) {
      
      subpcprop.qry <- paste0(
        pltidsWITHqry,
        "\n-------------------------------------------",
        "\n", sumpropqry)
        
      ## Run query to calculate adjustment factors
      if (pltaindb) {
        subpcprop <- tryCatch(
          DBI::dbGetQuery(dbconn, subpcprop.qry),
          error=function(e) {
            message(e,"\n")
            return(NULL)})
      } else {
        subpcprop <- tryCatch(
          sqldf::sqldf(subpcprop.qry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(subpcprop) || nrow(subpcprop) == 0) {
        message("invalid adjustment query...")
        message(subpcprop.qry)
        return(NULL)
      } else {
        names(subpcprop) <- toupper(names(subpcprop))
      }
      subpcpropkey <- c(cuniqueid, condid, prevpltcnnm, prevcondnm)
      setkeyv(setDT(subpcprop), c(cuniqueid, condid))
      dbqueries$subpcprop <- subpcprop.qry
      
      ## Save data
      if (savedata) {
        message("saving subpcprop...")
        outlst$out_layer <- "subpcprop"
        if (!append_layer) index.unique.subpcprop <- subpcpropkey
        datExportData(subpcprop, 
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "subpcprop")
        }
      }
    }
    
    ## Build FROM statement for summarized subplot proportions
    adjjoinqry <- getjoinqry(sccmid, pltidsid, "c.", pltidsa.)
    adjfromqry <- paste0("\n FROM pltids",
                         "\n JOIN subpcprop c ", adjjoinqry)
    
    ## Build final query using getADJqry() 
    ADJqry <- 
      getADJqry(popType = popType,
                adj = adj,
                propvars = propvars,
                adjfromqry = adjfromqry,
                pwhereqry = NULL,
                pltassgnid = pltassgnid,
                strunitvars = strunitvars,
                pltidsa. = pltidsa.,
                pltidsid = pltidsid,
                propqry = NULL)
    #message(ADJqry))
    
    ## Build final query for adjustment factors, including pltids WITH query
    adjfactors.qry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, ")",
      "\n-------------------------------------------",
      "\n", ADJqry
    )
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
            message(e,"\n")
            return(NULL)})
      } else {
        adjfactors <- tryCatch(
          sqldf::sqldf(adjfactors.qry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(adjfactors) || nrow(adjfactors) == 0) {
        message("invalid adjustment query...")
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
    adjvars <- sapply(toupper(propvars), function(x) {
      ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)),
             paste0(x, "_ADJ")) })
    pltidsadjSELECT.qry <- paste0(
      "SELECT ", toString(c(paste0(pltidsa., pltidsid), paste0(adja., adjvars))))
    
    ## if adj='samp', append query for plot-level adjustment factors 
    if (adj == "samp") {
      
      ## Build final query for adjustment factors, including pltids WITH query
      adjfactorsWITHqry <- paste0(
        pltidsWITHqry, ", ",
        "\n----- sum sampled subplot proportions",
        "\nsubpcprop AS ",
        "\n(", sumpropqry, "),",
        "\n----- calculate strata-level adjustment factors",
        "\nadjfactors AS",
        "\n(", ADJqry, ")")
      #message(adjfactorsWITH.qry)

      ## Next, build pltcondxadjFROM.qry
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
    
      ## Build WITH query to identify pltids, including adjustment factors
      pltidsadjWITHqry <- paste0(
        adjfactorsWITHqry, ",",
        "\n----- get plot-level adjustment factors",
        "\npltidsadj AS ",
        "\n(", pltidsadjSELECT.qry,
        pltidsadjFROM.qry, ")")
    
    } else {
  
      ## Build pltidsadj.qry
      pltidsadj.qry <- paste0(
        pltidsWITHqry,
        "\n----- sum sampled subplot proportions",
        "\nsubpcprop AS ",
        "\n(", sumpropqry, ")",
        "\n-------------------------------------------",
        "\n", ADJqry)
      #message(pltidsadj.qry)
        
      ## Build WITH query to identify pltids, including adjustment factors
      pltidsadjWITHqry <- paste0(
        pltidsWITHqry,
        "\n----- sum sampled subplot proportions",
        "\nsubpcprop AS ",
        "\n(", sumpropqry, ")",
        "\n----- calculate plot-level adjustment factors",
        "\n", ADJqry)
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
        message("saving pltidsadj...")
        outlst$out_layer <- "pltidsadj"
        if (!append_layer) index.unique.subpcprop <- pltidsid
        datExportData(pltidsadj, 
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "pltidsadj")
        }
      }
    }
    dbqueries$pltidsadj <- pltidsadj.qry
    
 
    ##############################################################################
    ## 7. Build and run queries for PLOT/COND (pltcondx).
    ##############################################################################

    ## 7.1. Build FROM query for pltcondx query
    ##################################################################
    pplotfromqry <- paste0(
      pfromqry,
      "\n JOIN ", SCHEMA., pltxnm, " pplot ", pplotjoinqry)
    
    ppcfromqry <- paste0(
      pplotfromqry,
      "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry,
      "\n JOIN ", SCHEMA., condxnm, " pcond ", pcondjoinqry)
    
    
    ## Build SELECT query for pltcondx query
    pvars <- pvars[pvars != "PREV_PLT_CN"]
    pselectqry <- toString(paste0(plota., pvars))
    pplotselectqry <- toString(paste0(pplota.,  pvars, " AS PREV_", pvars))

    ## Build SELECT query for pltcondx query
    condvars <- unique(c(condvars, cvars2keep))[!unique(c(condvars, cvars2keep)) %in% pvars]
    cselectqry <- toString(paste0(conda., condvars))
    pcondselectqry <- toString(paste0("pcond.", condvars, " AS PREV_", condvars))
    
    
    ## 7.2a. Add LANDUSECD to SELECT query
    ###############################################################
    landuseqry <- paste0(
      "\nCASE WHEN c.PRESNFCD IS NULL THEN c.COND_STATUS_CD",
      "\n     ELSE c.PRESNFCD END AS LANDUSECD")
    cselectqry <- paste0(cselectqry, ", ",
                         landuseqry)
    
    prev_landuseqry <- paste0(
        "\nCASE WHEN pcond.PRESNFCD IS NULL THEN pcond.COND_STATUS_CD", 
                "\n     ELSE pcond.PRESNFCD END AS PREV_LANDUSECD")
    pcondselectqry <- paste0(pcondselectqry, ", ",
                             prev_landuseqry)
    condvars <- c(condvars, "LANDUSECD")
    
    
    ## 7.2b. Add FORTYPGRP to SELECT query
    ###############################################################
    addfortypgrp <- FALSE
    if (addfortypgrp) {
      ref_fortypgrp <- ref_codes[ref_codes$VARIABLE == "FORTYPCD", c("VALUE", "GROUPCD")]
      ftypqry <- classqry(classcol = "c.FORTYPCD",
                          fromval = ref_fortypgrp$VALUE,
                          toval = ref_fortypgrp$GROUPCD,
                          classnm = "FORTYPGRPCD")
      cselectqry <- paste0(cselectqry, ", ",
                           "\n ", ftypqry)
      pftypqry <- classqry(classcol = "pcond.FORTYPCD",
                           fromval = ref_fortypgrp$VALUE,
                           toval = ref_fortypgrp$GROUPCD,
                           classnm = "FORTYPGRPCD")
      pcondselectqry <- paste0(pcondselectqry, ", ",
                               "\n ", pftypqry)
      #condvars <- c(condvars, "FORTYPGRPCD")
    }
    
    
    ## 7.3. Build query for pltcondx
    ###############################################################
    pvars <- c(paste0("PREV_", pvars), pvars)
    condvars <- c(paste0("PREV_", condvars), condvars)
    pltcondflds <- unique(condvars, pvars)
    
    pltcondx.qry <- paste0("SELECT ", cselectqry, ", ",
                           "\n", pcondselectqry, ", ",
                           "\n", pselectqry, ", 1 AS TOTAL,",
                           "\n", pplotselectqry, ", 1 AS PREV_TOTAL",
                           ppcfromqry)
    dbqueries$pltcondx <- pltcondx.qry
    
    
    ## 7.4. Build WITH queries for pltcondx
    ###############################################################
    
    # ## Build WITH query for pltcondx, including pltids WITH query
    # pltcondxWITH.qry <- paste0(pltidsWITHqry, ", ",
    #                            "\n----- get pltcondx",
    #                            "\npltcondx AS",
    #                            "\n(", pltcondx.qry, ")")
    # dbqueriesWITH$pltcondxWITH <- pltcondxWITH.qry
    # 
    # ## Build WITH query for pltcondx, including pltidsadj WITH query
    # pltcondxadjWITHqry <- paste0(pltidsadjWITHqry, ", ",
    #                              "\n----- pltcondx",
    #                              "\npltcondx AS",
    #                              "\n(", pltcondx.qry, ")")
    # dbqueriesWITH$pltcondxadjWITH <- pltcondxadjWITHqry
    
    
    ## 7.5. If returndata or savedata, run query for pltcondx
    ##################################################################
    if (returndata || savedata) {
      pltcondxqry <- paste0(pltidsWITHqry,
                            "\n", pltcondx.qry)
      
      ## Run final plot/cond query, including pltidsqry
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
    }
    
    
    ##############################################################################
    ## 8. Build CASE statement for adding adjustment factors to SELECT
    ##############################################################################
    if (adj %in% c("samp", "plot")) {
      areawt <- "CONDPROP_UNADJ"
      propbasisnm <- findnm("PROP_BASIS", condflds, returnNULL=TRUE)
      
      if ("COND" %in% names(propvars) && adjvars['COND'] %in% names(adjfactors)) {
        adjcase <- adjvars['COND']
        
      } else if (is.null(propbasisnm)) {
        adjcase <- paste0("\nCASE pc.", propvars['MACR'], " IS NULL", 
                          " THEN ", adjvars['SUBP'], 
                          " ELSE ", adjvars['MACR'], " END")
      } else {
        adjcase <- paste0("\nCASE pc.", propbasisnm, 
                          " WHEN 'MACR' THEN ", adjvars['MACR'], 
                          " ELSE ", adjvars['SUBP'], " END")
      }
    }
    
   
    ##############################################################################
    ## 9.	Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE. 
    ##############################################################################  
    returnlst <- list(popdatindb = popdatindb,   ## logical. if TRUE, population data are in database
                      pltcondflds = pltcondflds, ## vector of field names in pltcondx
                      pltflds = pvars,
                      condflds = condvars,
                      cuniqueid = cuniqueid,     ## unique identifier of plots in pltcondx
                      condid = condid,           ## unique identifier of conditions
                      sccmid = sccmid,           ## unique identifier of plots in sccm
                      areawt = areawt,           ## variable names used to calcuate area
                      adjcase = adjcase,         ## CASE statement for summarizing area weights in estimates
                      adjvarlst = adjvars,       ## named vector of adjustment factor variable names
                      pjoinid = pltidsid)        ## joinid for joining pltids WITH query to other tables
    
    if (returndata || savedata) {
      returnlst$pltcondx <- pltcondx    ## data frame of plot/condition variables
      returnlst$pltidsadj <- pltidsadj  ## data frame of plot-level adjustment factors
      
      if (returndata) {
        returnlst$subpcprop <- subpcprop
      }
    } else {
      returnlst$pltcondx <- "pltcondx"
      returnlst$pltidsadj <- "pltidsadj"
      returnlst$subpcprop <- "subpcprop"
    }
    if (returnadj) {
      returnlst$adjfactors <- adjfactors   ## data frame with adjustment factors
    }
    

    ##############################################################################
    ## 10. Build and run queries for other necessary tables (if returndata/savedata = TRUE)
    ##############################################################################  
    if (returndata || savedata) {
      message("returning data needed for estimation...")
      
      # ## 10.1 Return and/or save plot data (pltx / PLOT)
      # ##################################################################
      # 
      # if (is.null(pltx) && !is.null(plotnm)) {
      #   
      #   ## Build plot SELECT query
      #   pselectqry <- toString(paste0(plota., c(puniqueid, pdoms2keep)))
      #   pplotselectqry <- toString(paste0(pplota., c(puniqueid, pdoms2keep)))
      #   
      #   ## Build final plot query, including pltidsqry
      #   pltqry <- paste0(getdataWITHqry,
      #                    "\n-------------------------------------------",
      #                    "\n SELECT ", pselectqry,
      #                    ppcfromqry,
      #                    "\n UNION",
      #                    "\n SELECT ", pplotselectqry,
      #                    ppcfromqry) 
      #   dbqueries$PLOT <- pltqry
      #   
      #   ## un final plot query, including pltidsqry
      #   if (datindb) {
      #     pltx <- tryCatch(
      #       DBI::dbGetQuery(dbconn, pltqry),
      #       error=function(e) {
      #         message(e,"\n")
      #         return(NULL)})
      #   } else {
      #     pltx <- tryCatch(
      #       sqldf::sqldf(pltqry, connection = NULL),
      #       error = function(e) {
      #         message(e,"\n")
      #         return(NULL) })
      #   }
      #   if (is.null(pltx) || nrow(pltx) == 0) {
      #     message("invalid plot query...")
      #     message(pltqry)
      #     return(NULL)
      #   }
      # }
      # 
      # ## Return and/or save plot data
      # setkeyv(setDT(pltx), puniqueid)
      # if (!is.null(getdataCNs)) {
      #   pltx <- pltx[pltx[[puniqueid]] %in% getdataCNs,]
      # }
      # 
      # ## Add to returnlst 
      # if (returndata) {
      #   returnlst$puniqueid <- puniqueid
      # }
      # ## Save data
      # if (savedata) {
      #   message("saving PLOT...")
      #   outlst$out_layer <- "PLOT"
      #   if (!append_layer) index.unique.plot <- puniqueid
      #   datExportData(pltx, 
      #                 savedata_opts = outlst)
      # }
      # 
      # 
      # ## 10.2 Return and/or save cond data (condx / COND)
      # ##################################################################
      # 
      # if (is.null(condx)) {
      #   
      #   ## Build cond FROM query
      #   #condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
      #   #condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
      #   
      #   ## Build cond SELECT query
      #   if (defaultVars) {
      #     condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
      #   } else {
      #     condvars <- "*"
      #   }
      #   condselectqry <- toString(paste0(conda., condvars))
      #   pcondselectqry <- toString(paste0(pconda., condvars))
      #   
      #   ## Build final cond query, including pltidsqry
      # 
      #   ## Build final plot query, including pltidsqry
      #   condqry <- paste0(getdataWITHqry,
      #                    "\n-------------------------------------------",
      #                    "\n SELECT ", condselectqry,
      #                    pcfromqry,
      #                    "\n UNION",
      #                    "\n SELECT ", pcondselectqry,
      #                    pcfromqry) 
      #   dbqueries$COND <- condqry
      #   
      #   ## Run final cond query, including pltidsqry
      #   if (datindb) {
      #     condx <- tryCatch(
      #       DBI::dbGetQuery(dbconn, condqry),
      #       error=function(e) {
      #         message(e,"\n")
      #         return(NULL)})
      #   } else {
      #     condx <- tryCatch(
      #       sqldf::sqldf(condqry, connection = NULL),
      #       error = function(e) {
      #         message(e,"\n")
      #         return(NULL) })
      #   }
      #   if (is.null(condx) || nrow(condx) == 0) {
      #     message("invalid cond query...")
      #     message(condqry)
      #     return(NULL)
      #   }
      #   
      #   ## Return and/or save cond data
      #   condkey <- c(cuniqueid, condid)
      #   setkeyv(setDT(condx), condkey)
      #   if (!is.null(getdataCNs)) {
      #     condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
      #   }
      # }
      # 
      # ## Add to returnlst 
      # if (returndata) {
      #   returnlst$cuniqueid <- cuniqueid
      #   returnlst$condx <- condx
      # }
      # ## Save data
      # if (savedata) {
      #   message("saving COND...")
      #   outlst$out_layer <- "COND"
      #   if (!append_layer) index.unique.cond <- condkey
      #   datExportData(condx, 
      #                 savedata_opts = outlst)
      # }
      # rm(condx)  


      ## 10.3. Return and/or save subplot change data (sccmx / SUBP_COND_CHNG_MTRX)
      ##################################################################
      if (is.null(sccmx)) {    

        ## Build sccm FROM query
        sccmjoinqry <- getjoinqry(sccmid, pltidsid, sccma., pltidsa.)
        sccmfromqry <- paste0(
          "\nJOIN ", SCHEMA., sccmxnm, " sccm ", sccmjoinqry)
       
        ## Build sccm SELECT query
        sccmselectqry <- toString(paste0(sccma., sccmvars))
       
        ## Build final sccm query, including pltidsqry
        sccmqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", sccmselectqry,
                          "\n FROM pltids",
                          sccmfromqry) 
        dbqueries$SUBP_COND_CHNG_MTRX <- sccmqry
        
        ## Run final sccm query, including pltidsqry
        if (datindb) {
          
          ## check for index
          idxchk <- checkidx(dbconn, sccmxnm, schema = schema)
          if (nrow(idxchk) == 0) {
            message("no index exists on ", sccmxnm, " query may be slow")
          }
          
          sccmx <- tryCatch(
            DBI::dbGetQuery(dbconn, sccmqry),
            error=function(e) {
              warning(e)
              return(NULL)})
        } else {
          sccmx <- tryCatch(
            sqldf::sqldf(sccmqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(sccmx) || nrow(sccmx) == 0) {
          message("invalid subp_cond_chng_matrx query...")
          message(sccmqry)
          return(NULL)
        } else {
          names(sccmx) <- toupper(names(sccmx))
        }
      }  
      
      ## Set key and subset subp_cond_chng_mtrx data
      sccmkey <- c("PLT_CN", "CONDID", "PREV_PLT_CN", "PREVCOND")
      setkeyv(setDT(sccmx), sccmkey)
      if (!is.null(getdataCNs)) { 
        sccmx <- sccmx[sccmx[[sccmid]] %in% getdataCNs,]
      }
      
      ## Append adjusted SUBPTYP_PROP_CHNG
      sccmx[pltidsadj, 
            SUBPTYP_PROP_ADJ := SUBPTYP_PROP_CHNG * ADJ_FACTOR_COND]
      

      ## Add to returnlst and remove sccmx object
      if (returndata) {
        returnlst$sccmx <- sccmx
        returnlst$sccmid <- sccmid
        returnlst$sccmflds <- sccmflds
      }
      ## Save data
      if (savedata) {
        message("saving SUBP_COND_CHNG_MTRX...")
        outlst$out_layer <- "subp_cond_chng_mtrx"
        if (!append_layer) index.unique.subp_cond_chng_mtrx <- sccmkey
        datExportData(sccmx, 
                      savedata_opts = outlst)
        if (!returndata) {
          poptablst <- c(poptablst, "subp_cond_chng_mtrx")
          sccmnm <- "subp_cond_chng_mtrx"
        }
      }
      rm(sccmx)
    

      ##########################################################################
      ## If popType = 'GRM', get remeasured tree and seed data queries for GRM
      ##    and TREE_GRM_COMPENENT, TREE_GRM_BEGIN and TREE_GRM_MIDPT queries
      ##########################################################################
      if (popType == "GRM") {
        
        ## 7.4. Return and/or save tree data (treex / TREE)
        ##################################################################
        if (!is.null(treenm)) {
          treea. <- "t."
          ptreea. <- "ptree."
          
          ## Check join variables
          treecnnm <- findnm("CN", treeflds, returnNULL = TRUE)
          prevtrecnnm <- findnm("PREV_TRE_CN", treeflds, returnNULL = TRUE)
          tprevcondnm <- findnm("PREVCOND", treeflds, returnNULL = TRUE)
          
          ## check key variables
          tsubp <- findnm("SUBP", treeflds, returnNULL = TRUE)
          ttree <- findnm("TREE", treeflds, returnNULL = TRUE)
          keyvars <- c(treecnnm, prevtrecnnm, tprevcondnm, tsubp, ttree)
          if (any(sapply(keyvars, is.null))) {
            keymiss <- keyvars[sapply(keyvars, is.null)]
            stop("missing key variables in tree data: ", toString(keymiss))
          }
          
          ## Build tree FROM query
          tfromqry <- paste0(pcfromqry, 
                             "\n JOIN ", SCHEMA., treenm, " t ON (", treea., tuniqueid, " = ", conda., cuniqueid, 
                             "\n    AND t.", condid, " = c.", condid, " AND ", treea., tprevcondnm, " = ", pconda., condid, ")",
                             "\n LEFT JOIN ", SCHEMA., treenm, " ptree ON (", ptreea., treecnnm, " = ", treea., prevtrecnnm, ")")
          
          
          ## Build tree SELECT query
          prev_treevars <- paste0("PREV_", treevars)
          prev_treevars <- treevars[!prev_treevars %in% treevars]
          
          tselectqry <- toString(paste0(treea., unique(c(tuniqueid, treevars))))
          ptreeselectqry <- toString(paste0(ptreea., prev_treevars, " AS PREV_", prev_treevars))

          ## Build final tree query, including pltidsqry
          # treeqry <- paste0(pltidsWITHqry,
          #                   "\n SELECT ", tselectqry,
          #                   tfromqry,
          #                   "\nUNION",
          #                   "\n SELECT ", ptreeselectqry, 
          #                   tfromqry)
          treeqry <- paste0(pltidsWITHqry,
                            "\n SELECT ", tselectqry, ", ",
                            "\n", ptreeselectqry,
                            tfromqry)
          dbqueries$tree <- treeqry
          
          ## Run final tree query, including pltidsqry
          if (datindb) {
            message("query ", treenm, "...")
            treex <- tryCatch(
              DBI::dbGetQuery(dbconn, treeqry),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {    
            treex <- tryCatch(
              sqldf::sqldf(treeqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(treex) || nrow(treex) == 0) {
            message("invalid tree query...")
            message(treeqry)
            return(NULL)
          } else {
            names(treex) <- toupper(names(treex))
          }
        
  
          ## Set key and subset tree data
          treedataCNs <- NULL
          treekey <- c(tuniqueid, condid, tsubp, ttree)
          setkeyv(setDT(treex), treekey)
          if (!is.null(getdataCNs)) { 
            treex <- treex[treex[[tuniqueid]] %in% getdataCNs,]
            if (!is.null(treecnnm)) {
              treedataCNs <- unique(treex[[treecnnm]])
            }
          }
          
          ## Add to returnlst and remove treex object
          if (returndata) {
            returnlst$treex <- treex
            returnlst$tuniqueid <- tuniqueid
            returnlst$treeflds <- treevars
          }
          
          ## Save data
          if (savedata) {
            message("saving TREE...")
            outlst$out_layer <- "TREE"
            if (!append_layer) index.unique.tree <- treekey
            datExportData(treex, 
                          savedata_opts = outlst)
            if (!returndata) {
              poptablst <- c(poptablst, "tree")
              treenm <- "tree"
              
              outlst$out_layer <- "ref_species"
              datExportData(ref_species,
                            savedata_opts = outlst)
            }
          }
          rm(treex)
        }
      
      
        ## 10.5. Return grm data (grmx / TREE_GRM_COMPONENT)
        ##############################################################
        if (!is.null(grmnm)) {
          grma. <- "grm."
          
          ## Check key variables
          grmtrecn <- findnm("TRE_CN", grmflds, returnNULL = TRUE)      
          keyvars <- grmtrecn
          if (any(sapply(keyvars, is.null))) {
            keymiss <- keyvars[sapply(keyvars, is.null)]
            stop("missing key variables in tree_grm_component data: ", toString(keymiss))
          }
          
          ## Build grm FROM query
          grmtrecn <- findnm("TRE_CN", grmflds, returnNULL = TRUE)      
          grmjoinqry <- getjoinqry(grmtrecn, treecnnm, grma., treea.)
          grmfromqry <- paste0(tfromqry, 
                               "\n LEFT JOIN ", SCHEMA., grmnm, " grm ", grmjoinqry)
          
          
          ## Build grm SELECT query
          grmselectqry <- toString(paste0("grm.", unique(c(grmid, grmvars))))
          
          ## Build final grm query, including pltidsqry
          grmqry <- paste0(pltidsWITHqry,
                           "\n SELECT ", grmselectqry,
                           grmfromqry)
          dbqueries$grm <- grmqry
          
          
          ## Run final grm query, including pltidsqry
          if (datindb) {
            message("query ", grmnm, "...")
            grmx <- tryCatch(
              DBI::dbGetQuery(dbconn, grmqry),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {    
            grmx <- tryCatch(
              sqldf::sqldf(grmqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }

          if (is.null(grmx) || nrow(grmx) == 0) {
            message("invalid tree_grm_component query...")
            message(grmqry)
            return(NULL)
          } else {
            names(grmx) <- toupper(names(grmx))
          }
        
          ## Set key and subset grm data
          grmkey <- c(grmtrecn)
          setkeyv(setDT(grmx), grmkey)
          if (!is.null(treedataCNs)) {
            grmx <- grmx[grmx[[grmtrecn]] %in% treedataCNs,]
          }
          
          ## Add to returnlst 
          if (returndata) {
            returnlst$grmx <- grmx
            returnlst$grmflds <- grmvars
          }
          
          ## Save data
          if (savedata) {
            message("saving TREE_GRM_COMPONENT...")
            outlst$out_layer <- "tree_grm_component"
            if (!append_layer) index.unique.tree_grm_component <- grmkey
            datExportData(grmx, 
                          savedata_opts = outlst)
            if (!returndata) {
              poptablst <- c(poptablst, "tree_grm_component")
              treenm <- "tree_grm_component"
            }
          }
          rm(grmx)
        }  
        
        
        ## 10.6. Return grm begin data (beginx / TREE_GRM_BEGIN)
        ##############################################################
        if (!is.null(beginnm)) {
          begina. <- "begin."
          
          
          ## Check key variables
          begintrecn <- findnm("TRE_CN", beginflds, returnNULL = TRUE)      
          keyvars <- begintrecn
          if (any(sapply(keyvars, is.null))) {
            keymiss <- keyvars[sapply(keyvars, is.null)]
            stop("missing key variables in tree_grm_begin data: ", toString(keymiss))
          }
          
          ## Build begin FROM query
          beginjoinqry <- getjoinqry(begintrecn, treecnnm, begina., treea.)
          beginfromqry <- paste0(tfromqry, 
                                 "\n LEFT JOIN ", SCHEMA., beginnm, " begin ", beginjoinqry)
          
          
          ## Build begin SELECT query
          beginselectqry <- toString(paste0(begina., unique(c(grmid, beginvars))))
          
          ## Build final begin query, including pltidsqry
          ###################################################
          beginqry <- paste0(pltidsWITHqry,
                             "\n SELECT distinct ", beginselectqry,
                             beginfromqry)
          dbqueries$begin <- beginqry
          
          
          ## Run final begin query, including pltidsqry
          if (datindb) {
            message("query ", beginnm, "...")
            beginx <- tryCatch(
              DBI::dbGetQuery(dbconn, beginqry),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {    
            beginx <- tryCatch(
              sqldf::sqldf(beginqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(beginx) || nrow(beginx) == 0) {
            message("invalid tree_grm_begin query...")
            message(beginqry)
            return(NULL)
          } else {
            names(beginx) <- toupper(names(beginx))
          }
        
          ## Set key and subset begin data
          beginkey <- c(begintrecn)
          setkeyv(setDT(beginx), beginkey)
          if (!is.null(treedataCNs)) {
            beginx <- beginx[beginx[[grmtrecn]] %in% treedataCNs,]
          }
          
          ## Add to returnlst 
          if (returndata) {
            returnlst$beginx <- beginx
            returnlst$beginflds <- beginvars
          }
          
          ## Save data
          if (savedata) {
            message("saving TREE_GRM_BEGIN...")
            outlst$out_layer <- "tree_grm_begin"
            if (!append_layer) index.unique.tree_grm_begin <- beginkey
            datExportData(beginx, 
                          savedata_opts = outlst)
            if (!returndata) {
              poptablst <- c(poptablst, "tree_grm_begin")
              beginnm <- "tree_grm_begin"
            }
          }
          rm(beginx)
        }  
        
       
        
        ## 10.7. Return grm begin data (midptx / TREE_GRM_MIDPT)
        ##############################################################
        if (!is.null(midptnm)) {
          midpta. <- "midpt."
        
          ## Check key variables
          midpttrecn <- findnm("TRE_CN", midptflds, returnNULL = TRUE)      
          keyvars <- midpttrecn
          if (any(sapply(keyvars, is.null))) {
            keymiss <- keyvars[sapply(keyvars, is.null)]
            stop("missing key variables in tree_grm_midpt data: ", toString(keymiss))
          }
          
          ## Build midpt FROM query
          midptjoinqry <- getjoinqry(midpttrecn, treecnnm, midpta., treea.)
          midptfromqry <- paste0(tfromqry, 
                                 "\nLEFT JOIN ", SCHEMA., midptnm, " midpt ", midptjoinqry)
          
          ## Build midpt SELECT query
          midptselectqry <- toString(paste0(midpta., unique(c(grmid, midptvars))))
          
          ## Build final midpt query, including pltidsqry
          midptqry <- paste0(pltidsWITHqry,
                             "\n SELECT distinct ", midptselectqry,
                             midptfromqry)
          dbqueries$midpt <- midptqry
          
          
          ## Run final midpt query, including pltidsqry
          if (datindb) {
            message("query ", midptnm, "...")
            midptx <- tryCatch(
              DBI::dbGetQuery(dbconn, midptqry),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {    
            midptx <- tryCatch(
              sqldf::sqldf(midptqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(midptx) || nrow(midptx) == 0) {
            message("invalid tree_grm_midpt query...")
            message(midptqry)
            return(NULL)
          } else {
            names(midptx) <- toupper(names(midptx))
          }
        
          ## Set key and subset midpt data
          midptkey <- c(midpttrecn)
          setkeyv(setDT(midptx), midptkey)
          if (!is.null(treedataCNs)) {
            midptx <- midptx[midptx[[grmtrecn]] %in% treedataCNs,]
          }
          
          ## Add to returnlst 
          if (returndata) {
            returnlst$midptx <- midptx
          }
          ## Save data
          if (savedata) {
            message("saving TREE_GRM_MIDPT...")
            outlst$out_layer <- "tree_grm_midpt"
            if (!append_layer) index.unique.tree_grm_midpt <- midptkey
            datExportData(midptx, 
                          savedata_opts = outlst)
            if (!returndata) {
              poptablst <- c(poptablst, "tree_grm_midpt")
              midptnm <- "tree_grm_midpt"
            }
          }
          rm(midptx)
        }  
        
      
        ## 10.8. Return seedling data (seedx / SEEDLING)
        ##############################################################
        if (isseed && !is.null(seednm)) {
          seeda. <- "s."
          
          
          ## Check key variables
          scondidnm <- findnm("CONDID", seedflds, returnNULL = TRUE)
          ssubp <- findnm("SUBP", seedflds, returnNULL = TRUE)
          keyvars <- c(suniqueid, scondidnm, ssubp)
          if (any(sapply(keyvars, is.null))) {
            keymiss <- keyvars[sapply(keyvars, is.null)]
            stop("missing key variables in seedling data: ", toString(keymiss))
          }
          
          ## Build seed FROM query
          seedjoinqry <- getjoinqry(c(suniqueid, condid), c(cuniqueid, condid), seeda., conda.)
          sfromqry <- paste0(pcfromqry, 
                             "\n JOIN ", SCHEMA., seednm, " s ",
                             seedjoinqry)
          
          ## Build seed SELECT query
          sselectqry <- toString(paste0("s.", unique(c(suniqueid, seedvars))))
          
          ## Build final seed query, including pltidsqry
          seedqry <- paste0(pltidsWITHqry,
                            "\n SELECT ", sselectqry,
                            sfromqry)
          dbqueries$seed <- seedqry
          
          ## Run final seed query, including pltidsqry
          if (datindb) {
            message("query ", seednm, "...")
            seedx <- tryCatch(
              DBI::dbGetQuery(dbconn, seedqry),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {    
            seedx <- tryCatch(
              sqldf::sqldf(seedqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(seedx) || nrow(seedx) == 0) {
            message("invalid seedling query...")
            message(seedqry)
            return(NULL)
          } else {
            names(seedx) <- toupper(names(seedx))
          }
        
          ## Return and/or save seedling data
          seedkey <- c(suniqueid, condid, ssubp)
          setkeyv(setDT(seedx), seedkey)
          if (!is.null(getdataCNs)) { 
            seedx <- seedx[seedx[[suniqueid]] %in% getdataCNs,]
          }
          
          ## Add to returnlst 
          if (returndata) {
            returnlst$seedx <- seedx
          }
          
          ## Save data
          if (savedata) {
            message("saving SEEDLING...")
            outlst$out_layer <- "seedling"
            if (!append_layer) index.unique.seedling <- seedkey
            datExportData(seedx, 
                          savedata_opts = outlst)
            if (!returndata) {
              poptablst <- c(poptablst, "seedling")
              seednm <- "seedling"
            }
          }
          rm(seedx)
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
        "\n JOIN ", SCHEMA., plotnm, " pplot ON (", pplota., puniqueid, " = ", plota., puniqueid, ")",
        "\n JOIN ", SCHEMA., condnm, " c ON (", conda., cuniqueid, " = ", plota., puniqueid, ")",
        "\n JOIN ", SCHEMA., condnm, " pcond ON (", pconda., cuniqueid, " = ", plota., prevpltcnnm, ")")
    } else {
      estpcfromqry <- paste0(
        "\n FROM pltcondf cond",
        "\n JOIN pltcondf pcond ON (", pconda., cuniqueid, " = ", conda., prevpltcnnm, ")")
    }
    
    ## Add from statement for subp_cond_chng_matrx
    estfromqry <- paste0(estpcfromqry,  
                         "\n JOIN ", SCHEMA., sccmnm, " sccm ON(", sccma., sccmid, " = c.", cuniqueid, 
                         "\n   AND ", sccma., prevpltcnnm, " = ", pconda., cuniqueid,
                         "\n   AND ", sccma., condid, " = ", conda., condid, 
                         "\n   AND ", sccma., prevcondnm, " = ", pconda., condid, ") ") 
    
    
    ## 13. Return data objects
    ######################################################################################
    if (popdatindb || savedata) {
      returnlst$pop_datsource <- pop_datsource
      returnlst$popdbinfo <- list(popconn = popconn, 
                                  pop_dsn = pop_dsn,
                                  pop_schema = pop_schema,
                                  pop_datsource = pop_datsource,
                                  poptablst = poptablst)
    }
    
    if (popdatindb) {
      returnlst$sccmx <- sccmnm
      returnlst$sscmflds <- sccmvars
      
      if (popType == "GRM") {
        if (!is.null(treenm)) {
          returnlst$treex <- treenm
          returnlst$tuniqueid <- tuniqueid
          returnlst$treeflds <- treevars
        }
        returnlst$grmx <- grmnm
        returnlst$grmflds <- grmvars
        
        returnlst$beginx <- beginnm
        returnlst$beginflds <- beginvars
        
        returnlst$midptx <- midptnm
        returnlst$midptflds <- midptvars
        
        if (!is.null(seednm)) {
          returnlst$seedx <- seednm
          returnlst$suniqueid <- suniqueid
          returnlst$seedflds <- seedvars
        }
      }  
      if (popType == "LULC") {
        returnlst$lulcx <- lulcx
      }
    }
    returnlst$dbqueries <- dbqueries
    returnlst$dbqueriesWITH <- dbqueriesWITH
    returnlst$estfromqry <- estfromqry
    
    return(returnlst)
  }
