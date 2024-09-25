check.popdataVOL <- 
  function(tabs, tabIDs, popType, 
           datindb, pltaindb, 
           pltidsqry,
           pltidsid, pltidvars, 
           plotnm,
           pdoms2keep = NULL, 
           pltidsadjindb = FALSE, 
           defaultVars = TRUE,
           pltassgnid, 
           pltassgnx, pltx,
           POP_PLOT_STRATUM_ASSGN,
           adj, ACI, plotlst, 
           pltfromqry, 
           condid = "CONDID", 
           areawt = "CONDPROP_UNADJ", areawt2 = NULL,
           MICRO_BREAKPOINT_DIA = 5, 
           MACRO_BREAKPOINT_DIA = NULL, 
           unitvars = NULL, 
           strunitvars = NULL, 
           nonsamp.cfilter = NULL, 
           cvars2keep = NULL,
           dbconn = NULL, SCHEMA. = "",
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
    ## 2.	Check if data are in a database (datindb) and if dbconn is valid.
    ## 3.	Get tables used in estimation from tabs.
    ## - PLOT; COND; 
    ## - TREE (if popType = 'VOL'); SEEDLING (if popType = 'VOL')
    ## 4.	Check for necessary variables in tables.
    ##    cond - (cuniqueid, condid, cvars2keep)
    ## 5.	Build query for adjustment factors and append to pltids.
    ## 5.1.	Check proportion variables, including area weight.
    ## 5.2.	Build WITH query defining pltids in population.
    ## 5.3.	Build ADJ query to append adjustment factors to pltids.
    ## Note: If adj = ‘none’, adjustment factors = 1.
    ## 5.3.1.	Build ADJqry FROM statement.
    ## 5.3.2.	Append filters to ADJqry WHERE statement (i.e., excluding nonresponse).
    ##            COND_STATUS_CD <> 5; 
    ##            NF_COND_STATUS_CD is NULL or NF_COND_STATUS_CD <> 5 (ACI=TRUE)
    ## 5.3.3.	Build and run ADJ query for adjustment factors based on popType.
    ## 5.3.4.     Build final query to append adjustment factors to pltids, including ADJ query.
    ## 5.4.	Run query to append adjustment factors to pltids (pltidsadj).
    ## 6.	Build and run queries for PLOT/COND (pltcondx).
    ## 6.1.	PLOT/COND data (pltcondx)
    ## 6.2.	Adjusted area weight table (areax)
    ## 7. Create return list with pltidsadj, condx, pltcondx, and adjfactors.
    ## 8. Build and run queries for other necessary tables (if returndata = TRUE)
    ## 8.1. Return and/or save plot data (pltx / PLOT)
    ## 8.2. Return and/or save cond data (condx / COND)
    ## 8.3. Return and/or save tree data (treex / TREE)
    ##      Note: if returdata=TRUE, append adjustment factors to treex
    ## 8.4. Return and/or save seedling data (seedx / SEEDLING)
    ##      Note: if returdata=TRUE, append adjustment factors to seedx
    ## 9. Check COND_STATUS_CD and generate table with number of conditions
    ## 9.1. Sampled conditions
    ## 9.2. Sampled nonforest conditions
    ## 10. Add to returnlst: condsampcnt, other tables (if returndata=TRUE), dbqueries
    ###################################################################################
    
    
    ## Set global variables
    treenm=seednm=condsampcnt=areawt2nm=adjcase <- NULL
    dbqueries=dbqueriesWITH <- list()
    cpropvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ")
    tpropvars <- list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
    diavar <- "DIA"
    pltcondindb <- datindb
    
    
    ##############################################################################
    ## 1. Define variables necessary for estimation
    ##############################################################################
    cvars2keep <- unique(c(cvars2keep, "PROP_BASIS"))
    
    
    ##############################################################################
    ## 2.	Check if data are in a database (datindb) and if dbconn is valid
    ##############################################################################
    if (datindb) {
      if (is.null(dbconn) || !DBI::dbIsValid(dbconn)) {
        message("the database connection is invalid")
        stop()
      } else {
        dbtablst <- DBI::dbListTables(dbconn)
      }
    }
    
    ##############################################################################
    ## 3. Get tables used in estimation from tabs
    ##############################################################################
    
    ## plot table
    plotnm <- plotlst$tabnm
    puniqueid <- plotlst$tabid
    pltx <- plotlst$tabx
    pltxnm <- ifelse (!is.null(pltx), "pltx", plotnm) 
    
    ## cond table
    condlst <- popTabchk("cond", tabtext = "cond", 
                         tabs, tabIDs, dbtablst, dbconn, datindb) 
    condnm <- condlst$tabnm
    condflds <- condlst$tabflds
    cuniqueid <- condlst$tabid
    condx <- condlst$tabx
    plota. <- "p."
    conda. <- "c."
    pltidsa. <- "pltids."
    returnlst <- list()
    
    if (is.null(condnm)) {
      stop("must include cond for estimation")
    }
    if (datindb && !pltaindb) {
      
      ## 3.1. Build cond FROM query
      if (!is.null(getdataWITHqry) && !is.null(getdataCNs)) {
        condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
        condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
        
        ## 3.2. Build cond SELECT query
        if (defaultVars) {
          condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
        } else {
          condvars <- "*"
        }
        condselectqry <- toString(paste0(conda., condvars))
        
        ## 3.3. Build final cond query, including getdataWITHqry
        condqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", condselectqry,
                          "\n FROM pltids",
                          condfromqry) 
        dbqueries$COND <- condqry
        
        ## 3.4. Run final cond query, including pltidsqry
        if (datindb) {
          condx <- tryCatch(
            DBI::dbGetQuery(dbconn, condqry),
            error=function(e) {
              message("invalid cond query...")
              message(e,"\n")
              return(NULL)})
        } else {
          condx <- tryCatch(
            sqldf::sqldf(condqry, connection = NULL),
            error = function(e) {
              message("invalid cond query...")
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(condx) || nrow(condx) == 0) {
          message(condqry)
          return(NULL)
        }
        
        ## 3.5. Return and/or save cond data
        condkey <- c(cuniqueid, condid)
        setkeyv(setDT(condx), condkey)
        
        ## Subset condx to plots in pltassgn
        condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
        
      } else {
        assign(condnm, DBI::dbReadTable(dbconn, condnm))
      }
      
      ## Add to returnlst 
      if (returndata) {
        #returnlst$condx <- condx
        returnlst$cuniqueid <- cuniqueid
      }
      ## Save data
      if (savedata) {
        message("saving COND...")
        outlst$out_layer <- "COND"
        if (!append_layer) index.unique.cond <- condkey
        datExportData(condx, 
                      savedata_opts = outlst)
      }
    }
    
    if (popType == "VOL") {
      
      ## tree table
      treelst <- popTabchk(c("tree"), tabtext = "tree",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
      treenm <- treelst$tabnm
      treeflds <- treelst$tabflds
      tuniqueid <- treelst$tabid
      treex <- treelst$tabx
      
      ## seed table
      seedlst <- popTabchk(c("seed", "seedling"), tabtext = "seed",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
      seednm <- seedlst$tabnm
      seedflds <- seedlst$tabflds
      suniqueid <- seedlst$tabid
      seedx <- seedlst$tabx
      
      if (is.null(treenm) && is.null(seednm)) {
        stop("must include tree and/or seed for estimation")
      }
    }
    
    ##############################################################################
    ## 4. Check for necessary variables in tables
    ##############################################################################
    condxnm <- ifelse (!is.null(condx), "condx", condnm) 
    
    ## 4.1. Cond table
    ##############################################################################
    
    ## 4.1.1. Check cuniqueid
    cuniqueid <- pcheck.varchar(var2check = cuniqueid, varnm = "cuniqueid", gui=gui,
                                checklst = condflds, caption = "Unique identifier of plot in cond",
                                warn = paste(cuniqueid, "not in cond"), stopifnull = TRUE)
    
    ## 4.1.2. Check condid
    condid <- pcheck.varchar(var2check = condid, varnm = "condid", gui=gui,
                             checklst = condflds, caption = "Unique identifier of conditions in cond",
                             warn = paste(condid, "not in cond"), stopifnull = TRUE)
    
    ## 4.1.3. Check cvars2keep in cond
    #######################################################################
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
    
    ##############################################################################
    ## 5. Build query to append adjustment factors to pltids
    ##############################################################################
    
    ## 5.1. Check proportion variables, including area weight 
    #######################################################################
    cpropvars <- check.PROPvars(condflds,
                                propvars = unlist(cpropvars))
    if (!areawt %in% cpropvars) {
      stop("areawt not in dataset: ", areawt)
    }
    propvars <- cpropvars
    if (popType == "VOL") {
      tpropvars <- check.PROPvars(condflds, treeflds = treeflds,
                                  propvars = unlist(tpropvars),
                                  MICRO_BREAKPOINT_DIA = MICRO_BREAKPOINT_DIA,
                                  MACRO_BREAKPOINT_DIA = MACRO_BREAKPOINT_DIA)
      cvars2keep <- unique(c(cvars2keep, tpropvars))
      propvars <- c(cpropvars, tpropvars)
      propvars <- propvars[!duplicated(propvars)]
    }
    
    ## Define adjvars
    adjvars <- unlist(list(COND="ADJ_FACTOR_COND", SUBP="ADJ_FACTOR_SUBP", 
                           MACR="ADJ_FACTOR_MACR", MICR="ADJ_FACTOR_MICR"))
    
    
    ## 5.2. Build WITH query, defining pltids in population
    #######################################################################
    pltidsWITH.qry <- paste0(
      "WITH",
      "\npltids AS",
      "\n(", pltidsqry, ")")
    pltidsvars <- c(puniqueid, unitvars)
    ## message(pltidsWITH.qry)
    
    
    ## 5.3. Build ADJ query to append adjustment factors to pltids
    #######################################################################
    
    
    ## Build ADJqry FROM statement
    if (!is.null(pltxnm)) {
      plota. <- "p."
      pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
      cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
      pcfromqry <- paste0(
        "\n FROM pltids",
        "\n JOIN ", SCHEMA., pltxnm, " p ", pjoinqry,
        "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry)
    } else {
      cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., pltidsa.)
      pcfromqry <- paste0(
        "\n FROM pltids",
        "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry)
    }
    
    pltidsjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., "plta.")
    pltidsfromqry <- paste0(
      "\nFROM pltids plta ",
      "\nJOIN ", SCHEMA., condxnm, " c ", pltidsjoinqry)
    
    
    ## Build ADJqry WHERE statement (i.e., excluding nonresponse)
    adjwhereqry <- NULL
    if (adj != "none") {
      adjwhereqry <- getADJwherePLOT(condflds)
    }
    
    ## 5.4. Build query for adjustment factors based on popType (ADJqry)
    ADJqry <- 
      getADJqry(popType = popType,
                adj = adj,
                propvars = propvars,
                adjfromqry = pcfromqry,
                pwhereqry = adjwhereqry,
                pltidsid = pltidsid,
                pltassgnid = pltassgnid,
                strunitvars = strunitvars,
                pltidsa. = "pltids.",
                propqry = NULL)
    #message(ADJqry)
    
    
    ## 5.5. Build final query for adjustment factors, including pltids WITH query
    adjfactors.qry <- paste0(
      pltidsWITH.qry,
      "\n-------------------------------------------",
      "\n", ADJqry
    )
    ## message(adjfactors.qry)
    
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
    }
    setkeyv(setDT(adjfactors), strunitvars)
    dbqueries$adjfactors <- adjfactors.qry
    
    
    ## Check adjustment factors
    #evalid <- 81901
    #FIADBpop <- getFIADBpop(evalid = evalid, dbconn = FIAconn)$pop_stratum
    #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="01")
    #adjfactors <- replacepopfun(adjfactors, FIADBpop)
    #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="01")
    
    
    ## 5.6. Build WITH query to append adjustment factors to pltids, including ADJ query
    pltidsadjWITH.qry <- paste0(
      pltidsWITH.qry, ", ",
      "\n----- calculate adjustment factors",
      "\nadjfactors AS ",
      "\n(", ADJqry, ")")
    
    
    ## 5.7. Build and run final query to append adjustment factors to pltids, including ADJ query
    adja. <- "adj."
    adjvars <- sapply(propvars, function(x) {
      ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
             ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
                     paste0(x, "_ADJ"))) })
    selectvars <- toString(c(paste0(pltidsa., pltidvars), paste0(adja., adjvars)))
    
    if (adj == "samp") {
      adjjoinqry <- getjoinqry(strunitvars, strunitvars, adja., pltidsa.)
    } else { ## adj = "plot"
      adjjoinqry <- getjoinqry(pltidsid, pltidsid, adja., pltidsa.)
    }
    
    ## Build pltidsadjFROM.qry
    pltidsadjFROM.qry <- paste0(
      "\nFROM pltids",
      "\nJOIN adjfactors adj ", adjjoinqry)
    
    
    ## Build pltidsadjqry query
    pltidsadj.qry <- paste0(
      pltidsadjWITH.qry,
      "\n-------------------------------------------",
      paste0("\nSELECT ", selectvars,
             pltidsadjFROM.qry)
    )
    ## message(pltidsadj.qry)
    
    
    ## Run query to identify plotids, including adjustment factors
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
      message("invalid pltids query...")
      message(pltidsadj.qry)
      return(NULL)
    }
    setkeyv(setDT(pltidsadj), pltidsid) 
    dbqueries$pltidsadj <- pltidsadj.qry   
    
    
    ## 5.8. Build WITH query to identify pltids, including adjustment factors
    if (pltidsadjindb) {
      pltidsadjWITH.qry <- paste0( 
        "WITH pltids AS",
        "\n(SELECT ", toString(c(paste0(pltidsa., pltidsid), adjvars)), 
        "\n FROM pltidsadj pltids",
        "\n WHERE projectid = '", projectid, "'",
        "\n   AND pop_typ = '", popType, "')")
    } else {
      pltidsadjWITH.qry <- paste0(
        pltidsadjWITH.qry, ", ",
        "\n----- calculate plot-level adjustment factors",
        "\npltidsadj AS",
        paste0("\n(SELECT ", toString(c(paste0(pltidsa., pltidsid), adjvars)),
               "\n FROM pltids",
               "\n JOIN adjfactors adj ", adjjoinqry), ")")
    } 
    dbqueriesWITH$pltidsWITH <- pltidsWITH.qry   
    dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry   
    
    
    ##############################################################################
    ## 6. Get PLOT/COND data, including areawt (pltcondx) 
    ##############################################################################
    
    ## 6.1. Build SELECT query for pltcondx query, including FORTYPGRP
    ##################################################################
    if (defaultVars) {
      pvars <- pdoms2keep
    } else {
      pvars <- "*"
    }
    pselectqry <- toString(paste0(plota., pvars))
    
    if (defaultVars) {
      condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
    } else {
      condvars <- "*"
    }
    cselectqry <- toString(paste0(conda., unique(c(condvars, cvars2keep))))
    pltcondflds <- unique(c(condvars, cvars2keep, pvars))
    
    ## 6.2.	Add FORTYPGRP to SELECT query
    ref_fortypgrp <- ref_codes[ref_codes$VARIABLE == "FORTYPCD", c("VALUE", "GROUPCD")]
    ftypqry <- classqry(classcol = "c.FORTYPCD",
                        fromval = ref_fortypgrp$VALUE,
                        toval = ref_fortypgrp$GROUPCD,
                        classnm = "FORTYPGRPCD")
    cselectqry <- paste0(cselectqry, ", ",
                         "\n ", ftypqry)
    pltcondflds <- c(pltcondflds, "FORTYPGRPCD")
    
    ## 6.3. Build query for pltcondx
    pltcondx.qry <- paste0("SELECT ", cselectqry, ", ",
                           "\n", pselectqry, ", 1 AS TOTAL",
                           pcfromqry)
    dbqueries$pltcondx <- pltcondx.qry
    
    ## 6.4. Build WITH query for pltcondx, including pltids WITH query
    pltcondxWITH.qry <- paste0(pltidsWITH.qry, ", ",
                               "\n----- pltcondx",
                               "\npltcondx AS",
                               "\n(", pltcondx.qry, ")")
    dbqueriesWITH$pltcondxWITH <- pltcondxWITH.qry
    
    ## 6.5. Build WITH query for pltcondx, including pltids WITH query, including adjustments
    pltcondxadjWITH.qry <- paste0(pltidsadjWITH.qry, ", ",
                                  "\n----- pltcondx",
                                  "\npltcondx AS",
                                  "\n(", pltcondx.qry, ")")
    dbqueriesWITH$pltcondxadjWITH <- pltcondxadjWITH.qry
    
    
    ## 6.6. If returndata or savedata, run query for pltcondx
    ##################################################################
    if (returndata || savedata) {
      pltcondindb <- FALSE
      
      pltcondxqry <- paste0("WITH pltids AS ",
                            "\n(", pltidsqry, ")",
                            "\n", pltcondx.qry)
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
    
    ## 6.7. Build CASE statement for adding adjustment factors to SELECT
    ##################################################################
    if (adj %in% c("samp", "plot")) {
      propbasisnm <- findnm("PROP_BASIS", condflds, returnNULL=TRUE)
      
      if (is.null(propbasisnm)) {
        adjcase <- paste0("CASE pc.", propvars['MACR'], " IS NULL", 
                          " THEN ", adjvars['SUBP'], 
                          " ELSE ", adjvars['MACR'], " END")
      } else {
        adjcase <- paste0("CASE pc.", propbasisnm, 
                          " WHEN 'MACR' THEN ", adjvars['MACR'], 
                          " ELSE ", adjvars['SUBP'], " END")
      }
    }
    
    ##############################################################################
    ## 7.	Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE. 
    ##############################################################################  
    returnlst <- append(returnlst, list(pltidsadj = pltidsadj,
                                        pltcondflds = pltcondflds,
                                        cuniqueid = cuniqueid, condid = condid, 
                                        adjfactors = adjfactors,
                                        adjcase = adjcase,
                                        adjvarlst = adjvars))
    
    if (returndata || savedata) {
      returnlst$pltcondx <- pltcondx
    } else {
      returnlst$pltcondx <- "pltcondx"
    }
    
    
    ##############################################################################
    ## 8. Build and run queries for other necessary tables (if returndata/savedata = TRUE) 
    ##############################################################################  
    if ((returndata || savedata) && popType == "VOL") {
      
      ## 8.1 Return and/or save plot data (pltx / PLOT)
      ##################################################################
      
      if (is.null(pltx)) {
        ## 8.1.1. Build plot FROM query
        plotjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
        plotfromqry <- paste0("\n JOIN ", SCHEMA., plotnm, " p ", plotjoinqry)
        
        ## 8.1.2. Build plot SELECT query
        pselectqry <- toString(paste0(plota., c(puniqueid, pdoms2keep)))
        
        ## 8.1.3. Build final plot query, including pltidsqry
        pltqry <- paste0(getdataWITHqry,
                         "\n-------------------------------------------",
                         "\n SELECT ", pselectqry,
                         "\n FROM pltids",
                         plotfromqry) 
        dbqueries$PLOT <- pltqry
        
        ## 8.1.4. Run final plot query, including pltidsqry
        if (datindb) {
          pltx <- tryCatch(
            DBI::dbGetQuery(dbconn, pltqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
        } else {
          pltx <- tryCatch(
            sqldf::sqldf(pltqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(pltx) || nrow(pltx) == 0) {
          message("invalid plot query...")
          message(pltqry)
          return(NULL)
        }
      }
      
      ## 8.1.5. Return and/or save plot data
      setkeyv(setDT(pltx), puniqueid)
      pltx <- pltx[pltx[[puniqueid]] %in% getdataCNs,]
      
      ## Add to returnlst 
      if (returndata) {
        returnlst$puniqueid <- puniqueid
      }
      ## Save data
      if (savedata) {
        message("saving PLOT...")
        outlst$out_layer <- "PLOT"
        if (!append_layer) index.unique.plot <- puniqueid
        datExportData(pltx, 
                      savedata_opts = outlst)
      }
      
      
      ## 8.2 Return and/or save cond data (condx / COND)
      ##################################################################
      
      if (is.null(condx)) {
        
        ## 8.2.1. Build cond FROM query
        condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
        condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
        
        ## 8.2.2. Build cond SELECT query
        if (defaultVars) {
          condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
        } else {
          condvars <- "*"
        }
        condselectqry <- toString(paste0(conda., condvars))
        
        ## 8.2.3. Build final cond query, including pltidsqry
        condqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", condselectqry,
                          "\n FROM pltids",
                          condfromqry) 
        dbqueries$COND <- condqry
        
        ## 8.2.4. Run final cond query, including pltidsqry
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
        }
        
        ## 8.2.5. Return and/or save cond data
        condkey <- c(cuniqueid, condid)
        setkeyv(setDT(condx), condkey)
        condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
      }
      
      ## Add to returnlst 
      if (returndata) {
        returnlst$cuniqueid <- cuniqueid
        returnlst$condx <- condx
      }
      ## Save data
      if (savedata) {
        message("saving COND...")
        outlst$out_layer <- "COND"
        if (!append_layer) index.unique.cond <- condkey
        datExportData(condx, 
                      savedata_opts = outlst)
      }
      rm(condx)  
      
      
      ## 8.3. Return and/or save tree data (treex / TREE)
      ##################################################################
      if (!is.null(treenm)) {
        treea. <- "t."
        
        ## 8.3.1. Check variables
        tsubp <- findnm("SUBP", treeflds, returnNULL = TRUE)
        ttree <- findnm("TREE", treeflds, returnNULL = TRUE)
        keyvars <- c(tsubp, ttree)
        if (any(sapply(keyvars, is.null))) {
          keymiss <- keyvars[sapply(keyvars, is.null)]
          stop("missing key variables in tree data: ", toString(keymiss))
        }
        
        ## 8.3.2. Build tree FROM query
        tjoinqry <- getjoinqry(tuniqueid, pltidsid, treea., pltidsa.)
        tfromqry <- paste0(
          "\n JOIN ", SCHEMA., treenm, " t ", tjoinqry)
        
        ## 8.3.3. Build tree SELECT query
        if (defaultVars) {
          treevars <- treeflds[treeflds %in% c(DBvars.default(istree=TRUE)$treevarlst,
                                               DBvars.default(istree=TRUE)$tsumvarlst)]
        } else {
          treevars <- "*"
        }
        tselectqry <- toString(paste0(treea., treevars))
        
        
        ## 8.3.4. Build final tree query, including pltidsqry
        treeqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", tselectqry,
                          "\n FROM pltids",
                          tfromqry) 
        dbqueries$TREE <- treeqry
        
        ## 8.3.5. Run final tree query, including pltidsqry
        if (datindb) {
          treex <- tryCatch(
            DBI::dbGetQuery(dbconn, treeqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
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
        }
        
        ## 8.3.6. Return and/or save tree data
        treekey <- c(tuniqueid, condid, tsubp, ttree)
        setkeyv(setDT(treex), treekey)
        treex <- treex[treex[[tuniqueid]] %in% getdataCNs,]
        
        ## Add to returnlst 
        if (returndata) {
          ## Append adjustment factors to tree data
          treex[pltidsadj, 
                tadjfac := ifelse(TPA_UNADJ > 50, get(adjvars[["MICR"]]),
                                  ifelse(TPA_UNADJ > 0 & TPA_UNADJ < 5, get(adjvars[["MACR"]]),
                                         get(adjvars[["SUBP"]])))]
          returnlst$treex <- treex
          returnlst$tuniqueid <- tuniqueid
        }
        ## Save data
        if (savedata) {
          message("saving TREE...")
          outlst$out_layer <- "TREE"
          if (!append_layer) index.unique.tree <- treekey
          datExportData(treex, 
                        savedata_opts = outlst)
        }
        #rm(treex)
      } 
      
      
      ## 8.4. Return and/or save seedling data (seedx / SEEDLING)
      ##################################################################
      if (!is.null(seednm)) {
        seeda. <- "s."
        
        ## 8.4.1. Check variables
        scondid <- findnm("CONDID", seedflds, returnNULL = TRUE)
        ssubp <- findnm("SUBP", seedflds, returnNULL = TRUE)
        keyvars <- c(scondid, ssubp)
        if (any(sapply(keyvars, is.null))) {
          keymiss <- keyvars[sapply(keyvars, is.null)]
          stop("missing key variables in seedling data: ", toString(keymiss))
        }
        
        ## 8.4.2. Build seedling FROM query
        sjoinqry <- getjoinqry(suniqueid, pltidsid, seeda., pltidsa.)
        sfromqry <- paste0(
          "\n JOIN ", SCHEMA., seednm, " s ", sjoinqry)
        
        ## 8.2.3. Build seedling SELECT query
        if (defaultVars) {
          seedvars <- seedflds[seedflds %in% c(DBvars.default(isseed=TRUE)$seedvarlst,
                                               DBvars.default(isseed=TRUE)$ssumvarlst)]
        } else {
          seedvars <- "*"
        }
        sselectqry <- toString(paste0(seeda., seedvars))
        
        
        ## 8.4.4. Build final seedling query, including pltidsqry
        seedqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", sselectqry,
                          "\n FROM pltids",
                          sfromqry) 
        dbqueries$SEEDLING <- seedqry
        
        ## 8.4.5. Run final seedling query, including pltidsqry
        if (datindb) {
          seedx <- tryCatch(
            DBI::dbGetQuery(dbconn, seedqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
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
        }
        
        ## 8.4.6. Return and/or save seedling data
        seedkey <- c(suniqueid, scondid, ssubp)
        setkeyv(setDT(seedx), seedkey)
        seedx <- seedx[seedx[[tuniqueid]] %in% getdataCNs,]
        
        ## Add to returnlst 
        if (returndata) {
          ## Append adjustment factors to tree data
          seedx[pltidsadj, tadjfac := get(adjvars[["MICR"]])]
          
          returnlst$seedx <- seedx
          returnlst$suniqueid <- suniqueid
        }
        ## Save data
        if (savedata) {
          message("saving SEEDLING...")
          outlst$out_layer <- "SEEDLING"
          if (!append_layer) index.unique.seed <- seedkey
          datExportData(seedx, 
                        savedata_opts = outlst)
        }
        rm(seedx)
      } 
    }
    
    
    ##############################################################################
    ## 9. Check COND_STATUS_CD and generate table with number of conditions
    ##############################################################################
    
    ## condfromqry
    condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
    condfromqry <- paste0("\nJOIN ", SCHEMA., condnm, " c ", condjoinqry)
    
    ## 9.1. Sampled conditions
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
      if (!pltcondindb) {
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
        
        if (pltcondindb) {      
          condsampcnt <- tryCatch(
            DBI::dbGetQuery(dbconn, condsampcntqry),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        } else {
          condsampcnt <- tryCatch(
            sqldf::sqldf(condsampcntqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        
        if (is.null(condsampcnt)) {
          message("invalid condsampcnt query")
          message(condsampcntqry)
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
    
    ## 9.2. Sampled nonforest conditions
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
        
        if (!pltcondindb) {
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
          
          if (pltcondindb) {      
            nfcondsampcnt <- tryCatch(
              DBI::dbGetQuery(dbconn, nfcondsampcntqry),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          } else {
            nfcondsampcnt <- tryCatch(
              sqldf::sqldf(nfcondsampcntqry, connection = NULL),
              error = function(e) {
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(nfcondsampcnt)) {
            message("invalid nfcondsampcnt query")
            message(nfcondsampcntqry)
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
    
    ## 10. Build est FROM statement
    
    ## Build plot/cond from query
    if (datindb) {
      estfromqry <- paste0(
        "\n FROM ", SCHEMA., plotnm, " p",
        "\n JOIN ", SCHEMA., condnm, " c ON (", conda., cuniqueid, " = ", plota., puniqueid, ")")
    } else {
      estfromqry <- paste0(
        "\n FROM pltcondf cond")
    }
    
    
    ## 11. Return data objects
    ######################################################################################
    if (!returndata) {
      
      if (popType == "VOL") {
        if (!is.null(treenm)) {
          returnlst$treex <- treenm
          returnlst$tuniqueid <- tuniqueid
        }
        if (!is.null(seednm)) {
          returnlst$seedx <- seednm
          returnlst$suniqueid <- suniqueid
        }
      }
    }
    returnlst$dbqueries <- dbqueries
    returnlst$dbqueriesWITH <- dbqueriesWITH
    returnlst$estfromqry <- estfromqry
    
    return(returnlst)
  }
