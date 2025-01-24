check.popdataCHNG <- 
  function(tabs, tabIDs, popType, 
           datindb, pltaindb, 
           pltidsWITHqry,
           pltidsid, pltidvars, 
           plotnm,
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
           dbconn = NULL, SCHEMA. = "",
           getdataWITHqry = NULL,
           getdataCNs = NULL,
           returndata = FALSE, 
           savedata = FALSE, 
           outlst = NULL,
           gui = FALSE){
    
    ##############################################################################
    ## DESCRIPTION: Checks data inputs for CHNG estimation
    ## Define variables necessary for estimation:
    ## - cvars2keep = c('PROP_BASIS', 'COND_NONSAMPLE_REASN_CD')
    ## Check if data are in a database (datindb) and if dbconn is valid.
    ## 1.	Get table names used in estimation from tabs.
    ## - PLOT; COND; SUBP_COND_CHNG_MTRX
    ## - TREE (if popType = 'GRM'); SEEDLING (if popType = 'GRM')
    ## - TREE_GRM_COMPONENT, TREE_GRM_BEGIN, TREE_GRM_MIDPT (if popType = 'GRM')
    ## 2.	Check for necessary variables in tables.
    ##    plot - PREV_PLT_CN
    ##    cond - (cuniqueid, condid, cvars2keep)
    ##    subp_cond_chng_mtrx - PREVCOND
    ## 3. Build query for adjustment factors and append to pltids
    ## 4. Build and run queries for PLOT/COND (pltcondx)
    ## 5. Build CASE statement for adding adjustment factors to SELECT
    ## 6. Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE
    
    ## 7. Build and run queries for other necessary tables (if returndata/savedata = TRUE)
    ## 7.1 Return and/or save plot data (pltx / PLOT)
    ## 7.2 Return and/or save cond data (condx / COND)
    ## 7.3. Return and/or save subplot change data (sccmx / SUBP_COND_CHNG_MTRX)
    ## 7.4. Return and/or save tree data (treex / TREE)
    ## 7.5. Return grm data (grmx / TREE_GRM_COMPONENT)
    ## 7.6 Return grm begin data (beginx / TREE_GRM_BEGIN)
    ## 7.7. Return grm begin data (midptx / TREE_GRM_MIDPT)
    ## 7.8. Return seedling data (seedx / SEEDLING)
    ## 
    ## 8. Check COND_STATUS_CD and generate table with number of conditions
    ## 8.1. Sampled conditions
    ## 8.2. Sampled nonforest conditions
    ## 
    ## 9. Build FROM statement for estimation queries
    ## 10. Return data objects
    ###################################################################################
    
    ## Set global variables
    grmx=beginx=midptx=SUBPTYP_PROP_ADJ=SUBPTYP_PROP_CHNG=ADJ_FACTOR_COND <- NULL
    dbqueries=dbqueriesWITH <- list()
    cpropvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ")
    tpropvars <- list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
    diavar <- "DIA"
    pltcondindb <- datindb
    
    
    ###################################################################################
    ## Define variables necessary for estimation
    ###################################################################################
    cvars2keep <- unique(c(cvars2keep, "PROP_BASIS", "COND_NONSAMPLE_REASN_CD"))
    
    
    ##############################################################################
    ## Check if data are in a database (datindb) and if dbconn is valid
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
    ## 1. Get table names used in estimation from tabs
    ##############################################################################
    
    ## plot table
    plotnm <- plotlst$tabnm
    puniqueid <- plotlst$tabid
    pltx <- plotlst$tabx
    pltflds <- plotlst$tabflds
    if (is.null(pltx)) {
      pltxnm <- plotnm
    } else {
      pltxnm <- "pltx"
    }
    
    ## cond table
    tabnames <- c("condu", "cond")
    condlst <- popTabchk(tabnames, tabtext = "cond", 
                         tabs, tabIDs, dbtablst, dbconn, datindb) 
    condnm <- condlst$tabnm
    condflds <- condlst$tabflds
    cuniqueid <- condlst$tabid
    condx <- condlst$tabx
    plota. <- "p."
    conda. <- "c."
    sccma. <- "sccm."
    pltidsa. <- "pltids."

    ## subp_cond_chng_mtrx table
    sccmlst <- popTabchk(c("subp_cond_chng_mtrx", "sccm"), 
                         tabtext = "subp_cond_chng_mtrx",
                         tabs, tabIDs, dbtablst, dbconn, datindb)
    sccmnm <- sccmlst$tabnm
    sccmflds <- sccmlst$tabflds
    sccmid <- sccmlst$tabid
    sccmx <- sccmlst$tabx
    if (!is.null(sccmx) && is.data.frame(sccmx)) {
      sccmnm <- "sccmx"
    }
    
    if (is.null(condnm)) {
      stop("must include cond for CHNG estimates")
    }
    
    ## If pltassgn is not in database but all other tables are in database, 
    ## we need to load tables to memory and subset for population queries.
    if (datindb && !pltaindb) {
      
      ## Build cond FROM query
      if (!is.null(getdataWITHqry) && !is.null(getdataCNs)) {
        condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
        condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
        
        ## Build cond SELECT query
        if (defaultVars) {
          condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
        } else {
          condvars <- "*"
        }
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
          message(condqry)
          return(NULL)
        }
        
        ## Return and/or save cond data
        condkey <- c(cuniqueid, condid)
        setkeyv(setDT(condx), condkey)
        
        ## Subset condx to plots in pltassgn
        condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
        
        
        ## Build sccm FROM query
        sccmjoinqry <- getjoinqry(sccmid, pltidsid, sccma., pltidsa.)
        sccmfromqry <- paste0("\n JOIN ", SCHEMA., sccmnm, " c ", sccmjoinqry)
        
        ## Build sccm SELECT query
        if (defaultVars) {
          sccmvars <-  sccmflds[sccmflds %in% DBvars.default()$sccmvarlst]
        } else {
          sccmvars <- "*"
        }
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
        }
        
        ## Set key on data.table
        sccmkey <- c("PLT_CN", "CONDID", "PREV_PLT_CN", "PREVCOND")
        setkeyv(setDT(sccmx), sccmkey)
        
        ## Subset condx to plots in pltassgn
        sccmx <- sccmx[sccmx[[sccmid]] %in% getdataCNs,]
        
      } else {
        assign(condnm, DBI::dbReadTable(dbconn, condnm))
        assign(sccmnm, DBI::dbReadTable(dbconn, sccmnm))
      }
      

      ## Save data
      if (savedata) {
        message("saving SUBP_COND_CHNG_MTRX...")
        outlst$out_layer <- "SUBP_COND_CHNG_MTRX"
        if (!append_layer) index.unique.sccm <- sccmkey
        datExportData(sccmx, 
                      savedata_opts = outlst)
      }
    }
    
    if (popType == "GRM") {
      
      ## tree table
      treelst <- popTabchk(c("tree"), tabtext = "tree",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
      treenm <- treelst$tabnm
      treeflds <- treelst$tabflds
      tuniqueid <- treelst$tabid
      treex <- treelst$tabx
      
      if (is.null(treenm)) {
        stop("must include tree for estimation")
      }
      
      ## seedling table
      seedlst <- popTabchk(c("seed", "seedling"), tabtext = "seed",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
      seednm <- seedlst$tabnm
      seedflds <- seedlst$tabflds
      suniqueid <- seedlst$tabid
      seedx <- seedlst$tabx
      
      
      ## tree_grm_component table
      grmlst <- popTabchk(c("grm", "tree_grm_component"), tabtext = "tree_grm_component",
                          tabs, tabIDs, dbtablst, dbconn, datindb)
      grmnm <- grmlst$tabnm
      grmflds <- grmlst$tabflds
      grmid <- grmlst$tabid
      grmx <- grmlst$tabx
      
      if (is.null(grmnm)) {
        stop("must include tree_grm_component for estimation")
      }
      
      ## tree_grm_begin table
      beginlst <- popTabchk(c("begin", "tree_grm_begin"), tabtext = "tree_grm_begin",
                            tabs, tabIDs, dbtablst, dbconn, datindb)
      beginnm <- beginlst$tabnm
      beginflds <- beginlst$tabflds
      beginid <- beginlst$tabid
      beginx <- beginlst$tabx
      
      if (is.null(beginnm)) {
        stop("must include tree_grm_begin for estimation")
      }
      
      ## tree_grm_midpt table
      midptlst <- popTabchk(c("midpt", "tree_grm_midpt"), tabtext = "tree_grm_midpt",
                            tabs, tabIDs, dbtablst, dbconn, datindb)
      midptnm <- midptlst$tabnm
      midptflds <- midptlst$tabflds
      midptid <- midptlst$tabid
      midptx <- midptlst$tabx
      
      if (is.null(midptnm)) {
        stop("must include tree_grm_midpt for estimation")
      }
    }
    
    ##############################################################################
    ## 2. Check for necessary variables in tables
    ##############################################################################
    condxnm <- ifelse (!is.null(condx), "condx", condnm) 
    
    ## plot table
    ##############################################################################
    prevpltcnnm <- findnm("PREV_PLT_CN", pltflds, returnNULL = TRUE)
    if (is.null(prevpltcnnm)) {
      message("need PREV_PLT_CN in plot table for CHNG estimates")
      stop()
    }
    
    ## cond table
    ##############################################################################
    
    ## Check cuniqueid
    cuniqueid <- pcheck.varchar(var2check = cuniqueid, varnm="cuniqueid", gui=gui,
                                checklst = condflds, caption="Unique identifier of plot in cond",
                                warn = paste(cuniqueid, "not in cond"), stopifnull = TRUE)
    
    ## Check condid
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
    
    ## subp_cond_chng_mtrx table
    prevcondnm <- findnm("PREVCOND", sccmflds, returnNULL = TRUE)
    if (is.null(prevcondnm)) {
      message("need PREVCOND in subp_cond_chng_mtrx table for CHNG estimates")
    }
    
    
    ##############################################################################
    ## 3. Build query for adjustment factors and append to pltids
    ##############################################################################
    
    ## Check proportion variables, including area weight 
    #######################################################################
    cpropvars <- check.PROPvars(condflds,
                                propvars = unlist(cpropvars))
    if (!areawt %in% sccmflds) {
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
    
    
    ## Build and run query to calculate adjustment factors (ADJqry) 
    #######################################################################
    pca. <- "pc."
    sccma. <- "sccm."
    
    
    ## Build query to summarize sampled summarize subplot proportions (subcprop) 
    #######################################################################
   
    ## Build FROM query
    plota. <- "p."
    pplota. <- "pplot."
    conda. <- "c."
    pconda. <- "pcond."
    sccma. <- "sccm."
    
    ## Build FROM query
    pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
    pfromqry <- paste0("\n FROM pltids",
                       "\n JOIN ", SCHEMA., plotnm, " p ", pjoinqry)

    pplotjoinqry <- getjoinqry(puniqueid, prevpltcnnm, pplota., plota.)
    cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
    pcondjoinqry <- getjoinqry(cuniqueid, prevpltcnnm, pconda., plota.)
    pcfromqry <- paste0(
      pfromqry,
      #"\n JOIN ", SCHEMA., plotnm, " pplot ", pplotjoinqry,
      "\n JOIN ", SCHEMA., condnm, " c ", cjoinqry,
      "\n JOIN ", SCHEMA., condnm, " pcond ", pcondjoinqry)

    ## Add FROM statement for subp_cond_chng_matrx
    sccmjoinqry <- getjoinqry(c(sccmid, prevpltcnnm), c(pltidsid, prevpltcnnm), sccma., plota.)
    sccmjoinqry <- paste0(
      sccmjoinqry,
      "\n          AND ", sccma., condid, " = ", conda., condid, 
      " AND ", sccma., prevcondnm, " = ", pconda., condid)
    #message(sccmjoinqry)
   
    sccmfromqry <- paste0(pcfromqry,  
                      "\n JOIN ", SCHEMA., sccmnm, " sccm ", sccmjoinqry) 
    
    
    ## Build WHERE statement (i.e., excluding nonresponse)
    adjwhereqry <- NULL
    if (adj != "none") {
      adjwhereqry <- getADJwherePLOT(condflds, conda.="c.")
      
      ## Other filters for change
      #################################################################
      condpropnm <- findnm("CONDPROP_UNADJ", condflds, returnNULL = TRUE)
      nonsampreasonnm <- findnm("COND_NONSAMPLE_REASN_CD", condflds, returnNULL = TRUE)
      propbasisnm <- findnm("PROP_BASIS", condflds, returnNULL = TRUE)
      subtypnm <- findnm("SUBPTYP", sccmflds, returnNULL = TRUE)
      
      if (any(is.null(condpropnm), is.null(propbasisnm), is.null(subtypnm), 
              is.null(nonsampreasonnm))) {
        message("must include SUBTYP for CHNG estimates")
      } else {
        chg.filter <- paste0(
          conda., condpropnm, " IS NOT NULL",
          "\n   AND ((", sccma., subtypnm, " = 3 AND ", conda., propbasisnm, " = 'MACR')",
          "\n       OR (", sccma., subtypnm, " = 1 AND ", conda., propbasisnm, " = 'SUBP'))",
          "\n   AND COALESCE(", conda., nonsampreasonnm, ", 0) = 0",  
          "\n   AND COALESCE(", pconda., nonsampreasonnm, ", 0) = 0")
        if (is.null(adjwhereqry)) {
          adjwhereqry <- chg.filter
        } else {
          adjwhereqry <- paste0(adjwhereqry, 
                                "\n   AND ", chg.filter)
        }	
      }
    }  ## END adj = 'none'
    
    
    ## Run sumpropCHNGqry to build SELECT statement and final query
    sumpropqry <- sumpropCHNGqry(fromqry = sccmfromqry, 
                                 whereqry = adjwhereqry,
                                 ACI = ACI,
                                 frompltcondx = FALSE,
                                 selectvars = NULL,
                                 SCHEMA. = SCHEMA.)
    #message(sumpropqry) 
    
    
    ## Build and run query to calculate adjustment factors (ADJqry) 
    #######################################################################
    
    ## Build FROM query
    adjjoinqry <- getjoinqry(sccmid, pltidsid, "c.", pltidsa.)
    adjfromqry <- paste0("\n FROM pltids",
                         "\n JOIN subpcprop c ", adjjoinqry)
    
    ## Get getADJqry function 
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
    #message(ADJqry)
    
    
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
    if (adj == "samp") {
      setkeyv(setDT(adjfactors), strunitvars)
    } else {
      setkeyv(setDT(adjfactors), pltidsid)
    }
    dbqueries$adjfactors <- adjfactors.qry
 
    
    ## Check with FIADB population data - VOL
    #source("C:/_tsf/_GitHub/FIESTAnalysis/R/IEVALIDator_compare.R") 
    #FIADBpop <- getFIADBpop(state, evaltype = "03", evalyr, dbconn=dbconn)$pop_stratum
    #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="03")
    #popVOL_compare
    

    ## Build and run final query to append adjustment factors to pltids, including ADJ query
    if (adj == "samp") {
      adja. <- "adj."
      adjvars <- sapply(propvars, function(x) {
      ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
           ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
                   paste0(x, "_ADJ"))) })
      selectvars <- toString(c(paste0(pltidsa., pltidsid), paste0(adja., adjvars)))
      
      
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


      ## Build pltcondxadjFROM.qry
      adjjoinqry <- getjoinqry(strunitvars, strunitvars, adja., pltidsa.)
      pltidsadjFROM.qry <- paste0(
        "\nFROM pltids",
        "\nJOIN adjfactors adj ", adjjoinqry)
  
      
      ## Build pltidsadj.qry
      pltidsadj.qry <- paste0(
        adjfactorsWITHqry,
        "\n-------------------------------------------",
        paste0("\nSELECT ", selectvars,
               pltidsadjFROM.qry))
      ## message(pltidsadj.qry)
    
    
      ## Build WITH query to identify pltids, including adjustment factors
      pltidsadjWITHqry <- paste0(
        adjfactorsWITHqry, ",",
        "\n----- get plot-level adjustment factors",
        "\npltidsadj AS ",
        "\n(SELECT ", selectvars,
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
      
        
      ## Build WITH query to identify pltids, including adjustment factors
      pltidsadjWITHqry <- paste0(
        pltidsWITHqry,
        "\n----- sum sampled subplot proportions",
        "\nsubpcprop AS ",
        "\n(", sumpropqry, ")",
        "\n----- calculate plot-level adjustment factors",
        "\n", ADJqry)
    
    }
    dbqueriesWITH$pltidsWITH <- pltidsWITHqry   
    dbqueriesWITH$pltidsadjWITH <- pltidsadjWITHqry   

    
    ## Run query to identify plotids, including adjustment factors
    if (returndata || savedata) {
      if (datindb) {
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
      }
      setkeyv(setDT(pltidsadj), pltidsid)
    }
    dbqueries$pltidsadj <- pltidsadj.qry
    
 
    ##############################################################################
    ## 4. Build and run queries for PLOT/COND (pltcondx).
    ##############################################################################
    pltidsa. <- "pltids."
    
    
    ## Build FROM query for pltcondx query
    ##################################################################
    pplotfromqry <- paste0(
      pfromqry,
      "\n JOIN ", SCHEMA., plotnm, " pplot ", pplotjoinqry)
    
    ppcfromqry <- paste0(
      pplotfromqry,
      "\n JOIN ", SCHEMA., condnm, " c ", cjoinqry,
      "\n JOIN ", SCHEMA., condnm, " pcond ", pcondjoinqry)
    
    
    ## Build SELECT query for pltcondx query
    ##################################################################
    if (defaultVars) {
      pvars <- pdoms2keep
    } else {
      pvars <- "*"
    }
    pselectqry <- toString(paste0(plota., pvars[pvars != "PREV_PLT_CN"]))
    pplotselectqry <- toString(paste0(pplota.,  pvars, " AS PREV_", pvars))
    
    if (defaultVars) {
      condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
    } else {
      condvars <- "*"
    }
    cvars <- unique(c(condvars, cvars2keep))
    cselectqry <- toString(paste0(conda., cvars))
    pcondselectqry <- toString(paste0("pcond.", cvars, " AS PREV_", cvars))
    pltcondflds <- unique(c(cvars, pvars))
    
    ## Add FORTYPGRP to SELECT query
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
      
      pltcondflds <- c(pltcondflds, "FORTYPGRPCD")
    }
    
    ## Build query for pltcondx
    pltcondx.qry <- paste0("SELECT ", cselectqry, ", ",
                           "\n", pcondselectqry, ", ",
                           "\n", pselectqry, ", 1 AS TOTAL,",
                           "\n", pplotselectqry, ", 1 AS PREV_TOTAL",
                           ppcfromqry)
    pltcondxqry <- paste0(pltidsWITHqry,
                          "\n", pltcondx.qry)
    dbqueries$pltcondx <- pltcondxqry
    
    ## Build WITH query for pltcondx, including pltids WITH query
    pltcondxWITH.qry <- paste0(pltidsWITHqry, ", ",
                               "\n----- get pltcondx",
                               "\npltcondx AS",
                               "\n(", pltcondx.qry, ")")
    dbqueriesWITH$pltcondxWITH <- pltcondxWITH.qry
    
    ## Build WITH query for pltcondx, including pltidsadj WITH query
    pltcondxadjWITHqry <- paste0(pltidsadjWITHqry, ", ",
                                 "\n----- pltcondx",
                                 "\npltcondx AS",
                                 "\n(", pltcondx.qry, ")")
    dbqueriesWITH$pltcondxadjWITH <- pltcondxadjWITHqry
    
    
    ## If returndata or savedata, run query for pltcondx
    ##################################################################
    if (returndata || savedata) {
      pltcondindb <- FALSE
      
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
    
  
    ## 5. Build CASE statement for adding adjustment factors to SELECT
    ##################################################################
    if (adj %in% c("samp", "plot")) {
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
    ## 6.	Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE. 
    ##############################################################################  
    returnlst <- list(pltcondflds = pltcondflds,
                      cuniqueid = cuniqueid, 
                      condid = condid, sccmid = sccmid,
                      adjfactors = adjfactors,
                      adjcase = adjcase,
                      adjvarlst = adjvars)
    
    if (returndata || savedata) {
      returnlst$pltcondx <- pltcondx
      returnlst$pltidsadj <- pltidsadj
    } else {
      returnlst$pltcondx <- "pltcondx"
      returnlst$pltidsadj <- "pltidsadj"
    }
    

    ##############################################################################
    ## 7. Build and run queries for other necessary tables (if returndata = TRUE) 
    ##############################################################################  
    if (returndata || savedata) {
      message("returning data needed for estimation...")
      
      # ## 7.1 Return and/or save plot data (pltx / PLOT)
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
      # ## 7.2 Return and/or save cond data (condx / COND)
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
      

      ## 7.3. Return and/or save subplot change data (sccmx / SUBP_COND_CHNG_MTRX)
      ##################################################################
      if (is.null(sccmx)) {    

        ## Build sccm FROM query
        sccmjoinqry <- getjoinqry(sccmid, pltidsid, sccma., pltidsa.)
        sccmfromqry <- paste0(
          "\nJOIN ", SCHEMA., sccmnm, " sccm ", sccmjoinqry)
       
        ## Build sccm SELECT query
        if (defaultVars) {
          sccmvars <- "*"
        }
        #sccmselectqry <- toString(c(paste0("pltids.", strunitvars), paste0(sccma., sccmvars)))
        sccmselectqry <- toString(paste0(sccma., sccmvars))
       
        ## Build final sccm query, including pltidsqry
        sccmqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", sccmselectqry,
                          "\n FROM pltids",
                          sccmfromqry) 
        
        ## Run final sccm query, including pltidsqry
        if (datindb) {
          sccmx <- tryCatch(
            DBI::dbGetQuery(dbconn, sccmqry),
            error=function(e) {
              message("invalid subp_cond_chng_matrx query...")
              warning(e)
              return(NULL)})
        } else {
          sccmx <- tryCatch(
            sqldf::sqldf(sccmqry, connection = NULL),
            error = function(e) {
              message("invalid subp_cond_chng_matrx query...")
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(sccmx) || nrow(sccmx) == 0) {
          message(sccmqry)
          return(NULL)
        }

        ## Return and/or save sccm data
        sccmkey <- c("PLT_CN", "CONDID", "PREV_PLT_CN", "PREVCOND")
        setkeyv(setDT(sccmx), sccmkey)
        if (!is.null(getdataCNs)) { 
          sccmx <- sccmx[sccmx[[sccmid]] %in% getdataCNs,]
        }
      }
      
      ## Append adjusted SUBPTYP_PROP_CHNG
      sccmx[pltidsadj, 
            SUBPTYP_PROP_ADJ := SUBPTYP_PROP_CHNG * ADJ_FACTOR_COND]
      
      
      ## Add to returnlst and remove sccmx object
      if (returndata) {
        returnlst$sccmx <- sccmx
        returnlst$sccmid <- sccmid
      }
      ## Save data
      if (savedata) {
        message("saving SUBP_COND_CHNG_MATRX...")
        outlst$out_layer <- "SUBP_COND_CHNG_MATRX"
        if (!append_layer) index.unique.subp_cond_chng_mtrx <- sccmkey
        datExportData(sccmx, 
                      savedata_opts = outlst)
      }
      rm(sccmx)
      
     
      ##########################################################################
      ## If popType = 'GRM', get remeasured tree and seed data queries for GRM
      ##    and TREE_GRM_COMPENENT, TREE_GRM_BEGIN and TREE_GRM_MIDPT queries
      ##########################################################################
      if (popType == "GRM") {
        
        ## 7.4. Return and/or save seedling data (treex / TREE)
        ##################################################################
        treea. <- "t."
        ptreea. <- "ptree."
        
        ## Check variables
        treecnnm <- findnm("CN", treeflds, returnNULL = TRUE)
        prevtrecnnm <- findnm("PREV_TRE_CN", treeflds, returnNULL = TRUE)
        tprevcondnm <- findnm("PREVCOND", treeflds, returnNULL = TRUE)
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
        if (defaultVars) {
          treevars <- treeflds[treeflds %in% c(DBvars.default(istree=TRUE)$treevarlst,
                                               DBvars.default(istree=TRUE)$tsumvarlst)]
        } else {
          treevars <- "*"
        }
        tselectqry <- toString(paste0(treea., unique(c(tuniqueid, treevars))))
        ptreeselectqry <- toString(paste0(ptreea., unique(c(tuniqueid, treevars))))
        
        ## Build final tree query, including pltidsqry
        treeqry <- paste0(pltidsWITHqry,
                          "\n SELECT ", tselectqry,
                          tfromqry,
                          "\nUNION",
                          "\n SELECT ", ptreeselectqry,
                          tfromqry)
        dbqueries$tree <- treeqry
        
        ## Run final tree query, including pltidsqry
        if (datindb) {
          message("query ", treenm, "...")
          treex <- tryCatch(
            DBI::dbGetQuery(dbconn, treeqry),
            error = function(e) {
              message("invalid tree query...")
              message(e,"\n")
              return(NULL) })
        } else {    
          treex <- tryCatch(
            sqldf::sqldf(treeqry, connection = NULL),
            error = function(e) {
              message("invalid tree query...")
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(treex)) {
          message(treeqry)
        }
        
        ## 8.2.6. Return and/or save tree data
        treekey <- c(tuniqueid, condid, tsubp, ttree)
        setkeyv(setDT(treex), treekey)
        if (!is.null(getdataCNs)) { 
          treex <- treex[treex[[tuniqueid]] %in% getdataCNs,]
        }
        
        ## Add to returnlst and remove treex object
        if (returndata) {
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
        rm(treex)
        
        
        ## 7.5. Return grm data (grmx / TREE_GRM_COMPONENT)
        ##############################################################
        grma. <- "grm."
        
        ## Check variables
        grmtrecn <- findnm("TRE_CN", grmflds, returnNULL = TRUE)      
        keyvars <- grmtrecn
        if (any(sapply(keyvars, is.null))) {
          keymiss <- keyvars[sapply(keyvars, is.null)]
          stop("missing key variables in tree_grm_component data: ", toString(keymiss))
        }
        
        ## Build grm FROM query
        grmjoinqry <- getjoinqry(grmtrecn, treecnnm, grma., treea.)
        grmfromqry <- paste0(tfromqry, 
                             "\n LEFT JOIN ", SCHEMA., grmnm, " grm ", grmjoinqry)
        
        
        ## Build grm SELECT query
        if (defaultVars) {
          grmvars <- grmflds[grmflds %in% DBvars.default(isgrm=TRUE)$grmvarlst]
        } else {
          grmvars <- "*"
        }
        grmselectqry <- toString(paste0("grm.", unique(c(grmid, grmvars))))
        
        ## Build final grm query, including pltidsqry
        grmqry <- paste0(pltidsWITHqry,
                         "\n SELECT ", grmselectqry,
                         "\n FROM pltids",
                         grmfromqry)
        dbqueries$grm <- grmqry
        
        
        ## Run final grm query, including pltidsqry
        if (datindb) {
          message("query ", grmnm, "...")
          grmx <- tryCatch(
            DBI::dbGetQuery(dbconn, grmqry),
            error = function(e) {
              message("invalid tree_grm_component query...")
              message(e,"\n")
              return(NULL) })
        } else {    
          grmx <- tryCatch(
            sqldf::sqldf(grmqry, connection = NULL),
            error = function(e) {
              message("invalid tree_grm_component query...")
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(grmx)) {
          message(grmqry)
          return(NULL)
        }
        
        
        ## Return and/or save grm data
        grmkey <- c(grmtrecn)
        setkeyv(setDT(grmx), grmkey)

        ## Add to returnlst 
        if (returndata) {
          returnlst$grmx <- grmx
        }
        ## Save data
        if (savedata) {
          message("saving TREE_GRM_COMPONENT...")
          outlst$out_layer <- "TREE_GRM_COMPONENT"
          if (!append_layer) index.unique.tree_grm_component <- grmkey
          datExportData(grmx, 
                        savedata_opts = outlst)
        }
        rm(grmx)
        
        
        ## 7.6. Return grm begin data (beginx / TREE_GRM_BEGIN)
        ##############################################################
        begina. <- "begin."
        
        ## Check variables
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
        beginvars <- "*"
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
              message("invalid tree_grm_begin query...")
              message(e,"\n")
              return(NULL) })
        } else {    
          beginx <- tryCatch(
            sqldf::sqldf(beginqry, connection = NULL),
            error = function(e) {
              message("invalid tree_grm_begin query...")
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(beginx)) {
          message(beginqry)
          return(NULL)
        }
        
        ## Return and/or save begin data
        beginkey <- c(begintrecn)
        setkeyv(setDT(beginx), beginkey)
        
        ## Add to returnlst 
        if (returndata) {
          returnlst$beginx <- beginx
        }
        ## Save data
        if (savedata) {
          message("saving TREE_GRM_BEGIN...")
          outlst$out_layer <- "TREE_GRM_BEGIN"
          if (!append_layer) index.unique.tree_grm_begin <- beginkey
          datExportData(beginx, 
                       savedata_opts = outlst)
        }
        rm(beginx)
        
        
        ## 7.7. Return grm begin data (midptx / TREE_GRM_MIDPT)
        ##############################################################
        midpta. <- "midpt."
        
        ## Check variables
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
        midptvars <- "*"
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
              message("invalid tree_grm_midpt query...")
              message(e,"\n")
              return(NULL) })
        } else {    
          midptx <- tryCatch(
            sqldf::sqldf(midptqry, connection = NULL),
            error = function(e) {
              message("invalid tree_grm_midpt query...")
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(midptx)) {
          message(midptqry)
          return(NULL)
        }
        
        ## Return and/or save midpt data
        midptkey <- c(midpttrecn)
        setkeyv(setDT(midptx), midptkey)
        
        ## Add to returnlst 
        if (returndata) {
          returnlst$midptx <- midptx
        }
        ## Save data
        if (savedata) {
          message("saving TREE_GRM_MIDPT...")
          outlst$out_layer <- "TREE_GRM_MIDPT"
          if (!append_layer) index.unique.tree_grm_midpt <- midptkey
          datExportData(midptx, 
                        savedata_opts = outlst)
        }
        rm(midptx)
        
        ## 7.8. Return seedling data (seedx / SEEDLING)
        ##############################################################
        if (!is.null(seednm)) {
          seeda. <- "s."
          
          ## Check variables
          treecnnm <- findnm("CN", treeflds, returnNULL = TRUE)
          prevtrecnnm <- findnm("PREV_TRE_CN", treeflds, returnNULL = TRUE)
          tprevcondnm <- findnm("PREVCOND", treeflds, returnNULL = TRUE)
          scondidnm <- findnm("CONDID", seedflds, returnNULL = TRUE)
          ssubp <- findnm("SUBP", seedflds, returnNULL = TRUE)
          keyvars <- c(treecnnm, prevtrecnnm, tprevcondnm, scondidnm, ssubp)
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
          if (defaultVars) {
            seedvars <- seedflds[seedflds %in% c(DBvars.default(isseed=TRUE)$seedvarlst,
                                                 DBvars.default(isseed=TRUE)$ssumvarlst)]
          } else {
            seedvars <- "*"
          }
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
                message("invalid seedling query...")
                message(e,"\n")
                return(NULL) })
          } else {    
            seedx <- tryCatch(
              sqldf::sqldf(seedqry, connection = NULL),
              error = function(e) {
                message("invalid seedling query...")
                message(e,"\n")
                return(NULL) })
          }
          if (is.null(seedx)) {
            message(seedqry)
          }
          
          ## Return and/or save seedling data
          seedkey <- c(suniqueid, condid, ssubp)
          setkeyv(setDT(seedx), seedkey)
          
          ## Add to returnlst 
          if (returndata) {
            returnlst$seedx <- seedx
          }
          ## Save data
          if (savedata) {
            message("saving SEEDLING...")
            outlst$out_layer <- "SEEDLING"
            if (!append_layer) index.unique.seedling <- seedkey
            datExportData(seedx, 
                          savedata_opts = outlst)
          }
          rm(seedx)
        }
      }
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
    
    
    ##############################################################################
    ## 8. Check COND_STATUS_CD and generate table with number of conditions
    ##############################################################################
    
    ## condfromqry
    condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
    condfromqry <- paste0("\nJOIN ", SCHEMA., condnm, " c ", condjoinqry)
    
    ## 8.1. Sampled conditions
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
    
    ## 8.2. Sampled nonforest conditions
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
    
    ## 9. Build FROM statement for estimation queries
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
    
    
    ## 10. Return data objects
    ######################################################################################
    if (!returndata) {
      returnlst$sccmx <- sccmnm
      if (popType == "GRM") {
        if (!is.null(treenm)) {
          returnlst$treex <- treenm
          returnlst$tuniqueid <- tuniqueid
        }
        returnlst$grmx <- grmnm
        returnlst$beginx <- beginnm
        returnlst$midptx <- midptnm
        
        if (!is.null(seednm)) {
          returnlst$seedx <- seednm
          returnlst$suniqueid <- suniqueid
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
