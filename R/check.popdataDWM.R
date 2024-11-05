check.popdataDWM <- 
  function(tabs, tabIDs, popType, 
           datindb, pltaindb, 
           pltidsWITHqry,
           pltidsid, projidvars = NULL,
           pltidvars, 
           pdoms2keep = NULL, 
           pltidsadjindb = FALSE, 
           defaultVars = TRUE,
           pltassgnid, 
           pltx, pltassgnx,
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
           dwmvars2keep = NULL,
           dbconn = NULL, SCHEMA. = "",
           getdataWITHqry = NULL,
           getdataCNs = NULL,
           returndata = FALSE, 
           savedata = FALSE, 
           outlst = NULL,
           gui = FALSE){
    
    ##############################################################################
    ## DESCRIPTION: Checks data inputs for AREA/VOL estimation
    ## Define variables necessary for estimation:
    ## - cvars2keep = 'PROP_BASIS'
    ## Check if data are in a database (datindb) and if dbconn is valid.
    ## 1.	Get table names used in estimation from tabs.
    ## - PLOT; COND; 
    ## - TREE (if popType = 'VOL'); SEEDLING (if popType = 'VOL')
    ## 2.	Check for necessary variables in tables.
    ##    cond - (cuniqueid, condid, cvars2keep)
    ## 3. Build query for adjustment factors and append to pltids
    ## 4. Build and run queries for PLOT/COND (pltcondx)
    ## 5. Build CASE statement for adding adjustment factors to SELECT
    ## 6. Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE
    ## 
    ## 7. Build and run queries for other necessary tables (if returndata/savedata = TRUE)
    ## 7.1 Return and/or save plot data (pltx / PLOT)
    ## 7.2 Return and/or save cond data (condx / COND)
    ## 7.3. Return and/or save cond_dwm_calc data (dwmx / COND_DWM_CALC)
    ## 
    ## 8. Check COND_STATUS_CD and generate table with number of conditions
    ## 8.1. Sampled conditions
    ## 8.2. Sampled nonforest conditions
    ## 
    ## 9. Build FROM statement for estimation queries
    ## 10. Return data objects
    ###################################################################################
    
    
    ## Set global variables
    cond_dwm_calcnm=condsampcnt=areawt2nm=adjcase <- NULL
    dbqueries=dbqueriesWITH <- list()
    cpropvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ")
    dwmpropvars <- list(CWD="CONDPROP_CWD", FWD_SM="CONDPROP_FWD_SM", FWD_MD="CONDPROP_FWD_MD", FWD_LG="CONDPROP_FWD_LG")
    diavar <- "DIA"
    pltcondindb <- datindb
    
    
    ##############################################################################
    ## Define variables necessary for estimation
    ##############################################################################
    cvars2keep <- unique(c(cvars2keep, "PROP_BASIS"))
    
    cwdvars2keep <- c("CWD_LPA_UNADJ", "CWD_VOLCF_UNADJ", "CWD_DRYBIO_UNADJ",
                      "CWD_CARBON_UNADJ")
    fwdvars2keep <- c("FWD_SM_VOLCF_UNADJ", "FWD_SM_DRYBIO_UNADJ", "FWD_SM_CARBON_UNADJ",
                      "FWD_MD_VOLCF_UNADJ", "FWD_MD_DRYBIO_UNADJ", "FWD_MD_CARBON_UNADJ",
                      "FWD_LG_VOLCF_UNADJ", "FWD_LG_DRYBIO_UNADJ", "FWD_LG_CARBON_UNADJ")
    pilevars2keep <- c("PILE_VOLCF_UNADJ", "PILE_DRYBIO_UNADJ", "PILE_CARBON_UNADJ")
    duffvars2keep <- c("DUFF_VOLCF_UNADJ", "DUFF_DRYBIO_UNADJ", "DUFF_CARBON_UNADJ")
    dwmdoms2keep <- c(cwdvars2keep, fwdvars2keep)
    
    
    
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
 
    ## cond_dwm_calc table
    dwmlst <- popTabchk("cond_dwm_calc", tabtext = "cond_dwm_calc", 
                         tabs, tabIDs, dbtablst, dbconn, datindb) 
    dwmnm <- dwmlst$tabnm
    dwmflds <- dwmlst$tabflds
    dwmid <- dwmlst$tabid
    dwmx <- dwmlst$tabx
    dwma. <- "dwm."


    if (is.null(condnm)) {
      stop("must include cond for estimation")
    }
    if (is.null(dwmnm)) {
      stop("must include cond_dwm_calc for estimation")
    }
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
          message("invalid cond query...")
          message(condqry)
          return(NULL)
        }
        
        ## Return and/or save cond data
        condkey <- c(cuniqueid, condid)
        setkeyv(setDT(condx), condkey)
        
        ## Subset condx to plots in pltassgn
        if (!is.null(getdataCNs)) {
          condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
        }
        
        
        ## Build dwm FROM query
        dwmjoinqry <- getjoinqry(dwmid, pltidsid, dwma., pltidsa.)
        dwmfromqry <- paste0("\n JOIN ", SCHEMA., dwmnm, " dwm ", dwmjoinqry)
        
        ## Build dwm SELECT query
        if (defaultVars) {
          dwmvars <-  dwmflds[dwmflds %in% DBvars.default(isdwm=TRUE)$dwmvarlst]
        } else {
          dwmvars <- "*"
        }
        dwmselectqry <- toString(paste0(dwma., dwmvars))
        
        ## Build final dwm query, including getdataWITHqry
        dwmqry <- paste0(getdataWITHqry,
                         "\n-------------------------------------------",
                         "\n SELECT ", dwmselectqry,
                         "\n FROM pltids",
                         dwmfromqry) 
        dbqueries$COND_DWM_CALC <- dwmqry
        
        ## Run final dwmquery, including pltidsqry
        if (datindb) {
          dwmx <- tryCatch(
            DBI::dbGetQuery(dbconn, dwmqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
        } else {
          dwmx <- tryCatch(
            sqldf::sqldf(dwmqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(dwmx) || nrow(dwmx) == 0) {
          message("invalid dwm query...")
          message(dwmqry)
          return(NULL)
        }
        
        ## Return and/or save dwm data
        dwmkey <- c(dwmid, condid)
        setkeyv(setDT(dwmx), dwmkey)
        
        ## Subset dwmx to plots in pltassgn
        if (!is.null(getdataCNs)) {
          dwmx <- dwmx[dwmx[[cuniqueid]] %in% getdataCNs,]
        }
        
      } else {
        assign(condnm, DBI::dbReadTable(dbconn, condnm))
        assign(dwmnm, DBI::dbReadTable(dbconn, dwmnm))
      }
      
      ## Save data
      if (savedata) {
        message("saving COND...")
        outlst$out_layer <- "COND"
        if (!append_layer) index.unique.cond <- condkey
        datExportData(condx, 
                      savedata_opts = outlst)
        
        message("saving COND_DWM_CALC...")
        outlst$out_layer <- "COND_DWM_CALC"
        if (!append_layer) index.unique.dwm <- dwmkey
        datExportData(dwmx, 
                      savedata_opts = outlst)
        
      }
    }
    
    ##############################################################################
    ## 2. Check for necessary variables in tables
    ##############################################################################
    condxnm <- ifelse (!is.null(condx), "condx", condnm) 
    dwmxnm <- ifelse (!is.null(dwmx), "dwmx", dwmnm) 
    
    ## cond table
    ##############################################################################
    
    ## Check cuniqueid
    cuniqueid <- pcheck.varchar(var2check = cuniqueid, varnm = "cuniqueid", gui=gui,
                                checklst = condflds, caption = "Unique identifier of plot in cond",
                                warn = paste(cuniqueid, "not in cond"), stopifnull = TRUE)
    
    ## Check condid
    condid <- pcheck.varchar(var2check = condid, varnm = "condid", gui=gui,
                             checklst = condflds, caption = "Unique identifier of conditions in cond",
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
    
    
    ##############################################################################
    ## 3. Build query for adjustment factors and append to pltids
    ##############################################################################
    
    ## Check proportion variables
    dwmpropvars <- check.PROPvars(dwmflds,
                                propvars = unlist(dwmpropvars))
    cpropvars <- check.PROPvars(condflds,
                                propvars = unlist(cpropvars))
    areawt <- findnm(areawt, cpropvars, returnNULL = TRUE)
    if (is.null(areawt)) {
      stop("areawt not in dataset: ", areawt)
    }
    
    dwmjoinqry <- getjoinqry(dwmid, pltidsid, "dwm.", pltidsa.)
    dwmWITHqry <- paste0(
      pltidsWITHqry, ", ",
      "\ndwm AS",
      "\n(SELECT DISTINCT ", toString(c(dwmid, condid, dwmpropvars)),
      "\n FROM pltids",
      "\n JOIN ", dwmnm, " dwm ", dwmjoinqry, ")")
    #message(dwmWITHqry)
    
    
    ## Build ADJqry WHERE statement (i.e., excluding nonresponse)
    adjwhereqry <- NULL
    if (adj != "none") {
      adjwhereqry <- getADJwherePLOT(condflds, conda.="c.")
    }
    
    ## Build FROM query
    adjjoinqry <- getjoinqry(cuniqueid, pltidsid, "c.", pltidsa.)
    dwmjoinqry <- getjoinqry(c(cuniqueid, condid), c(cuniqueid, condid), "dwm.", "c.")
    adjfromqry <- paste0("\n FROM pltids",
                         "\n JOIN ", condnm, " c ", adjjoinqry,
                         "\n JOIN dwm ", dwmjoinqry)
    
    ## Get getADJqry function 
    ADJqry <- 
      getADJqry(popType = popType,
                adj = adj,
                propvars = cpropvars,
                dwmpropvar = dwmpropvars,
                adjfromqry = adjfromqry,
                pwhereqry = adjwhereqry,
                pltidsid = pltidsid,
                pltassgnid = pltassgnid,
                strunitvars = unique(c(projidvars, strunitvars)),
                pltidsa. = "pltids.",
                propqry = NULL)
    #message(ADJqry)
    
    
    ## Build final query for adjustment factors, including pltids WITH query
    adjfactors.qry <- paste0(
      dwmWITHqry, 
      "\n-------------------------------------------",
      "\n", ADJqry)
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
    
    ## Check adjustment factors
    #source("C:/_tsf/_GitHub/FIESTAnalysis/R/IEVALIDator_compare.R") 
    #evalid <- 81907
    #FIADBpop <- getFIADBpop(evalid = evalid, dbconn = FIAconn)$pop_stratum
    #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="07")
    #adjfactors <- replacepopfun(adjfactors, FIADBpop)
    #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="07")
    #popVOL_compare
    
    
    ## Build and run final query to append adjustment factors to pltids, including ADJ query
    if (adj == "samp") {
      adja. <- "adj."
      adjvars <- sapply(cpropvars, function(x) {
        ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
               ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
                       paste0(x, "_ADJ"))) })
      dwmadjvars <- sapply(dwmpropvars, function(x) {
        ifelse(grepl("CONDPROP", x), paste0("ADJ_FACTOR", sub("CONDPROP", "", x)), 
               ifelse (grepl("condprop", x), paste0("ADJ_FACTOR", toupper(sub("condprop", "", x))), 
                       paste0(x, "_ADJ"))) })
      
      #selectvars <- toString(c(paste0(pltidsa., pltidvars), paste0(adja., adjvars)))
      selectvars <- toString(c(paste0(pltidsa., pltidsid), paste0(adja., c(adjvars, dwmadjvars))))
      
      ## Build WITH query for adjustment factors, including pltids WITH query
      adjfactorsWITHqry <- paste0(
        dwmWITHqry, ",",
        "\n----- calculate strata-level adjustment factors",
        "\nadjfactors AS",
        "\n(", ADJqry, ")")
      #message(adjfactorsWITHqry)
      
 
      ## Build pltidsadjFROM.qry
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
        dwmWITHqry,
        "\n", ADJqry)
      
      ## Build WITH query to identify pltids, including adjustment factors
      pltidsadjWITHqry <- paste0(
        dwmWITHqry, ",",
        "\n----- calculate plot-level adjustment factors",
        "\n", ADJqry)
      
    }
    dbqueriesWITH$pltidsWITH <- pltidsWITHqry   
    dbqueriesWITH$pltidsadjWITH <- pltidsadjWITHqry   
    
    
    ## Run query to identify plotids, including adjustment factors
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
      }
      setkeyv(setDT(pltidsadj), pltidsid)
    }
    dbqueries$pltidsadj <- pltidsadj.qry
    
    
    ##############################################################################
    ## 4. Build and run queries for PLOT/COND (pltcondx)
    ##############################################################################
    pltidsa. <- "pltids."
    
    
    ## Build FROM query for pltcondx query
    plota. <- "p."
    conda. <- "c."
    dwma. <- "dwm."
    
    pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
    cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
    pcfromqry <- paste0(
      "\n FROM pltids",
      "\n JOIN ", SCHEMA., pltxnm, " p ", pjoinqry,
      "\n JOIN ", SCHEMA., condxnm, " c ", cjoinqry)
    
    
    ## Build SELECT query for pltcondx query
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
    cselectqry <- toString(c(paste0(conda., unique(c(condvars, cvars2keep)))))
    pltcondflds <- unique(c(condvars, cvars2keep, pvars))
    
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
      pltcondflds <- c(pltcondflds, "FORTYPGRPCD")
    }
    
    ## Build query for pltcondx
    pltcondx.qry <- paste0("SELECT DISTINCT ", cselectqry, ", ",
                           "\n", pselectqry, ", 1 AS TOTAL",
                           pcfromqry)
    dbqueries$pltcondx <- pltcondx.qry
    
    ## Build WITH query for pltcondx, including pltids WITH query
    pltcondxWITHqry <- paste0(pltidsWITHqry, ", ",
                               "\n----- pltcondx",
                               "\npltcondx AS",
                               "\n(", pltcondx.qry, ")")
    dbqueriesWITH$pltcondxWITH <- pltcondxWITHqry
    
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
      
      pltcondxqry <- paste0(pltidsWITHqry,
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
    
   
    ## 5. Build CASE statement for adding adjustment factors to SELECT
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
    ## 6. Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE
    ##############################################################################  
    returnlst <- list(pltcondflds = pltcondflds,
                      cuniqueid = cuniqueid, 
                      condid = condid, 
                      adjfactors = adjfactors,
                      adjcase = adjcase,
                      adjvarlst = adjvars,
                      dwmpropvars = dwmpropvars)
    
    if (returndata || savedata) {
      returnlst$pltcondx <- pltcondx
    } else {
      returnlst$pltcondx <- "pltcondx"
    }
   
    ##############################################################################
    ## 7. Build and run queries for other necessary tables (if returndata/savedata = TRUE) 
    ##############################################################################  
    if ((returndata || savedata) && popType == "DWM") {
      
      ## 7.1 Return and/or save plot data (pltx / PLOT)
      ##################################################################
      
      if (is.null(pltx)) {
        ## Build plot FROM query
        plotjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
        plotfromqry <- paste0("\n JOIN ", SCHEMA., plotnm, " p ", plotjoinqry)
        
        ## Build plot SELECT query
        pselectqry <- toString(paste0(plota., c(puniqueid, pdoms2keep)))
        
        ## Build final plot query, including pltidsqry
        pltqry <- paste0(getdataWITHqry,
                         "\n-------------------------------------------",
                         "\n SELECT ", pselectqry,
                         "\n FROM pltids",
                         plotfromqry) 
        dbqueries$PLOT <- pltqry
        
        ## Run final plot query, including pltidsqry
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
      
      ## Return and/or save plot data
      setkeyv(setDT(pltx), puniqueid)
      if (!is.null(getdataCNs)) {
        pltx <- pltx[pltx[[puniqueid]] %in% getdataCNs,]
      }
      
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
      
      
      ## 7.2 Return and/or save cond data (condx / COND)
      ##################################################################
      
      if (is.null(condx)) {
        
        ## Build cond FROM query
        condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
        condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
        
        ## Build cond SELECT query
        if (defaultVars) {
          condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
        } else {
          condvars <- "*"
        }
        condselectqry <- toString(paste0(conda., condvars))
        
        ## Build final cond query, including pltidsqry
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
        }
        
        ## Return and/or save cond data
        condkey <- c(cuniqueid, condid)
        setkeyv(setDT(condx), condkey)
        if (!is.null(getdataCNs)) {
          condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
        }
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
    
      
      ## 7.3 Return and/or save cond_dwm_calc data (dwmx / COND_DWM_CALC)
      ##################################################################
      dwma. <- "dwm."
      
      if (is.null(dwmx)) {
        
        ## Build cond FROM query
        dwmjoinqry <- getjoinqry(cuniqueid, pltidsid, dwma., pltidsa.)
        dwmfromqry <- paste0("\n JOIN ", SCHEMA., dwmnm, " dwm ", dwmjoinqry)
        
        ## Build cond SELECT query
        if (defaultVars) {
          #dwmvars <-  dwmflds[dwmflds %in% DBvars.default(isdwm=TRUE)$dwmvarlst]
          dwmvars <- c(dwmid, condid, dwmdoms2keep)
        } else {
          dwmvars <- "*"
        }
        dwmselectqry <- toString(paste0(dwma., dwmvars))
        
        ## Build final cond query, including pltidsqry
        dwmqry <- paste0(getdataWITHqry,
                          "\n-------------------------------------------",
                          "\n SELECT ", dwmselectqry,
                          "\n FROM pltids",
                         dwmfromqry) 
        dbqueries$COND_DWM_CALC <- dwmqry
        
        ## Run final cond query, including pltidsqry
        if (datindb) {
          dwmx <- tryCatch(
            DBI::dbGetQuery(dbconn, dwmqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
        } else {
          dwmx <- tryCatch(
            sqldf::sqldf(dwmqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(dwmx) || nrow(dwmx) == 0) {
          message("invalid cond_dwm_calc query...")
          message(dwmqry)
          return(NULL)
        }

        ## Return and/or save cond data
        dwmkey <- c(dwmid, condid)
        setkeyv(setDT(dwmx), dwmkey)
        if (!is.null(getdataCNs)) {
          dwmx <- dwmx[dwmx[[dwmid]] %in% getdataCNs,]
        }
      }
      
      ## Add to returnlst 
      if (returndata) {
        returnlst$dwmid <- cuniqueid
        returnlst$dwmx <- dwmx
      }
      ## Save data
      if (savedata) {
        message("saving COND_DWM_CALC...")
        outlst$out_layer <- "COND_DWM_CALC"
        if (!append_layer) index.unique.dwm <- dwmkey
        datExportData(dwmx, 
                      savedata_opts = outlst)
      }
      rm(dwmx)  
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
      estfromqry <- paste0(
        "\n FROM ", SCHEMA., plotnm, " p",
        "\n JOIN ", SCHEMA., condnm, " c ON (", conda., cuniqueid, " = ", plota., puniqueid, ")")
    } else {
      estfromqry <- paste0(
        "\n FROM pltcondf cond")
    }
    
    
    ## 10. Return data objects
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
