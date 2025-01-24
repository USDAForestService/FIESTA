check.popdataP2VEG <- 
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
  ## DESCRIPTION: Checks data inputs for P2VEG estimation
  ## Define variables necessary for estimation:
  ## - cvars2keep = c('PROP_BASIS', 'COND_NONSAMPLE_REASN_CD')
  ## Check if data are in a database (datindb) and if dbconn is valid.
  ## 1.	Get table names used in estimation from tabs.
  ## - PLOT; COND; SUBPLOT; SUBP_COND; P2VEG_SUBP_STRUCTRUE
  ## - P2VEG_SUBPLOT_SPP (if in database)
  ## 3. Build query for adjustment factors and append to pltids
  ## 3.1. Build query for adjustment factors based on popType (ADJqry)
  ## 3.2. Next, build query for P2VEG adjustments
  ## 4. Build and run queries for PLOT/COND (pltcondx)
  ## 5. Build CASE statement for adding adjustment factors to SELECT
  ## 6. Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE
  ## 
  ## 7. Build and run queries for other necessary tables (if returndata/savedata = TRUE)
  ## 7.1 Return and/or save plot data (pltx / PLOT)
  ## 7.2 Return and/or save cond data (condx / COND)
  ## 7.3. Return and/or save subplot data (subplotx / SUBPLOT)
  ## 7.4 Return and/or save subp_cond data (subp_condx / SUBP_COND)
  ## 7.5 Return and/or save p2veg_subp_structure (vsubstrx / P2VEG_SUBP_STRUCTRUE)
  ## 7.6 Return and/or save p2veg_subplot_spp (vsubsppx / P2VEG_SUBPLOT_SPP)
  ## 
  ## 8. Check COND_STATUS_CD and generate table with number of conditions
  ## 8.1. Sampled conditions
  ## 8.2. Sampled nonforest conditions
  ## 
  ## 9. Build FROM statement for estimation queries
  ## 10. Return data objects
  ###################################################################################
  
  ## Set global variables
  vsubpsppx=vadjfac=ADJ_FACTOR_P2VEG_SUBP <- NULL
  subpid <- "SUBP"
  dbqueries=dbqueriesWITH <- list()
  propvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
#  vpropvars <- list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
#  subppropvars <- c("MICRCOND", "SUBPCOND", "MACRCOND")
#  subpcpropvars <- c("MICRCOND_PROP", "SUBPCOND_PROP", "MACRCOND_PROP")
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
  returnlst <- list()
  
  ## subplot table
  subplotlst <- popTabchk(c("subplot"), tabtext = "subplot",
                          tabs, tabIDs, dbtablst, dbconn, datindb)
  subplotnm <- subplotlst$tabnm
  subplotflds <- subplotlst$tabflds
  subplotid <- subplotlst$tabid
  subplotx <- subplotlst$tabx
  if (is.null(subplotnm)) {
    stop("must include subplot for P2VEG estimates")
  }
  
  ## subp_cond table
  subp_condlst <- popTabchk(c("subp_cond", "subpcond"), 
                            tabtext = "subp_cond",
                            tabs, tabIDs, dbtablst, dbconn, datindb)
  subp_condnm <- subp_condlst$tabnm
  subp_condflds <- subp_condlst$tabflds
  subp_condid <- subp_condlst$tabid
  subp_condx <- subp_condlst$tabx
  if (is.null(subplotnm)) {
    stop("must include subp_cond for P2VEG estimates")
  }
  
  ## p2veg_subp_structure table
  vsubpstrlst <- popTabchk(c("p2veg_subp_structure", "vsubpstr"), 
                           tabtext = "p2veg_subp_structure",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
  vsubpstrnm <- vsubpstrlst$tabnm
  vsubpstrflds <- vsubpstrlst$tabflds
  vsubpstrid <- vsubpstrlst$tabid
  vsubpstrx <- vsubpstrlst$tabx
  if (is.null(vsubpstrnm)) {
    stop("must include p2veg_subp_structure for P2VEG estimates")
  } else {
    vsubpstra. <- "vsubpstr."
  }
  
  ## p2veg_subplot_spp table
  vsubpspplst <- popTabchk(c("p2veg_subplot_spp", "vsubpspp"), 
                           tabtext = "p2veg_subplot_spp",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
  vsubpsppnm <- vsubpspplst$tabnm
  vsubpsppflds <- vsubpspplst$tabflds
  vsubpsppid <- vsubpspplst$tabid
  vsubpsppx <- vsubpspplst$tabx
  if (!is.null(vsubpsppnm)) {
    vsubpsppa. <- "vsubpspp."
  }

  if (is.null(condnm)) {
    stop("must include cond for CHNG estimates")
  }
  if (datindb && !pltaindb) {
    
    ## Build cond FROM query
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
      condx <- condx[condx[[cuniqueid]] %in% getdataCNs,]
      
    } else {
      assign(condnm, DBI::dbReadTable(dbconn, condnm))
    }
   
    
    ## Get subplot data
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
      if (defaultVars) {
        subpvars <-  subplotflds[subplotflds %in% DBvars.default(issubp = TRUE)$subpvarlst]
      } else {
        subpvars <- "*"
      }
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
      }
    
      ## Set key on data.table
      subplotkey <- c(subplotid, subp)
      setkeyv(setDT(subplotx), subplotid)
    
    
      ## Get subp_cond data
      ##################################################################
      subpca. <- "subpc."
    
      ## 8.4.1. Check variables
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
      if (defaultVars) {
        subpcvars <-  subp_condflds[subp_condflds %in% DBvars.default(issubp = TRUE)$subpcvarlst]
      } else {
        subpcvars <- "*"
      }
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
      }
    
      ## Return and/or save subp_cond data
      subp_condkey <- c(subp_condid, subpcondid, subp)
      setkeyv(setDT(subp_condx), subp_condid)
      
    } else {
      
      assign(subplotnm, DBI::dbReadTable(dbconn, subplotnm))
      assign(subp_condnm, DBI::dbReadTable(dbconn, subp_condnm))
    }
    
    
  #   if (!is.null(getdataWITHqry) && !is.null(getdataCNs)) {
  #     
  #     ## 8.5 Return and/or save p2veg_subp_structure (vsubstrx / P2VEG_SUBP_STRUCTRUE)
  #     ##################################################################
  #     
  #     ## 8.5.1. Check variables
  #     vcondid <- findnm("CONDID", vsubpstrflds, returnNULL = TRUE)
  #     vsubp <- findnm("SUBP", vsubpstrflds, returnNULL = TRUE)
  #     keyvars <- c(vcondid, vsubp)
  #     if (any(sapply(keyvars, is.null))) {
  #       keymiss <- keyvars[sapply(keyvars, is.null)]
  #       stop("missing key variables in subp_cond data: ", toString(keymiss))
  #     }
  #     
  #     ## 8.5.2. Build p2veg_subp_structure FROM query
  #     vsubpstrjoinqry <- getjoinqry(vsubpstrid, pltidsid, vsubpstra., pltidsa.)
  #     vsubpstrfromqry <- paste0(
  #       "\nFROM pltids",
  #       "\nJOIN ", SCHEMA., vsubpstrnm, " vsubpstr ", vsubpstrjoinqry) 
  #     
  #     ## 8.5.3. Build p2veg_subp_structure SELECT query
  #     if (defaultVars) {
  #       vsubpstrvars <-  vsubpstrflds[vsubpstrflds %in% DBvars.default(isveg = TRUE)$vsubpstrvarlst]
  #     } else {
  #       vsubpstrvars <- "*"
  #     }
  #     vsubpstrselectqry <- toString(paste0(vsubpstra., vsubpstrvars))
  #     
  #     ## 8.5.4. Build final p2veg_subp_structure query, including pltidsqry
  #     vsubpstrqry <- paste0(getdataWITHqry,
  #                           "\n-------------------------------------------",
  #                           "\n SELECT ", vsubpstrselectqry,
  #                           vsubpstrfromqry) 
  #     dbqueries$vsubpstr <- vsubpstrqry
  #     
  #     ## 8.5.5. Run final p2veg_subp_structure query, including pltidsqry
  #     if (datindb) {
  #       vsubpstrx <- tryCatch(
  #         DBI::dbGetQuery(dbconn, vsubpstrqry),
  #         error=function(e) {
  #           message(e,"\n")
  #           return(NULL)})
  #     } else {
  #       vsubpstrx <- tryCatch(
  #         sqldf::sqldf(vsubpstrqry, connection = NULL),
  #         error = function(e) {
  #           message(e,"\n")
  #           return(NULL) })
  #     }
  #     if (is.null(vsubpstrx) || nrow(vsubpstrx) == 0) {
  #       message("invalid p2veg_subp_structure query...")
  #       message(vsubpstrqry)
  #       return(NULL)
  #     }
  #     
  #     ## 8.5.6. Return and/or save vsubpstr data
  #     vsubpstrkey <- c(vsubpstrid, vcondid, vsubp)
  #     setkeyv(setDT(vsubpstrx), vsubpstrid)
  #     
  #     
  #     ## 8.6 Return and/or save p2veg_subplot_spp (vsubsppx / P2VEG_SUBPLOT_SPP)
  #     ##################################################################
  #     if (!is.null(vsubpsppnm)) {
  # 
  #       ## 8.6.1. Check variables
  #       vcondid <- findnm("CONDID", vsubpsppflds, returnNULL = TRUE)
  #       vsubp <- findnm("SUBP", vsubpsppflds, returnNULL = TRUE)
  #       keyvars <- c(vcondid, vsubp)
  #       if (any(sapply(keyvars, is.null))) {
  #         keymiss <- keyvars[sapply(keyvars, is.null)]
  #         stop("missing key variables in subp_cond data: ", toString(keymiss))
  #       }
  #       
  #       ## 8.6.2. Build p2veg_subplot_spp FROM query
  #       vsubpsppjoinqry <- getjoinqry(vsubpsppid, pltidsid, vsubpsppa., pltidsa.)
  #       vsubpsppfromqry <- paste0(
  #         "\nFROM pltids",
  #         "\nJOIN ", SCHEMA., vsubpsppnm, " vsubpspp ", vsubpsppjoinqry) 
  #       
  #       ## 8.6.3. Build p2veg_subplot_spp SELECT query
  #       if (defaultVars) {
  #         vsubpsppvars <-  vsubpsppflds[vsubpsppflds %in% DBvars.default(isveg = TRUE)$vsubpsppvarlst]
  #       } else {
  #         vsubpsppvars <- "*"
  #       }
  #       vsubpsppselectqry <- toString(paste0(vsubpsppa., vsubpsppvars))
  #       
  #       ## 8.6.4. Build final p2veg_subplot_spp query, including pltidsqry
  #       vsubpsppqry <- paste0(getdataWITHqry,
  #                             "\n-------------------------------------------",
  #                             "\n SELECT ", vsubpsppselectqry,
  #                             vsubpsppfromqry) 
  #       dbqueries$vsubpspp <- vsubpsppqry
  #       
  #       ## 8.6.5. Run final p2veg_subplot_spp query, including pltidsqry
  #       if (datindb) {
  #         vsubpsppx <- tryCatch(
  #           DBI::dbGetQuery(dbconn, vsubpsppqry),
  #           error=function(e) {
  #             message(e,"\n")
  #             return(NULL)})
  #       } else {
  #         vsubpsppx <- tryCatch(
  #           sqldf::sqldf(vsubpsppqry, connection = NULL),
  #           error = function(e) {
  #             message(e,"\n")
  #             return(NULL) })
  #       }
  #       if (is.null(vsubpsppx) || nrow(vsubpsppx) == 0) {
  #         message("invalid p2veg_subplot_spp query...")
  #         message(vsubpsppqry)
  #         return(NULL)
  #       }
  #       
  #       ## 8.6.6. Return and/or save vsubpspp data
  #       vsubpsppkey <- c(vsubpsppid, vcondid, vsubp)
  #       setkeyv(setDT(vsubpsppx), vsubpsppid)
  #     
  #     } else {
  #     
  #       assign(vsubpstrnm, DBI::dbReadTable(dbconn, vsubpstrnm))
  #       assign(vsubpsppnm, DBI::dbReadTable(dbconn, vsubpsppnm))
  #     }
  #   }
  }

  ##############################################################################
  ## 2. Check for necessary variables in tables
  ##############################################################################
  condxnm <- ifelse (!is.null(condx), "condx", condnm) 
  
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
  
  ## subplot table
  ##############################################################################
  
  ## Check subpid
  subpid <- pcheck.varchar(var2check = subpid, varnm="subpid", gui=gui,
          checklst = subplotflds, caption="Unique identifier of subplots",
          warn = paste(subpid, "not in subplot"), stopifnull = TRUE)
  
  ## Check subpvars2keep in subplot/subp_cond
  #subpflds <- unique(c(subplotflds, subp_condflds))

  
  
  ##############################################################################
  ## 3. Build query for adjustment factors and append to pltids
  ##############################################################################
  
  ## Check proportion variables, including area weight 
  #######################################################################
  propvars <- check.PROPvars(condflds,
                              propvars = propvars)
  if (!areawt %in% condflds) {
    stop("areawt not in dataset: ", areawt)
  }

  ## Check subplot proportion variables
#  subppropvars <- check.PROPvars(subplotflds,
#                              propvars = unlist(subppropvars))
#  subpcpropvars <- check.PROPvars(subp_condflds,
#                                 propvars = unlist(subpcpropvars))
#  P2VEGpropvars <- c(subppropvars, subpcpropvars)
  

  
  ## Build and run query to calculate adjustment factors for subplot (ADJqry) 
  #######################################################################
  plota. <- "p."
  conda. <- "c."
  subpa. <- "subp."
  subpca. <- "subpc."
  
  ## Build FROM statement
  pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
  cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
  pcfromqry <- paste0(
    "\n FROM pltids",
    "\n JOIN ", SCHEMA., plotnm, " p ", pjoinqry,
    "\n JOIN ", SCHEMA., condnm, " c ", cjoinqry)
  
  ## Add FROM statement for subplot and subp_cond		 
  P2VEGjoinqry <- getjoinqry(subplotid, puniqueid, subpa., plota.)
  P2VEGfromqry <- paste0(
    pcfromqry,                    
    "\n JOIN ", SCHEMA., subplotnm, " subp ", P2VEGjoinqry,
    "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subplotid, " = ", conda., cuniqueid, 
    " AND ", subpca., condid, " = ", conda., condid, 
    " AND ", subpca., subpid, " = ", subpa., subpid, ")")
  
  
  ## Build ADJqry WHERE statement (i.e., excluding nonresponse)
  adjwhereqry <- NULL
  if (adj != "none") {
    adjwhereqry=P2VEGwhereqry <- getADJwherePLOT(condflds)
    
    ## Subplot filters
    #################################################################
    P2VEGwhereqry <- getADJwhereSUBP(subplotflds, adjwhereqry=P2VEGwhereqry)
    
    
    ## P2 vegetation filters
    #################################################################
    
    ## Filter for nonsampled P2VEG subplots in subplot (on field sampled plots)
    ## P2VEG_SAMPLING_STATUS_CD < 3 AND
    ## ((SAMP_METHOD_CD = 1 AND P2VEG_SAMPLING_STATUS_CD = 1) OR SAMP_METHOD_CD = 2)
    p2vegstatusnm <- findnm("P2VEG_SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
    if (is.null(p2vegstatusnm)) {
      message("P2VEG_SUBP_STATUS_CD is not in dataset... assuming all subplots sample P2VEG")
    } else {
      ## Build where query to remove subplots that didn't sample P2VEG
      p2vegstatus.filter <- paste0(subpa., p2vegstatusnm, " < 3")
      
      sampmethodnm <- findnm("SAMP_METHOD_CD", pltflds, returnNULL = TRUE)
      if (!is.null(sampmethodnm)) {
        p2vegstatus.filter <- paste0(p2vegstatus.filter,
              "\n    AND ((", plota., sampmethodnm, " = 1 AND ", subpa., p2vegstatusnm, " = 1)",
              "\n          OR ", sampmethodnm, " = 2)")
      }
      if (is.null(P2VEGwhereqry)) {
        P2VEGwhereqry <- paste0("\n WHERE ", p2vegstatus.filter)
      } else {
        P2VEGwhereqry <- paste0(P2VEGwhereqry, 
                                "\n    AND ", p2vegstatus.filter)
      }	
    }
  }  ## END adj = 'none'

 
  ## 3.1. Build query for adjustment factors based on popType (ADJqry)
  ##############################################################################
  
  ## First, build query for VOL adjustments
  ADJqry <- 
    getADJqry(popType = "VOL",
              adj = adj,
              propvars = propvars,
              adjfromqry = pcfromqry,
              pwhereqry = adjwhereqry,
              pltassgnid = pltassgnid,
              pltidsid = pltidsid,
              strunitvars = strunitvars,
              pltidsa. = pltidsa.,
              propqry = NULL)
  #message(ADJqry)
  dbqueries$ADJqry <- ADJqry    

  ## Build final query for adjustment factors, including pltids WITH query
  adjfactors.qry <- paste0(
    pltidsWITHqry, 
    "\n-------------------------------------------",
    "\n", ADJqry)
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
  if (adj == "samp") {
    setkeyv(setDT(adjfactors), strunitvars)
  } else {
    setkeyv(setDT(adjfactors), pltidsid)
  }
  dbqueries$adjfactors <- adjfactors.qry
  
  
  ## Check adjustment factors
  #source("C:/_tsf/_GitHub/FIESTAnalysis/R/IEVALIDator_compare.R") 
  #evalid <- 81901
  #FIADBpop <- getFIADBpop(evalid = evalid, dbconn = FIAconn)$pop_stratum
  #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="01")
  #adjfactors <- replacepopfun(adjfactors, FIADBpop)
  #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="01")
  
  
  # adja. <- "adj."
  # adjvars <- sapply(propvars, function(x) {
  #   ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
  #          ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
  #                  paste0(x, "_ADJ"))) })
  # 
  # ## Build and run final query to append adjustment factors to pltids, including ADJ query
  # if (adj == "samp") {
  #   adja. <- "adj."
  #   adjvars <- sapply(propvars, function(x) {
  #     ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
  #            ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
  #                    paste0(x, "_ADJ"))) })
  #   #selectvars <- toString(c(paste0(pltidsa., pltidvars), paste0(adja., adjvars)))
  #   selectvars <- toString(c(paste0(pltidsa., pltidsid), paste0(adja., adjvars)))
  #   
  #   ## Build WITH query for adjustment factors, including pltids WITH query
  #   adjfactorsWITHqry <- paste0(
  #     pltidsWITHqry, ",",
  #     "\n----- calculate strata-level adjustment factors",
  #     "\nadjfactors AS",
  #     "\n(", ADJqry, ")")
  #   #message(adjfactorsWITHqry)
  #   
  #   
  #   ## Build pltidsadjFROM.qry
  #   adjjoinqry <- getjoinqry(strunitvars, strunitvars, adja., pltidsa.)
  #   pltidsadjFROM.qry <- paste0(
  #     "\nFROM pltids",
  #     "\nJOIN adjfactors adj ", adjjoinqry)
  #   
  #   
  #   ## Build pltidsadj.qry
  #   pltidsadj.qry <- paste0(
  #     adjfactorsWITHqry,
  #     "\n-------------------------------------------",
  #     paste0("\nSELECT ", selectvars,
  #            pltidsadjFROM.qry))
  #   ## message(pltidsadj.qry)
  #   
  #   
  #   ## Build WITH query to identify pltids, including adjustment factors
  #   pltidsadjWITHqry <- paste0(
  #     adjfactorsWITHqry, ",",
  #     "\n----- get plot-level adjustment factors",
  #     "\npltidsadj AS ",
  #     "\n(SELECT ", selectvars,
  #     pltidsadjFROM.qry, ")")
  #   
  # } else {
  #   
  #   ## Build pltidsadj.qry
  #   pltidsadj.qry <- paste0(
  #     pltidsWITHqry,
  #     "\n", ADJqry)
  #   
  #   ## Build WITH query to identify pltids, including adjustment factors
  #   pltidsadjWITHqry <- paste0(
  #     pltidsWITHqry, ",",
  #     "\n----- calculate plot-level adjustment factors",
  #     "\n", ADJqry)
  #   
  # }
  # dbqueriesWITH$pltidsWITH <- pltidsWITHqry   
  # dbqueriesWITH$pltidsadjWITH <- pltidsadjWITHqry   
  # 
  # 
  # ## Run query to identify plotids, including adjustment factors
  # if (returndata || savedata) {
  #   if (pltaindb) {
  #     pltidsadj <- tryCatch(
  #       DBI::dbGetQuery(dbconn, pltidsadj.qry),
  #       error=function(e) {
  #         message(e,"\n")
  #         return(NULL)})
  #   } else {
  #     pltidsadj <- tryCatch(
  #       sqldf::sqldf(pltidsadj.qry, connection = NULL),
  #       error = function(e) {
  #         message(e,"\n")
  #         return(NULL) })
  #   }
  #   
  #   if (is.null(pltidsadj) || nrow(pltidsadj) == 0) {
  #     message("invalid adjustment query...")
  #     message(pltidsadj.qry)
  #     return(NULL)
  #   }
  #   setkeyv(setDT(pltidsadj), pltidsid)
  # }
  # dbqueries$pltidsadj <- pltidsadj.qry
  # 


  ## Next, build query for P2VEG adjustments
  ###############################################################################
  
  ## First, get query for summarizing subplot sampled proportions
  sumpropqry <- sumpropP2VEGqry(fromqry = P2VEGfromqry, 
                               whereqry = P2VEGwhereqry,
                               ACI = ACI,
                               selectvars = NULL,
                               SCHEMA. = SCHEMA.)
  

  ## Next, add sumpropqry to get getADJqry to build a subquery
  if (adj == "samp") {
    adjjoinqry <- getjoinqry(strunitvars, strunitvars, "adj.", pltidsa.)
  } else {
    adjjoinqry <- getjoinqry(pltidsid, pltidsid, "adj.", pltidsa.)
  }
  adjfromqry <- paste0(
    "\n FROM pltids",
    "\n LEFT OUTER JOIN subpcprop c ON (", pltidsa., pltidsid, " = c.", subplotid, ")",
    "\n JOIN adjfactors adj ", adjjoinqry)
  othervars <- c(propvars['SUBP'],propvars['MACR'],propvars['MICR'])
  
  
  ## 5.7. Build and run final query to append adjustment factors to pltids, including ADJ query
  adja. <- "adj."
  adjP2VEG. <- "adjP2VEG."
  adjvars <- sapply(propvars, function(x) {
    ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
           ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
                   paste0(x, "_ADJ"))) })
  adjvars['P2VEG'] <- "ADJ_FACTOR_P2VEG_SUBP"
  #selectvars <- toString(c(paste0(pltidsa., pltidvars), paste0(adja., adjvars)))
  selectvars <- toString(c(paste0(pltidsa., pltidsid), adjvars))

  if (adj == "plot") {
  
    ADJqryP2VEGplot <- 
      getADJqry(popType = popType,
              adj = adj,
              propvars = propvars['COND'],
              adjfromqry = adjfromqry,
              pwhereqry = NULL,
              pltidsid = pltidsid,
              pltassgnid = pltassgnid,
              strunitvars = strunitvars,
              pltidsa. = pltidsa.,
              othervars <- adjvars[c('COND', 'SUBP', 'MACR', "MICR")],
              propqry = NULL)
    #message(ADJqryP2VEGplot)  
    
    ## Build final query for adjustment factors, including pltids WITH query
    pltidsadj.qry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- calculate VOL adjustment factors",
      "\nadjfactors AS ",
      "\n(", ADJqry, "),",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, ")",
      "\n-------------------------------------------",
      "\n", ADJqryP2VEGplot)
    #message(pltidsadj.qry)
    
    
    ## Build final query for adjustment factors, including pltids WITH query
    pltidsadjWITHqry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- calculate VOL adjustment factors",
      "\nadjfactors AS ",
      "\n(", ADJqry, "),",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, "),",
      "\n----- get plot-level adjustment factors",
      "\n(", ADJqryP2VEGplot, ")")
    #message(pltidsadjWITHqry)
    
  } else {  
  
    ADJqryP2VEG <- 
      getADJqry(popType = popType,
              adj = adj,
              propvars = propvars['COND'],
              adjfromqry = adjfromqry,
              pwhereqry = NULL,
              pltidsid = pltidsid,
              pltassgnid = pltassgnid,
              strunitvars = strunitvars,
              pltidsa. = pltidsa.,
              othervars <- adjvars[c('COND', 'SUBP', 'MACR', "MICR")],
              propqry = NULL)
    #message(ADJqryP2VEG)


    ## Build final query for adjustment factors, including pltids WITH query
    adjfactorsP2VEG.qry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- calculate VOL adjustment factors",
      "\nadjfactors AS ",
      "\n(", ADJqry, "),",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, ")",
      "\n-------------------------------------------",
      "\n", ADJqryP2VEG)
    #message(adjfactorsP2VEG.qry)
    
    ## Build WITH query to append adjustment factors to pltids, including ADJ query
    adjfactorsP2VEGWITHqry <- paste0(
      pltidsWITHqry, ", ",
      "\n----- calculate VOL adjustment factors",
      "\nadjfactors AS ",
      "\n(", ADJqry, "),",
      "\n----- sum sampled subplot proportions",
      "\nsubpcprop AS ",
      "\n(", sumpropqry, "),",
      "\n----- calculate P2VEG adjustment factor",
      "\nadjfactorsP2VEG AS ",
      "\n(", ADJqryP2VEG, ")")
    #message(adjfactorsP2VEGWITHqry)
    
    
    
    ## Build pltidsadjFROM.qry
    adjjoinqry <- getjoinqry(strunitvars, strunitvars, adja., pltidsa.)
    adjP2VEGjoinqry <- getjoinqry(strunitvars, strunitvars, "adjP2VEG.", pltidsa.)
    pltidsadjFROM.qry <- paste0(
      "\nFROM pltids",
      "\nJOIN adjfactors adj ", adjjoinqry,
      "\nJOIN adjfactorsP2VEG adjP2VEG ", adjP2VEGjoinqry)
    
    
    ## Build pltidsadj.qry
    pltidsadj.qry <- paste0(
      adjfactorsP2VEGWITHqry,
      "\n-------------------------------------------",
      paste0("\nSELECT ", selectvars,
             pltidsadjFROM.qry))
    ## message(pltidsadj.qry)
    
    
    ## Build WITH query to identify pltids, including adjustment factors
    pltidsadjWITHqry <- paste0(
      adjfactorsP2VEGWITHqry, ",",
      "\n----- get plot-level adjustment factors",
      "\npltidsadj AS ",
      "\n(SELECT ", selectvars,
      pltidsadjFROM.qry, ")")
  }  
 
  ## Run query to calculate adjustment factors
  if (datindb) {
    adjfactorsP2VEG <- tryCatch(
        DBI::dbGetQuery(dbconn, adjfactorsP2VEG.qry),
                  error=function(e) {
                    message("invalid adjustment query...")
                    message(e,"\n")
                    return(NULL)})
  } else {
      adjfactorsP2VEG <- tryCatch(
         sqldf::sqldf(adjfactorsP2VEG.qry, connection = NULL),
                  error = function(e) {
                    message("invalid adjustment query...")
                    message(e,"\n")
                    return(NULL) })
  }
  if (is.null(adjfactorsP2VEG) || nrow(adjfactorsP2VEG) == 0) {
    message(adjfactorsP2VEG.qry)
    return(NULL)
  }
  dbqueries$adjfactors <- adjfactorsP2VEG.qry
  dbqueriesWITH$pltidsWITH <- pltidsWITHqry   
  dbqueriesWITH$pltidsadjWITH <- pltidsadjWITHqry   
  
  
  # Check with FIADB population data - P2VEG
  # source("C:/_tsf/_GitHub/FIESTAnalysis/R/IEVALIDator_compare.R")
  # FIADBpop <- getFIADBpop(state, evaltype = "10", evalyr, dbconn=dbconn)$pop_stratum
  # popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactorsP2VEG, evaltype="10")
  # popVOL_compare
  # popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactorsP2VEG, evaltype="10")
  # popVOL_compare

  
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
  plota. <- "p."
  conda. <- "c."
  
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
  cselectqry <- toString(paste0(conda., unique(c(condvars, cvars2keep))))
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
  pltcondx.qry <- paste0("SELECT ", cselectqry, ", ",
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
                    varadjP2VEG = "vadjfac",
                    adjvarlst = adjvars)
  
  if (returndata || savedata) {
    returnlst$pltcondx <- pltcondx
    returnlst$pltidsadj <- pltidsadj
    #returnlst$pltidsadjP2VEG <- pltidsadjP2VEG
  } else {
    returnlst$pltcondx <- "pltcondx"
    returnlst$pltidsadj <- "pltidsadj"
    #returnlst$pltidsadjP2VEG <- "pltidsadjP2VEG"
  }
  

  ##############################################################################
  ## 7. Build and run queries for other necessary tables (if returndata = TRUE) 
  ##############################################################################  
  if (returndata || savedata) {
    message("returning data needed for estimation...")
    
    # ## 7.1 Return and/or save plot data (pltx / PLOT)
    # ##################################################################
    # 
    # if (is.null(pltx)) {
    #   ## Build plot FROM query
    #   plotjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
    #   plotfromqry <- paste0("\n JOIN ", SCHEMA., plotnm, " p ", plotjoinqry)
    #   
    #   ## Build plot SELECT query
    #   pselectqry <- toString(paste0(plota., c(puniqueid, pdoms2keep)))
    #   
    #   ## Build final plot query, including pltidsqry
    #   pltqry <- paste0(getdataWITHqry,
    #                    "\n-------------------------------------------",
    #                    "\n SELECT ", pselectqry,
    #                    "\n FROM pltids",
    #                    plotfromqry) 
    #   dbqueries$PLOT <- pltqry
    #   
    #   ## Run final plot query, including pltidsqry
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
    #   condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
    #   condfromqry <- paste0("\n JOIN ", SCHEMA., condnm, " c ", condjoinqry)
    #   
    #   ## Build cond SELECT query
    #   if (defaultVars) {
    #     condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
    #   } else {
    #     condvars <- "*"
    #   }
    #   condselectqry <- toString(paste0(conda., condvars))
    #   
    #   ## Build final cond query, including pltidsqry
    #   condqry <- paste0(getdataWITHqry,
    #                     "\n-------------------------------------------",
    #                     "\n SELECT ", condselectqry,
    #                     "\n FROM pltids",
    #                     condfromqry) 
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
    
    
    ## 7.3. Return and/or save subplot data (subplotx / SUBPLOT)
    ##################################################################
    if (is.null(subplotx)) {
      subpa. <- "subp."
      
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
      if (defaultVars) {
        subpvars <-  subplotflds[subplotflds %in% DBvars.default(issubp = TRUE)$subpvarlst]
      } else {
        subpvars <- "*"
      }
      subpselectqry <- toString(paste0(subpa., subpvars))
      
      ## Build final subplot query, including pltidsqry
      subplotqry <- paste0(getdataWITHqry,
                           "\n-------------------------------------------",
                           "\n SELECT ", subpselectqry,
                           "\nFROM pltids", 
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
      }
      
      ## Set key on data.table
      subplotkey <- c(subplotid, subp)
      setkeyv(setDT(subplotx), subplotid)
      if (!is.null(getdataCNs)) {
        subplotx <- subplotx[subplotx[[subplotid]] %in% getdataCNs,]
      }
    }
    
    ## Add to returnlst 
    if (returndata) {
      returnlst$subplotx <- subplotx
      returnlst$subplotid <- subplotid
    }
    ## Save data
    if (savedata) {
      message("saving SUBPLOT...")
      outlst$out_layer <- "SUBPLOT"
      if (!append_layer) index.unique.subplot <- subplotkey
      datExportData(subplotx, 
                    savedata_opts = outlst)
    }
    #rm(subplotx)  

    
    ## 7.4 Return and/or save subp_cond data (subp_condx / SUBP_COND)
    ##################################################################
    if (is.null(subp_condx)) {
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
        "\nJOIN ", SCHEMA., subp_condnm, " subpc ", subpcjoinqry)
      
      
      ## Build subp_cond SELECT query
      if (defaultVars) {
        subpcvars <-  subp_condflds[subp_condflds %in% DBvars.default(issubp = TRUE)$subpcvarlst]
      } else {
        subpcvars <- "*"
      }
      subpcselectqry <- toString(paste0(subpca., subpcvars))
      
      ## Build final subp_cond query, including pltidsqry
      subp_condqry <- paste0(getdataWITHqry,
                             "\n-------------------------------------------",
                             "\n SELECT ", subpcselectqry,
                             "\nFROM pltids",
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
      }
      
      ## Return and/or save subp_cond data
      subp_condkey <- c(subp_condid, subpcondid, subp)
      setkeyv(setDT(subp_condx), subp_condid)
      if (!is.null(getdataCNs)) {
        subp_condx <- subp_condx[subp_condx[[subp_condid]] %in% getdataCNs,]
      }
    }
      
    ## Add to returnlst 
    if (returndata) {
      returnlst$subp_condx <- subp_condx
      returnlst$subp_condid <- subp_condid
    }
    ## Save data
    if (savedata) {
      message("saving SUBP_COND...")
      outlst$out_layer <- "SUBP_COND"
      if (!append_layer) index.unique.subp_cond <- subp_condkey
      datExportData(subp_condx, 
                    savedata_opts = outlst)
    }
    #rm(subp_condx)

    
    ## 7.5 Return and/or save p2veg_subp_structure (vsubstrx / P2VEG_SUBP_STRUCTRUE)
    ##################################################################
    if (is.null(vsubpstrx)) {  
      vsubpstra. <- "vsubpstr."

      ## Check variables
      vcondid <- findnm("CONDID", vsubpstrflds, returnNULL = TRUE)
      vsubp <- findnm("SUBP", vsubpstrflds, returnNULL = TRUE)
      keyvars <- c(vsubpstrid, vcondid, vsubp)
      if (any(sapply(keyvars, is.null))) {
        keymiss <- keyvars[sapply(keyvars, is.null)]
        stop("missing key variables in subp_cond data: ", toString(keymiss))
      }
      
      ## 8.5.2. Build p2veg_subp_structure FROM query
      vsubpstrjoinqry <- getjoinqry(vsubpstrid, pltidsid, vsubpstra., pltidsa.)
      vsubpstrfromqry <- paste0(
        "\nJOIN ", SCHEMA., vsubpstrnm, " vsubpstr ", vsubpstrjoinqry) 
      
      ## 8.5.3. Build p2veg_subp_structure SELECT query for summarizing to condition
      # if (defaultVars) {
      #   vsubpstrvars <-  vsubpstrflds[vsubpstrflds %in% DBvars.default(isveg = TRUE)$vsubpstrvarlst]
      # } else {
      #   vsubpstrvars <- "*"
      # }
      vsubpstrvars <- c("GROWTH_HABIT_CD", "LAYER")
      vsubpstrgrpvars <- paste0(c(vsubpstrid, vcondid, vsubpstrvars))
      vsubpstrsumvar <- "\n  COALESCE(SUM(COVER_PCT * 1.0) / 4 / 100 ,0) AS COVER_PCT_SUM"
      vsubpstrselectqry <- toString(c(paste0(vsubpstra., vsubpstrgrpvars), vsubpstrsumvar))
      
      ## Build final p2veg_subp_structure query, including pltidsqry
      vcondstrqry <- paste0(getdataWITHqry,
                            "\n-------------------------------------------",
                            "\nSELECT ", vsubpstrselectqry,
                            "\nFROM pltids",
                            vsubpstrfromqry, 
                            "\nGROUP BY ", toString(paste0(vsubpstra., vsubpstrgrpvars)))
      dbqueries$vcondstr <- vcondstrqry
      
      ## Run final p2veg_subp_structure query, including pltidsqry
      if (datindb) {
        vcondstrx <- tryCatch(
          DBI::dbGetQuery(dbconn, vcondstrqry),
          error=function(e) {
            message(e,"\n")
            return(NULL)})
      } else {
        vcondstrx <- tryCatch(
          sqldf::sqldf(vcondstrqry, connection = NULL),
          error = function(e) {
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(vcondstrx) || nrow(vcondstrx) == 0) {
        message("invalid condition-level p2veg_subp_structure query...")
        message(vcondstrqry)
        return(NULL)
      }
      
      ## Return and/or save vsubpstr data
      vcondstrkey <- c(vsubpstrid, vcondid)
      setkeyv(setDT(vcondstrx), vsubpstrid)
      if (!is.null(getdataCNs)) {
        vcondstrx <- vcondstrx[vcondstrx[[vsubpstrid]] %in% getdataCNs,]
      }
    }
    
    ## Add to returnlst 
    if (returndata) {
        
      ## Append adjustment factors to tree data
      vcondstrx[pltidsadj, vadjfac := ADJ_FACTOR_P2VEG_SUBP]
        
      returnlst$vcondstrx <- vcondstrx
      returnlst$vcondstrid <- vsubpstrid
    }
    ## Save data
    if (savedata) {
      message("saving P2VEG_SUBP_STRUCTRUE...")
      outlst$out_layer <- "P2VEG_SUBP_STRUCTRUE"
      if (!append_layer) index.unique.p2veg_subp_structure <- vcondstrkey
      datExportData(vsubpstrx, 
                    savedata_opts = outlst)
    }
    #rm(vsubpstrx)  


    ## 7.6 Return and/or save p2veg_subplot_spp (vsubsppx / P2VEG_SUBPLOT_SPP)
    ##################################################################
    if (!is.null(vsubpsppnm)) {
      if (is.null(vsubpsppx)) {
        vsubpsppa. <- "vsubpspp."

        ## Check variables
        vcondid <- findnm("CONDID", vsubpsppflds, returnNULL = TRUE)
        vsubp <- findnm("SUBP", vsubpsppflds, returnNULL = TRUE)
        keyvars <- c(vcondid, vsubp)
        if (any(sapply(keyvars, is.null))) {
          keymiss <- keyvars[sapply(keyvars, is.null)]
          stop("missing key variables in subp_cond data: ", toString(keymiss))
        }
           
        ## Build p2veg_subplot_spp FROM query
        vsubpsppjoinqry <- getjoinqry(vsubpsppid, pltidsid, vsubpsppa., pltidsa.)
        vsubpsppfromqry <- paste0(
          "\nJOIN ", SCHEMA., vsubpsppnm, " vsubpspp ", vsubpsppjoinqry) 
      
        ## Build p2veg_subplot_spp SELECT query
        # if (defaultVars) {
        #   vsubpsppvars <-  vsubpsppflds[vsubpsppflds %in% DBvars.default(isveg = TRUE)$vsubpsppvarlst]
        # } else {
        #   vsubpsppvars <- "*"
        # }
        vsubpsppvars <- c("VEG_FLDSPCD", "VEG_SPCD", "GROWTH_HABIT_CD", "LAYER")
        vsubpsppgrpvars <- paste0(c(vsubpsppid, vcondid, vsubpsppvars))
        vsubpsppsumvar <- "\n  COALESCE(SUM(COVER_PCT * 1.0) / 4 / 100 ,0) AS COVER_PCT_SUM"
        vsubpsppselectqry <- toString(c(paste0(vsubpsppa., vsubpsppgrpvars), vsubpsppsumvar))

        ## Build final p2veg_subplot_spp query, including pltidsqry
        vcondsppqry <- paste0(getdataWITHqry,
                            "\n-------------------------------------------",
                            "\n SELECT ", vsubpsppselectqry,
                            "\nFROM pltids",
                            vsubpsppfromqry,
                            "\nGROUP BY ", toString(paste0(vsubpsppa., vsubpsppgrpvars)))
        dbqueries$vcondspp <- vcondsppqry

        ## Run final p2veg_subplot_spp query, including pltidsqry
        if (datindb) {
          vcondsppx <- tryCatch(
            DBI::dbGetQuery(dbconn, vcondsppqry),
            error=function(e) {
              message(e,"\n")
              return(NULL)})
        } else {
          vcondsppx <- tryCatch(
            sqldf::sqldf(vcondsppqry, connection = NULL),
            error = function(e) {
              message(e,"\n")
              return(NULL) })
        }
        if (is.null(vcondsppx) || nrow(vcondsppx) == 0) {
          message("invalid condition-level p2veg_subplot_spp query...")
          message(vcondsppqry)
          return(NULL)
        }
      
        ## Return and/or save vsubpspp data
        vcondsppkey <- c(vsubpsppid, vcondid, vsubp)
        setkeyv(setDT(vcondsppx), vsubpsppid)
        if (!is.null(getdataCNs)) {
          vcondsppx <- vcondsppx[vcondsppx[[vsubpsppid]] %in% getdataCNs,]
        }
      } 
      
      ## Add to returnlst 
      if (returndata) {
        
        ## Append adjustment factors to tree data
        vcondsppx[pltidsadj, vadjfac := ADJ_FACTOR_P2VEG_SUBP]
        
        returnlst$vcondsppx <- vcondsppx
        returnlst$vcondsppid <- vsubpsppid
      }
      ## Save data
      if (savedata) {
        message("saving condition-level P2VEG_SUBPLOT_SPP...")
        outlst$out_layer <- "P2VEG_COND_SPP"
        if (!append_layer) index.unique.p2veg_cond_spp <- vcondsppkey
        datExportData(vcondsppx, 
                      savedata_opts = outlst)
      }
      rm(vcondsppx)
    }
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
  
  
  ## 10. Return data objects
  ######################################################################################
  if (!returndata) {
    returnlst$subplotx <- subplotnm
    returnlst$subp_condx <- subp_condnm
    returnlst$p2veg_subp_structure <- vsubpstrnm
    returnlst$p2veg_subplot_spp <- vsubpsppnm
  }
  returnlst$dbqueries <- dbqueries
  returnlst$dbqueriesWITH <- dbqueriesWITH
  returnlst$estfromqry <- estfromqry

  return(returnlst)
}
