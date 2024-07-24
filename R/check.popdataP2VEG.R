check.popdataP2VEG <- 
  function(tabs, tabIDs, popType, 
           datindb, pltaindb, pltidsqry,
           pltidsid, pltidvars, 
           pdoms2keep = NULL, 
           pltidsadjindb = FALSE, 
           defaultVars = TRUE,
           pltassgnid, pltx, adj, ACI, plotlst, 
           pltfromqry, pwhereqry = NULL, 
           condid = "CONDID", 
           areawt = "CONDPROP_UNADJ", areawt2 = NULL,
           MICRO_BREAKPOINT_DIA = 5, 
           MACRO_BREAKPOINT_DIA = NULL, 
           unitvars = NULL, 
           strunitvars = NULL, 
           nonsamp.cfilter = NULL, 
           cvars2keep = NULL, 
           dbconn = NULL, schema = NULL, schemadev = FALSE,
           returndata = FALSE, 
           savedata = FALSE, 
           outlst = NULL,
           gui = FALSE){

  ##############################################################################
  ## DESCRIPTION: Checks data inputs for CHNG estimation
  ## 1. Define variables necessary for estimation:
  ## - cvars2keep = c('PROP_BASIS', 'COND_NONSAMPLE_REASN_CD')
  ## 2.	Check if data are in a database (datindb) and if dbconn is valid.
  ## 3.	Get tables used in estimation from tabs.
  ## - PLOT; COND; SUBP_COND_CHNG_MTRX
  ## - TREE (if popType = 'GRM'); SEEDLING (if popType = 'GRM')
  ## - TREE_GRM_COMPONENT, TREE_GRM_BEGIN, TREE_GRM_MIDPT (if popType = 'GRM')
  ## 4.	Check for necessary variables in tables.
  ##    plot - PREV_PLT_CN
  ##    cond - (cuniqueid, condid, cvars2keep)
  ##    subp_cond_chng_mtrx - PREVCOND
  ## 5.	Build query for adjustment factors and append to pltids.
  ## 5.1.	Check proportion variables, including area weight.
  ## 5.2.	Build WITH query defining pltids in population.
  ## 5.3.	Build ADJ query to append adjustment factors to pltids.
  ## Note: If adj = ‘none’, adjustment factors = 1.
  ## 5.3.1.	Build ADJqry FROM statement.
  ## 5.3.2.	Append filters to ADJqry WHERE statement (i.e., excluding nonresponse).
  ##            COND_STATUS_CD <> 5; 
  ##            NF_COND_STATUS_CD is NULL or NF_COND_STATUS_CD <> 5 (ACI=TRUE)
  ##            c.CONDPROP_UNADJ IS NOT NULL
  ##            (sccm.SUBPTYP = 3 AND c.PROP_BASIS = 'MACR')
  ##               OR (sccm.SUBPTYP = 1 AND c.PROP_BASIS = 'SUBP')
  ##             COALESCE(c.COND_NONSAMPLE_REASN_CD, 0) = 0
  ##             COALESCE(pcond.COND_NONSAMPLE_REASN_CD, 0) = 0
  ## 5.3.3.	Build and run ADJ query for adjustment factors based on popType.
  ## 5.3.4.     Build final query to append adjustment factors to pltids, including ADJ query.
  ## 5.4.	Run query to append adjustment factors to pltids (pltidsadj).
  ## 6.	Build and run queries for PLOT/COND (pltcondx).
  ## 6.1.	PLOT/COND data (pltcondx)
  ## 6.2.	Adjusted area weight table (areawtx).
  ## 7. Create return list with pltidsadj, condx, pltcondx, and adjfactors.
  ## 8. Build and run queries for other necessary tables (if returndata = TRUE)
  ## 8.1. Return and/or save plot data (pltx / PLOT)
  ## 8.2. Return and/or save cond data (condx / COND)
  ## 8.3. Return and/or save tree data (treex / TREE)
  ##      Note: if returndata=TRUE, append adjustment factors to treex
  ## 8.4. Return and/or save seedling data (seedx / SEEDLING)
  ##      Note: if returndata=TRUE, append adjustment factors to seedx
  ## 9. Check COND_STATUS_CD and generate table with number of conditions
  ## 9.1. Sampled conditions
  ## 9.2. Sampled nonforest conditions
  ## 10. Add to returnlst: condsampcnt, other tables (if returndata=TRUE), dbqueries
  ###################################################################################
  
  
  ## Set global variables
  vsubpsppx <- NULL
  subpid <- "SUBP"
  SCHEMA.=suffix <- ""
  dbqueries=dbqueriesWITH <- list()
  propvars <- list(COND="CONDPROP_UNADJ", SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
#  vpropvars <- list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")
#  subppropvars <- c("MICRCOND", "SUBPCOND", "MACRCOND")
#  subpcpropvars <- c("MICRCOND_PROP", "SUBPCOND_PROP", "MACRCOND_PROP")
  diavar <- "DIA"
  pltcondindb <- datindb

#  if (schemadev) {
#    suffix <- "@fiadb_dev"
#  }
  suffix=whereqry=areawt2nm <- NULL
  
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
    } else {
      dbtablst <- DBI::dbListTables(dbconn)
      SCHEMA. <- ifelse (is.null(schema), "", paste0(schema, "."))
    }
  }

  ##############################################################################
  ## 3. Get tables used in estimation from tabs
  ##############################################################################

  ## plot table
  plotnm <- plotlst$tabnm
  plotflds <- plotlst$tabflds
  puniqueid <- plotlst$tabid
  plotx <- plotlst$tabx

  ## cond table
  tabnames <- c("condu", "cond")
  condlst <- popTabchk(tabnames, tabtext = "cond", 
                       tabs, tabIDs, dbtablst, dbconn, datindb) 
  condnm <- condlst$tabnm
  condflds <- condlst$tabflds
  cuniqueid <- condlst$tabid
  condx <- condlst$tabx

  if (is.null(condnm)) {
    stop("must include cond for CHNG estimates")
  }
  if (datindb && !pltaindb) {
    assign(condnm, DBI::dbReadTable(dbconn, condnm))
  }

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
  }
  
  ## p2veg_subplot_spp table
  vsubpspplst <- popTabchk(c("p2veg_subplot_spp", "vsubpspp"), 
                           tabtext = "p2veg_subplot_spp",
                           tabs, tabIDs, dbtablst, dbconn, datindb)
  vsubpsppnm <- vsubpspplst$tabnm
  vsubpsppflds <- vsubpspplst$tabflds
  vsubpsppid <- vsubpspplst$tabid
  vsubpsppx <- vsubpspplst$tabx
  
 
  ##############################################################################
  ## 4. Check for necessary variables in tables
  ##############################################################################
  
  ## 4.1. Cond table
  ##############################################################################
  
  ## 4.1.1. Check cuniqueid
  cuniqueid <- pcheck.varchar(var2check = cuniqueid, varnm="cuniqueid", gui=gui,
        checklst = condflds, caption="Unique identifier of plot in cond",
        warn = paste(cuniqueid, "not in cond"), stopifnull = TRUE)
  
  ## 4.1.2. Check condid
  condid <- pcheck.varchar(var2check = condid, varnm="condid", gui=gui,
        checklst = condflds, caption="Unique identifier of conditions in cond",
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
  
  ## 4.2. Subplot table
  ##############################################################################
  
  ## 4.2.1. Check subpid
  subpid <- pcheck.varchar(var2check = subpid, varnm="subpid", gui=gui,
          checklst = subplotflds, caption="Unique identifier of subplots",
          warn = paste(subpid, "not in subplot"), stopifnull = TRUE)
  
  ## 4.2.2 Check subpvars2keep in subplot/subp_cond
  #subpflds <- unique(c(subplotflds, subp_condflds))

  
  ##############################################################################
  ## 5. Build query for adjustment factors and append to pltids
  ##############################################################################
  
  ## 5.1. Check proportion variables, including area weight 
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
  

  ## 5.2. Build WITH query defining pltids in population
  #######################################################################
  pltidsWITH.qry <- paste0(
    "WITH",
    "\npltids AS",
    "\n(", pltidsqry, ")")
  pltidsvars <- c(puniqueid, unitvars)
  pltidsa. <- "pltids."
  ## message(pltidsWITH.qry)
  
  
  ## 5.3. Build ADJ query to append adjustment factors to pltids
  #######################################################################
  plota. <- "p."
  conda. <- "c."
  subpa. <- "subp."
  subpca. <- "subpc."
  
  ## Build ADJqry FROM statement
  pjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
  cjoinqry <- getjoinqry(cuniqueid, puniqueid, conda., plota.)
  pcfromqry <- paste0(
    "\n FROM pltids",
    "\n JOIN ", SCHEMA., plotnm, suffix, " p ", pjoinqry,
    "\n JOIN ", SCHEMA., condnm, suffix, " c ", cjoinqry)
  
  ## Append joins from subplot and subp_cond		 
  subpjoinqry <- getjoinqry(subplotid, puniqueid, subpa., plota.)
  P2VEGfromqry <- paste0(
    pcfromqry,                    
    "\n JOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry,
    "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subplotid, " = ", conda., cuniqueid, 
    " AND ", subpca., condid, " = ", conda., condid, 
    " AND ", subpca., subpid, " = ", subpa., subpid, ")")
  
  
  ## Build ADJqry FROM statement (i.e., excluding nonresponse)
  adjwhereqry <- NULL
  
  if (adj != "none") {
    whereqry <- pwhereqry
    P2VEGwhereqry <- {}
    
    ## Condition filters
    #################################################################
    
    ## Filter for nonsampled conditions
    ## (COND_STATUS_CD <> 5)
    cstatusnm <- findnm("COND_STATUS_CD", condflds, returnNULL = TRUE)
    if (is.null(cstatusnm)) {
      message("COND_STATUS_CD is not in dataset... assuming all conditions are sampled")
    } else {
      ## Build where query to remove conditions that were not sampled
      cstatus.filter <- paste0(conda., cstatusnm, " <> 5")
      if (is.null(adjwhereqry)) {
        adjwhereqry <- cstatus.filter
      } else {
        adjwhereqry <- paste0(adjwhereqry, 
                              "\n   AND ", cstatus.filter)
      }	
      if (is.null(P2VEGwhereqry)) {
        P2VEGwhereqry <- paste0("\n WHERE ", cstatus.filter)
      } else {
        P2VEGwhereqry <- paste0(P2VEGwhereqry, 
                                "\n   AND ", cstatus.filter)
      }	
    }
    
    ## If ACI, filter for nonsampled nonforest conditions 
    ## (NF_COND_STATUS_CD is NULL or NF_COND_STATUS_CD <> 5)
    if (ACI) {
      cnfstatusnm <- findnm("NF_COND_STATUS_CD", condflds, returnNULL = TRUE)
      if (is.null(cnfstatusnm)) {
        message("NF_COND_STATUS_CD is not in dataset... assuming all nonforest conditions are sampled")
      } else {
        ## Build where query to remove nonforest conditions that were not sampled
        cnfstatus.filter <- paste0("(", conda., cnfstatusnm, " IS NULL OR ", conda., cnfstatusnm, " <> 5)")
        if (is.null(adjwhereqry)) {
          adjwhereqry <- cnfstatus.filter
        } else {
          adjwhereqry <- paste0(adjwhereqry, 
                                "\n   AND ", cnfstatus.filter)
        }	
        if (is.null(P2VEGwhereqry)) {
          P2VEGwhereqry <- paste0("\n WHERE ", cnfstatus.filter)
        } else {
          P2VEGwhereqry <- paste0(P2VEGwhereqry, 
                                  "\n   AND ", cnfstatus.filter)
        }	
      }
    }

    ## Subplot filters
    #################################################################
    
    ## Filter for nonsampled subplots in subplot
    ## (SUBP_STATUS_CD <> 3)
    subpstatusnm <- findnm("SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
    if (is.null(subpstatusnm)) {
      message("SUBP_STATUS_CD is not in dataset... assuming all subplots are sampled")
    } else {
      ## Build where query to remove subplots that wasn't sampled
      subpstatus.filter <- paste0(subpa., subpstatusnm, " <> 3")
      if (is.null(P2VEGwhereqry)) {
        P2VEGwhereqry <- paste0("\n WHERE ", subpstatus.filter)
      } else {
        P2VEGwhereqry <- paste0(P2VEGwhereqry, 
                                "\n    AND ", subpstatus.filter)
      }	
    }
    
    ## If ACI, filter for nonsampled nonforest subplots in subplot
    ## (NF_SUBP_STATUS_CD is NULL or NF_SUBP_STATUS_CD <> 3)
    subpnfstatusnm <- findnm("SUBP_STATUS_CD", subplotflds, returnNULL = TRUE)
    if (is.null(subpnfstatusnm)) {
      message("NF_SUBP_STATUS_CD is not in dataset... assuming all nonforest subplots are sampled")
    } else {
      ## Build where query to remove nonforest subplots that were not sampled
      subpnfstatus.filter <- paste0("(", subpa., subpnfstatusnm, " IS NULL OR ", subpa., subpnfstatusnm, " <> 3)")
      if (is.null(P2VEGwhereqry)) {
        P2VEGwhereqry <- paste0("\n WHERE ", subpnfstatus.filter)
      } else {
        P2VEGwhereqry <- paste0(P2VEGwhereqry, 
                                "\n    AND ", subpnfstatus.filter)
      }	
    }
    
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
      
      sampmethodnm <- findnm("SAMP_METHOD_CD", plotflds, returnNULL = TRUE)
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
  
  
  ## 5.4. Build query for adjustment factors based on popType (ADJqry)
  
  ## 5.4.1. First, build query for VOL adjustments
  ADJqry <- 
    getADJqry(popType = "VOL",
              adj = adj,
              propvars = propvars,
              adjfromqry = pcfromqry,
              pwhereqry = paste0("\n WHERE ", adjwhereqry),
              pltassgnid = pltassgnid,
              cuniqueid = cuniqueid,
              strunitvars = strunitvars,
              selecta. = "pltids.",
              propqry = NULL)
  #message(ADJqry)
  dbqueries$ADJqry <- ADJqry    
  
  ## 5.5. Build final query for adjustment factors, including pltids WITH query
  adjfactors.qry <- paste0(
    pltidsWITH.qry, 
    "\n-------------------------------------------",
    "\n", ADJqry
  )
  ## message(adjfactors.qry)
  
  ## Run query to calculate adjustment factors
  if (datindb) {
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
  }
  dbqueries$adjfactors <- adjfactors.qry
  

  ## 5.4.2. Next, build query for P2VEG adjustments
  
  ## First, get query for summarizing subplot sampled proportions
  sumpropqry <- sumpropP2VEGqry(fromqry = P2VEGfromqry, 
                               whereqry = P2VEGwhereqry,
                               ACI = ACI,
                               selectvars = toString(paste0("pltids.", strunitvars)),
                               SCHEMA. = SCHEMA.)
  

  ## Next, add sumpropqry to get getADJqry to build a subquery
  adjjoinqry <- getjoinqry(strunitvars, strunitvars, "adj.", "c.")
  adjfromqry <- paste0(
    "\n FROM subpcprop c",
    "\n JOIN adjfactors adj ", adjjoinqry)
  othervars <- c(propvars['SUBP'],propvars['MACR'],propvars['MICR'])
  
  ADJqryP2VEG <- 
    getADJqry(popType = popType,
              adj = adj,
              propvars = propvars['COND'],
              adjfromqry = adjfromqry,
              pwhereqry = NULL,
              cuniqueid = cuniqueid,
              pltassgnid = pltassgnid,
              strunitvars = strunitvars,
              selecta. = "c.",
              othervars <- othervars,
              propqry = NULL)
  #message(ADJqryP2VEG)

  
  ## 5.5. Build final query for adjustment factors, including pltids WITH query
  adjfactorsP2VEG.qry <- paste0(
    pltidsWITH.qry, ", ",
    "\n----- calculate VOL adjustment factors",
    "\nadjfactors AS ",
    "\n(", ADJqry, "),",
    "\n----- sum sampled subplot proportions",
    "\nsubpcprop AS ",
    "\n(", sumpropqry, ")",
    "\n-------------------------------------------",
    "\n", ADJqryP2VEG)
  ##message(adjfactorsP2VEG.qry)

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
    message(adjfactorsP2VEGqry)
    return(NULL)
  }
  dbqueries$adjfactorsP2VEG <- adjfactorsP2VEG.qry
  
  
  ## Check with FIADB population data - VOL
  #source("C:\\_tsf\\FIESTA\\FIESTA_WebApp\\EVALIDator_compare\\compareADJ.R") 
  #FIADBpop <- getFIADBpop(state, evaltype = "03", evalyr, dbconn=dbconn)$pop_stratum
  #popVOL_compare <- checkpop(FIADBpop, FIESTApop = adjfactors, evaltype="03")
  #popVOL_compare
  
  
  ## 5.6. Build WITH query to append adjustment factors to pltids, including ADJ query
  pltidsadjWITH.qry <- paste0(
    pltidsWITH.qry, ", ",
    "\n----- calculate VOL adjustment factors",
    "\nadjfactors AS ",
    "\n(", ADJqry, "),",
    "\n----- sum sampled subplot proportions",
    "\nsubpcprop AS ",
    "\n(", sumpropqry, "),",
    "\n----- calculate P2VEG adjustment factor",
    "\nadjfactorsP2VEG AS ",
    "\n(", ADJqryP2VEG, ")")
  #message(pltidsadjWITH.qry)
  
  ## 5.7. Build and run final query to append adjustment factors to pltids, including ADJ query
  adja. <- "adj."
  adjP2VEG. <- "adjP2VEG."
  adjvars <- sapply(propvars, function(x) {
    ifelse(grepl("PROP_UNADJ", x), paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)), 
           ifelse (grepl("prop_unadj", x), paste0("ADJ_FACTOR_", toupper(sub("prop_unadj", "", x))), 
                   paste0(x, "_ADJ"))) })
  adjvars['P2VEG'] <- "ADJ_FACTOR_P2VEG_SUBP"
  #selectvars <- toString(c(paste0(pltidsa., pltidvars), paste0(adja., adjvars)))
  selectvars <- toString(c(paste0(pltidsa., pltidvars), adjvars))
  
  if (adj == "samp") {
    adjjoinqry <- getjoinqry(strunitvars, strunitvars, adja., pltidsa.)
    adjP2VEGjoinqry <- getjoinqry(strunitvars, strunitvars, adjP2VEG., pltidsa.)
  } else { ## adj = "plot"
    adjjoinqry <- getjoinqry(pltassgnid, pltassgnid, adja., pltidsa.)
    adjP2VEGjoinqry <- getjoinqry(strunitvars, strunitvars, adjP2VEG., pltidsa.)
  }
  
  ## Build pltidsadjFROM.qry
  pltidsadjFROM.qry <- paste0(
    "\nFROM pltids",
    "\nJOIN adjfactors adj ", adjjoinqry,
    "\nJOIN adjfactorsP2VEG adjP2VEG ", adjP2VEGjoinqry)
  
  
  ## Build pltidsadjqry query
  pltidsadj.qry <- paste0(
    pltidsadjWITH.qry,
    "\n-------------------------------------------",
    paste0("\nSELECT ", selectvars,
           pltidsadjFROM.qry)
  )
  #message(pltidsadj.qry)
  
  
  ## Run query to identify plotids, including adjustment factors
  if (datindb) {
    pltidsadj <- tryCatch(
        DBI::dbGetQuery(dbconn, pltidsadj.qry),
                 error=function(e) {
                   message("invalid pltids query...")
                   message(e,"\n")
                   return(NULL)})
  } else {
    pltidsadj <- tryCatch(
        sqldf::sqldf(pltidsadj.qry, connection = NULL),
                  error = function(e) {
                    message("invalid pltids query...")
                    message(e,"\n")
                    return(NULL) })
  }
  if (is.null(pltidsadj) || nrow(pltidsadj) == 0) {
    message(pltidsadj.qry)
    return(NULL)
  }
  setkeyv(setDT(pltidsadj), pltidsid) 
  dbqueries$pltidsadj <- pltidsadj.qry   
  
  
  ## 5.8. Build WITH query to identify pltids, including adjustment factors
  if (pltidsadjindb) {
    pltidsadjWITH.qry <- paste0( 
      "WITH pltids AS",
      "\n(SELECT ", toString(adjvars), 
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
  ## 6. Build and run queries for PLOT/COND (pltcondx).
  ##############################################################################
  
  ## 6.1.	Build SELECT query for pltcondx query, including FORTYPGRP
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
  pcondselectqry <- toString(paste0("pcond.", unique(c(condvars, cvars2keep))))
  pltcondflds <- unique(c(condvars, cvars2keep, pvars))

  ## 6.2.	Add FORTYPGRP to SELECT query
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
  
    ## 6.1.4. Run final plot/cond query, including pltidsqry
    if (datindb) {
      pltcondx <- tryCatch(
        DBI::dbGetQuery(dbconn, pltcondxqry),
                  error=function(e) {
                    message("invalid pltcondx query...")
                    warning(e)
                    return(NULL)})
    } else {
      pltcondx <- tryCatch(
        sqldf::sqldf(pltcondxqry, connection = NULL),
                  error = function(e) {
                    message("invalid pltcondx query...")
                    message(e,"\n")
                    return(NULL) })
    }
    if (is.null(pltcondx) || nrow(pltcondx) == 0) {
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
      areawtcase <- paste0("\nCASE pc.", propvars['MACR'], " IS NULL", 
                           " THEN ", adjvars['SUBP'], 
                           " ELSE ", adjvars['MACR'], " END")
    } else {
      areawtcase <- paste0("\nCASE pc.", propbasisnm, 
                           " WHEN 'MACR' THEN ", adjvars['MACR'], 
                           " ELSE ", adjvars['SUBP'], " END")
    }
  }
    
  ## 6.7. Build CASE statement for adding adjustment factors to SELECT
  ##################################################################
  if (adj %in% c("samp", "plot")) {
    propbasisnm <- findnm("PROP_BASIS", condflds, returnNULL=TRUE)
    
    if (is.null(propbasisnm)) {
      areawtcase <- paste0("CASE pc.", propvars['MACR'], " IS NULL", 
                           " THEN ", adjvars['SUBP'], 
                           " ELSE ", adjvars['MACR'], " END")
    } else {
      areawtcase <- paste0("CASE pc.", propbasisnm, 
                           " WHEN 'MACR' THEN ", adjvars['MACR'], 
                           " ELSE ", adjvars['SUBP'], " END")
    }
  }

  ##############################################################################
  ## 7.	Create return list with pltidsadj, adjfactors, and pltcondx/areawtx, if returndata=TRUE. 
  ##############################################################################  
  returnlst <- list(pltidsadj = pltidsadj,
                    pltcondflds = pltcondflds,
                    cuniqueid = cuniqueid, condid = condid, 
                    adjfactors = adjfactors,
                    areawtcase = areawtcase,
                    varadjP2VEG = "ADJ_FACTOR_P2VEG_SUBP",
                    adjvarlst = adjvars)
  
  if (returndata || savedata) {
    returnlst$pltcondx <- pltcondx
  } else {
    returnlst$pltcondx <- "pltcondx"
  }

  
  ##############################################################################
  ## 8. Build and run queries for other necessary tables (if returndata = TRUE) 
  ##############################################################################  
  if (returndata || savedata) {

    ## 8.1 Return and/or save plot data (pltcondx)
    ##################################################################
    # if (is.null(pltx)) {
    #   ## 8.1.1. Build plot FROM query
    #   plotjoinqry <- getjoinqry(puniqueid, pltidsid, plota., pltidsa.)
    #   plotfromqry <- paste0("\nJOIN ", SCHEMA., plotnm, " p ", plotjoinqry)
    #   
    #   ## 8.1.2. Build plot SELECT query
    #   pselectqry <- toString(paste0(plota., c(puniqueid, pdoms2keep)))
    #   
    #   ## 8.1.3. Build final plot query, including pltidsqry
    #   pltqry <- paste0("\nSELECT ", pselectqry,
    #                    "\nFROM pltids",
    #                    plotfromqry) 
    #   pltqry <- paste0("WITH pltids AS ",
    #                    "\n(", pltidsqry, ")",
    #                    pltqry)
    #   dbqueries$PLOT <- pltqry
    #   
    #   ## 8.1.4. Run final plot query, including pltidsqry
    #   if (datindb) {
    #     pltx <- tryCatch(
    #       DBI::dbGetQuery(dbconn, pltqry),
    #       error=function(e) {
    #         message("invalid plot query...")
    #         message(e,"\n")
    #         return(NULL)})
    #   } else {
    #     pltx <- tryCatch(
    #       sqldf::sqldf(pltqry, connection = NULL),
    #       error = function(e) {
    #         message("invalid plot query...")
    #         message(e,"\n")
    #         return(NULL) })
    #   }
    #   if (is.null(pltx) || nrow(pltx) == 0) {
    #     message(pltqry)
    #     return(NULL)
    #   }
    # }
    # 
    # ## 8.1.5. Return and/or save plot data
    # setkeyv(setDT(pltx), puniqueid)
    # 
    # ## Add to returnlst 
    # if (returndata) {
    #   #returnlst$pltx <- pltx
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
    # rm(pltx) 
    # 
    # 
    # ## 8.2 Return and/or save cond data (condx / COND)
    # ##################################################################
    # 
    # ## 8.2.1. Build cond FROM query
    # condjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
    # condfromqry <- paste0("\nJOIN ", SCHEMA., condnm, " c ", condjoinqry)
    # 
    # ## 8.2.2. Build cond SELECT query
    # if (defaultVars) {
    #   condvars <-  condflds[condflds %in% DBvars.default()$condvarlst]
    # } else {
    #   condvars <- "*"
    # }
    # condselectqry <- toString(paste0(conda., condvars))
    # 
    # ## 8.2.3. Build final cond query, including pltidsqry
    # condqry <- paste0("\nSELECT ", condselectqry,
    #                   "\nFROM pltids",
    #                   condfromqry) 
    # condqry <- paste0("WITH pltids AS ",
    #                   "\n(", pltidsqry, ")",
    #                   condqry)
    # dbqueries$COND <- condqry
    # 
    # ## 8.2.4. Run final cond query, including pltidsqry
    # if (datindb) {
    #   condx <- tryCatch(
    #     DBI::dbGetQuery(dbconn, condqry),
    #     error=function(e) {
    #       message("invalid cond query...")
    #       message(e,"\n")
    #       return(NULL)})
    # } else {
    #   condx <- tryCatch(
    #     sqldf::sqldf(condqry, connection = NULL),
    #     error = function(e) {
    #       message("invalid cond query...")
    #       message(e,"\n")
    #       return(NULL) })
    # }
    # if (is.null(condx) || nrow(condx) == 0) {
    #   message(condqry)
    #   return(NULL)
    # }
    # 
    # ## 8.2.5. Return and/or save cond data
    # condkey <- c(cuniqueid, condid)
    # setkeyv(setDT(condx), condkey)
    # 
    # ## Add to returnlst 
    # if (returndata) {
    #   #returnlst$condx <- condx
    #   returnlst$cuniqueid <- cuniqueid
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
    
    
    ## 8.3 Return subp_cond_chng_matrx data
    ###################################################################

    ## 8.3.1. Check variables
    subp <- findnm("SUBP", subplotflds, returnNULL = TRUE)
    keyvars <- c(subp)
    if (any(sapply(keyvars, is.null))) {
      keymiss <- keyvars[sapply(keyvars, is.null)]
      stop("missing key variables in subplot data: ", toString(keymiss))
    }
    
    ## 8.3.2. Build subplot FROM query
    subpjoinqry <- getjoinqry(subplotid, pltidsid, subpa., pltidsa.)
    subpfromqry <- paste0(
      "\nFROM pltids", 
      "\nJOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry)
    
    ## 8.3.3. Build subplot SELECT query
    if (defaultVars) {
      subpvars <-  subplotflds[subplotflds %in% DBvars.default(issubp = TRUE)$subpvarlst]
    } else {
      subpvars <- "*"
    }
    subpselectqry <- toString(paste0(subpa., subpvars))
    
    ## 8.3.4. Build final subplot query, including pltidsqry
    subplotqry <- paste0("\n SELECT ", subpselectqry,
                         subpfromqry) 
    subplotqry <- paste0("WITH pltids AS ",
                         "\n(", pltidsqry, ")",
                         "\n-------------------------------------------",
                         subplotqry)
    dbqueries$subplot <- subplotqry
    
    ## 8.3.5. Run final subplot query, including pltidsqry
    if (datindb) {
      subplotx <- tryCatch(
        DBI::dbGetQuery(dbconn, subplotqry),
        error=function(e) {
          message("invalid subplot query...")
          message(e,"\n")
          return(NULL)})
    } else {
      subplotx <- tryCatch(
        sqldf::sqldf(subplotqry, connection = NULL),
        error = function(e) {
          message("invalid subplot query...")
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(subplotx) || nrow(subplotx) == 0) {
      message(subplotqry)
      return(NULL)
    }
    
    ## Set key on data.table
    subplotkey <- c(subplotid, subp)
    setkeyv(setDT(subplotx), subplotid)
    
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
    
    
    ## 8.4 Return and/or save subp_cond data (subp_condx / SUBP_COND)
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
    
    ## 8.4.2. Build subp_cond FROM query
    subpcjoinqry <- getjoinqry(subp_condid, pltidsid, subpca., pltidsa.)
    subpcfromqry <- paste0(
      "\nFROM pltids",
      "\nJOIN ", SCHEMA., subp_condnm, " subpc ", subpcjoinqry)

        
    ## 8.4.3. Build subp_cond SELECT query
    if (defaultVars) {
      subpcvars <-  subp_condflds[subp_condflds %in% DBvars.default(issubp = TRUE)$subpcvarlst]
    } else {
      subpcvars <- "*"
    }
    subpcselectqry <- toString(paste0(subpca., subpcvars))
    
    ## 8.4.4. Build final subp_cond query, including pltidsqry
    subp_condqry <- paste0("\n SELECT ", subpcselectqry,
                           subpcfromqry) 
    subp_condqry <- paste0("WITH pltids AS ",
                           "\n(", pltidsqry, ")",
                           "\n-------------------------------------------",
                           subp_condqry)
    dbqueries$subp_cond <- subp_condqry
    
    ## 8.4.5. Run final subp_cond query, including pltidsqry
    if (datindb) {
      subp_condx <- tryCatch(
        DBI::dbGetQuery(dbconn, subp_condqry),
        error=function(e) {
          message("invalid subp_cond query...")
          message(e,"\n")
          return(NULL)})
    } else {
      subp_condx <- tryCatch(
        sqldf::sqldf(subp_condqry, connection = NULL),
        error = function(e) {
          message("invalid subp_cond query...")
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(subp_condx) || nrow(subp_condx) == 0) {
      message(subp_condqry)
      return(NULL)
    }
    
    ## 8.4.6. Return and/or save subp_cond data
    subp_condkey <- c(subp_condid, subpcondid, subp)
    setkeyv(setDT(subp_condx), subp_condid)
    
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
      datExportData(SUBP_COND, 
                    savedata_opts = outlst)
    }
    #rm(subp_condx)
    
    
    
    ## 8.5 Return and/or save p2veg_subp_structure (vsubstrx / P2VEG_SUBP_STRUCTRUE)
    ##################################################################
    vsubpstra. <- "vsubpstr."
    
    ## 8.5.1. Check variables
    vcondid <- findnm("CONDID", vsubpstrflds, returnNULL = TRUE)
    vsubp <- findnm("SUBP", vsubpstrflds, returnNULL = TRUE)
    keyvars <- c(vcondid, vsubp)
    if (any(sapply(keyvars, is.null))) {
      keymiss <- keyvars[sapply(keyvars, is.null)]
      stop("missing key variables in subp_cond data: ", toString(keymiss))
    }
    
    ## 8.5.2. Build p2veg_subp_structure FROM query
    vsubpstrjoinqry <- getjoinqry(vsubpstrid, pltidsid, vsubpstra., pltidsa.)
    vsubpstrfromqry <- paste0(
      "\nFROM pltids",
      "\nJOIN ", SCHEMA., vsubpstrnm, " vsubpstr ", vsubpstrjoinqry) 
    
    ## 8.5.3. Build p2veg_subp_structure SELECT query
    if (defaultVars) {
      vsubpstrvars <-  vsubpstrflds[vsubpstrflds %in% DBvars.default(isveg = TRUE)$vsubpstrvarlst]
    } else {
      vsubpstrvars <- "*"
    }
    vsubpstrselectqry <- toString(paste0(vsubpstra., vsubpstrvars))
    
    ## 8.5.4. Build final p2veg_subp_structure query, including pltidsqry
    vsubpstrqry <- paste0("\n SELECT ", vsubpstrselectqry,
                          vsubpstrfromqry) 
    vsubpstrqry <- paste0("WITH pltids AS ",
                          "\n(", pltidsqry, ")",
                          "\n-------------------------------------------",
                          vsubpstrqry)
    dbqueries$vsubpstr <- vsubpstrqry
    
    ## 8.5.5. Run final p2veg_subp_structure query, including pltidsqry
    if (datindb) {
      vsubpstrx <- tryCatch(
        DBI::dbGetQuery(dbconn, vsubpstrqry),
        error=function(e) {
          message("invalid p2veg_subp_structure query...")
          message(e,"\n")
          return(NULL)})
    } else {
      vsubpstrx <- tryCatch(
        sqldf::sqldf(vsubpstrqry, connection = NULL),
        error = function(e) {
          message("invalid p2veg_subp_structure query...")
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(vsubpstrx) || nrow(vsubpstrx) == 0) {
      message(vsubpstrqry)
      return(NULL)
    }
    
    ## 8.5.6. Return and/or save vsubpstr data
    vsubpstrkey <- c(vsubpstrid, vcondid, vsubp)
    setkeyv(setDT(vsubpstrx), vsubpstrid)
    
    ## Add to returnlst 
    if (returndata) {
      
      ## Append adjustment factors to tree data
      vsubpstrx[pltidsadj, vadjfac := ADJ_FACTOR_P2VEG_SUBP]
      
      returnlst$vsubpstrx <- vsubpstrx
      returnlst$vsubpstrid <- vsubpstrid
    }
    ## Save data
    if (savedata) {
      message("saving P2VEG_SUBP_STRUCTRUE...")
      outlst$out_layer <- "P2VEG_SUBP_STRUCTRUE"
      if (!append_layer) index.unique.p2veg_subp_structure <- vsubpstrkey
      datExportData(vsubpstrx, 
                    savedata_opts = outlst)
    }
    #rm(vsubpstrx)  
    
    
    ## 8.6 Return and/or save p2veg_subplot_spp (vsubsppx / P2VEG_SUBPLOT_SPP)
    ##################################################################
    if (!is.null(vsubpsppnm)) {
      vsubpsppa. <- "vsubpspp."
      
      ## 8.6.1. Check variables
      vcondid <- findnm("CONDID", vsubpsppflds, returnNULL = TRUE)
      vsubp <- findnm("SUBP", vsubpsppflds, returnNULL = TRUE)
      keyvars <- c(vcondid, vsubp)
      if (any(sapply(keyvars, is.null))) {
        keymiss <- keyvars[sapply(keyvars, is.null)]
        stop("missing key variables in subp_cond data: ", toString(keymiss))
      }
           
      ## 8.6.2. Build p2veg_subplot_spp FROM query
      vsubpsppjoinqry <- getjoinqry(vsubpsppid, pltidsid, vsubpsppa., pltidsa.)
      vsubpsppfromqry <- paste0(
        "\nFROM pltids",
        "\nJOIN ", SCHEMA., vsubpsppnm, " vsubpspp ", vsubpsppjoinqry) 
      
      ## 8.6.3. Build p2veg_subplot_spp SELECT query
      if (defaultVars) {
        vsubpsppvars <-  vsubpsppflds[vsubpsppflds %in% DBvars.default(isveg = TRUE)$vsubpsppvarlst]
      } else {
        vsubpsppvars <- "*"
      }
      vsubpsppselectqry <- toString(paste0(vsubpsppa., vsubpsppvars))
      
      ## 8.6.4. Build final p2veg_subplot_spp query, including pltidsqry
      vsubpsppqry <- paste0("\n SELECT ", vsubpsppselectqry,
                            vsubpsppfromqry) 
      vsubpsppqry <- paste0("WITH pltids AS ",
                            "\n(", pltidsqry, ")",
                            "\n-------------------------------------------",
                            vsubpsppqry)
      dbqueries$vsubpspp <- vsubpsppqry
      
      ## 8.6.5. Run final p2veg_subplot_spp query, including pltidsqry
      if (datindb) {
        vsubpsppx <- tryCatch(
          DBI::dbGetQuery(dbconn, vsubpsppqry),
          error=function(e) {
            message("invalid p2veg_subplot_spp query...")
            message(e,"\n")
            return(NULL)})
      } else {
        vsubpsppx <- tryCatch(
          sqldf::sqldf(vsubpsppqry, connection = NULL),
          error = function(e) {
            message("invalid p2veg_subplot_spp query...")
            message(e,"\n")
            return(NULL) })
      }
      if (is.null(vsubpsppx) || nrow(vsubpsppx) == 0) {
        message(vsubpsppqry)
        return(NULL)
      }
      
      ## 8.6.6. Return and/or save vsubpspp data
      vsubpsppkey <- c(vsubpsppid, vcondid, vsubp)
      setkeyv(setDT(vsubpsppx), vsubpsppid)
      
      ## Add to returnlst 
      if (returndata) {
        
        ## Append adjustment factors to tree data
        vsubpsppx[pltidsadj, vadjfac := ADJ_FACTOR_P2VEG_SUBP]
        
        returnlst$vsubpsppx <- vsubpsppx
        returnlst$vsubpsppid <- vsubpsppid
      }
      ## Save data
      if (savedata) {
        message("saving P2VEG_SUBPLOT_SPP...")
        outlst$out_layer <- "P2VEG_SUBPLOT_SPP"
        if (!append_layer) index.unique.p2veg_subplot_spp <- vsubpsppkey
        datExportData(vsubpsppx, 
                      savedata_opts = outlst)
      }
      rm(vsubpsppx)        
    }
  }

  ##############################################################################
  ## 9. Check COND_STATUS_CD and generate table with number of conditions
  ##############################################################################
  pltcondflds <- names(pltcondx)
  
  
  ## 9.1. Sampled conditions
  ##########################################################################
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
    condsampcnt <- pltcondx[, .N, by=cstatuscdnm]
    setnames(condsampcnt, "N", "NBRCONDS")
    
    condsampcnt <-
      cbind(COND_STATUS_NM = ref_cond_status_cd[match(condsampcnt$COND_STATUS_CD,
                                                      ref_cond_status_cd$VALUE), "MEANING"], condsampcnt)
    
    ## Create nonsamp.cfilter
    if (!is.null(cstatuscdnm) && (is.null(nonsamp.cfilter) || nonsamp.cfilter == "")) {
      nonsamp.cfilter <- paste0("c.", "COND_STATUS_CD <> 5")
    }
    nbrnonsampled <- condsampcnt$NBRPLOTS[condsampcnt$COND_STATUS_CD == 5]
    if (length(nbrnonsampled) > 0) {
      message("removing ", nbrnonsampled, " nonsampled forest conditions")
    }						  
  } 
  
  ## 9.2. Sampled nonforest conditions
  ##########################################################################
  
  ## If ACI, check NF_PLOT_STATUS_CD and generate table with number of plots
  ##########################################################################
  if (popFilter$ACI) {
    nfcstatuschk <- findnm("NF_COND_STATUS_CD", pltcondflds, returnNULL=TRUE)
    if (is.null(nfcstatuschk)) {
      message("NF_COND_STATUS_CD not in dataset.. assuming all ACI nonforest conditions sampled")
    } else {  
      ref_nf_cond_status_cd <- ref_codes[ref_codes$VARIABLE == "NF_COND_STATUS_CD", ]
      if (length(nfcstatuschk) > 1) {
        nfcstatuscdnm <- nfcstatuschk[1]
      } else {
        nfcstatuscdnm <- nfcstatuschk
      }  
      
      ## Generate table of sampled/nonsampled plots (if ACI, nonforest status included)
      nfcondsampcnt <- pltcondx[, .N, by=nfcstatuscdnm]
      setnames(nfcondsampcnt, "N", "NBRCONDS")
      
      nfcondsampcnt <- nfcondsampcnt[!is.na(nfcondsampcnt$NF_COND_STATUS_CD), ]
      if (nrow(nfcondsampcnt) > 0) {
        nfcondsampcnt <-
          cbind(NF_COND_STATUS_NM = ref_nf_cond_status_cd[match(nfcondsampcnt$NF_COND_STATUS_CD,
                                                                ref_nf_cond_status_cd$VALUE), "MEANING"], nfcondsampcnt)
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
          message("removing ", nbrnfnonsampled, " nonsampled nonforest conditions")
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
    estpcfromqry <- paste0(
      "\n FROM ", SCHEMA., plotnm, " p",
      "\n JOIN ", SCHEMA., condnm, " c ON (", conda., cuniqueid, " = ", plota., puniqueid, ")")
  } else {
    estpcfromqry <- paste0(
      "\n FROM pltcondf cond")
  }
 
    
  ## Add from statement for subp_cond_chng_matrx
  estfromqry <- paste0(estpcfromqry,  
      "\n JOIN ", SCHEMA., vsubpsppnm, " vsubpspp ON(", vsubpsppa., vsubpsppid, " = c.", cuniqueid) 
  estfromqry <- paste0(estpcfromqry,  
      "\n JOIN ", SCHEMA., vsubpstrnm, " vsubpstr ON(", vsubpstra., vsubpstrid, " = c.", cuniqueid) 
  
  
  ## 11. Return data objects
  ######################################################################################
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
