wwwCheckPlots <- function(popType,
                          popFilter,
                          byeach,
                          AOI_table_name,
                          AOI_domain_unit_values,
                          minplots = 50,
                          dbconn, 
                          schema = NULL,
                          pltassgnnm,
                          plotnm = NULL) {
    
    
  ## DESCRIPTION:
  ## Check pop filters and returns where queries

           
#  ## Define pvars
#  selectpvars <- tolower(
#    c("CN", "PLOT_STATUS_CD", "SAMP_METHOD_CD",
#      "INTENSITY", "MEASYEAR", 
#      "KINDCD", "DESIGNCD", 
#      "NF_PLOT_STATUS_CD", "NF_SAMPLING_STATUS_CD",         
#      "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD", 
#      "INVASIVE_SAMPLING_STATUS_CD"))
  
  ## Set global variables
  province_names <- NULL
  SAE <- FALSE

  ##############################################################################
  ## 1. Check database connection (dbconn) or dsn and define SCHEMA.
  ##############################################################################
  if (!DBI::dbIsValid(dbconn)) {
    stop("dbconn is invalid")
  }
  #dbinfo <- DBI::dbGetInfo(dbconn)
  SCHEMA. <- ifelse (is.null(schema), "", paste0(schema, "."))
  


  #################################################################################
  ## 2. Build from query and set plot parameters
  #################################################################################
  if (is.null(plotnm)) {
    #pltassgnnm <- "plot_assign_plot_ppsa"
#    pltfromqry <- paste0(
#      "\nFROM ", pltassgnnm, " plta")
    
    pltflds <- NULL
    pjoinid=puniqueid=plt. <- NULL
    pltassgnflds <- DBI::dbListFields(dbconn, pltassgnnm)
    pltassgnid <- "CN"
    
  } else {    
    #plotnm <- "plot"
    pltassgn. <- "plta."
    plt. <- "p."
    puniqueid <- "CN"
    pltassgnid=pjoinid <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
    #pltassgnnm <- "plot_assign"
 
    pjoinqry <- getjoinqry(pltassgnid, pjoinid, pltassgn., plt.)
#    pltfromqry <- paste0(
#      "\nFROM ", pltassgnnm, " plta", 
#      "\nJOIN ", plotnm, " p ", pjoinqry)
    
    pltflds <- DBI::dbListFields(dbconn, plotnm)
    pltassgnflds <- DBI::dbListFields(dbconn, pltassgnnm)
  }
 
  ## Get plot_assign fields
  pltassgn. <- "plta."
  pflds <- c(pltassgnflds, pltflds)
  
  
  ## Append domain_unit filter to popFilter
  domain_unit.filter <- getfilter(AOI_table_name, AOI_domain_unit_values, syntax = "sql")
  popFilter$pfilter <- domain_unit.filter
  

  ##################################################################################
  ## 3. Check plot filters for AOI
  ##################################################################################
  datfilter <- suppressMessages(
    getpopFilterqry(popType = popType,
                  popFilter = popFilter,
                  pltassgnnm = pltassgnnm,
                  pltassgnid = pltassgnid,
                  pltassgnflds = pltassgnflds,
                  plotnm = plotnm,
                  pltflds = pltflds,
                  pjoinid = pjoinid,
                  plt. = plt.,
                  dbconn = dbconn,
                  dbTabs = dbTables(),
                  datindb = TRUE,
                  pltaindb = TRUE))
  pwhereqry <- datfilter$pwhereqry
  pltafromqry <- datfilter$pltafromqry
  pfilter <- datfilter$pfilter
  popwhereqry <- datfilter$popwhereqry
  states <- datfilter$states
  invyrs <- datfilter$invyrs
  
  
  ######################################################################################
  ## 4. Get plot counts by domain unit
  ######################################################################################
  pstatuscdnm <- findnm("PLOT_STATUS_CD", pflds, returnNULL=TRUE)
  if (!is.null(pltflds)) {
    pstatuscda. <- ifelse(pstatuscdnm %in% pltflds, plt., pltassgn.)
  } else {
    pstatuscda. <- pltassgn.
  }

  ## Build select for plot counts
  unitvars <- AOI_table_name
  pltcnt_grpbyvars <- paste0(pltassgn., unitvars)
  pltcnt_selectqry <- paste0(
      "\nSELECT ", toString(pltcnt_grpbyvars), ", COUNT(*) NBRPLOTS")
  
  pltcnt_selectqry <- paste0(
    pltcnt_selectqry, ", ",
    "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD == 1 THEN 1 ELSE 0 END) AS FOREST,",
    "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD == 2 THEN 1 ELSE 0 END) AS NONFOREST")
  
  ## Build query for plot counts
  plotunitcntqry <- paste0(
    pltcnt_selectqry, 
    pltafromqry, 
    pwhereqry,
    #"\n    AND ", pfilter,
    "\nGROUP BY ", toString(pltcnt_grpbyvars),
    "\nORDER BY ", toString(pltcnt_grpbyvars))
  
  ## Run query to get plot counts
  plotcnt <- DBI::dbGetQuery(dbconn, plotunitcntqry)
  
  
  ######################################################################################
  ## 5. Check number of plots to determine whether to use SAE methods
  ######################################################################################
  if (byeach) {
    if (any(plotcnt$NBRPLOTS < minplots)) {
      SAE <- TRUE
    }
  } else {
    if (sum(plotcnt$NBRPLOTS) < minplots) {
      SAE <- TRUE
    }
  }
  
  
  ######################################################################################
  ## 6. If SAE, get province names with max overlap to AOI
  ######################################################################################
  if (SAE) {
    message("number of plots are less then ", minplots, "... using SAE methods...")
    
    domain.filter <- getfilter("domain_unit", AOI_domain_unit_values, syntax = "sql")
  
    if (byeach) {
    
      province_max.qry <- paste0(
        "SELECT domain_unit, ecomap_province, max(acres_sum)
        FROM
        (SELECT domain_unit, ecomap_province, SUM(overlap_area) AS acres_sum",
        "\nFROM ecomap_province_overlap",
        "\nWHERE table_name = '", AOI_table_name, "'",
        "\n  AND ", domain.filter,
        "\nGROUP BY domain_unit, ecomap_province)",
        "\nGROUP BY domain_unit")
    } else {
    
      province_max.qry <- paste0(
        "SELECT ecomap_province, max(acres_sum)
        FROM
        (SELECT ecomap_province, SUM(overlap_area) AS acres_sum",
        "\nFROM ecomap_province_overlap",
        "\nWHERE table_name = '", AOI_table_name, "'",
        "\n  AND ", domain.filter,
        "\nGROUP BY ecomap_province)")
    }
  
  
    ## Run query to get province name(s) with max overlap
    province_max <- DBI::dbGetQuery(dbconn, province_max.qry)  
    #province_max 
    province_names <- unique(province_max$ecomap_province)
    #province_names
    
    ## Get province filter
    province.filter <- getfilter("ecomap_province", province_names, syntax = "sql")
    
    
    ## Get states that intersect province_names
    state.qry <- paste0(
      "SELECT DISTINCT statecd ",
      "\nFROM ", pltassgnnm, 
      "\nWHERE ", province.filter)
    states <- DBI::dbGetQuery(dbconn, state.qry)[[1]]  
    popFilter$states <- states 
    

    ##################################################################################
    ## 7. Check plot filters for AOI plus provinces
    ##################################################################################
    datfilter <- suppressMessages(
      getpopFilterqry(popType = popType,
                      popFilter = popFilter,
                      pltassgnnm = pltassgnnm,
                      pltassgnid = pltassgnid,
                      pltassgnflds = pltassgnflds,
                      plotnm = plotnm,
                      pltflds = pltflds,
                      pjoinid = pjoinid,
                      plt. = plt.,
                      dbconn = dbconn,
                      dbTabs = dbTables(),
                      datindb = TRUE,
                      pltaindb = TRUE))
    
    pwhereqry <- datfilter$pwhereqry
    pltafromqry <- datfilter$pltafromqry
    pfilter <- datfilter$pfilter
    popwhereqry <- datfilter$popwhereqry
    states <- datfilter$states
    invyrs <- datfilter$invyrs
    
  }  
  
  
  ##################################################################################
  ## 8. Build select query for AOI domain units
  ##################################################################################
  if (SAE) {
    aoiselectqry <- paste0(
      "\n     CASE WHEN ", pfilter, 
      "\n          THEN ", AOI_table_name, " ELSE ecomap_subsection END AS domain_unit")
    
    if (is.null(popwhereqry)) {
      aoiwhereqry <- paste0("\nWHERE ", province.filter)
    } else {
      aoiwhereqry <- paste0(popwhereqry, 
                             "\n  AND ", province.filter)
    }
    
  } else {
    aoiselectqry <- paste0(
      AOI_table_name, " AS domain_unit")
    
    aoiwhereqry <- pwhereqry
  }
  

  ##################################################################################
  ## 8. Return data
  ##################################################################################
  
  ## Create a list with pltassgn information to pass to modWWWpop
  pltadata <- list(pltassgnnm = pltassgnnm,
                   pltassgnflds = pltassgnflds,
                   pltassgn. = pltassgn.)
  
  
  
  returnlst <- list(plotcnt = plotcnt,
                    plotcntqry = plotunitcntqry,
                    SAE = SAE,
                    province_names = province_names,
                    aoiselectqry = aoiselectqry,
                    aoiwhereqry = aoiwhereqry,
                    pltafromqry = pltafromqry,
                    #popwhereqry = popwhereqry,
                    pwhereqry = pwhereqry,
                    pfilter = pfilter,
                    pltadata = pltadata,
                    states = states,
                    invyrs = invyrs
                    )
  
  return(returnlst)
}



wwwGetAdjqryVOL <- function(strvar,
                            module = "GB",
                            pltidsWITH = NULL) {
  
  dbqueries <- {}
  dbqueriesWITH <- {}
  
  adjwhereqry <- "\nWHERE c.cond_status_cd <> 5"
  if (ACI) {
    adjwhereqry <- paste0(
      adjwhereqry,
      "\n    AND nf_cond_status_cd is NULL OR nf_cond_status_cd <> 5")
  }
  
  adjfromqry <- paste0(
    "\nFROM pltids",
    "\nJOIN plot p ON (p.CN = pltids.CN)",
    "\nJOIN cond c ON (c.PLT_CN = p.CN)")
  
  
  if (module == "GB") {
    grpbyqry <- paste0("\nGROUP BY pltids.domain_unit, pltids.", strvar)
    adjselectqry <- paste0(
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(condprop_unadj),0), 0) AS adj_factor_cond,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(subpprop_unadj),0), 0) AS adj_factor_subp,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(macrprop_unadj),0), 0) AS adj_factor_macr,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(micrprop_unadj),0), 0) AS adj_factor_micr")
    

    ## Get query for calculating strata-level adjustment factors
    adjqry <- paste0(
      "SELECT pltids.domain_unit, pltids.", strvar, ", ",
      adjselectqry,
      adjfromqry,
      adjwhereqry, 
      grpbyqry)
    
    if (!is.null(pltidsWITH)) {
      adjfactors.qry <- paste0(
        pltidsWITH,
        "\n", adjqry)
    }
    message(adjfactors.qry)    
    #adjfactors <- DBI::dbGetQuery(dbconn, adjfactors.qry)
    head(adjfactors)
    dbqueries$adjfactors <- adjfactors.qry
    
    ## Get WITH query for calculating strata-level adjustment factors
    adjWITHqry <- paste0(
      "\nadjfactors AS", 
      "\n(", adjqry, ")")      
    message(adjWITHqry)    
    
    if (!is.null(pltidsWITH)) {
      joinqry <- getjoinqry(c("domain_unit", strvar), c("domain_unit", strvar), "adj.", "pltids.")
      pltidsadj.qry <- paste0(
        pltidsWITH, ", ",
        adjWITHqry, 
        "\n----- calculate plot-level adjustment factors",
        "\nSELECT pltids.cn, adj_factor_cond, adj_factor_subp, adj_factor_macr, adj_factor_micr",
        "\n FROM pltids",
        "\n JOIN adjfactors adj ", joinqry)
      message(pltidsadj.qry)
      #pltidsadj <- DBI::dbGetQuery(dbconn, pltidsadj.qry)
      head(pltidsadj)
      dbqueries$pltidsadj <- pltidsadj.qry
        
        
      pltidsadjWITH.qry <- paste0(
        pltidsWITH, ", ",
        adjWITHqry, ", ",
        "\n----- calculate plot-level adjustment factors",
        "\npltidsadj AS",
        "\n(SELECT pltids.cn, adj_factor_cond, adj_factor_subp, adj_factor_macr, adj_factor_micr",
        "\n FROM pltids",
        "\n JOIN adjfactors adj ", joinqry, ")")
    
      message(pltidsadjWITH.qry)
      dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry
    }
    
    
  } else {
    grpbyqry <- paste0("\nGROUP BY pltids.cn")
    adjselectqry <- paste0(
      "\n        COALESCE(1 / NULLIF(SUM(condprop_unadj),0), 0) AS adj_factor_cond,",
      "\n        COALESCE(1 / NULLIF(SUM(subpprop_unadj),0), 0) AS adj_factor_subp,",
      "\n        COALESCE(1 / NULLIF(SUM(macrprop_unadj),0), 0) AS adj_factor_macr,",
      "\n        COALESCE(1 / NULLIF(SUM(micrprop_unadj),0), 0) AS adj_factor_micr")
    
    adjqry <- paste0(
      "SELECT pltids.domain_unit, pltids.", strvar, ", ", 
      adjselectqry,
      adjfromqry,
      adjwhereqry, 
      grpbyqry)
    
    ## Get WITH query for calculating strata-level adjustment factors
    adjWITHqry <- paste0(
      "\npltidsadj AS", 
      "\n(", adjqry, ")")      
    message(adjWITHqry)    
    
    if (!is.null(pltidsWITH)) {
      pltidsadj.qry <- paste0(
        pltidsWITH, 
        "\n", adjqry)
      message(pltidsadj.qry)
      #pltidsadj <- DBI::dbGetQuery(dbconn, pltidsadj.qry)
      head(pltidsadj)
      dbqueries$pltidsadj <- pltidsadj.qry
      
      pltidsadjWITH.qry <- paste0(
        pltidsWITH, ", ",
        adjWITHqry)
    }
    message(pltidsadjWITH.qry)
    dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry
  }
  
  return(list(dbqueries = dbqueries, dbqueriesWITH = dbqueriesWITH))
}    


wwwGetpltcondqry <- function(popType = "VOL", pltidsadjWITH.qry = NULL) {
  
  ###################################################################################
  ## 1. Define a set of plot-level variables that are necessary to keep for estimation. 
  ###################################################################################
  defaultVars <- TRUE
  pvars2keep <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT",
                  "PLOT_STATUS_CD", "PLOT_NONSAMPLE_REASN_CD", "INTENSITY", "SUBCYCLE") 
  
  ## Set additional pvars2keep depending on popType
  if (popType %in% c("GRM", "CHNG", "LULC")) {
    pvars2keep <- unique(c(pvars2keep, c("PREV_PLT_CN", "REMPER")))
  } else if (popType == "P2VEG") {
    pvars2keep <- c(pvars2keep, "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
                    "SAMP_METHOD_CD")
  } else if (popType == "INV") {
    pvars2keep <- c(pvars2keep, "INVASIVE_SAMPLING_STATUS_CD", "INVASIVE_SPECIMEN_RULE_CD")
  }  
  
  pdoms <- c("INVYR", "MEASYEAR", "RDDISTCD", "WATERCD", "ELEV", "ECOSUBCD", "CONGCD",
             "DESIGNCD", "EMAP_HEX")
  pgeomdoms <- c("CONGCD", "ECOSUBCD", "HUC", "ECOMAP_HEX", "ROADLESSCD", "ALP_ADFORCD", 
                 "FVS_VARIANT", "FVS_REGION", "FVS_FOREST", "FVS_DISTRICT")
  
  if (defaultVars) {
    #pvars <- c(pvars2keep, pdoms)
    pvars <- c(pvars2keep, "RDDISTCD", "WATERCD")
  } else {
    pvars <- "*"
  }
  pselectqry <- toString(paste0(plota., pvars))

  if (defaultVars) {
    condvars <-  DBvars.default()$condvarlst
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
  
  ## Build pfromqry
  pjoinqry <- getjoinqry("CN", "CN", "p.", "pltids.")
  cjoinqry <- getjoinqry("PLT_CN", "CN", "c.", "p.")
  pcfromqry <- paste0(
    "\n FROM pltids",
    "\n JOIN ", SCHEMA., "plot p ", pjoinqry,
    "\n JOIN ", SCHEMA., "cond c ", cjoinqry)
  message(pcfromqry)

  ## Build query for pltcondx
  pcqry <- paste0(
    "SELECT ", cselectqry, ", ",
    "\n", pselectqry, ", 1 AS TOTAL",
    pcfromqry)
  
  ## Build query for pltcondx, including pltids WITH query
  pltcondx.qry <- paste0(
    pltidsadjWITH.qry,
      "\n", pcqry)
  message(pltcondx.qry)
  #pltcondx <- DBI::dbGetQuery(dbconn, pltcondx.qry)

  ## Build WITH query for pltcondx, including pltids WITH query
  pltcondxWITH.qry <- paste0(
    pltidsadjWITH.qry, ", ",
    "\n----- pltcondx",
    "\npltcondx AS",
    "\n(", pcqry, ")")
  
  return(list(pltcondx.qry = pltcondx.qry, pltcondxWITH.qry = pltcondxWITH.qry))

}
  

