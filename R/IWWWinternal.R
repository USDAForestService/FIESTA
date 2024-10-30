## wwwCheckPlots
## wwwGetAdjqryVOL - Build queries for plot-level adjustment factors for VOL
## wwwGetpltcondqry - Build queries for getting plot / cond data.
## wwwGettreeqry - Build query to get tree data and sum tree-level data.
## wwwGetcondqry - Build query to get sum condition-level data.



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
  ## Check database connection (dbconn) and define schema
  ##############################################################################
  if (!DBI::dbIsValid(dbconn)) {
    stop("dbconn is invalid")
  }
  #dbinfo <- DBI::dbGetInfo(dbconn)
  SCHEMA. <- ifelse (is.null(schema), "", paste0(schema, "."))


  #################################################################################
  ## Check plotnm and get column names and unique identifiers
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
  ## 1. Check plot filters for AOI and get where statement for querying pltids
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
  ## 2. Get plot counts by domain unit within the AOI
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
    "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD = 1 THEN 1 ELSE 0 END) AS FOREST,",
    "\n  SUM(CASE WHEN ", pstatuscda., "PLOT_STATUS_CD = 2 THEN 1 ELSE 0 END) AS NONFOREST")
  
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
  ## 3. Check number of plots to determine whether to use SAE methods
  ######################################################################################
  if (byeach) {
    if (any(plotcnt$nbrplots < minplots)) {
      SAE <- TRUE
    }
  } else {
    if (sum(plotcnt$nbrplots) < minplots) {
      SAE <- TRUE
    }
  }
  
  
  ######################################################################################
  ## 4. If SAE methods, redefine the extent of the AOI
  ######################################################################################
  if (SAE) {
    message("number of plots are less then ", minplots, "... using SAE methods...")
    
    
    ## 4.1. If SAE, get province name(s) with max overlap to AOI
    ######################################################################################
    domain.filterPG <- RPostgres::dbQuoteLiteral(dbconn, getfilter("lyr1.domain_unit", AOI_domain_unit_values, syntax = "sql"))
    AOI_table_namePG <- RPostgres::dbQuoteLiteral(dbconn, AOI_table_name)

    
    if (byeach) {
      province_max.qry <- paste0(
        "SELECT domain_unit, ecomap_province, MAX(overlap_area) as max_overlap_area ",
        "FROM f_province_overlap(", AOI_table_namePG, ", ", domain.filterPG, ") ",
        "GROUP BY domain_unit, ecomap_province")
      
    } else {
      province_max.qry <- paste0(
        "SELECT ecomap_province, SUM(overlap_area) as sum_overlap_area ",
        "\nFROM f_province_overlap(", AOI_table_namePG, ", ", domain.filterPG, ") ", 
        "\nGROUP BY ecomap_province",
        "\nORDER BY sum_overlap_area DESC",
        "\nLIMIT 1;")
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
    

    ## 4.2. Check plot filters for AOI plus provinces
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
  
  
    ## 4.3. Build select query for AOI domain units and Ecomap subsections
    ##################################################################################
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
    
    ## 5. Build select query for AOI domain units 
    ##################################################################################
    aoiselectqry <- paste0(
      AOI_table_name, " AS domain_unit")
    
    aoiwhereqry <- pwhereqry
  }
  

  ##################################################################################
  ## Return data
  ##################################################################################
  
  ## Create a list with pltassgn information to pass to modWWWpop
  pltadata <- list(pltassgnnm = pltassgnnm,
                   pltassgnflds = pltassgnflds,
                   pltassgn. = pltassgn.)
  
  
  
  returnlst <- list(plotcnt = plotcnt,
                    plotcntqry = plotunitcntqry,
                    SAE = SAE,
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
  if (SAE) {
    returnlst$province_max <- province_max
  }
  return(returnlst)
}




wwwGetAdjqryVOL <- function(strvar,
                            module = "GB",
                            pltidsWITHqry = NULL) {
  
  ## DESCRIPTION: Build queries for plot-level adjustment factors for VOL
  ## If module='GB', build strata-level adjustment factors first.
  
  ## Set global variables
  ACI <- FALSE
  SCHEMA. <- ""
  
  
  ## Define database queries lists
  dbqueries <- {}
  dbqueriesWITH <- {}
  
  
  ## Build WHERE statement for removing nonresponse
  adjwhereqry <- "\nWHERE c.cond_status_cd <> 5"
  if (ACI) {
    adjwhereqry <- paste0(
      adjwhereqry,
      "\n    AND nf_cond_status_cd is NULL OR nf_cond_status_cd <> 5")
  }
  
  ## Build FROM statement 
  adjfromqry <- paste0(
    "\nFROM pltids",
    #"\nJOIN pltcondx pc ON (pc.plt_cn = pltids.cn)")
    "\nJOIN plot p ON (p.cn = pltids.cn)",
    "\nJOIN cond c ON (c.plt_cn = p.cn)")
  
  
  ## Build SELECT stateement 
  if (module == "GB") {
    
    ## If module = GB, divide N by the sum of sampled proportions by strata
    grpbyqry <- paste0("\nGROUP BY pltids.domain_unit, pltids.", strvar)
    adjselectqry <- paste0(
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(condprop_unadj),0), 0) AS adj_factor_cond,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(subpprop_unadj),0), 0) AS adj_factor_subp,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(macrprop_unadj),0), 0) AS adj_factor_macr,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(micrprop_unadj),0), 0) AS adj_factor_micr")
    

    ## Build query for calculating strata-level adjustment factors
    adjqry <- paste0(
      "SELECT pltids.domain_unit, pltids.", strvar, ", ",
      adjselectqry,
      adjfromqry,
      adjwhereqry, 
      grpbyqry)
    
    ## Append pltidsWITHqry to strata-level adjustment query
    if (!is.null(pltidsWITHqry)) {
      adjfactors.qry <- paste0(
        pltidsWITHqry,
        "\n", adjqry)
    }
    #message(adjfactors.qry)    
    dbqueries$adjfactors <- adjfactors.qry
    
    ## Build WITH query for calculating strata-level adjustment factors
    adjWITHqry <- paste0(
      "\nadjfactors AS", 
      "\n(", adjqry, ")")      
    #message(adjWITHqry)    
    
    
    ## Build query to create plot-level adjustment factors
    if (!is.null(pltidsWITHqry)) {
      joinqry <- getjoinqry(c("domain_unit", strvar), c("domain_unit", strvar), "adj.", "pltids.")
      pltidsadj.qry <- paste0(
        pltidsWITHqry, ", ",
        adjWITHqry, 
        "\n----- calculate plot-level adjustment factors",
        "\nSELECT pltids.cn, adj_factor_cond, adj_factor_subp, adj_factor_macr, adj_factor_micr",
        "\n FROM pltids",
        "\n JOIN adjfactors adj ", joinqry)
      #message(pltidsadj.qry)
      dbqueries$pltidsadj <- pltidsadj.qry
        
        
      pltidsadjWITH.qry <- paste0(
        pltidsWITHqry, ", ",
        adjWITHqry, ", ",
        "\n----- calculate plot-level adjustment factors",
        "\npltidsadj AS",
        "\n(SELECT pltids.cn, adj_factor_cond, adj_factor_subp, adj_factor_macr, adj_factor_micr",
        "\n FROM pltids",
        "\n JOIN adjfactors adj ", joinqry, ")")
    
      #message(pltidsadjWITH.qry)
      dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry
    }
    
    
  } else {
    
    ## If module != GB, divide 1 by the sum of sampled proportions by plot
    grpbyqry <- paste0("\nGROUP BY pltids.cn")
    adjselectqry <- paste0(
      "\n        COALESCE(1 / NULLIF(SUM(condprop_unadj),0), 0) AS adj_factor_cond,",
      "\n        COALESCE(1 / NULLIF(SUM(subpprop_unadj),0), 0) AS adj_factor_subp,",
      "\n        COALESCE(1 / NULLIF(SUM(macrprop_unadj),0), 0) AS adj_factor_macr,",
      "\n        COALESCE(1 / NULLIF(SUM(micrprop_unadj),0), 0) AS adj_factor_micr")
    
    ## Build query for calculating plot-level adjustment factors
    adjqry <- paste0(
      "SELECT pltids.cn,", 
      adjselectqry,
      adjfromqry,
      adjwhereqry, 
      grpbyqry)
    
    
    ## Build WITH query for calculating plot-level adjustment factors
    adjWITHqry <- paste0(
      "\npltidsadj AS", 
      "\n(", adjqry, ")")      
    message(adjWITHqry)    
    
    
    ## Append pltidsWITHqry to plot-level adjustment query
    if (!is.null(pltidsWITHqry)) {
      pltidsadj.qry <- paste0(
        pltidsWITHqry, 
        "\n", adjqry)
      message(pltidsadj.qry)
      #pltidsadj <- DBI::dbGetQuery(dbconn, pltidsadj.qry)
      #head(pltidsadj)
      dbqueries$pltidsadj <- pltidsadj.qry
      
      pltidsadjWITH.qry <- paste0(
        pltidsWITHqry, ", ",
        adjWITHqry)
    }
    
    #message(pltidsadjWITH.qry)
    dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry
  }
  
  ## Return queries
  return(list(dbqueries = dbqueries, dbqueriesWITH = dbqueriesWITH))
}    



wwwGetAdjqryCHNG <- function(module = "GB",
                             strvar,
                             pltidsWITHqry = NULL) {
  
  ## DESCRIPTION: Build queries for plot-level adjustment factors for CHNG
  ## If module='GB', build strata-level adjustment factors first.
  
  ## Set global variables
  ACI <- FALSE
  SCHEMA. <- ""
  
  
  ## Define database queries lists
  dbqueries <- {}
  dbqueriesWITH <- {}
  
  
  ## Build query to sum subplot proportions
  subcpropWITHqry <- 
    "----- sum sampled subplot proportions
    subpcprop AS 
    (SELECT c.plt_cn, pcond.plt_cn AS prev_plt_cn, 
        pcond.condid AS prevcond, c.condid, 
     SUM(sccm.subptyp_prop_chng * 
          (CASE WHEN ((sccm.subptyp = 3 AND c.prop_basis = 'MACR') OR
                      (sccm.subptyp = 1 AND c.prop_basis = 'SUBP'))
           THEN 1 ELSE 0 END) /4) AS condprop_unadj,
    SUM(sccm.subptyp_prop_chng * 
          (CASE WHEN sccm.subptyp = 1 THEN 1 ELSE 0 END) /4) AS subpprop_unadj,
    SUM(sccm.subptyp_prop_chng * 
          (CASE WHEN sccm.subptyp = 2 THEN 1 ELSE 0 END) /4) AS micrprop_unadj,
    SUM(sccm.subptyp_prop_chng * 
          (CASE WHEN sccm.subptyp = 3 THEN 1 ELSE 0 END) /4) AS macrprop_unadj
    FROM pltids
    JOIN PLOT p ON (p.cn = pltids.plt_cn)
    JOIN PLOT pplot ON (pplot.cn = p.prev_plt_cn)
    JOIN COND c ON (c.plt_cn = p.cn)
    JOIN COND pcond ON (pcond.plt_cn = p.prev_plt_cn)
    JOIN SUBP_COND_CHNG_MTRX sccm ON (sccm.plt_cn = c.plt_cn
                                      AND sccm.prev_plt_cn = pcond.plt_cn
                                      AND sccm.condid = c.condid
                                      AND sccm.prevcond = pcond.condid) 
    WHERE c.cond_status_cd <> 5
    AND c.condprop_unadj IS NOT NULL
    AND ((sccm.subptyp = 3 AND c.prop_basis = 'MACR')
         OR (sccm.subptyp = 1 AND c.prop_basis = 'SUBP'))
    AND COALESCE(c.cond_nonsample_reasn_cd, 0) = 0
    AND COALESCE(pcond.cond_nonsample_reasn_cd, 0) = 0
    GROUP BY c.plt_cn, pcond)"
  
  
  ## Build FROM statement 
  adjfromqry <- paste0(
    "\nFROM pltids",
    "\nLEFT OUTER JOIN subpcprop c ON (pltids.plt_cn = c.plt_cn)")
  
  ## Build SELECT stateement 
  if (module == "GB") {
    
    ## If module = GB, divide N by the sum of sampled proportions by strata
    grpbyqry <- paste0("\nGROUP BY pltids.domain_unit, pltids.", strvar)
    adjselectqry <- paste0(
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(condprop_unadj),0), 0) AS adj_factor_cond,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(subpprop_unadj),0), 0) AS adj_factor_subp,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(macrprop_unadj),0), 0) AS adj_factor_macr,",
      "\n        COALESCE(COUNT(DISTINCT pltids.cn) / NULLIF(SUM(micrprop_unadj),0), 0) AS adj_factor_micr")
    
    
    ## Build query for calculating strata-level adjustment factors
    adjqry <- paste0(
      "SELECT pltids.domain_unit, pltids.", strvar, ", ",
      adjselectqry,
      adjfromqry,
      grpbyqry)
    
    ## Append pltidsWITHqry to strata-level adjustment query
    if (!is.null(pltidsWITHqry)) {
      adjfactors.qry <- paste0(
        pltidsWITHqry,
        "\n", subcpropWITHqry, ", ",
        "\n", adjqry)
    }
    #message(adjfactors.qry)    
    dbqueries$adjfactors <- adjfactors.qry
    
    ## Build WITH query for calculating strata-level adjustment factors
    adjWITHqry <- paste0(
      "\nadjfactors AS", 
      "\n(", adjqry, ")")      
    #message(adjWITHqry)    
    
    
    ## Build query to create plot-level adjustment factors
    if (!is.null(pltidsWITHqry)) {
      joinqry <- getjoinqry(c("domain_unit", strvar), c("domain_unit", strvar), "adj.", "pltids.")
      pltidsadj.qry <- paste0(
        pltidsWITHqry, ", ",
        adjWITHqry, 
        "\n----- calculate plot-level adjustment factors",
        "\nSELECT pltids.cn, adj_factor_cond, adj_factor_subp, adj_factor_macr, adj_factor_micr",
        "\n FROM pltids",
        "\n JOIN adjfactors adj ", joinqry)
      #message(pltidsadj.qry)
      dbqueries$pltidsadj <- pltidsadj.qry
      
      
      pltidsadjWITH.qry <- paste0(
        pltidsWITHqry, ", ",
        adjWITHqry, ", ",
        "\n----- calculate plot-level adjustment factors",
        "\npltidsadj AS",
        "\n(SELECT pltids.cn, adj_factor_cond, adj_factor_subp, adj_factor_macr, adj_factor_micr",
        "\n FROM pltids",
        "\n JOIN adjfactors adj ", joinqry, ")")
      
      #message(pltidsadjWITH.qry)
      dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry
    }
    
    
  } else {
    
    ## If module != GB, divide 1 by the sum of sampled proportions by plot
    grpbyqry <- paste0("\nGROUP BY pltids.cn")
    adjselectqry <- paste0(
      "\n        COALESCE(1 / NULLIF(SUM(condprop_unadj),0), 0) AS adj_factor_cond,",
      "\n        COALESCE(1 / NULLIF(SUM(subpprop_unadj),0), 0) AS adj_factor_subp,",
      "\n        COALESCE(1 / NULLIF(SUM(macrprop_unadj),0), 0) AS adj_factor_macr,",
      "\n        COALESCE(1 / NULLIF(SUM(micrprop_unadj),0), 0) AS adj_factor_micr")
    
    ## Build query for calculating plot-level adjustment factors
    adjqry <- paste0(
      "SELECT pltids.domain_unit, pltids.", strvar, ", ", 
      adjselectqry,
      adjfromqry,
      adjwhereqry, 
      grpbyqry)
    
    
    ## Build WITH query for calculating plot-level adjustment factors
    adjWITHqry <- paste0(
      "\npltidsadj AS", 
      "\n(", adjqry, ")")      
    message(adjWITHqry)    
    
    
    ## Append pltidsWITHqry to plot-level adjustment query
    if (!is.null(pltidsWITHqry)) {
      pltidsadj.qry <- paste0(
        pltidsWITHqry, 
        "\n", adjqry)
      message(pltidsadj.qry)
      #pltidsadj <- DBI::dbGetQuery(dbconn, pltidsadj.qry)
      head(pltidsadj)
      dbqueries$pltidsadj <- pltidsadj.qry
      
      pltidsadjWITH.qry <- paste0(
        pltidsWITHqry, ", ",
        adjWITHqry)
    }
    
    #message(pltidsadjWITH.qry)
    dbqueriesWITH$pltidsadjWITH <- pltidsadjWITH.qry
  }
  
  ## Return queries
  return(list(dbqueries = dbqueries, dbqueriesWITH = dbqueriesWITH))
}    


wwwGetpltcondqry <- function(popType = "VOL", pltidsWITHqry = NULL,
                             pcwhereqry = NULL,
                             addfortypgrp = FALSE) {
  
  ## DESCRIPTION: Build queries for getting plot / cond data.
  
  
  ## Set global variables
  defaultVars <- TRUE
  SCHEMA. <- ""
  
  
  ## Define plot-level variables 
  ###############################################################################
  pvars2keep <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT",
                  "PLOT_STATUS_CD", "PLOT_NONSAMPLE_REASN_CD", "INTENSITY", "SUBCYCLE") 
  
  ## Set additional pvars2keep depending on popType
  if (popType %in% c("GRM", "CHNG")) {
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
    pvars <- tolower(c(pvars2keep, "RDDISTCD", "WATERCD"))
  } else {
    pvars <- "*"
  }
  pselectqry <- toString(paste0("p.", pvars))
  
  
  ## Define condition-level variables 
  ###############################################################################
  if (defaultVars) {
    condvars <-  tolower(DBvars.default()$condvarlst)
  } else {
    condvars <- "*"
  }
  cselectqry <- toString(paste0("c.", condvars))
  #pltcondflds <- unique(c(condvars, pvars))
  
  
  ## Add FORTYPGRP to SELECT query
  if (addfortypgrp) {
    ref_fortypgrp <- ref_codes[ref_codes$VARIABLE == "FORTYPCD", c("VALUE", "GROUPCD")]
    ftypqry <- classqry(classcol = "c.fortypcd",
                        fromval = ref_fortypgrp$VALUE,
                        toval = ref_fortypgrp$GROUPCD,
                        classnm = "fortypgrpcd")
    cselectqry <- paste0(cselectqry, ", ",
                         "\n ", ftypqry)
  }
  #pltcondflds <- c(pltcondflds, "FORTYPGRPCD")
  
  
  
  ## Build FROM query for pltcondx
  pjoinqry <- getjoinqry("CN", "CN", "p.", "pltids.")
  cjoinqry <- getjoinqry("PLT_CN", "CN", "c.", "p.")
  pcfromqry <- paste0(
    "\n FROM pltids",
    "\n JOIN ", SCHEMA., "plot p ", pjoinqry,
    "\n JOIN ", SCHEMA., "cond c ", cjoinqry)
  #message(pcfromqry)
  
  
  ## Append SELECT query and FROM query for pltcondx
  pcqry <- paste0(
    "SELECT ", cselectqry, ", ",
    "\n", pselectqry,
    pcfromqry)
  
  ## Add where statement for plot/conditions
  if (!is.null(pcwhereqry)) {
    pcqry <- paste0(
      pcqry, 
      "\nWHERE ", pcwhereqry)
  }
  
  
  ## Append pltidsWITHqry to plotcondx query
  pltcondxqry <- paste0(
    pltidsWITHqry,
    "\n", pcqry)
  #message(pltcondx.qry)
  #pltcondx <- DBI::dbGetQuery(dbconn, pltcondx.qry)
  
  
  ## build WITH query for pltcondx
  pltcondxWITHqry <- paste0(
    pltidsWITHqry, ", ",
    "\n----- pltcondx",
    "\npltcondx AS",
    "\n(", pcqry, ")")
  
  
  ## Return queries fo pltcondx
  return(list(pltcondxqry = pltcondxqry, pltcondxWITHqry = pltcondxWITHqry))
}



wwwGettreeqry <- function(estvar, 
                          tfilter = NULL,
                          pcdomainlst = NULL, 
                          pcwhereqry = NULL, 
                          estseed = "none") {
  
  
  ## DESCRIPTION: Build query to get tree data and sum tree-level data.
  
  ## Set global variables
  defaultVars <- TRUE
  SCHEMA. <- ""
  
  
  ## Check estseed and define name for estvar
  estvar <- tolower(estvar)
  estvarlst <- estvar
  if (estvar == "tpa_unadj") {
    if (estseed == "add") {
      treenm <- "count_tpa_tree"
      seednm <- "count_tpa_seed"
    } 
    tsumnm <- "count_tpa"
  } else {
    estvarlst <- c(estvar, "tpa_unadj")
    if (estseed %in% c("only", "add")) {
      stop("estvar must be equal to tpa_unadj")
    }
    tsumnm <- paste0(estvar, "_tpa")
  }
  tsumnm <- "estimated_tvalue"
  
  
  
  ## Build tree WITH qry
  #########################################################################
  if (!estseed == "only") {
    tWITHwhereqry=twhereqry <- NULL
    
    #treeflds <- DBI::dbListFields(popconn, "tree")
    
    tWITHselectqry <- paste0(
      "SELECT 'TREE' src, plt_cn, condid, CONDID, SUBP, TREE, ", toString(estvarlst), ",   
        CASE WHEN dia IS NULL THEN adj_factor_subp
        WHEN MIN(dia, 5 - 0.001) THEN adj_factor_micr
        WHEN MIN(dia, 9999 - 0.001) THEN adj_factor_subp
        ELSE adj_factor_macr END AS tadjfac")
    
    tWITHfromqry <- paste0(
      "\nFROM tree t",
      "\nJOIN pltidsadj adj ON (adj.cn = t.plt_cn)")

    if (!is.null(tfilter)) {
      tWITHwhereqry <- paste0("\nWHERE ", tfilter)
    }

    if (estseed == "add") {
      
      sWITHselectqry <- paste0(
        "\nSELECT 'SEED' src, plt_cn, condid, CONDID, SUBP, 0, TPA_UNADJ, adj.adj_factor_micr AS tadjfac")
      
      sWITHfromqry <- paste0(
        "\nFROM seedling s",
        "\nJOIN pltidsadj adj ON (adj.cn = s.plt_cn)")
      
      ## Build final tree WITH query
      treeWITHqry <- paste0(
        "\n----- get tree data",
        "\ntdat AS",
        "\n(", tWITHselectqry,
        tWITHfromqry,
        tWITHwhereqry,
        "\nUNION",
        sWITHselectqry,
        sWITHfromqry, ")")
     
    } else {
      
      ## Build final tree WITH query
      treeWITHqry <- paste0(
        "\n----- get tree data",
        "\ntdat AS",
        "\n(", tWITHselectqry,
        tWITHfromqry,
        tWITHwhereqry, ")")
    }
  } else {
    
    sWITHselectqry <- paste0(
      "SELECT 'SEED' src, plt_cn, condid, CONDID, SUBP, TPA_UNADJ, adj.adj_factor_micr AS tadjfac")
    
    sWITHfromqry <- paste0(
      "\nFROM seedling s",
      "\nJOIN pltidsadj adj ON (adj.cn = s.plt_cn)")

    ## Build final tree WITH query
    treeWITHqry <- paste0(
      "\n----- get tree data",
      "(", sWITHselectqry,
      sWITHfromqry, ")")
    
  }
  
  
  ## Build tree SUM query
  #########################################################################
  grpbyvars <- paste0("pc.", c("plt_cn", "condid", pcdomainlst))
  tselectqry <- paste0(
    "SELECT ", toString(grpbyvars), ", ")

  tsumselectqry <- paste0(
    tselectqry, 
    "ROUND(COALESCE(SUM(", paste(estvarlst, collapse = " * "), " * tadjfac),0), 8) AS ", tsumnm)
  
  tfromqry <- paste0(
    "\nFROM pltcondx pc",
    "\nLEFT JOIN tdat ON (tdat.plt_cn = pc.plt_cn AND tdat.condid = pc.condid)")
  
  ## Build final tree query
  tqry <- paste0(
    treeWITHqry,
    "\n-------------------------------------------",
    "\n", tsumselectqry,
    tfromqry,
    pcwhereqry,
    "\nGROUP BY ", toString(grpbyvars))
  #message(tqry)
  
  
  ## Return query, including tree WITH query (tdat) and tree SUM query
  return(tqry)
}


wwwGetcondqry <- function(pcdomainlst = NULL, 
                          pcwhereqry = NULL) {
  
  ## DESCRIPTION: Build query to get sum condition-level data.
  
  ## Set global variables
  defaultVars <- TRUE
  SCHEMA. <- ""
  
  csumnm <- "estimated_value"
  
  ## Get cond SUM qry
  #########################################################################
  grpbyvars <- paste0("pc.", c("plt_cn", "condid", pcdomainlst))
  cselectqry <- paste0(
    "SELECT ", toString(grpbyvars), ", ")
  
  csumselectqry <- paste0(
    cselectqry, 
    "SUM(COALESCE(pc.condprop_unadj * CASE pc.prop_basis WHEN 'MACR' THEN adj_factor_macr ELSE adj_factor_subp END, 0)) AS ", csumnm)
  
  cfromqry <- paste0(
    "\nFROM pltidsadj",
    "\nLEFT JOIN pltcondx pc ON (pc.plt_cn = pltidsadj.cn)")
  
  ## Build final tree query
  cqry <- paste0(
    "\n-------------------------------------------",
    "\n", csumselectqry,
    cfromqry,
    pcwhereqry,
    "\nGROUP BY ", toString(grpbyvars))
  #message(tqry)
  
  return(cqry)
}










