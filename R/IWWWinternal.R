wwwCheckPlots <- function(popType,
                          popFilter,
                          dbconn, 
                          schema = NULL,
                          pltassgnnm,
                          plotnm = NULL) {
    
    
  ## DESCRIPTION:
  ## Check pop filters and returns where queries

           
  ## Define pvars
  selectpvars <- tolower(
    c("CN", "PLOT_STATUS_CD", "SAMP_METHOD_CD",
      "INTENSITY", "MEASYEAR", 
      "KINDCD", "DESIGNCD", 
      "NF_PLOT_STATUS_CD", "NF_SAMPLING_STATUS_CD",         
      "P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD", 
      "INVASIVE_SAMPLING_STATUS_CD"))
  


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
    pltfromqry <- paste0(
      "\nFROM ", pltassgnnm, " plta")
    
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
    pltfromqry <- paste0(
      "\nFROM ", pltassgnnm, " plta", 
      "\nJOIN ", plotnm, " p ", pjoinqry)
    
    pltflds <- DBI::dbListFields(dbconn, plotnm)
    pltassgnflds <- DBI::dbListFields(dbconn, pltassgnnm)
  }
 
  ## Get plot_assign fields
  pltassgn. <- "plta."
  pflds <- c(pltassgnflds, pltflds)
  

  ##################################################################################
  ## 3. Check plot filters.
  ##################################################################################
  datfilter <- 
    getpopFilterqry(popType = popType,
                  popFilter = popFilter,
                  pfromqry = pltfromqry,
                  plotnm = plotnm,
                  pltassgnnm = pltassgnnm,
                  pltflds = pltflds,
                  pltassgnflds = pltassgnflds,
                  pjoinid = pjoinid,
                  pltassgnid = pltassgnid,
                  selectpvars = selectpvars,
                  pltassgn. = pltassgn.,
                  plt. = plt.,
                  dbconn = dbconn,
                  dbTabs = dbTables(),
                  datindb = TRUE,
                  pltaindb = TRUE)
  names(datfilter)
  pwhereqry <- datfilter$pwhereqry
  popwhereqry <- datfilter$popwhereqry
  pltafromqry <- datfilter$pltafromqry
  pfilter <- datfilter$pfilter

  
  
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
  
  plotunitcnt <- DBI::dbGetQuery(dbconn, plotunitcntqry)


  returnlst <- list(plotcnt = plotunitcnt,
                    plotcntqry = plotunitcntqry,
                    pltafromqry = pltafromqry,
                    pwhereqry = pwhereqry,
                    popwhereqry = popwhereqry,
                    pfilter = pfilter
                    )
  
  return(returnlst)
}
  

