check.condCHNG <- function(areawt, areawt2, 
                           adj, adjcase, 
                           cuniqueid, condid, 
                           chngtype,
                           rowvar = NULL,
                           colvar = NULL,
                           popdatindb,
                           popconn = NULL,
                           pltcondx = NULL,
                           pltidsadj = NULL,
                           pltcondxadjWITHqry = NULL,
                           pcwhereqry = NULL,
                           landarea.filter = NULL) {
  ###################################################################################
  ### Get condition-level domain data
  ###################################################################################
  estnm <- "ESTIMATED_VALUE"
  estvara. <- "sccm."
  sccmnm <- ifelse(popdatindb, sccmx, "sccmx")
  
  ## Add past landarea.filter to WHERE statement
  if (!is.null(landarea.filter)) {
    landarea.filterCHNG <- gsub("pc.", "ppc.", landarea.filter)
    pcwhereqry <- paste0(pcwhereqry,
                         "\n  AND ", landarea.filterCHNG)
  }
  
  ## Define select query for estimates
  estvarqry <- paste0(estvara., areawt)
  if (!is.null(areawt2)) {
    estvarqry <- areawt * areawt2
  }
  
  ## Define select query for estimates
  if (chngtype == "annual") {
    estvarqry <- paste0(estvarqry, " / 4 / pc.REMPER")
  } else {
    estvarqry <- paste0(estvarqry, " / 4")
  }
  
  if (adj %in% c("samp", "plot")) {
    estvarqry <- paste0(estvarqry, " * ", adjcase)
  }
  
  ## Define select query for estimates
  if (chngtype == "annual") {
    estvarqry <- paste0("SUM(COALESCE(", estvarqry, ", 0)) AS ", estnm)
  } else {
    estvarqry <- paste0("SUM(COALESCE(", estvarqry, ", 0)) AS ", estnm)
  }
  
  if (is.null(pcwhereqry)) {
    pcwhereqry <- paste0(
      "\nWHERE pc.CONDPROP_UNADJ IS NOT NULL",
      "\n    AND ((sccm.SUBPTYP = 3 AND pc.PROP_BASIS = 'MACR') OR 
              (sccm.SUBPTYP = 1 AND pc.PROP_BASIS = 'SUBP'))",
      "\n    AND COALESCE(pc.COND_NONSAMPLE_REASN_CD, 0) = 0",
      "\n    AND COALESCE(ppc.COND_NONSAMPLE_REASN_CD, 0) = 0")
  } else {
    pcwhereqry <- paste0(pcwhereqry, 
                         "\n    AND pc.CONDPROP_UNADJ IS NOT NULL",
                         "\n    AND ((sccm.SUBPTYP = 3 AND pc.PROP_BASIS = 'MACR') OR 
              (sccm.SUBPTYP = 1 AND pc.PROP_BASIS = 'SUBP'))",
                         "\n    AND COALESCE(pc.COND_NONSAMPLE_REASN_CD, 0) = 0",
                         "\n    AND COALESCE(ppc.COND_NONSAMPLE_REASN_CD, 0) = 0")
  }
  
  
  ## Build SELECT query
  byvars <- paste0("pc.", c(cuniqueid, condid))
  if (rowvar == "TOTAL") {
    cdomdatvars <- "pc.TOTAL"
    
  } else if (colvar == "NONE") {
    cdomdatvars <- c(paste0("ppc.", rowvar, " AS PREV_", rowvar), paste0("pc.", rowvar))
    grpbyvars <- c(byvars, c(paste0("ppc.", rowvar), paste0("pc.", rowvar)))
    colvar <- rowvar
    col.orderby <- row.orderby
    colvarnm <- rowvarnm
    title.colvar <- title.rowvar
    rowvar <- paste0("PREV_", rowvar)
    
    if (!is.null(row.orderby)) {
      row.orderby <- paste0("PREV_", row.orderby)
    }
    rowvarnm <- paste0("PREV_", rowvarnm)
    title.rowvar <- paste0("PREV_", title.rowvar)
    grpvar <- c(rowvar, colvar)
    
    uniquecol <- uniquerow
    names(uniquerow) <- paste0("PREV_", names(uniquerow))
    
  } else {
    cdomdatvars <- c(paste0("ppc.", rowvar, " AS PREV_", rowvar), paste0("pc.", colvar))
    grpbyvars <- c(byvars, c(paste0("ppc.", rowvar), paste0("pc.", colvar)))
    rowvar <- paste0("PREV_", rowvar)
    
    if (!is.null(row.orderby)) {
      row.orderby <- paste0("PREV_", row.orderby)
    }
    rowvarnm <- paste0("PREV_", rowvarnm)
    title.rowvar <- paste0("PREV_", title.rowvar)
    title.colvar <- title.rowvar
    grpvar <- c(rowvar, colvar)
  }
  cdomdatvars <- c(byvars, cdomdatvars)
  cdomdatselectqry <- 
    paste0("SELECT ", toString(cdomdatvars), ", ",
           "\n    ", estvarqry)
  
  ## Build cdomdat FROM query
  joinqry <- getjoinqry(joinid1 = cuniqueid, joinid2 = cuniqueid,
                        alias1 = "pltidsadj.", alias2 = "pc.")
  
  cdomdatfromqry <- 
    paste0("\nFROM pltidsadj",
           "\nJOIN pltcondx pc ", joinqry)
  
  cdomdatfromqry <- 
    paste0(cdomdatfromqry, 
           "\nJOIN pltcondx ppc ON (ppc.PLT_CN = pc.PREV_PLT_CN)",
           "\nJOIN ", sccmnm, " sccm ON (sccm.plt_cn = pc.plt_cn 
                          AND sccm.prev_plt_cn = ppc.plt_cn 
                          AND sccm.condid = pc.condid 
                          AND sccm.prevcond = ppc.condid)")
  
  ## Build cdomdat query
  cdomdat.qry <- 
    paste0(cdomdatselectqry, 
           cdomdatfromqry,
           pcwhereqry,
           "\nGROUP BY ", toString(grpbyvars))

  #Run query for cdomdat
  if (!popdatindb) {
    cdomdat <- tryCatch(
      sqldf::sqldf(cdomdat.qry, connection = NULL),
      error = function(e) {
        message(e,"\n")
        return(NULL) })
  } else {
    ## Append WITH query
    if (!is.null(pltcondxadjWITHqry)) {
      cdomdat.qry <- paste0(pltcondxadjWITHqry,
                            "\n-------------------------------------------",
                            "\n", cdomdat.qry)
      cdomdat <- tryCatch(
        DBI::dbGetQuery(popconn, cdomdat.qry),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }  
  }
  if (is.null(cdomdat) || nrow(cdomdat) == 0) {
    message("invalid cdomdat query...\n")
    message(cdomdat.qry)
    return(NULL)
  }
  setkeyv(setDT(cdomdat), c(cuniqueid, condid)) 
  
  return(list(cdomdat = cdomdat, 
              cdomdatqry = cdomdat.qry,
              estnm = estnm))
  
}
