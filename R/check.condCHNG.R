check.condCHNG <- function(areawt, areawt2, 
                           adj, adjcase, 
                           cuniqueid, condid, 
                           chngtype,
                           rowvar, rowvarnm, row.orderby, 
                           uniquerow, title.rowvar,
                           colvar, colvarnm, col.orderby,
                           uniquecol, title.colvar,
                           pcdomainlst = NULL,
                           popdatindb,
                           popconn = NULL,
                           pltcondx,
                           sccmx,
                           pltidsadj = NULL,
                           pltcondxadjWITHqry = NULL,
                           pltidsid = "PLT_CN",
                           pcwhereqry = NULL,
                           classifyrow = NULL,
                           classifycol = NULL) {
  ###################################################################################
  ### Get condition-level domain data
  ###################################################################################
  estnm <- "ESTIMATED_VALUE"
  estvara. <- "sccm."
  sccmnm <- ifelse(popdatindb, sccmx, "sccmx")


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
  

  ## Build WHERE statement
  if (is.null(pcwhereqry)) {
    pcwhereqry <- paste0(
      "\nWHERE pc.CONDPROP_UNADJ IS NOT NULL",
      "\n    AND ((sccm.SUBPTYP = 3 AND pc.PROP_BASIS = 'MACR') OR 
              (sccm.SUBPTYP = 1 AND pc.PROP_BASIS = 'SUBP'))",
      "\n    AND COALESCE(pc.COND_NONSAMPLE_REASN_CD, 0) = 0",
      "\n    AND COALESCE(pc.PREV_COND_NONSAMPLE_REASN_CD, 0) = 0")
  } else {
    pcwhereqry <- paste0(pcwhereqry, 
                         "\n    AND pc.CONDPROP_UNADJ IS NOT NULL",
                         "\n    AND ((sccm.SUBPTYP = 3 AND pc.PROP_BASIS = 'MACR') OR 
                                    (sccm.SUBPTYP = 1 AND pc.PROP_BASIS = 'SUBP'))",
                         "\n    AND COALESCE(pc.COND_NONSAMPLE_REASN_CD, 0) = 0",
                         "\n    AND COALESCE(pc.PREV_COND_NONSAMPLE_REASN_CD, 0) = 0")
  }
  
  ## Build SELECT query
  byvars <- paste0("pc.", c(cuniqueid, condid))
  cselectqry <- paste0("SELECT ", toString(byvars))
  
  ## Check pcdomainlst
  if (is.null(pcdomainlst)) {
    pcdomainlst <- c(rowvar, colvar)
    pcdomainlst <- pcdomainlst[pcdomainlst != "NONE"]
  }
  
  ## Append classified variables to query
  if (!is.null(rowvar) && rowvar %in% pcdomainlst) {
    if (!is.null(classifyrow)) {
      cselectqry <- paste0(cselectqry, ", \n",
                           classifyrow$rowclassqry)
      rowvarnm <- classifyrow$rowclassnm
      byvars <- c(byvars, rowvarnm)
    } else {
      cselectqry <- paste0(cselectqry, ", pc.", rowvar)
      rowvarnm <- rowvar
      byvars <- c(byvars, paste0("pc.", rowvar))
    }
  } else {
    rowvarnm <- "NONE"
  }
  if (!is.null(colvar) && colvar != "NONE" && colvar %in% pcdomainlst) {
    if (!is.null(classifycol)) {
      cselectqry <- paste0(cselectqry, ", \n",
                           classifycol$colclassqry)
      byvars <- c(byvars, classifycol$colclassnm)
      colvarnm <- classifycol$colclassnm
    } else {
      cselectqry <- paste0(cselectqry, ", pc.", colvar)
      colvarnm <- colvar
      byvars <- c(byvars, paste0("pc.", colvar))
    }
  } else {
    colvarnm <- "NONE"
  }
  if (!is.null(colvar) && colvar == "NONE") {
    colvarnm <- colvar
  }
  
  ## Append classified variables to query
  totalnm <- findnm("TOTAL", pcdomainlst, returnNULL = TRUE)
  if (!is.null(totalnm)) {
    cselectqry <- paste0(cselectqry, ", pc.", "PREV_", totalnm, ", pc.", totalnm)
    byvars <- c(byvars, paste0("pc.", "PREV_", totalnm), paste0("pc.", totalnm))
  }
  
 
  ## Final select query
  cdomdatselectqry <- 
    paste0(cselectqry, ", ",
           "\n  ", estvarqry)
  
  ## Build cdomdat FROM query
  joinqry <- getjoinqry(joinid1 = cuniqueid, joinid2 = pltidsid,
                        alias1 = "pc.", alias2 = "pltidsadj.")
  cdomdatfromqry <- 
    paste0("\nFROM pltidsadj",
           "\nJOIN pltcondx pc ", joinqry)
  
  cdomdatfromqry <- 
    paste0(cdomdatfromqry, 
           "\nJOIN ", sccmnm, " sccm ON (sccm.plt_cn = pc.plt_cn 
                          AND sccm.prev_plt_cn = pc.prev_plt_cn 
                          AND sccm.condid = pc.condid 
                          AND sccm.prevcond = pc.prev_condid)")
    
  ## Build cdomdat query
  cdomdat.qry <- 
    paste0(cdomdatselectqry, 
           cdomdatfromqry,
           pcwhereqry,
           "\nGROUP BY ", toString(byvars))

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
              estnm = estnm,
              rowvar = rowvarnm, 
              colvar = colvarnm,
              grpvar = c(rowvarnm, colvarnm)))
  
}
