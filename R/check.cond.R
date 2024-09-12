check.cond <- function(areawt, areawt2, 
                       adj, adjcase, 
                       cuniqueid, condid, 
                       rowvar, colvar,
                       pcdomainlst = NULL,
                       popdatindb,
                       popconn = NULL,
                       pltcondx = NULL,
                       pltidsadj = NULL,
                       pltcondxadjWITHqry = NULL,
                       pcwhereqry = NULL,
                       classifyrow = NULL,
                       classifycol = NULL) {
  ###################################################################################
  ### Get condition-level domain data
  ###################################################################################
  estnm <- "ESTIMATED_VALUE"
  estvara. <- "pc."
  
  ## Define select query for estimates
  estvarqry <- paste0(estvara., areawt)
  if (!is.null(areawt2)) {
    estvarqry <- areawt * areawt2
  }
  if (adj %in% c("samp", "plot")) {
    estvarqry <- paste0(estvarqry, " * ", adjcase)
  }
  estvarqry <- paste0("SUM(COALESCE(", estvarqry, ", 0)) AS ", estnm)
  
  
  ## Build SELECT query
  byvars <- paste0("pc.", c(cuniqueid, condid))
  cselectqry <- paste0("SELECT ", toString(byvars))
  #cdomdatvars <- paste0("pc.", byvars)
  
  ## Check pcdomainlst
  if (is.null(pcdomainlst)) {
    pcdomainlst <- c(rowvar, colvar)
  }
  
  ## Append classified variables to query
  if (!is.null(rowvar) && rowvar %in% pcdomainlst) {
    if (!is.null(classifyrow)) {
      cselectqry <- paste0(cselectqry, ", \n",
                         classifyrow$rowclassqry)
      byvars <- c(byvars, classifyrow$rowclassnm)
      #rowvar <- rowclassnm
    } else {
      cselectqry <- paste0(cselectqry, ", pc.", rowvar)
    }
  }
  if (!is.null(colvar) && colvar != "NONE" && colvar %in% pcdomainlst) {
    if (!is.null(classifycol)) {
      cselectqry <- paste0(cselectqry, ", \n",
                         classifyrow$colclassqry)
      byvars <- c(byvars, classifyrow$colclassnm)
      #colvar <- colclassnm
    } else {
      cselectqry <- paste0(cselectqry, ", pc.", colvar)
    }
  }

  #  cdomdatselectqry <- 
  #    paste0("SELECT ", toString(cdomdatvars), ", 1 AS TOTAL, ",
  #           "\n    ", estvarqry)
  cdomdatselectqry <- 
    paste0(cselectqry, ", ",
           "\n  ", estvarqry)
  
  ## Build cdomdat FROM query
  joinqry <- getjoinqry(joinid1 = cuniqueid, joinid2 = cuniqueid,
                        alias1 = "pltidsadj.", alias2 = "pc.")
  cdomdatfromqry <- 
    paste0("\nFROM pltidsadj",
           "\nLEFT JOIN pltcondx pc ", joinqry)
  
  ## Build cdomdat query
  cdomdatqry <- 
    paste0(cdomdatselectqry, 
           cdomdatfromqry,
           pcwhereqry,
           "\nGROUP BY ", toString(byvars))

  #Run query for cdomdat
  if (!popdatindb) {
    cdomdat <- tryCatch(
      sqldf::sqldf(cdomdatqry, connection = NULL),
      error = function(e) {
        message("invalid cdomdat query...")
        message(e,"\n")
        return(NULL) })
  } else {
    ## Append WITH query
    if (!is.null(pltcondxadjWITHqry)) {
      cdomdatqry <- paste0(pltcondxadjWITHqry,
                           "\n-------------------------------------------",
                           "\n", cdomdatqry)
    }  
    cdomdat <- tryCatch(
      DBI::dbGetQuery(popconn, cdomdatqry),
      error = function(e) {
        message("invalid cdomdat query...")
        message(e,"\n")
        return(NULL) })
  }
  if (is.null(cdomdat) || nrow(cdomdat) == 0) {
    message(cdomdatqry)
    return(NULL)
  }
  setkeyv(setDT(cdomdat), c(cuniqueid, condid))
  
  return(list(cdomdat = cdomdat, 
              cdomdatqry = cdomdatqry,
              estnm = estnm))
  
}
