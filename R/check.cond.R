check.cond <- function(areawt, 
                       areawt2 = NULL, 
                       adj, 
                       adjcase, 
                       cuniqueid = "PLT_CN", 
                       condid = "CONDID", 
                       rowvar, 
                       colvar, 
                       pcdomainlst = NULL,
                       popdatindb = TRUE,
                       popconn = NULL,
                       pltcondx = NULL,
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
  estvara. <- "pc."
  rowvarnm=colvarnm <- NULL
  
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
      colvarnm <- classifycol$colclassnm
      byvars <- c(byvars, colvarnm)
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
  if (rowvar != "TOTAL") {
    totalnm <- findnm("TOTAL", pcdomainlst, returnNULL = TRUE)
    if (!is.null(totalnm)) {
      cselectqry <- paste0(cselectqry, ", pc.", totalnm)
      byvars <- c(byvars, paste0("pc.", totalnm))
    }
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
  
  
  if (rowvar == "ALP_ADFORCD" && any(cdomdat[[rowvar]] == 9999)) {
    #cdomdat[cdomdat[[rowvar]] == 9999, rowvar] <- NA
    cdomdat[is.na(cdomdat[[rowvar]]), rowvar] <- 9999
  }

  return(list(cdomdat = cdomdat, 
              cdomdatqry = cdomdatqry,
              estnm = estnm,
              rowvar = rowvarnm, 
              colvar = colvarnm, 
              grpvar = c(rowvarnm, colvarnm)))
  
}
