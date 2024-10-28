check.condCHNG <- function(areawt, areawt2, 
                           adj, adjcase, 
                           cuniqueid, condid, 
                           chngtype,
                           rowvar, row.orderby, 
                           uniquerow, title.rowvar,
                           colvar, col.orderby,
                           uniquecol, title.colvar,
                           pcdomainlst = NULL,
                           popdatindb,
                           popconn = NULL,
                           pltcondx,
                           sccmx,
                           pltidsadj = NULL,
                           pltcondxadjWITHqry = NULL,
                           pcwhereqry = NULL,
                           landarea.filter = NULL,
                           classifyrow = NULL,
                           classifycol = NULL) {
  ###################################################################################
  ### Get condition-level domain data
  ###################################################################################
  estnm <- "ESTIMATED_VALUE"
  estvara. <- "sccm."
  sccmnm <- ifelse(popdatindb, sccmx, "sccmx")
  rowvarnm=colvarnm <- NULL
  
  ## Add past landarea.filter to WHERE statement
  if (!is.null(landarea.filter)) {
    landarea.filterCHNG <- gsub("pc.", "pc.PREV_", landarea.filter)
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
  
  # if (is.null(pcwhereqry)) {
  #   pcwhereqry <- paste0(
  #     "\nWHERE pc.CONDPROP_UNADJ IS NOT NULL",
  #     "\n    AND ((sccm.SUBPTYP = 3 AND pc.PROP_BASIS = 'MACR') OR 
  #             (sccm.SUBPTYP = 1 AND pc.PROP_BASIS = 'SUBP'))",
  #     "\n    AND COALESCE(pc.COND_NONSAMPLE_REASN_CD, 0) = 0",
  #     "\n    AND COALESCE(ppc.COND_NONSAMPLE_REASN_CD, 0) = 0")
  # } else {
  #   pcwhereqry <- paste0(pcwhereqry, 
  #                        "\n    AND pc.CONDPROP_UNADJ IS NOT NULL",
  #                        "\n    AND ((sccm.SUBPTYP = 3 AND pc.PROP_BASIS = 'MACR') OR 
  #                                   (sccm.SUBPTYP = 1 AND pc.PROP_BASIS = 'SUBP'))",
  #                        "\n    AND COALESCE(pc.COND_NONSAMPLE_REASN_CD, 0) = 0",
  #                        "\n    AND COALESCE(ppc.COND_NONSAMPLE_REASN_CD, 0) = 0")
  # }
  
  
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

    ## If colvar = "NONE"
    if (is.null(colvar) || colvar == "NONE") {
      colvar <- rowvar
      col.orderby <- row.orderby
      title.colvar <- title.rowvar
      uniquecol <- copy(uniquerow)
      classifycol <- classifyrow

      if (!is.null(classifycol)) {
        names(classifycol)[names(classifycol) %in% c("row.classify", "rowclassnm", "rowclassqry")] <- 
                c("col.classify", "colclassnm", "colclassqry")
      }
    }
    
    if (!is.null(classifyrow)) {
      cselectqry <- paste0(cselectqry, ", \n",
                           classifyrow$rowclassqry)
      rowclassnm <- classifyrow$rowclassnm
      cselectqry <- sub(rowclassnm, paste0("PREV_", rowclassnm), cselectqry)
      #cselectqry <- gsub(paste0("pc.", rowvar), paste0("ppc.", rowvar), cselectqry)
      rowvarnm <- paste0("PREV_", rowclassnm)
      byvars <- c(byvars, rowvarnm)
      setnames(uniquerow, rowclassnm, rowvarnm)
    } else {
      cselectqry <- paste0(cselectqry, ", pc.PREV_", rowvar)
      rowvarnm <- paste0("PREV_", rowvar)
      byvars <- c(byvars, paste0("pc.", rowvarnm))
      if (!is.null(uniquerow))
        names(uniquerow) <- paste0("PREV_", names(uniquerow))
      #setnames(uniquerow, rowvar, rowvarnm)
    }
  }
    
  ## Append classified variables to query
  if (colvar %in% pcdomainlst) {
    if (!is.null(classifycol)) {
      cselectqry <- paste0(cselectqry, ", \n",
                           classifycol$colclassqry)
      byvars <- c(byvars, classifycol$colclassnm)
      colvarnm <- classifycol$colclassnm
    } else {
      cselectqry <- paste0(cselectqry, ", pc.", colvar)
      byvars <- c(byvars, paste0("pc.", colvar))
      colvarnm <- colvar
    }
  }
  
  ## Append classified variables to query
  totalnm <- findnm("TOTAL", pcdomainlst, returnNULL = TRUE)
  if (!is.null(totalnm)) {
    cselectqry <- paste0(cselectqry, ", pc.", "PREV_", totalnm, ", pc.", totalnm)
  }
  
  
  ## Rename rowvar variables with prefix 'PREV_'
  rowvar <- rowvarnm
  if (!is.null(row.orderby)) {
    row.orderby <- paste0("PREV_", row.orderby)
  }
  title.rowvar <- paste0("Previous ", title.rowvar)
    
  
  ## Final select query
  cdomdatselectqry <- 
    paste0(cselectqry, ", ",
           "\n  ", estvarqry)
  
  ## Build cdomdat FROM query
  joinqry <- getjoinqry(joinid1 = cuniqueid, joinid2 = cuniqueid,
                        alias1 = "pltidsadj.", alias2 = "pc.")
  cdomdatfromqry <- 
    paste0("\nFROM pltidsadj",
           "\nJOIN pltcondx pc ", joinqry)
  
  # cdomdatfromqry <- 
  #   paste0(cdomdatfromqry, 
  #          "\nJOIN pltcondx ppc ON (ppc.PLT_CN = pc.PREV_PLT_CN)",
  #          "\nJOIN ", sccmnm, " sccm ON (sccm.plt_cn = pc.plt_cn 
  #                         AND sccm.prev_plt_cn = ppc.plt_cn 
  #                         AND sccm.condid = pc.condid 
  #                         AND sccm.prevcond = ppc.condid)")

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
              rowvar = rowvarnm, row.orderby = row.orderby, 
              title.rowvar = title.rowvar, uniquerow = uniquerow,
              colvar = colvarnm, col.orderby = col.orderby, 
              title.colvar = title.colvar, uniquecol = uniquecol,
              grpvar = c(rowvarnm, colvarnm)
              ))
  
}
