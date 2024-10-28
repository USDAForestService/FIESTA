modWWWpop <- function(popType = "VOL",
                      aoidata,
                      dbconn,
                      minplotnum.strat = 2,
                      minplotnum.unit = 2){
  
  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data, including strata weights, number
  ## of plots by strata and estimation unit
  ## - builds query for calculating adjustment factors.
  ##################################################################################
  
  ## Set global variables
  SCHEMA. <- ""
  
  ## Set default parameters
  condid <- "condid"
  defaultVars <- TRUE
  areawt <- "condprop_unadj"
  ACI <-  FALSE
  areaunits <- "acres"
  unit.action = "remove"

  ## Define database queries lists
  dbqueries <- {}
  dbqueriesWITH <- {}
  
  
  ###################################################################################
  ## Load data
  ###################################################################################
  
  ## Getata from Josh function (wwwGetAOI)
  auxdata <- aoidata$auxdata
  aoiqueries <- aoidata$aoiqueries
  aoiselectqry <- aoiqueries$aoiselect
  aoifromqry <- aoiqueries$aoifromqry
  aoiwhereqry <- aoiqueries$aoiwhereqry
  SAE <- aoidata$SAE
  AOI_table_name <- AOI_table_name
  
  pltassgnid <- aoidata$pltassgnid
  pltidsid <- aoidata$pltidsid
  prednames <- aoidata$prednames
  predfac <- aoidata$predfac
  strvar <- aoidata$strvar
  
  pltassgn <- data.table(auxdata$pltassgn)
  unitarea <- data.table(auxdata$unitarea)
  auxlut <- data.table(auxdata$unitzonal)
  
  
  ## Query to define pltids
  pltidsqry <- aoidata$pltidsqry
  
  ## If SAE, check for strata variable
  if (!SAE) {
    module <- "GB"
    if (is.null(strvar)) {
      strata <- FALSE
    } else {
      strata <- TRUE
    }
    ## If strata, pivot auxlut to stratalut
    if (!strata) {
      auxlut$ONESTRAT.1 = pltassgn$ONESTRAT <- 1
      strvar <- "onestrat"
      strata <- TRUE
    }
    strunitvars <- c(unitvar, strvar)
    stratalut <- strat.pivot(auxlut, 
                             unitvars = unitvar, 
                             strvar = strvar,
                             strwtvar = "strwt")
  } else {
    module <- "SA"
    strunitvars <- unitvar
  }
  
  ## Create P2POINTCNT with number of plots by strata and domain unit
  P2POINTCNT <- pltassgn[, .N, by=strunitvars]
  setnames(P2POINTCNT, "N", "NBRPLOTS")
  
  
  ###################################################################################
  ## Check Auxiliary Data
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unit.action='combine', combines estimation units to reach minplotnum.unit.
  ## If unitvar and unitvar2, concatenates variables to 1 unitvar
  ###################################################################################
  if (!SAE) {
    auxdatGB <- 
      check.auxiliaryWWW(module = "GB",
                         pltassgn = pltassgn, 
                         unitarea = unitarea, 
                         auxlut = stratalut, 
                         strata = strata, 
                         strvar = strvar,
                         prednames = prednames,
                         P2POINTCNT = P2POINTCNT)
    if (is.null(auxdatGB)) return(0)
    pltassgnxGB <- setDT(auxdatGB$pltx)
    unitarea <- auxdatGB$unitarea
    stratalut <- auxdatGB$auxlut
    stratcombinelut <- auxdatGB$stratcombinelut
    setkeyv(pltassgnxGB, pltassgnid)

        
    ## Build new query for collapsing strata and/or domain units
    if (!is.null(stratcombinelut)) {
      
      ## change name of column in lut from domain_unit to AOI_table_name
      setnames(stratcombinelut, "domain_unit", AOI_table_name)
      fromcols <- c(AOI_table_name, strvar)
      
      ## define classcols and to cols
      classcols <- {}
      if (auxdatGB$unitvar != unitvar) {
        classcols <- auxdatGB$unitvar
        tocols <- c(auxdatGB$unitvar, auxdatGB$strvar)
      }
      if (auxdatGB$strvar != strvar) {
        classcols <- c(classcols, auxdatGB$strvar)
        tocols <- auxdatGB$strvar
      }
      
      ## Get select join for new strata variables
      combineqry <- 
        getcombineqry(lut = stratcombinelut,
                      classcols = classcols,
                      fromcols = fromcols,
                      tocols = tocols,
                      tab. = "plta.")
      #message(combineqry)
      
      pltidsqry <- paste0(
        "SELECT plta.", pltidsid, ", ",
        aoiselectqry, ", ",
        combineqry,
        aoifromqry,
        aoiwhereqry)
      #message(pltidsqry)
    } 
    
    if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid) 
    unitvar <- auxdatGB$unitvar
    unitvars <- auxdatGB$unitvars
    strvar <- auxdatGB$strvar
    strunitvars <- c(unitvars, strvar)
    
    pltassgnx <- auxdatGB$pltx
    setkeyv(pltassgnx, pltassgnid)

    strvar <- auxdatGB$strvar
    stratalut <- auxdatGB$auxlut
  } 
 
  auxdat <- 
    check.auxiliaryWWW(module = "MA",
                       pltassgn = pltassgn, 
                       unitarea = unitarea, 
                       auxlut = auxlut, 
                       prednames = prednames, 
                       predfac = predfac,
                       P2POINTCNT = P2POINTCNT)
    
  if (is.null(auxdat)) return(0)
  pltassgnx <- setDT(auxdat$pltx)
  setkeyv(pltassgnx, pltassgnid)
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar

  unitlut <- auxdat$auxlut
  prednames <- auxdat$prednames
  predfac <- auxdat$predfac
  unitNA <- auxdat$unitNA
  npixels <- auxdat$npixels

  
  ## Build WITH query from pltids
  pltidsWITHqry <- paste0(
    "WITH pltids AS ",
    "\n(", pltidsqry, ")")
  #message(pltidsWITHqry)
  
  
  ###################################################################################
  ## Build Population Data - adjustment queries
  ###################################################################################
  if (popType %in% c("ALL", "CURR", "VOL")) {
    
    
    ## Get database queries for adjustment factors
    ##############################################################
    dbqueriesADJ <- wwwGetAdjqryVOL(module = module,
                                    strvar = strvar,
                                    pltidsWITHqry = pltidsWITHqry)
    names(dbqueriesADJ$dbqueries)
    names(dbqueriesADJ$dbqueriesWITH)
    
    
    ## Run query to get ajustment factors by strata
    adjfactors.qry <- dbqueriesADJ$dbqueries$adjfactors
    #adjfactors <- DBI::dbGetQuery(dbconn, adjfactors.qry)
    #head(adjfactors)
    #pltidsadj <- DBI::dbGetQuery(dbconn, pltidsadj.qry)
    #head(pltidsadj)
    
    ## pltids WITH query, including adjustment factors
    pltidsadjWITHqry <- dbqueriesADJ$dbqueriesWITH$pltidsadjWITH
    
    
    
    ## Append pltcond to WITH query including adjustment factors
    ##############################################################
    dbqueriesPC <- wwwGetpltcondqry(popType = "VOL", 
                                    pltidsWITHqry = pltidsadjWITHqry,
                                    pcwhereqry = NULL)
    names(dbqueriesPC)
    pltcondxWITHqry <- dbqueriesPC$pltcondxWITHqry
    message(pltcondxWITHqry)
    
    ## Run query to get pltcondx data
    pltcondx.qry <- dbqueriesPC$pltcondxqry
    #pltcondx <- DBI::dbGetQuery(dbconn, pltcondx.qry)
    #head(pltcondx)

       
    ## Compile queries
    dbqueriesWITH$pltidsadjVOL = pltidsadjWITHqry
    dbqueriesWITH$pltcondxadjVOL = pltcondxadjWITHqry
    
    dbqueries$adjfactorsVOL = adjfactors.qry
    dbqueries$pltcondx = pltcondx.qry
  }
  
  
  if (popType %in% c("CHNG", "GRM")) {
    
    ## Get database queries for adjustment factors
    ##############################################################
    dbqueriesADJ <- wwwGetAdjqryCHNG(module = module,
                                     strvar = strvar,
                                     pltidsWITHqry = pltidsWITHqry)
    names(dbqueriesADJ$dbqueries)
    names(dbqueriesADJ$dbqueriesWITH)
    
    
    ## Run query to get ajustment factors by strata
    adjfactors.qry <- dbqueriesADJ$dbqueries$adjfactors
    #adjfactors <- DBI::dbGetQuery(dbconn, adjfactors.qry)
    #head(adjfactors)
    #pltidsadj <- DBI::dbGetQuery(dbconn, pltidsadj.qry)
    #head(pltidsadj)
    
    ## pltids WITH query, including adjustment factors
    pltidsadjWITHqry <- dbqueriesADJ$dbqueriesWITH$pltidsadjWITH
    
    
    ## Append pltcond to WITH query including adjustment factors
    ##############################################################
    dbqueriesPC <- wwwGetpltcondqry(popType = "CHNG", 
                                    pltidsadjWITHqry = pltidsadjWITHqry)
    names(dbqueriesPC)
    pltcondxadjWITHqry <- dbqueriesPC$pltcondxadjWITHqry
    #message(pltcondxadjWITHqry)
    
    ## Run query to get pltcondx data
    pltcondx.qry <- dbqueriesPC$pltcondxqry
    #pltcondx <- DBI::dbGetQuery(dbconn, pltcondx.qry)
    #head(pltcondx)
    
    
    ## Compile queries
    dbqueriesWITH$pltidsadjCHNG = pltidsadjWITHqry
    dbqueriesWITH$pltcondxadjCHNG = pltcondxadjWITHqry
    
    dbqueries$adjfactorsCHNG = adjfactors.qry
    dbqueries$pltcondx = pltcondx.qry
  }
  
  
  ## Build list of data to return
  ###################################################################################
  returnlst <- list(SAE = SAE,
                    pltassgnx = pltassgnx,  
                    unitarea = unitarea,
                    unitlut = unitlut,
                    prednames = prednames,
                    predfac = predfac,
                    areavar = areavar,
                    popconn = dbconn,
                    dbqueries = dbqueries,
                    dbqueriesWITH = dbqueriesWITH)
  
  if (!SAE) {
    returnlst <- append(returnlst, 
                        list(pltassgnxGB = pltassgnxGB, 
                             stratalut = stratalut,
                             strata = strata,
                             strvar = strvar,
                             npixels = npixels))
  }
  
  return(returnlst)
}
