getGBestimates <- function(esttype, 
                           ratiotype = "PERACRE",
                           domdatn, 
                           domdatd = NULL, 
                           uniqueid,
                           estvarn.name, 
                           estvard.name = NULL,
                           rowvar, 
                           colvar, 
                           grpvar,
                           pltassgnx,
                           pltassgnid,
                           unitarea, 
                           unitvar, 
                           areavar,
                           stratalut, 
                           strvar,
                           strwtvar,
                           totals, 
                           sumunits, 
                           uniquerow, 
                           uniquecol,
                           row.add0 = FALSE, 
                           col.add0 = FALSE,
                           row.orderby = NULL, 
                           col.orderby = NULL,
                           row.NAname = "Other",
                           col.NAname = "Other") {


  ## Set global variables
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=
    strwt=n.total=n.strata=ONEUNIT <- NULL
  addtotal <- ifelse(rowvar %in% c("PREV_TOTAL", "TOTAL") || 
                       length(unique(domdatn[[rowvar]])) > 1, TRUE, FALSE)
  strunitvars <- c(unitvar, strvar)
  
  message("getting estimates using GB...")
  
  ## Append TOTAL to domdatn
  if (addtotal && !"TOTAL" %in% names(domdatn)) {
    domdatn$TOTAL <- 1
  }
  
  ## Join domdat to pltassgnx using data.table key
  domdatn <- pltassgnx[domdatn]
  if (esttype == "RATIO") {
    if (addtotal && !"TOTAL" %in% names(domdatd)) {
      domdatd$TOTAL <- 1
    }
    domdatd <- pltassgnx[domdatd]
  }
  
  ## Get total estimate and merge area	
  if (addtotal) {
    ## Sum numerator to plot, domain (TOTAL) level
    domdattot <- 
      domdatn[, lapply(.SD, sum, na.rm=TRUE), 
              by = c(strunitvars, uniqueid, "TOTAL"), .SDcols=estvarn.name]
    if (esttype == "RATIO") {
      ## Sum denominator to plot, domain (TOTAL) level 
      domdatdtot <- 
        domdatd[, lapply(.SD, sum, na.rm=TRUE), 
              by = c(strunitvars, uniqueid, "TOTAL"), .SDcols=estvard.name]
      domdattot <- merge(domdattot, domdatdtot, by=c(strunitvars, uniqueid, "TOTAL"))
    }

    unit_totest <- 
      GBest.pbar(sumyn = estvarn.name, 
                 sumyd = estvard.name,
                 ysum = domdattot,
                 esttype = esttype, 
                 ratiotype = ratiotype,
                 uniqueid = uniqueid, 
                 stratalut = stratalut,
                 unitvar = unitvar, 
                 strvar = strvar, 
                 domain = "TOTAL")
    tabs <- check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)     
    unit_totest <- unit_totest[unitarea, nomatch=0]

    if (totals) {
      unit_totest <- 
        getpse(unit_totest, 
               areavar = areavar, 
               esttype = esttype)
    } else {
      unit_totest <- 
        getpse(unit_totest, 
               esttype = esttype)
    }
  }

  ## Get row estimate
  if (is.null(rowvar)) rowvar <- "TOTAL"
  if (rowvar != "TOTAL") {

    ## Check uniquerow with domain-level data - add NA factor values if necessary
    uniquerow <- check.unique(x = domdatn, 
                              uniquex = uniquerow,
                              xvar = rowvar, 
                              NAname = row.NAname)

    ## Sum numerator to plot, rowvar level
    #domdatn <- domdatn[!is.na(domdatn[[rowvar]]),] 
    domdattot <- 
      domdatn[, lapply(.SD, sum, na.rm=TRUE), 
              by = c(strunitvars, uniqueid, rowvar), .SDcols=estvarn.name]
    if (esttype == "RATIO") {
      ## Sum denominator to plot, domain (TOTAL) level 
      if (rowvar %in% names(domdatd)) {
        domdatdtot <- 
          domdatd[, lapply(.SD, sum, na.rm=TRUE), 
                  by = c(strunitvars, uniqueid, rowvar), .SDcols = estvard.name]
       
        ## Check class of ddomdattot and domdatdtot and merge tables
        tabchk <- check.matchclass(domdattot, domdatdtot, c(strunitvars, uniqueid, rowvar))
        domdattot <- tabchk$tab1
        domdatdtot <- tabchk$tab2
        
        domdattot <- merge(domdattot, domdatdtot, 
                            by = c(strunitvars, uniqueid, rowvar))
      } else {
        domdatdtot <- 
          domdatd[, lapply(.SD, sum, na.rm=TRUE),
                  by = c(strunitvars, uniqueid), .SDcols = estvard.name]
        domdattot <- merge(domdattot, domdatdtot, 
                            by = c(strunitvars, uniqueid))
      }
    }  
    unit_rowest <- 
      GBest.pbar(sumyn = estvarn.name, 
                 sumyd = estvard.name,
                 ysum = domdattot,
                 esttype = esttype, 
                 ratiotype = ratiotype,
                 uniqueid = uniqueid, 
                 stratalut = stratalut,
                 unitvar = unitvar, 
                 strvar = strvar, 
                 domain = rowvar)
  }
  
  ## Get column (and cell) estimate  
  if (is.null(colvar)) colvar <- "NONE"
  if ( colvar != "NONE") {

    ## Check uniquecol with domain-level data - add NA factor values if necessary
    uniquecol <- check.unique(x = domdatn, 
                              uniquex = uniquecol,
                              xvar = colvar, 
                              NAname = col.NAname)
    
    ## Sum numerator to plot, colvar level
    #domdatn <- domdatn[!is.na(domdatn[[colvar]]),] 
    domdattot <- 
      domdatn[, lapply(.SD, sum, na.rm=TRUE), 
              by = c(strunitvars, uniqueid, colvar), .SDcols=estvarn.name]

    if (esttype == "RATIO") {
      ## Sum denominator to plot, domain (TOTAL) level 
      if (colvar %in% names(domdatd)) {
        domdatdtot <- 
          domdatd[, lapply(.SD, sum, na.rm=TRUE), 
                  by = c(strunitvars, uniqueid, colvar), .SDcols = estvard.name]
        
        ## Check class of ddomdattot and domdatdtot and merge tables
        tabchk <- check.matchclass(domdattot, domdatdtot, c(strunitvars, uniqueid, colvar))
        domdattot <- tabchk$tab1
        domdatdtot <- tabchk$tab2

        domdattot <- merge(domdattot, domdatdtot, 
                           by = c(strunitvars, uniqueid, colvar))
      } else {
        domdatdtot <- 
          domdatd[, lapply(.SD, sum, na.rm=TRUE),
                  by = c(strunitvars, uniqueid), .SDcols = estvard.name]
        domdattot <- merge(domdattot, domdatdtot, 
                           by = c(strunitvars, uniqueid))
      }
    }  
    unit_colest <- 
      GBest.pbar(sumyn = estvarn.name, 
                 sumyd = estvard.name,
                 ysum = domdattot,
                 esttype = esttype, 
                 ratiotype = ratiotype,
                 uniqueid = uniqueid, 
                 stratalut = stratalut,
                 unitvar = unitvar, 
                 strvar = strvar, 
                 domain = colvar)

   ## Sum numerator to plot, grpvar level
   #domdatn <- domdatn[!is.na(domdatn[[rowvar]]) & domdatn[[rowvar]] != "NA",] 
   #domdatn <- domdatn[!is.na(domdatn[[colvar]]) & domdatn[[colvar]] != "NA",] 
   domdattot <- 
      domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvars, uniqueid, grpvar), .SDcols=estvarn.name]
  
   if (esttype == "RATIO") {

     if (all(grpvar %in% names(domdatd))) {
       ## Sum denominator to plot, grpvar level 
       domdatd <- domdatd[!is.na(domdatd[[rowvar]]) & domdatd[[rowvar]] != "NA",] 
       domdatd <- domdatd[!is.na(domdatd[[colvar]]) & domdatd[[colvar]] != "NA",] 
       
       domdatdtot <- 
         domdatd[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvars, uniqueid, grpvar), .SDcols = estvard.name]
       domdattot <- merge(domdattot, domdatdtot, 
                          by = c(strunitvars, uniqueid, grpvar))
     } else {
       domdatdtot <- 
         domdatd[, lapply(.SD, sum, na.rm=TRUE),
                 by = c(strunitvars, uniqueid), .SDcols = estvard.name]
       domdattot <- merge(domdattot, domdatdtot, 
                          by = c(strunitvars, uniqueid))
     }
   }  
   unit_grpest <- 
      GBest.pbar(sumyn = estvarn.name, 
                 sumyd = estvard.name,
                 ysum = domdattot,
                 esttype = esttype,
                 ratiotype = ratiotype,
                 uniqueid = uniqueid, 
                 stratalut = stratalut,
                 unitvar = unitvar, 
                 strvar = strvar, 
                 domain = grpvar)
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- 
      add0unit(x = unit_rowest, 
               xvar = rowvar, 
               uniquex = uniquerow, 
               unitvar = unitvar, 
               xvar.add0 = row.add0)
    tabs <- check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2

    if (!is.null(row.orderby) && row.orderby != "NONE") {
      setorderv(unit_rowest, c(row.orderby))
    }
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
  
    if (totals) {
      unit_rowest <- 
        getpse(unit_rowest, 
               areavar = areavar, 
               esttype = esttype)
    } else {
      unit_rowest <- 
        getpse(unit_rowest, 
               esttype = esttype)
    }      
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit_colest)) {
    unit_colest <- 
      add0unit(x = unit_colest, 
               xvar = colvar, 
               uniquex = uniquecol,
               unitvar = unitvar, 
               xvar.add0 = col.add0)
    tabs <- check.matchclass(unitarea, unit_colest, unitvar)
    unitarea <- tabs$tab1
    unit_colest <- tabs$tab2
    
    if (!is.null(col.orderby) && col.orderby != "NONE") {
      setorderv(unit_colest, c(col.orderby))
    }
    setkeyv(unit_colest, unitvar)
    unit_colest <- unit_colest[unitarea, nomatch=0]
  
    if (totals) {
      unit_colest <- 
        getpse(unit_colest, 
               areavar = areavar, 
               esttype = esttype)
    } else {
      unit_colest <- 
        getpse(unit_colest, 
               esttype = esttype)
    }      
    setkeyv(unit_colest, c(unitvar, colvar))
  }

  if (!is.null(unit_grpest)) {
    unit_grpest <-
      add0unit(x = unit_grpest, 
               xvar = rowvar, 
               uniquex = uniquerow, 
               unitvar = unitvar, 
               xvar.add0 = row.add0, 
               xvar2 = colvar, 
               uniquex2 = uniquecol, 
               xvar2.add0 = col.add0)
    tabs <- check.matchclass(unitarea, unit_grpest, unitvar)
    unitarea <- tabs$tab1
    unit_grpest <- tabs$tab2 

    #if (!row.add0 && any(unit_grpest$nhat == 0)) {
    #  unit_grpest <- unit_rowest[unit_grpest$nhat > 0,]
    #}
    
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      if (!is.null(col.orderby) && col.orderby != "NONE") {
        setorderv(unit_grpest, c(row.orderby, col.orderby))
      } else {
        setorderv(unit_grpest, c(row.orderby))
      }         
    } else if (!is.null(col.orderby) && col.orderby != "NONE") {
      setorderv(unit_grpest, c(col.orderby))
    }         
    setkeyv(unit_grpest, unitvar)
    unit_grpest <- unit_grpest[unitarea, nomatch=0]
  
    if (totals) {
      unit_grpest <- 
        getpse(unit_grpest, 
               areavar = areavar, 
               esttype = esttype)
    } else {
      unit_grpest <- 
        getpse(unit_grpest, 
               esttype = esttype)
    }      
    setkeyv(unit_grpest, c(unitvar, rowvar, colvar))
  }

  ###################################################################################
  ## Get row and column totals for units if sumunits=FALSE
  ###################################################################################
  ## For sumunits=FALSE, get estimation unit totals
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && !is.null(grpvar))) {
  
    ## AGGREGATE UNIT stratalut FOR ROWVAR and GRAND TOTAL
    stratalut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    stratalut2 <- 
      stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
                    by = strunitvars2, .SDcols=c(strwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(strwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvars2)
  
    unitarea2 <- data.table(unitarea, ONEUNIT=1)
    unitarea2 <- unitarea2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
                         .SDcols=areavar]
    setkey(unitarea2, "ONEUNIT")
  
    domdatn[, ONEUNIT := 1]
  
    ## CALCULATE UNIT TOTALS FOR ROWVAR
    domdatnsum <- 
      domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvars2, uniqueid, rowvar), .SDcols=estvarn.name]
    rowunit <- 
      GBest.pbar(sumyn = estvarn.name, 
                 ysum = domdatnsum,
                 uniqueid = uniqueid, 
                 stratalut = stratalut2,
                 unitvar = "ONEUNIT", 
                 strvar = strvar, 
                 domain = rowvar)
    rowunit <- 
      add0unit(x = rowunit, 
               xvar = rowvar, 
               uniquex = uniquerow, 
               unitvar = "ONEUNIT", 
               xvar.add0 = row.add0)
    tabs <- check.matchclass(unitarea2, rowunit, "ONEUNIT")
    unitarea2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitarea2, nomatch=0]
    if (totals) {
      rowunit <- 
        getpse(rowunit, 
               areavar = areavar, 
               esttype = esttype)
    } else {
      rowunit <- 
        getpse(rowunit, 
               esttype = esttype)
    }      
    setkeyv(rowunit, c("ONEUNIT", rowvar))
  
    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    domdatnsum <- 
      domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvars2, uniqueid, "TOTAL"), .SDcols=estvarn.name]
    totunit <- 
      GBest.pbar(sumyn = estvarn.name, 
                 ysum = domdatnsum,
                 uniqueid = uniqueid, 
                 stratalut = stratalut2,
                 unitvar = "ONEUNIT", 
                 strvar = strvar, 
                 domain = "TOTAL")
    tabs <- check.matchclass(unitarea2, totunit, "ONEUNIT")
    unitarea2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitarea2, nomatch=0]
    if (totals) {
      totunit <- getpse(totunit, areavar=areavar, esttype=esttype)
    } else {
      totunit <- getpse(totunit, esttype=esttype)
    }      
  }
  #unitarea[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]

  returnlst <- list(unit_totest = unit_totest,
                    unit_rowest = unit_rowest,
                    unit_colest = unit_colest,
                    unit_grpest = unit_grpest,
                    rowunit = rowunit, 
                    totunit = totunit,
                    unitvar = unitvar
                    )
  return(returnlst)
}
