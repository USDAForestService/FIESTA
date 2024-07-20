getGBestimates <- function(domdat, cuniqueid, estvar.name,
                         rowvar, colvar, grpvar,
                         pltassgnx, pltassgnid,
                         unitarea, unitvar, 
                         stratalut, strvar,
                         totals, sumunits, 
                         uniquerow, uniquecol,
                         row.add0, col.add0,
                         row.orderby, col.orderby) {

  ## Set global variables
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(domdat[[rowvar]])) > 1, TRUE, FALSE)
  strunitvar <- c(unitvar, strvar)
  
  message("getting estimates using GB...")

  ## Set key to domvar and join to pltassgnx
  domdat <- pltassgnx[domdat]

  ## Get total estimate and merge area	
  domdattot <- 
    domdat[, lapply(.SD, sum, na.rm=TRUE), 
               by = c(strunitvar, cuniqueid, "TOTAL"), .SDcols=estvar.name]
  unit_totest <- 
    GBest.pbar(sumyn = estvar.name, 
               ysum = domdattot,
               uniqueid = cuniqueid, 
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

  ## Get row estimate  
  if (rowvar != "TOTAL") {
    domdat <- domdat[!is.na(domdat[[rowvar]]),] 	
    domdatsum <- 
      domdat[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvar, cuniqueid, rowvar), .SDcols=estvar.name]
    unit_rowest <- 
      GBest.pbar(sumyn = estvar.name, 
                 ysum = domdatsum,
                 uniqueid = cuniqueid, 
                 stratalut = stratalut,
                 unitvar = unitvar, 
                 strvar = strvar, 
                 domain = rowvar)
  }

  ## Get column (and cell) estimate  
  if (colvar != "NONE") {
    domdat <- domdat[!is.na(domdat[[colvar]]),] 	
    domdatsum <- 
      domdat[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvar, cuniqueid, colvar), .SDcols=estvar.name]
    unit_colest <- 
      GBest.pbar(sumyn = estvar.name, 
                 ysum = domdatsum, 
                 uniqueid = cuniqueid, 
                 stratalut = stratalut,
                 unitvar = unitvar, 
                 strvar = strvar, 
                 domain = colvar)
  
    domdatsum <- 
      domdat[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvar, cuniqueid, grpvar), .SDcols=estvar.name]
    unit_grpest <- 
      GBest.pbar(sumyn = estvar.name, 
                 ysum = domdatsum,
                 uniqueid = cuniqueid, 
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
    strunitvar2 <- c("ONEUNIT", strvar)
    stratalut2 <- 
      stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
                    by = strunitvar2, .SDcols=c(strwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(strwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvar2)
  
    unitarea2 <- data.table(unitarea, ONEUNIT=1)
    unitarea2 <- unitarea2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
                         .SDcols=areavar]
    setkey(unitarea2, "ONEUNIT")
  
    domdat[, ONEUNIT := 1]
  
    ## CALCULATE UNIT TOTALS FOR ROWVAR
    domdatsum <- 
      domdat[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvar2, cuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- 
      GBest.pbar(sumyn = estvar.name, 
                 ysum = domdatsum,
                 uniqueid = cuniqueid, 
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
    domdatsum <- 
      domdat[, lapply(.SD, sum, na.rm=TRUE), 
                 by = c(strunitvar2, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- 
      GBest.pbar(sumyn = estvar.name, 
                 ysum = domdatsum,
                 uniqueid = cuniqueid, 
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
