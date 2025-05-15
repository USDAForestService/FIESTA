getGBestimates <- function(esttype,
                           ratiotype = "PERACRE",
                           domdatn,
                           domdatd = NULL,
                           uniqueid,
                           condid = "CONDID",
                           estvarn.name,
                           estvard.name = NULL,
                           tdomvar = NULL,
                           tdomvar2 = NULL,
                           tdomvarlstn = NULL,
                           tdomvarlstd = NULL,
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
                           rowvar = NULL,
                           colvar = NULL,
                           grpvar = NULL,
                           uniquerow = NULL,
                           uniquecol = NULL,
                           row.add0 = FALSE,
                           col.add0 = FALSE,
                           row.orderby = NULL,
                           col.orderby = NULL,
                           row.NAname = "Other",
                           col.NAname = "Other") {

  ## DESCRIPTION:
  ## Generates estimates using Green-Book estimators by estimation unit for
  ## total, row totals, column totals, and groups (cells)
  ## 1. merges domain-level data with pltassgn
  ## 2. aggregates data from plot/cond level to plot/domain level
  ## 3. generates estimates by estimation unit
  ## 4. merges row/column names to estimates
  ## 5. returns estimates by estimation unit

  ## Set global variables
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=
    strwt=n.total=n.strata=ONEUNIT=domvard=variable <- NULL
  strunitvars <- c(unitvar, strvar)

  message("generating post-stratified estimates using FIESTA...")

  ## Get estimates for total
  if (is.null(rowvar)) rowvar <- "TOTAL"
  addtotal <- ifelse(rowvar %in% c("PREV_TOTAL", "TOTAL") ||
                       length(unique(domdatn[[rowvar]])) > 1, TRUE, FALSE)

  ## Append TOTAL to domdatn
  if (addtotal && !"TOTAL" %in% names(domdatn)) {
    domdatn$TOTAL <- 1
  }

  ## Join domdat to pltassgnx using data.table key and all.y=TRUE
  domdatn <- pltassgnx[domdatn]
  
  if (esttype == "RATIO") {
    ## Append TOTAL to domdatn
    if (addtotal && !"TOTAL" %in% names(domdatd)) {
      domdatd$TOTAL <- 1
    }
    domdatd <- pltassgnx[domdatd]

    ## Check to make sure class of join variables match
    chk <- FIESTAutils::check.matchclass(domdatd, domdatn, c(uniqueid, condid))
    domdatd <- chk$tab1
    domdatn <- chk$tab2
  }

  ## Get total estimate
  #############################################################################
  if (addtotal) {

    ## sum condition/domain-level data to plot/domain-level
    domdatplt <- domdatn[, lapply(.SD, sum, na.rm=TRUE),
                          by=c(strunitvars, uniqueid, "TOTAL"), .SDcols=estvarn.name]

    if (esttype == "RATIO") {
      ## denominator: sum condition/domain-level data to plot/domain-level
      domdatdplt <- domdatd[, lapply(.SD, sum, na.rm=TRUE),
                              by=c(strunitvars, uniqueid, "TOTAL"), .SDcols=estvard.name]
      ## merge denominator and numerator data keeping all denominator data (all.x=TRUE)
      domdatplt <- merge(domdatdplt, domdatplt, by=c(strunitvars, uniqueid, "TOTAL"), all.x = TRUE)
    }

    ## generate estimates by estimation unit
    unit_totest <-
      GBest.pbar(sumyn = estvarn.name,
                 sumyd = estvard.name,
                 ysum = copy(domdatplt),
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

    ## calculate percent standard errors (pse) by estimation unit
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

  ## Sum data to condition/domain-level
  #############################################################################
  domvars <- c(rowvar, colvar)
  domvars <- domvars[domvars != "NONE"]
  byvars <- c(strunitvars, uniqueid, condid)
  byvarsplt <- c(strunitvars, uniqueid)

  if (esttype == "RATIO") {
    domvard <- domvars[domvars %in% names(domdatd)]
    domvarn <- domvars[domvars %in% names(domdatn)]
    
    ## denominator: sum estimation variable to condition/domain-level
    byvarsd <- c(byvars, domvard)
    domdatdcond <- domdatd[, lapply(.SD, sum, na.rm=TRUE), by = byvarsd, .SDcols = estvard.name]


    ## numerator: sum estimation variable to condition/domain-level
    if (!is.null(tdomvar)) {
      
      ## Check to make sure class of join variables match
      chk <- FIESTAutils::check.matchclass(domdatdcond, domdatn, byvarsd)
      domdatdcond <- chk$tab1
      domdatn <- chk$tab2
      

      ## merge denominator and numerator data keeping all rows in denominator (all.x=TRUE)
      domdattmp <- merge(domdatdcond, domdatn, by = byvarsd, all.x = TRUE)
      ## change any NA values to 0 values
      domdattmp <- DT_NAto0(domdattmp, c(tdomvarlstn, estvarn.name, "TOTAL"))

      ## numerator: transpose columns to rows for estimation
      domdatncondt <- data.table(transpose2row(domdattmp, uniqueid = byvarsd,
                                               tvars = as.character(tdomvarlstn)))
      setnames(domdatncondt, "value", estvarn.name)

      if (!is.null(tdomvar2)) {
        domdatncondt[, (grpvar) := tstrsplit(variable, "#")][, variable := NULL]
      } else {
        setnames(domdatncondt, "variable", tdomvar)
      }

      ## numerator: total sums
      domdatcond <- domdattmp[, c(byvarsd, estvard.name, estvarn.name), with = FALSE]

    } else {
      ## merge denominator and numerator data keeping rows in denominator (all.x=TRUE)
      domdatcond <- merge(domdatdcond, domdatn, by = byvarsd, all.x = TRUE)
    }

  } else {

    domvarn <- domvars[domvars %in% names(domdatn)]
    byvarsn <- c(byvars, domvarn)

    domdatcond <- domdatn[, lapply(.SD, sum, na.rm=TRUE),
                          by = byvarsn, .SDcols = estvarn.name]
  }


  ## Get row estimate
  #############################################################################
  if (is.null(rowvar) || rowvar == "NONE") rowvar <- "TOTAL"
  if (rowvar != "TOTAL") {

    ## Check uniquerow with domain-level data - add NA factor values if necessary
    uniquerow <- check.unique(x = domdatn,
                              uniquex = uniquerow,
                              xvar = rowvar,
                              NAname = row.NAname)


    ## sum estimation variable to plot/domain-level by rowvar
    ###########################################################################
    if (esttype == "RATIO") {

      ## denominator: sum estimates by rowvar and plot if in denominator
      domvardrow <- domvard[domvard %in% rowvar]
      domdatdplt <- domdatdcond[, lapply(.SD, sum, na.rm=TRUE),
                                by = c(byvarsplt, domvardrow), .SDcols = estvard.name]

      if (!is.null(tdomvar) && (!is.null(tdomvar2) || tdomvar == rowvar)) {
        ## numerator: sum estimates by rowvar
        domdatnplt <- domdatncondt[, lapply(.SD, sum, na.rm=TRUE),
                                   by = c(byvarsplt, rowvar), .SDcols = estvarn.name]
        
        ## merge denominator and numerator
        domdatrow <- merge(domdatdplt, domdatnplt, by = byvarsplt, all.x=TRUE)
        
      } else {
        domvarnrow <- domvarn[domvarn %in% rowvar]
        domdatnplt <- domdatcond[, lapply(.SD, sum, na.rm=TRUE),
                                  by = c(byvarsplt, domvarnrow), .SDcols = estvarn.name]
        
        ## merge denominator and numerator
        domdatrow <- merge(domdatdplt, domdatnplt, by = c(byvarsplt, domvarnrow), all.x=TRUE)
      }

    } else {

      domvarsrow <- domvars[domvars %in% rowvar]
      domdatrow <- domdatcond[, lapply(.SD, sum, na.rm=TRUE),
                              by = c(byvarsplt, domvarsrow), .SDcols = estvarn.name]
      if (length(domvarsrow) > 0) {
        domdatrow <- domdatrow[!is.na(domdatrow[[domvarsrow]]),]
      }
    }

    ## generate estimates by estimation unit and rowvar
    unit_rowest <-
      GBest.pbar(sumyn = estvarn.name,
                 sumyd = estvard.name,
                 ysum = copy(domdatrow),
                 esttype = esttype,
                 ratiotype = ratiotype,
                 uniqueid = uniqueid,
                 stratalut = stratalut,
                 unitvar = unitvar,
                 strvar = strvar,
                 domain = rowvar)
  }


  ## Get column (and cell) estimate
  #############################################################################
  if (is.null(colvar)) colvar <- "NONE"
  if ( colvar != "NONE") {

    ## Check uniquecol with domain-level data - add NA factor values if necessary
    uniquecol <- check.unique(x = domdatn,
                              uniquex = uniquecol,
                              xvar = colvar,
                              NAname = col.NAname)


    ## sum estimation variable to plot/domain-level by rowvar
    ###########################################################################
    if (esttype == "RATIO") {
      ## denominator: sum estimates by colvar if in denominator
      domvardcol <- domvard[domvard == colvar]
      domdatdplt <- domdatdcond[, lapply(.SD, sum, na.rm=TRUE),
                                by = c(byvarsplt, domvardcol), .SDcols = estvard.name]

      if (!is.null(tdomvar) && (!is.null(tdomvar2) || tdomvar == colvar)) {
        ## numerator: sum estimates by rowvar
        domdatnplt <- domdatncondt[, lapply(.SD, sum, na.rm=TRUE),
                                   by = c(byvarsplt, colvar), .SDcols = estvarn.name]
        
        ## merge denominator and numerator
        domdatcol <- merge(domdatdplt, domdatnplt, by = byvarsplt)
        
      } else {
        domvarncol <- domvarn[domvarn %in% colvar]
        domdatnplt <- domdatcond[, lapply(.SD, sum, na.rm=TRUE),
                                 by = c(byvarsplt, domvarncol), .SDcols = estvarn.name]
        
        ## merge denominator and numerator
        domdatcol <- merge(domdatdplt, domdatnplt, by = c(byvarsplt, domvarncol))
      }

    } else {
      
      domvarscol <- domvars[domvars %in% colvar]
      domdatcol <- domdatcond[, lapply(.SD, sum, na.rm=TRUE),
                              by = c(byvarsplt, domvarscol), .SDcols = estvarn.name]
      
      if (length(domvarscol) > 0) {
        domdatcol <- domdatcol[!is.na(domdatcol[[domvarscol]]),]
      }
    }

    ## generate estimates by estimation unit and colvar
    unit_colest <-
      GBest.pbar(sumyn = estvarn.name,
                 sumyd = estvard.name,
                 ysum = copy(domdatcol),
                 esttype = esttype,
                 ratiotype = ratiotype,
                 uniqueid = uniqueid,
                 stratalut = stratalut,
                 unitvar = unitvar,
                 strvar = strvar,
                 domain = colvar)


    ## Get estimates for cell values (grpvar)
    #############################################################################

    ## sum estimation variable to plot/domain-level by grpvar
    ###########################################################################
    if (esttype == "RATIO") {
      ## denominator: sum estimates by grpvar if in denominator
      domvardgrp <- domvard[domvard %in% grpvar]
      domdatdplt <- domdatdcond[, lapply(.SD, sum, na.rm=TRUE),
                                by = c(byvarsplt, domvardgrp), .SDcols = estvard.name]

      if (!is.null(tdomvar)) {
        if (!is.null(tdomvar2)) {
          ## numerator: sum estimates by rowvar and colvar
          domdatnplt <- domdatncondt[, lapply(.SD, sum, na.rm=TRUE),
                                   by = c(byvarsplt, grpvar), .SDcols = estvarn.name]

          ## merge denominator and numerator
          domdatgrp <- merge(domdatdplt, domdatnplt, by = byvarsplt)

        } else {
          ## numerator: sum estimates by rowvar
          domdatnplt <- domdatncondt[, lapply(.SD, sum, na.rm=TRUE),
                                 by = c(byvarsplt, grpvar), .SDcols = estvarn.name]

          ## merge denominator and numerator
          domdatgrp <- merge(domdatdplt, domdatnplt, by = c(byvarsplt, domvard))
        }
      } else {
        ## numerator: sum estimates by rowvar
        domdatnplt <- domdatcond[, lapply(.SD, sum, na.rm=TRUE),
                                   by = c(byvarsplt, grpvar), .SDcols = estvarn.name]

        ## merge denominator and numerator
        domdatgrp <- merge(domdatdplt, domdatnplt, by = c(grpvar, byvarsplt))
      }
    } else {
      domdatgrp <- domdatcond[, lapply(.SD, sum, na.rm=TRUE),
                              by = c(byvarsplt, grpvar), .SDcols = estvarn.name]
    }

    unit_grpest <-
      GBest.pbar(sumyn = estvarn.name,
                 sumyd = estvard.name,
                 ysum = copy(domdatgrp),
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
