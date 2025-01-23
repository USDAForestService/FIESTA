getMAestimates <- function(esttype, 
                           ratiotype = "PERACRE",
                           domdatn, 
                           domdatd = NULL, 
                           uniqueid,
                           estvarn.name, 
                           estvard.name = NULL,
                           rowvar, 
                           colvar, grpvar,
                           MAmethod, 
                           modelselect,
                           prednames, 
                           FIA = TRUE, 
                           bootstrap = FALSE,
                           pltassgnx, 
                           unitarea, 
                           unitvar, 
                           areavar,
                           unitlut, 
                           npixels, 
                           totals, 
                           uniquerow, 
                           uniquecol,
                           row.add0 = FALSE, 
                           col.add0 = FALSE,
                           row.orderby = NULL, 
                           col.orderby = NULL,
                           row.NAname = "Other",
                           col.NAname = "Other") {
  
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
#  addtotal <- ifelse(rowvar %in% c("PREV_TOTAL", "TOTAL") || 
#                       length(unique(domdatn[[rowvar]])) > 1, TRUE, FALSE)
  addtotal <- TRUE
  modelselect_bydomain <- FALSE
  response <- estvarn.name
  predselectlst <- list()
  
  
  ## Check column names
  prednames <- names(data.frame(matrix(NA,1,length(prednames), 
                                       dimnames=list(rownames=NULL, colnames=prednames))))
  pltassgnx <- data.table(pltassgnx, check.names = TRUE)
  unitarea <- data.table(unitarea, check.names = TRUE)
  unitlut <- data.table(unitlut, check.names = TRUE)
  
  
  masemethod <- switch(MAmethod,
                       PS = "postStrat",
                       greg = "greg",
                       gregRatio = "gregRatio",
                       gregEN = "gregElasticNet",
                       ratio = "ratioEstimator",
                       ht = "horvitzThompson")

  message("generating estimates using mase::", masemethod, " function...\n")
  
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
  
  ## Get unique estimation unit values
  estunits <- sort(unique(domdatn[[unitvar]]))
  
  predselect.overall <- NULL
  
  
  if (masemethod == "greg" && modelselect == T) {
    
    # want to do variable selection on plot level data...
    pltlvl <- domdatn[ , lapply(.SD, sum, na.rm = TRUE), 
                       by=c(unitvar, uniqueid, "TOTAL", prednames),
                       .SDcols=response]
    
    y <- pltlvl[[response]]
    xsample <- pltlvl[ , prednames, with = F, drop = F]
    
    # need to go means -> totals -> summed totals
    xpop <- unitlut[ , c(unitvar, prednames), with = F, drop = F]
    if (is.factor(xpop[[unitvar]])) {
      npix_temp <- npixels[ ,(unitvar) := as.factor(get(unitvar))]
    } else {
      npix_temp <- npixels
    }
    xpop_npix <- merge(xpop, npix_temp, by = unitvar, all.x = TRUE)
    # multiply unitvar level population means by corresponding npixel values to get population level totals
    xpop_npix[ ,2:ncol(xpop)] <- lapply(xpop_npix[ ,2:ncol(xpop)], function(x) xpop_npix[["npixels"]] * x)
    # sum those values
    xpop_totals <- colSums(xpop_npix[ ,2:ncol(xpop)])
    
    # format xpop for mase input
    xpop_totals <- data.frame(as.list(xpop_totals), check.names=FALSE)
    
    N <- sum(npixels[["npixels"]])
    xpop_means <- xpop_totals/N
    
    # since we want to do modelselection + get the coefficients, just use MAest.greg
    coefs_select <- MAest.greg(y = y,
                               N = N,
                               x_sample = data.frame(xsample),
                               x_pop = data.frame(xpop_means),
                               modelselect = TRUE)
    predselect.overall <- coefs_select$predselect
    prednames <- names(predselect.overall[ ,(!is.na(predselect.overall))[1,], with = F])
    message(paste0("Predictors ", "[", paste0(prednames, collapse = ", "), "]", " were chosen in model selection.\n"))
    
  }
  
  if (!masemethod %in% c("horvitzThompson", "postStrat")) {
    message("using the following predictors...", toString(prednames))
  }
  getweights <- ifelse(masemethod %in% c("greg", "postStrat", "horvitzThompson"), TRUE, FALSE) 
 
  if(bootstrap) {
    if(masemethod %in% c('greg', 'gregElasticNet', 'ratioEstimator')) {
      var_method <- "bootstrapSRS"
    }
  } else {
    if(masemethod %in% c('greg', 'gregElasticNet', 'ratioEstimator')) {
      var_method <- "LinHTSRS"
    }
  }

  if (addtotal) {
    
    domdattot <- domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                         by=c(unitvar, uniqueid, "TOTAL", prednames), .SDcols=response]
    unit_totestlst <- lapply(estunits, MAest.unit, 
                             dat = domdattot, cuniqueid = uniqueid, 
                             unitlut = unitlut, unitvar = unitvar, 
                             esttype = esttype, 
                             MAmethod = MAmethod, 
                             strvar = NULL, prednames = prednames, 
                             domain = "TOTAL", response = response, 
                             npixels = npixels, 
                             FIA = FIA, 
                             modelselect = modelselect_bydomain, 
                             getweights = getweights,
                             var_method = var_method)
    unit_totest <- do.call(rbind, sapply(unit_totestlst, '[', "unitest"))
    unit_weights <- do.call(rbind, sapply(unit_totestlst, '[', "weights")) 
    unit_weights$areaweights <- unit_weights$weights * sum(unitarea[[areavar]])
    if (MAmethod %in% c("greg", "gregEN")) {
      predselectlst$totest <- do.call(rbind, sapply(unit_totestlst, '[', "predselect"))
    }
    tabs <- check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    if (totals) {
      unit_totest <- getpse(unit_totest, areavar=areavar, esttype=esttype)
    } else {
      unit_totest <- getpse(unit_totest, esttype=esttype)
    }      
  }
  
  if (rowvar != "TOTAL") {
    
    ## Check uniquerow - add NA factor value
    uniquerow <- check.unique(x = domdatn, 
                              uniquex = uniquerow,
                              xvar = rowvar, 
                              NAname = row.NAname)
    
    domdattot <- domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                         by=c(unitvar, uniqueid, rowvar, prednames), .SDcols=response]
    
    unit_rowestlst <- lapply(estunits, MAest.unit, 
                             dat = domdattot, cuniqueid = uniqueid, 
                             unitlut = unitlut, unitvar = unitvar, 
                             esttype = esttype, 
                             MAmethod = MAmethod, 
                             strvar = NULL, prednames = prednames, 
                             domain = rowvar, response=response, 
                             npixels = npixels, 
                             FIA = FIA, 
                             modelselect = modelselect_bydomain, 
                             getweights = getweights,
                             var_method = var_method)
    unit_rowest <- do.call(rbind, sapply(unit_rowestlst, '[', "unitest"))
    if (MAmethod %in% c("greg", "gregEN")) {
      predselectlst$rowest <- do.call(rbind, sapply(unit_rowestlst, '[', "predselect"))
    }
    
    if (colvar != "NONE") {
      
      ## Check uniquercol- add NA factor value
      uniquecol <- check.unique(x = domdatn, 
                                uniquex = uniquecol,
                                xvar = colvar, 
                                NAname = col.NAname)
      
      domdattot <- domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                            by=c(unitvar, uniqueid, colvar, prednames), .SDcols=response]
      
      unit_colestlst <- lapply(estunits, MAest.unit, 
                               dat=domdattot, cuniqueid=uniqueid, 
                               unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                               MAmethod=MAmethod, strvar=NULL, prednames=prednames, 
                               domain=colvar, response=response, npixels=npixels, 
                               FIA=FIA, modelselect=modelselect_bydomain, var_method=var_method)
      unit_colest <- do.call(rbind, sapply(unit_colestlst, '[', "unitest"))
      if (MAmethod %in% c("greg", "gregEN")) {
        predselectlst$colest <- do.call(rbind, sapply(unit_colestlst, '[', "predselect"))
      }
      
      domdatn <- domdatn[!is.na(domdatn[[rowvar]]) & domdatn[[rowvar]] != "NA",] 
      domdatn <- domdatn[!is.na(domdatn[[colvar]]) & domdatn[[colvar]] != "NA",] 
      
      domdattot <- domdatn[, lapply(.SD, sum, na.rm=TRUE), 
                           by=c(unitvar, uniqueid, grpvar, prednames), .SDcols=response]
      
      domdattot[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]
      
      unit_grpestlst <- lapply(estunits, MAest.unit, 
                               dat=domdattot, cuniqueid=uniqueid, 
                               unitlut=unitlut, unitvar=unitvar, esttype=esttype, 
                               MAmethod=MAmethod, strvar=NULL, prednames=prednames, 
                               domain="grpvar", response=response, npixels=npixels, 
                               FIA=FIA, modelselect=modelselect_bydomain, var_method=var_method)
      
      unit_grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "unitest"))
      if (MAmethod %in% c("greg", "gregEN")) {
        predselectlst$grpest <- do.call(rbind, sapply(unit_grpestlst, '[', "predselect"))
      }
      if (any(unit_grpest$grpvar == "NA#NA")) {
        unit_grpest <- unit_grpest[unit_grpest$grpvar != "NA#NA", ]
      }
      unit_grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
    }
  }

  ###############################################################################
  ## Check add0 and Add area
  ###############################################################################
  if (!is.null(unit_rowest)) {
    unit_rowest <- add0unit(x=unit_rowest,
                            xvar=rowvar,
                            uniquex=uniquerow, 
                            unitvar=unitvar,
                            xvar.add0=row.add0)
    tabs <- check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2
    
    if (!is.null(row.orderby) && row.orderby != "NONE") {
      setorderv(unit_rowest, c(row.orderby))
    }
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
    
    if (totals) {
      unit_rowest <- getpse(unit_rowest, 
                            areavar=areavar,
                            esttype=esttype)
    } else {
      unit_rowest <- getpse(unit_rowest, 
                            esttype=esttype)
    }
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }
  
  if (!is.null(unit_colest)) {
    unit_colest <- add0unit(x=unit_colest,
                            xvar=colvar,
                            uniquex=uniquecol, 
                            unitvar=unitvar,
                            xvar.add0=col.add0)
    tabs <- check.matchclass(unitarea, unit_colest, unitvar)
    unitarea <- tabs$tab1
    unit_colest <- tabs$tab2
    
    if (!is.null(col.orderby) && col.orderby != "NONE") {
      setorderv(unit_colest, c(col.orderby))
    }
    setkeyv(unit_colest, unitvar)
    unit_colest <- unit_colest[unitarea, nomatch=0]
    if (totals) {
      unit_colest <- getpse(unit_colest,
                            areavar=areavar,
                            esttype=esttype)
    } else {
      unit_colest <- getpse(unit_colest,
                            esttype=esttype)
    }      
    setkeyv(unit_colest, c(unitvar, colvar))
  }
  
  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest,
                            xvar=rowvar,
                            uniquex=uniquerow, 
                            unitvar=unitvar,
                            xvar.add0=row.add0,
                            xvar2=colvar,
                            uniquex2=uniquecol,
                            xvar2.add0=col.add0)
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
      unit_grpest <- getpse(unit_grpest,
                            areavar=areavar,
                            esttype=esttype)
    } else {
      unit_grpest <- getpse(unit_grpest,
                            esttype=esttype)
    }      
    setkeyv(unit_grpest, c(unitvar, rowvar, colvar))
  }
  

  returnlst <- list(unit_totest = unit_totest,
                    unit_rowest = unit_rowest,
                    unit_colest = unit_colest,
                    unit_grpest = unit_grpest,
                    predselectlst = predselectlst,
                    predselect.overall = predselect.overall,
                    unit_weights = unit_weights)
  
  return(returnlst)
  
  
  
}