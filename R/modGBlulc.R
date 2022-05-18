modGBlulc <- function(GBpopdat, 
                      landarea = "ALL", 
                      pcfilter = NULL, 
                      rowvar = NULL, 
                      colvar = NULL, 
                      gainloss = FALSE,
                      gainloss.vals = NULL, 
                      returntitle = FALSE, 
                      savedata = FALSE,
                      table_opts = NULL, 
                      title_opts = NULL,
                      savedata_opts = NULL, 
                      gui = FALSE, 
                      ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(GBpopdat)) {
    gui <- TRUE
  } 
  
  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
      tuniqueid=savedata=addtitle=returntitle=rawdata=rawonly=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL 
    #if (!col.FIAname) col.FIAname <- NULL 
  }
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL
  
  
 ## Set parameters
  esttype <- "LULC"
  parameters <- FALSE
  returnlst <- list()
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(modGBlulc)),
		names(formals(modGBpop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts, 
                table_opts=table_opts, title_opts=title_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(FIESTA::savedata_options)[-length(formals(FIESTA::savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ## Set table defaults
  table_defaults_list <- formals(FIESTA::table_options)[-length(formals(FIESTA::table_options))]
  
  for (i in 1:length(table_defaults_list)) {
    assign(names(table_defaults_list)[[i]], table_defaults_list[[i]])
  }
  
  ## Set user-supplied table values
  if (length(table_opts) > 0) {
    for (i in 1:length(table_opts)) {
      if (names(table_opts)[[i]] %in% names(table_defaults_list)) {
        assign(names(table_opts)[[i]], table_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(table_opts)[[i]]))
      }
    }
  }
  
  ## Set title defaults
  title_defaults_list <- formals(FIESTA::title_options)[-length(formals(FIESTA::title_options))]
  
  for (i in 1:length(title_defaults_list)) {
    assign(names(title_defaults_list)[[i]], title_defaults_list[[i]])
  }
  
  ## Set user-supplied title values
  if (length(title_opts) > 0) {
    for (i in 1:length(title_opts)) {
      if (names(title_opts)[[i]] %in% names(title_defaults_list)) {
        assign(names(title_opts)[[i]], title_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(title_opts)[[i]]))
      }
    }
  }
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check gainloss 
  gainloss <- pcheck.logical(gainloss, varnm="gainloss", 
		title="Gain-loss estimates?", first="NO", gui=gui) 
  if (gainloss) {
    rawdata <- TRUE
    if (is.null(colvar)) {
      stop("must include rowvar and colvar for gainloss estimates")
    }
  }
 
  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  
  list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
	                "unitarea", "unitvar", "stratalut", "strvar",
                  "plotsampcnt", "condsampcnt")
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  
  if (is.null(GBpopdat)) return(NULL)
  condx <- GBpopdat$condx
  pltcondx <- GBpopdat$pltcondx
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  unitvar <- GBpopdat$unitvar
  unitvars <- GBpopdat$unitvars
  stratalut <- GBpopdat$stratalut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  estvar.name <- GBpopdat$estvar.area
  stratcombinelut <- GBpopdat$stratcombinelut
  getwtvar <- GBpopdat$getwtvar
  adj <- GBpopdat$adj
  strunitvars <- c(unitvar, strvar)
 
  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, 
                  cuniqueid=cuniqueid, condid=condid, 
                  sumunits=sumunits, landarea=landarea, pcfilter=pcfilter, 
                  allin1=allin1, estround=estround, pseround=pseround, 
                  divideby=divideby, addtitle=addtitle, returntitle=returntitle, 
                  rawdata=rawdata, rawonly=rawonly, savedata=savedata, 
                  outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
                  overwrite_layer=overwrite_layer, outfn.pre=outfn.pre,
                  outfn.date=outfn.date, append_layer=append_layer, 
                  raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  sumunits <- estdat$sumunits
  landarea <- estdat$landarea
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
  rawonly <- estdat$rawonly
  savedata <- estdat$savedata
  outfolder <- estdat$outfolder
  overwrite_layer <- estdat$overwrite_layer
  raw_fmt <- estdat$raw_fmt
  raw_dsn <- estdat$raw_dsn
  rawfolder <- estdat$rawfolder

  if ("STATECD" %in% names(pltcondf)) {
    states <- pcheck.states(sort(unique(pltcondf$STATECD)))
  }
  if ("INVYR" %in% names(pltcondf)) {
    invyr <- sort(unique(pltcondf$INVYR))
  }    

  ###################################################################################
  ### Check row and column data
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, 
                    condf=pltcondf, cuniqueid=cuniqueid, 
                    rowvar=rowvar, rowvar.filter=rowvar.filter, 
                    colvar=colvar, colvar.filter=colvar.filter, 
                    row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
                    row.orderby=row.orderby, col.orderby=col.orderby, 
                    row.add0=row.add0, col.add0=col.add0, 
                    title.rowvar=title.rowvar, title.colvar=title.colvar, 
                    rowlut=rowlut, collut=collut, 
                    rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
                    landarea=landarea)
  condf <- rowcolinfo$condf
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
  rowvar <- rowcolinfo$rowvar
  colvar <- rowcolinfo$colvar
  domain <- rowcolinfo$grpvar
  row.orderby <- rowcolinfo$row.orderby
  col.orderby <- rowcolinfo$col.orderby
  row.add0 <- rowcolinfo$row.add0
  col.add0 <- rowcolinfo$col.add0
  title.rowvar <- rowcolinfo$title.rowvar
  title.colvar <- rowcolinfo$title.colvar
  rowgrpnm <- rowcolinfo$rowgrpnm
  title.rowgrp <- rowcolinfo$title.rowgrp
  grpvar <- rowcolinfo$grpvar
  rm(rowcolinfo)

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }

  if (landarea == "CHANGE") {
    condf <- condf[condf[[rowvar]] != condf[[colvar]], ]
  }

  ## Merge filtered condition data (condf) to all conditions (condx)
  ###################################################################################
  setkeyv(condx, c(cuniqueid, condid))
  setkeyv(condf, c(cuniqueid, condid))
  cdomdat <- condx[condf]

  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=cdomdat, esttype=esttype, sumunits=sumunits,
                    title.main=title.main, title.ref=title.ref, 
                    title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, 
                    title.colvar=title.colvar, title.unitvar=title.unitvar,
	                  title.filter=title.filter, unitvar=unitvar, 
                    rowvar=rowvar, colvar=colvar, 
	                  addtitle=addtitle, rawdata=rawdata, 
                    states=states, invyrs=invyrs, landarea=landarea, 
                    pcfilter=pcfilter, allin1=allin1, divideby=divideby, 
                    outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) outfn.rawdat <- alltitlelst$outfn.rawdat
 

  ############################################################################
  ## GENERATE ESTIMATES
  ############################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=tdomdattot <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
  #estvar.name <- estvar 

  message("getting estimates...")
#  if (addtotal) {
    ## Get total estimate and merge area
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    unit_totest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdattot, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain="TOTAL")
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
#  }

  ## Get row estimate  
  if (rowvar != "TOTAL") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    unit_rowest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		                    uniqueid=cuniqueid, stratalut=stratalut, 
		                    unitvar=unitvar, strvar=strvar, domain=rowvar)
  }

  ## Get column (and cell) estimate  
  if (colvar != "NONE") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
    unit_colest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
                        uniqueid=cuniqueid, stratalut=stratalut, 
                        unitvar=unitvar, strvar=strvar, domain=colvar)

    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
    unit_grpest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
                        uniqueid=cuniqueid, stratalut=stratalut, 
                        unitvar=unitvar, strvar=strvar, domain=grpvar)
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- add0unit(x=unit_rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)
    tabs <- check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
    
    if (totals) {
      unit_rowest <- getpse(unit_rowest, areavar=areavar, esttype=esttype)
    } else {
      unit_rowest <- getpse(unit_rowest, esttype=esttype)
    }      
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit_colest)) {
    unit_colest <- add0unit(x=unit_colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)
    tabs <- check.matchclass(unitarea, unit_colest, unitvar)
    unitarea <- tabs$tab1
    unit_colest <- tabs$tab2
    setkeyv(unit_colest, unitvar)
    unit_colest <- unit_colest[unitarea, nomatch=0]
    
    if (totals) {
      unit_colest <- getpse(unit_colest, areavar=areavar, esttype=esttype)
    } else {
      unit_colest <- getpse(unit_colest, esttype=esttype)
    }      
    setkeyv(unit_colest, c(unitvar, colvar))
  }
  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)
    tabs <- check.matchclass(unitarea, unit_grpest, unitvar)
    unitarea <- tabs$tab1
    unit_grpest <- tabs$tab2
    setkeyv(unit_grpest, unitvar)
    unit_grpest <- unit_grpest[unitarea, nomatch=0]
    
    if (totals) {
      unit_grpest <- getpse(unit_grpest, areavar=areavar, esttype=esttype)
    } else {
      unit_grpest <- getpse(unit_grpest, esttype=esttype)
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
    if (is.null(getwtvar) || !getwtvar %in% names(stratalut2)) {
      getwtvar <- "strwt"
    }
    stratalut2 <- stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(getwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(getwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvars2)

    unitacres2 <- data.table(unitarea, ONEUNIT=1)
    unitacres2 <- unitacres2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitacres2, "ONEUNIT")

    cdomdat[, ONEUNIT := 1]

    ## CALCULATE UNIT TOTALS FOR ROWVAR
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, cuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
                    uniqueid=cuniqueid, stratalut=stratalut2, 
                    unitvar="ONEUNIT", strvar=strvar, domain=rowvar)
    rowunit <- add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
    tabs <- check.matchclass(unitacres2, rowunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    if (totals) {
      rowunit <- getpse(rowunit, areavar=areavar, esttype=esttype)
    } else {
      rowunit <- getpse(rowunit, esttype=esttype)
    }      
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
                    uniqueid=cuniqueid, stratalut=stratalut2, 
                    unitvar="ONEUNIT", strvar=strvar, domain="TOTAL")
    tabs <- check.matchclass(unitacres2, totunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitacres2, nomatch=0]
    if (totals) {
      totunit <- getpse(totunit, areavar=areavar, esttype=esttype)
    } else {
      totunit <- getpse(totunit, esttype=esttype)
    }
  }          
 
  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est" 
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	        unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest,
	        unit_rowest=unit_rowest, unit_colest=unit_colest, unit_grpest=unit_grpest,
 	        rowvar=rowvar, colvar=colvar, uniquerow=uniquerow, uniquecol=uniquecol, 
	        rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
	        allin1=allin1, savedata=savedata, addtitle=addtitle, 
	        title.ref=title.ref, title.colvar=title.colvar, title.rowvar=title.rowvar, 
	        title.rowgrp=title.rowgrp, title.unitvar=title.unitvar, 
	        title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	        rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, 
	        outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite_layer, 
	        estnm=estnm, estround=estround, pseround=pseround, divideby=divideby, 
	        returntitle=returntitle, estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (rawdata) {
    rawdat <- tabs$rawdat
  }

  ## GAIN/LOSS
  if (gainloss) {
    cdomdat <- setDT(cdomdat)

    if (is.null(rowvar) || is.null(colvar)) {
      stop("must have rowvar and colvar to calculate gain/loss") 
    }
    ## Check
    rowcolvals <- unique(c(cdomdat[[rowvar]], cdomdat[[colvar]]))

    if (is.null(gainloss.vals)) {
      gainloss.vals <- rowcolvals
    } else {
      if (any(!gainloss.vals %in% rowcolvals)) {
       valsnotin <- gainloss.vals[which(!gainloss.vals %in% rowcolvals)]
       stop(paste("invalid gainloss.vals.. ", paste(valsnotin, collapse=", "), 
		"not in data"))
      }
    }
 
    numvars <- c("gain.est", "gain.se", "loss.est", "loss.se", "diff.est", "diff.se")
    charvars <- c(unitvar, "gain.val", "loss.val")

    tabtype <- "AREA"
    if (length(rowcolvals) == 2) {
      test <- lapply(gainloss.vals, getgainloss, 
		cdomdat, cuniqueid, rowvar, colvar, stratalut, unitvar, strvar,
		tabtype, areavar, unitarea, sumunits, value.var=estvar.name)

      est.gainloss <- data.table(t(sapply(gainloss.vals, getgainloss, 
		cdomdat, cuniqueid, rowvar, colvar, stratalut, unitvar, strvar,
		tabtype, areavar, unitarea, sumunits, value.var=estvar.name)))
    } else {
      est.gainloss <- data.table(t(sapply(gainloss.vals, getgainloss, 
		cdomdat, cuniqueid, rowvar, colvar, stratalut, unitvar, strvar,
		tabtype, areavar, unitarea, sumunits, value.var=estvar.name)))
    }
    est.gainloss[, (numvars) := lapply(.SD, as.numeric), .SDcols=numvars]
    est.gainloss[, (charvars) := lapply(.SD, as.character), .SDcols=charvars]

    ## Add 95 and 68% confidence intervals for gain.est, loss.est, diff.est
    CInames <- c("CI95left", "CI95right", "CI68left", "CI68right")
####################
    est.gainloss <- addCI(est.gainloss, estnm="gain.est", 
		senm="gain.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("gain.", CInames))

    est.gainloss <- addCI(est.gainloss, estnm="loss.est", 
		senm="loss.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("loss.", CInames))

    est.gainloss <- addCI(est.gainloss, estnm="diff.est", 
		senm="diff.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("diff.", CInames))

    if (savedata) {
      datExportData(est.gainloss,                   
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="gainloss", 
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    }
  }

  if (returntitle) {
    titlelst <- tabs$titlelst
  }

  if (rawdata) {
    if (length(unitvars) > length(unitvar)) {
      cdomdat[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
      cdomdat[, (unitvar) := NULL]
      setcolorder(cdomdat, c(unitvars, 
		names(cdomdat)[!names(cdomdat) %in% unitvars])) 
    }
    rawdat$domdat <- setDF(cdomdat)
    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- paste(title.est, title.ref, sep="; ")
      }
      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm) 
        if (tabnm %in% c("plotsampcnt", "condsampcnt", "stratcombinelut")) {
          write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
                    outfn.date=outfn.date, overwrite=overwrite_layer)
        } else if (is.data.frame(rawtab)) {
          if (raw_fmt != "csv") {
            out_layer <- tabnm 
          } else {
            out_layer <- outfn.rawtab
          }
          datExportData(rawtab,                   
              savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=out_layer, 
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE))
        }
      }
    }
  }  

  ## GET VALUES TO RETURN
  if (!is.null(est2return)) {
    returnlst$est <- setDF(est2return)
  }
  if (!is.null(pse2return)) {
    returnlst$pse <- setDF(pse2return)
  }
  if (gainloss) {
    returnlst$est.gainloss <- est.gainloss
  }
  if (rawdata) {
    rawdat$esttype <- "LULC"
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    if (gainloss) {
      rawdat$est.gainloss <- est.gainloss
    }
    returnlst$raw <- rawdat
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }

  return(returnlst)
}
