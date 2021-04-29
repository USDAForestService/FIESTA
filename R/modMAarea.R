modMAarea <- function(MAmethod="greg", prednames=NULL, 
	landarea="ALL", pfilter=NULL, cfilter=NULL, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, 
	rowgrpnm=NULL, rowgrpord=NULL, sumunits=FALSE, allin1=FALSE, 
	estround=1, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, rawdata=FALSE, rawonly=FALSE, outfolder=NULL, 
	outfn.pre=NULL, outfn.date=TRUE, overwrite=FALSE, addtitle=TRUE, 
	returntitle=FALSE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.filter=NULL, 
	MApopdat=NULL, gui=FALSE, ...){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ###################################################################################### 

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modMAarea)),
		names(formals(FIESTA::modMApop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(MApopdat)) {
    gui <- TRUE
  } 
 
  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=PSstrvar=areavar <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL


  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  minplotnum <- 10
  title.rowgrp=NULL
  esttype="AREA"
  returnMApopdat <- TRUE
  parameters <- FALSE
  returnlst <- list()


  ## Check MAmethod 
  MAmethodlst <- c("HT", "PS", "greg", "gregEN")
  MAmethod <- FIESTA::pcheck.varchar(var2check=MAmethod, varnm="MAmethod", gui=gui, 
		checklst=MAmethodlst, caption="MAmethod", multiple=FALSE, stopifnull=TRUE)

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check outfolder 
  ########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    if (rawdata && !file.exists(paste(outfolder, "rawdata", sep="/"))) 
      dir.create(paste(outfolder, "rawdata", sep="/"))
  }

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(MApopdat)) {
    MApopdat <- modMApop(gui=gui, MAmethod=MAmethod, prednames=prednames, ...)
  } else {
    returnMApopdat <- FALSE
    list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "unitarea", "unitvar", "unitlut", "npixels",
		"npixelvar", "expcondtab", "plotsampcnt", "condsampcnt", "MAmethod")
    if (MAmethod == "PS") {
      list.items <- c(list.items, "PSstrvar")
    }
    if (MAmethod == "greg") {
      list.items <- c(list.items, "prednames")
    }
    MApopdat <- FIESTA::pcheck.object(MApopdat, "MApopdat", list.items=list.items)
  }
		
  if (is.null(MApopdat)) return(NULL)
  condx <- MApopdat$condx
  pltcondx <- MApopdat$pltcondx
  cuniqueid <- MApopdat$cuniqueid
  condid <- MApopdat$condid
  ACI.filter <- MApopdat$ACI.filter
  unitarea <- MApopdat$unitarea
  areavar <- MApopdat$areavar
  unitvar <- MApopdat$unitvar
  unitvars <- MApopdat$unitvars
  unitcombine <- MApopdat$unitcombine
  unitlut <- MApopdat$unitlut
  npixels <- MApopdat$npixels
  npixelvar <- MApopdat$npixelvar
  expcondtab <- MApopdat$expcondtab
  plotsampcnt <- MApopdat$plotsampcnt
  condsampcnt <- MApopdat$condsampcnt
  states <- MApopdat$states
  invyrs <- MApopdat$invyrs
  MAmethod <- MApopdat$MAmethod
  estvar.name <- MApopdat$estvar.area
  stratcombinelut <- MApopdat$stratcombinelut
  predfac <- MApopdat$predfac
  PSstrvar <- MApopdat$PSstrvar
 
  if (MAmethod %in% c("greg", "gregEN")) {
    if (is.null(prednames)) {
      prednames <- MApopdat$prednames
    } else {
      if (!all(prednames %in% MApopdat$prednames))
        stop("invalid prednames: ", 
			toString(prednames[!prednames %in% MApopdat$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  } 

  ## Convert predfac if MAmethod="greg"
  if ("greg" %in% MAmethod && !is.null(predfac)) {
    for (fac in predfac) {
      ## Get factor levels
      fac.levels <- sort(unique(condx[[fac]]))

      ## Set factor levels to keep and delete from unitlut.
      fac.unitcol.keep <- paste(fac, fac.levels[-1], sep=".")
      fac.unitcol.del <- paste(fac, fac.levels[1], sep=".")
      unitlut[[fac.unitcol.del]] <- NULL
  
      ## Rename factor variables and add names to predictor list
      facs <- paste0(fac, fac.levels[-1])
      names(unitlut)[names(unitlut) %in% fac.unitcol.keep] <- facs
      unitpreds <- c(prednames[prednames != fac], facs)

      ## Create dummy variables for factor levels - 1
      dtfac <- condx[, as.data.table(model.matrix(~., 
				data=condx[, fac, with=FALSE]))][,-1]
      condx <- cbind(condx, dtfac)
      condx[, (fac) := NULL]

      ## Remove old name and add new names to predictor list
      prednames <- unique(c(prednames[prednames != fac], facs))
    }
  }

  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 		condid=condid, sumunits=sumunits, landarea=landarea,
 		ACI.filter=ACI.filter, pfilter=pfilter, cfilter=cfilter, 
		allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 		addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
		rawonly=rawonly, savedata=savedata, outfolder=outfolder)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  sumunits <- estdat$sumunits
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
  estround <- estdat$estround
  pseround <- estdat$pseround
  landarea <- estdat$landarea
  if (sumunits && nrow(unitarea) == 1) sumunits <- FALSE 

  if ("STATECD" %in% names(pltcondf)) {
    states <- pcheck.states(sort(unique(pltcondf$STATECD)))
  }
  if ("INVYR" %in% names(pltcondf)) {
    invyr <- sort(unique(pltcondf$INVYR))
  }


  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, condf=pltcondf, 
	cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar,
 	colvar.filter=colvar.filter, row.FIAname=row.FIAname, col.FIAname=col.FIAname,
 	row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0, 
	col.add0=col.add0, title.rowvar=title.rowvar, title.colvar=title.colvar,
	rowlut=rowlut, collut=collut, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowgrpord=rowgrpord, landarea=landarea)
  condf <- rowcolinfo$condf
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
  rowvar <- rowcolinfo$rowvar
  colvar <- rowcolinfo$colvar
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
  

  ## Merge filtered condition data (condf) to all conditions (condx)
  #####################################################################################
  setkeyv(condx, c(cuniqueid, condid))
  setkeyv(condf, c(cuniqueid, condid))
  cdomdat <- merge(condx, condf, by=key(condx), all.x=TRUE)


  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=cdomdat, esttype=esttype, sumunits=sumunits, 
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.unitvar,
	title.filter=title.filter, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, invyrs=invyrs, 
	landarea=landarea, pfilter=pfilter, cfilter=cfilter, 
	allin1=allin1, divideby=divideby, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
    outfn.rawdat <- paste0(outfn.rawdat, "_modMA_mase", "_", MAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modMA_mase", "_", MAmethod) 
  
  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit.totest=unit.rowest=unit.colest=unit.grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
  estunits <- sort(unique(cdomdat[[unitvar]]))

  message("getting estimates...")
  if (MAmethod %in% c("greg", "gregEN")) {
    message("using the following predictors...", toString(prednames))
  }
  if (addtotal) {
    ## Get total estimate and merge area
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, "TOTAL", PSstrvar, prednames), .SDcols=estvar.name]
    unit.totest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=cdomdattot, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames,
 		domain="TOTAL", response=estvar.name, npixels=npixels, FIA=TRUE))
    tabs <- FIESTA::check.matchclass(unitarea, unit.totest, unitvar)
    unitarea <- tabs$tab1
    unit.totest <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- getarea(unit.totest, areavar=areavar, esttype=esttype)
  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, rowvar, PSstrvar, prednames), .SDcols=estvar.name]
    unit.rowest <- do.call(rbind, lapply(estunits, MAest.unit, 
	dat=cdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
	esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
	domain=rowvar, response=estvar.name, npixels=npixels, FIA=TRUE))
  }
  if (colvar != "NONE") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, colvar, PSstrvar, prednames), .SDcols=estvar.name]
    unit.colest <- do.call(rbind, lapply(estunits, MAest.unit, 
	dat=cdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
	esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
	domain=colvar, response=estvar.name, npixels=npixels, FIA=TRUE))

    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, grpvar, PSstrvar, prednames), .SDcols=estvar.name]
    cdomdatsum[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]

    unit.grpest <- do.call(rbind, lapply(estunits, MAest.unit, 
	dat=cdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
	esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
	domain="grpvar", response=estvar.name, npixels=npixels, FIA=TRUE))
    unit.grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit.rowest)) {
    unit.rowest <- FIESTA::add0unit(x=unit.rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit.rowest, unitvar)
    unitarea <- tabs$tab1
    unit.rowest <- tabs$tab2
    setkeyv(unit.rowest, unitvar)
    unit.rowest <- unit.rowest[unitarea, nomatch=0]
    unit.rowest <- FIESTA::getarea(unit.rowest, areavar=areavar, esttype=esttype)
    setkeyv(unit.rowest, c(unitvar, rowvar))
  }
  if (!is.null(unit.colest)) {
    unit.colest <- FIESTA::add0unit(x=unit.colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit.colest, unitvar)
    unitarea <- tabs$tab1
    unit.colest <- tabs$tab2
    setkeyv(unit.colest, unitvar)
    unit.colest <- unit.colest[unitarea, nomatch=0]
    unit.colest <- FIESTA::getarea(unit.colest, areavar=areavar, esttype=esttype)
    setkeyv(unit.colest, c(unitvar, colvar))
  }
  if (!is.null(unit.grpest)) {
    unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit.grpest, unitvar)
    unitarea <- tabs$tab1
    unit.grpest <- tabs$tab2
    setkeyv(unit.grpest, unitvar)
    unit.grpest <- unit.grpest[unitarea, nomatch=0]
    unit.grpest <- FIESTA::getarea(unit.grpest, areavar=areavar, esttype=esttype)
    setkeyv(unit.grpest, c(unitvar, rowvar, colvar))
  }


  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit.totest=unit.totest, unit.rowest=unit.rowest, 
	unit.colest=unit.colest, unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (rawdata) {
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(cdomdat)
  }
  if (returntitle) {
    titlelst <- tabs$titlelst
  }

  if (savedata) {
    if (rawdata) {
      rawfolder <- paste(outfolder, "rawdata", sep="/")
      if (!file.exists(rawfolder)) dir.create(rawfolder)

      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
      }

      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        if (!tabnm %in% c(prednames)) {
          rawtab <- rawdat[[i]]
          outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm, ".csv") 
          if (tabnm %in% c("plotsampcnt", "condsampcnt", "unitlut", "unitarea")) {
            write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite)
          } else  { 
            suppressWarnings(save1tab(tab=rawtab, tab.title=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
          }
        }
      }
    }
  }
  
  if (!is.null(est2return)) {
    returnlst$est <- est2return
  } 
  if (!is.null(pse2return)) {
    returnlst$pse <- pse2return 
  }
  if (rawdata) {
    rawdat$esttype <- "AREA"
    rawdat$MAmethod <- MAmethod
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    returnlst$raw <- rawdat
  }
  if (returntitle) {
    returnlst$titlelst <- titlelst
  }
  if (returnMApopdat) {
    returnlst$MApopdat <- MApopdat
  }
    
  return(returnlst)
}

