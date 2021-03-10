modMAtree <- function(MApopdat=NULL, MAmethod="greg", prednames=NULL, 
	estseed="none", landarea="ALL", pfilter=NULL, cfilter=NULL, estvar=NULL, 
	estvar.filter=NULL, rowvar=NULL, colvar=NULL, row.FIAname=FALSE, col.FIAname=FALSE, 
	row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, 
	rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, 
	sumunits=FALSE, allin1=FALSE, estround=1, pseround=2, estnull="--", psenull="--", 
	divideby=NULL, savedata=FALSE, rawdata=FALSE, rawonly=FALSE, outfolder=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, addtitle=TRUE, 
	returntitle=FALSE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.estvar=NULL, title.filter=NULL, 
	gui=FALSE, ...){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ######################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modMAtree)),
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
    tree=landarea=PSstrvar=areavar <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL
 

  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  minplotnum <- 10
  esttype="TREE"
  returnMApopdat <- TRUE
  parameters <- FALSE
  returnlst <- list()


  ## Check MAmethod 
  MAmethodlst <- c("HT", "PS", "greg", "gregEN")
  MAmethod <- FIESTA::pcheck.varchar(var2check=MAmethod, varnm="MAmethod", gui=gui, 
		checklst=MAmethodlst, caption="MAmethod", multiple=FALSE, stopifnull=TRUE)

  ## Check estseed 
  ########################################################
  estseedlst <- c("none", "only", "add")
  estseed <- FIESTA::pcheck.varchar(var2check=estseed, varnm="estseed", 
		checklst=estseedlst, caption="Seedlings", stopifnull=TRUE)

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
  treex <- MApopdat$treex
  seedx <- MApopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  cuniqueid <- MApopdat$cuniqueid
  condid <- MApopdat$condid
  tuniqueid <- MApopdat$tuniqueid
  ACI.filter <- MApopdat$ACI.filter
  unitarea <- MApopdat$unitarea
  areavar <- MApopdat$areavar
  unitvar <- MApopdat$unitvar
  unitlut <- MApopdat$unitlut
  unitvar2 <- MApopdat$unitvar2
  npixels <- MApopdat$npixels
  npixelvar <- MApopdat$npixelvar
  expcondtab <- MApopdat$expcondtab
  plotsampcnt <- MApopdat$plotsampcnt
  condsampcnt <- MApopdat$condsampcnt
  states <- MApopdat$states
  invyrs <- MApopdat$invyrs
  MAmethod <- MApopdat$MAmethod
  stratcombinelut <- MApopdat$stratcombinelut
  predfac <- MApopdat$predfac
  PSstrvar <- MApopdat$PSstrvar
  adj <- MApopdat$adj

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

  ## Check estseed 
  ########################################################
  estseedlst <- c("none", "only", "add")
  estseed <- FIESTA::pcheck.varchar(var2check=estseed, varnm="estseed", 
		checklst=estseedlst, caption="Seedlings", stopifnull=TRUE)
  if (estseed == "none") {
    seedx <- NULL
  } else {
    if (is.null(seedx)) {
      stop("no seedling data in population data")
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
 		condid=condid, treex=treex, seedx=seedx, sumunits=sumunits, 
		landarea=landarea, ACI.filter=ACI.filter, pfilter=pfilter, 
		cfilter=cfilter, allin1=allin1, estround=estround, pseround=pseround,
 		divideby=divideby, addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
		rawonly=rawonly, savedata=savedata, outfolder=outfolder, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  treef <- estdat$treef
  seedf <- estdat$seedf
  tuniqueid <- estdat$tuniqueid
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

  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf,
	condf=pltcondf, cuniqueid=cuniqueid, tuniqueid=tuniqueid, rowvar=rowvar,
 	rowvar.filter=rowvar.filter, colvar=colvar, colvar.filter=colvar.filter,
 	row.FIAname=row.FIAname, col.FIAname=col.FIAname, row.orderby=row.orderby, 
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, rowlut=rowlut, 
	collut=collut, rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	landarea=landarea) 
  treef <- rowcolinfo$treef
  seedf <- rowcolinfo$seedf
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
  bytdom <- rowcolinfo$bytdom
  tdomvar <- rowcolinfo$tdomvar
  tdomvar2 <- rowcolinfo$tdomvar2
  grpvar <- rowcolinfo$grpvar
  #rm(rowcolinfo)  
  
  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }

  
  #####################################################################################
  ### GET ESTIMATION DATA FROM TREE TABLE
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- check.tree(gui=gui, treef=treef, seedf=seedf, estseed=estseed, 
	bycond=TRUE, condf=condf, bytdom=bytdom, tuniqueid=tuniqueid, 
	cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
	estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, 
	tdomvar2=tdomvar2, adjtree=adjtree)
  if (is.null(treedat)) return(NULL)
  tdomdat <- merge(condx, treedat$tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst

  ## remove NA values
  #if (!is.null(tdomvar) && !is.null(tdomvar2))
  #  tdomdat <- tdomdat[!is.na(tdomdat[[rowvar]]) & !is.na(tdomdat[[colvar]]),]


  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
	sumunits=sumunits, title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.unitvar,
	title.filter=title.filter, title.estvarn=title.estvar, unitvar=unitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, estvarn.filter=estvar.filter, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, states=states, 
	invyrs=invyrs, landarea=landarea, pfilter=pfilter, cfilter=cfilter, 
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
  unit.totest=unit.tdomest=unit.domest=unit.rowest=unit.colest=unit.grpest=
	rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlst) && length(tdomvarlst) > 1)), TRUE, FALSE)
  response <- estvar.name
  estunits <- sort(unique(tdomdat[[unitvar]]))

  message("getting estimates...")
  if (MAmethod %in% c("greg", "gregEN")) {
    message("using the following predictors...", toString(prednames))
  }

#  if (addtotal) {
    ## Get total estimate and merge area
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, "TOTAL", PSstrvar, prednames), .SDcols=response]
    unit.totest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdattot, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
		domain="TOTAL", response=response, npixels=npixels, FIA=TRUE))
    tabs <- FIESTA::check.matchclass(unitarea, unit.totest, unitvar)
    unitarea <- tabs$tab1
    unit.totest <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- getarea(unit.totest, areavar=areavar, esttype=esttype)
#  }
 
  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, rowvar, PSstrvar, prednames), .SDcols=response]
    unit.rowest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar,
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
		domain=rowvar, response=response, npixels=npixels, FIA=TRUE))
    #unit.rowest <- unit.rowest[!is.na(unit.rowest[[rowvar]]), ]

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, colvar, PSstrvar, prednames), .SDcols=response]
      unit.colest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
		domain=colvar, response=response, npixels=npixels, FIA=TRUE))
      #unit.colest <- unit.colest[!is.na(unit.colest[[colvar]]), ]

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, grpvar, PSstrvar, prednames), .SDcols=response]
      tdomdatsum[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]

      unit.grpest <- do.call(rbind, lapply(estunits, MAest.unit,
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
		domain="grpvar", response=response, npixels=npixels, FIA=TRUE))
      unit.grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
    }
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
    unit.grpest <- add0unit(x=unit.grpest, xvar=rowvar, uniquex=uniquerow, 
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
  result <- list()
  if (rawdata) {
    rawdat <- list()
    rawdat$domdat <- setDF(tdomdat)
    rawdat$estvar <- estvar.name
    rawdat$estvar.filter <- estvar.filter
  }

  estnm <- "est"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvar2=unitvar2, unit.totest=unit.totest, unit.rowest=unit.rowest, 
	unit.colest=unit.colest, unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, rawdat=rawdat, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 
 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse
  rawdat <- tabs$rawdat
  titlelst <- tabs$titlelst

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
        if (!tabnm %in% c(estvar, prednames)) {
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

  returnlst$est <- est2return 
  if (!is.null(pse2return)) returnlst$pse <- pse2return 
  if (rawdata) {
    rawdat$esttype <- "TREE"
    rawdat$MAmethod <- MAmethod
    rawdat$estvar <- estvar
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    returnlst$raw <- rawdat
  }
  if (returntitle) returnlst$titlelst <- titlelst
  if (returnMApopdat) returnlst$MApopdat <- MApopdat
    
  return(returnlst)
}

