modMAtree <- function(tree=NULL, cond=NULL, plt=NULL, pltassgn=NULL, seed=NULL, dsn=NULL, 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", puniqueid="CN", 
	pltassgnid="CN", pjoinid="CN", evalid=NULL, invyrs=NULL, ACI=FALSE, adj="samp",
 	MAmethod="GREG", plt.nonsamp.filter=NULL, cond.nonsamp.filter=NULL, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", unitcombine=FALSE,
 	minplotnum.unit=10, unitlut=NULL, npixelvar="npixels", prednames=NULL, predfac=NULL, 
 	PSstrvar=NULL, stratcombine=TRUE, landarea="ALL", plt.filter=NULL, cond.filter=NULL, 
	estvar=NULL, estvar.filter=NULL, estvar.name=NULL, rowvar=NULL, colvar=NULL, 
 	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, 
	rowgrpnm=NULL, rowgrpord=NULL, sumunits=FALSE, allin1=FALSE, estround=1, 
	pseround=2, estnull="--", psenull="--", divideby=NULL, savedata=FALSE, rawdata=FALSE, 
	outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, 
	addtitle=TRUE, returntitle=FALSE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.estvar=NULL, title.filter=NULL, 
	MApopdat=NULL, MAdata=NULL, gui=FALSE){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ######################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 || is.null(tree) && is.null(MApopdat) && is.null(MAdata)) gui <- TRUE 

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL
 

  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
 
  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  minplotnum <- 10
  esttype="TREE"
  returnMApopdat <- TRUE

  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(MApopdat)) {
    MApopdat <- modMApop(tree=tree, cond=cond, plt=plt, dsn=dsn, pltassgn=pltassgn, 
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, puniqueid=puniqueid,  
	pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid, invyrs=invyrs, 
	ACI=ACI, adj=adj, plt.nonsamp.filter=plt.nonsamp.filter,
 	cond.nonsamp.filter=cond.nonsamp.filter, MAmethod=MAmethod, unitvar=unitvar, 
	unitvar2=unitvar2, unitarea=unitarea, areavar=areavar, unitcombine=unitcombine, 
	minplotnum.unit=minplotnum.unit, unitlut=unitlut, npixelvar=npixelvar, 
	prednames=prednames, predfac=predfac, PSstrvar=PSstrvar, 
	stratcombine=stratcombine, MAdata=MAdata, gui=gui)
  } else {
    returnMApopdat <- FALSE
    if (!is.list(MApopdat))
      stop("MApopdat must be a list")
    listitems <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "unitarea", "unitvar", "unitlut", "npixels",
		"npixelvar", "PSstrvar", "prednames", "expcondtab", "plotsampcnt",
		"condsampcnt", "MAmethod")
    if (!all(listitems %in% names(MApopdat))) {
      items.miss <- listitems[!listitems %in% names(MApopdat)]
      stop("invalid MApopdat... missing items: ", paste(items.miss, collapse=", "))
    }   
  }	

  if (is.null(MApopdat)) return(NULL)	
  condx <- MApopdat$condx
  pltcondx <- MApopdat$pltcondx
  treex <- MApopdat$treex
  cuniqueid <- MApopdat$cuniqueid
  condid <- MApopdat$condid
  tuniqueid <- MApopdat$tuniqueid
  ACI.filter <- MApopdat$ACI.filter
  unitarea <- MApopdat$unitarea
  areavar <- MApopdat$areavar
  unitvar <- MApopdat$unitvar
  unitlut <- MApopdat$unitlut
  npixels <- MApopdat$npixels
  npixelvar <- MApopdat$npixelvar
  expcondtab <- MApopdat$expcondtab
  plotsampcnt <- MApopdat$plotsampcnt
  condsampcnt <- MApopdat$condsampcnt
  states <- MApopdat$states
  invyrs <- MApopdat$invyrs
  MAmethod <- MApopdat$MAmethod
  stratcombinelut <- MApopdat$stratcombinelut

  if (is.null(prednames))
    prednames <- MApopdat$prednames
  if (is.null(predfac)) 
    predfac <- MApopdat$predfac
  if (is.null(predfac)) 
    PSstrvar <- MApopdat$PSstrvar


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 		condid=condid, treex=treex, sumunits=sumunits, landarea=landarea,
 		ACI.filter=ACI.filter, plt.filter=plt.filter, cond.filter=cond.filter, 
		allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 		addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
		savedata=savedata, outfolder=outfolder, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  treef <- estdat$treef
  tuniqueid <- estdat$tuniqueid
  sumunits <- estdat$sumunits
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
  savedata <- estdat$savedata
  outfolder <- estdat$outfolder
  estround <- estdat$estround
  pseround <- estdat$pseround
  landarea <- estdat$landarea
  if (sumunits && nrow(unitarea) == 1) sumunits <- FALSE 


  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, 
	condf=pltcondf, cuniqueid=cuniqueid, tuniqueid=tuniqueid, rowvar=rowvar,
 	rowvar.filter=rowvar.filter, colvar=colvar, colvar.filter=colvar.filter,
 	row.FIAname=row.FIAname, col.FIAname=col.FIAname, row.orderby=row.orderby, 
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, rowlut=rowlut, 
	collut=collut, rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, 
	landarea=landarea) 
  treef <- rowcolinfo$treef
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
  concat <- rowcolinfo$concat
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
  treedat <- check.tree(gui=gui, treef=treef, bycond=TRUE, condf=condf, bytdom=bytdom, 
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
	estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, adjtree=adjtree)
  if (is.null(treedat)) return(NULL)
  tdomdat <- merge(condx, treedat$tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst

  ## add separate columns
  if (concat) {
    if (is.null(grpvar)) {
      tdomdatkey <- key(tdomdat)
      setkeyv(tdomdat, c(rowvar, colvar))   
      grpvar <- paste(rowvar, colvar, sep="#")
      tdomdat[, (grpvar) := paste(tdomdat[[rowvar]], tdomdat[[colvar]], sep="#")]
      setkeyv(tdomdat, tdomdatkey)
    } else if (!is.null(tdomvar) && grpvar == tdomvar) { 
      tdomdat[,(rowvar) := sapply(get(grpvar), 
			function(x){strsplit(as.character(x), "#")[[1]][1]})]
      tdomdat[,(colvar) := sapply(get(grpvar), 
			function(x){strsplit(as.character(x), "#")[[1]][2]})]
    }
  } 

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=tdomdat, esttype=esttype, sumunits=sumunits, 
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.unitvar,
	title.filter=title.filter, title.estvarn=title.estvar, unitvar=unitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, estvarn.filter=estvar.filter, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, states=states, 
	invyrs=invyrs, landarea=landarea, plt.filter=plt.filter, cond.filter=cond.filter, 
	allin1=allin1, divideby=divideby, outfn=outfn, outfn.pre=outfn.pre)
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


#  if (addtotal) {
    ## Get total estimate and merge area
    tdomdat$TOTAL <- 1

    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, "TOTAL", PSstrvar, prednames), .SDcols=estvar.name]
#print(table(tdomdattot$fnf))
#print(dim(tdomdattot))
#print(head(tdomdattot))
#print(sum(tdomdattot[[estvar.name]]))
#print(unitlut)

#test <- tdomdattot[, .N, by=c("COUNTYFIPS", "fnf")]
#setorder(test, "COUNTYFIPS", "fnf")
#test

#unit=1
#dat=tdomdattot
#domain="TOTAL"
#FIA=TRUE

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
		domain=rowvar, response=response, npixels=npixels))
    unit.rowest <- unit.rowest[!is.na(unit.rowest[[rowvar]]), ]

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, colvar, PSstrvar, prednames), .SDcols=response]
      unit.colest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
		domain=colvar, response=response, npixels=npixels))
      unit.colest <- unit.colest[!is.na(unit.colest[[colvar]]), ]

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, grpvar, PSstrvar, prednames), .SDcols=response]
      unit.grpest <- do.call(rbind, lapply(estunits, MAest.unit,
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, 
		domain=grpvar, response=response, npixels=npixels))
      unit.grpest <- unit.grpest[!is.na(unit.grpest[[grpvar]]), ]
    }
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit.rowest)) {
    unit.rowest <- FIESTA::add0unit(unit.rowest, rowvar, uniquerow, unitvar, row.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit.rowest, unitvar)
    unitarea <- tabs$tab1
    unit.rowest <- tabs$tab2
    setkeyv(unit.rowest, unitvar)
    unit.rowest <- unit.rowest[unitarea, nomatch=0]
    unit.rowest <- FIESTA::getarea(unit.rowest, areavar=areavar, esttype=esttype)
    setkeyv(unit.rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit.colest)) {
    unit.colest <- FIESTA::add0unit(x=unit.colest, colvar, uniquecol, unitvar, col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit.colest, unitvar)
    unitarea <- tabs$tab1
    unit.colest <- tabs$tab2
    setkeyv(unit.colest, unitvar)
    unit.colest <- unit.colest[unitarea, nomatch=0]
    unit.colest <- FIESTA::getarea(unit.colest, areavar=areavar, esttype=esttype)
    setkeyv(unit.colest, c(unitvar, colvar))
  }

  if (!is.null(unit.grpest)) {
    if (!is.null(grpvar)) {
      ## SEPARATE COLUMNS
      unit.grpest[,(rowvar) := sapply(get(grpvar), 
			function(x){strsplit(as.character(x), "#")[[1]][1]})]
      unit.grpest[,(colvar) := sapply(get(grpvar), 
			function(x){strsplit(as.character(x), "#")[[1]][2]})]
      unit.grpest[,(grpvar) := NULL]
    } 

    if (row.add0 && col.add0) {
      unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			add0=TRUE, xvar2=colvar, uniquex2=uniquecol)

    } else {
      ordnames <- {}

      if (row.add0) {
        if (!is.null(uniquecol))  {
          unit.grpest[, (unitvar) := paste(get(unitvar), get(colvar), sep="#")][, 
			(colvar) := NULL]
          unit.grpest[,(colvar) := sapply(get(unitvar), 
			function(x){strsplit(as.character(x), "#")[[1]][2]})]
          unit.grpest[,(unitvar) := sapply(get(unitvar), 
			function(x){strsplit(as.character(x), "#")[[1]][1]})]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, colvar, uniquecol, unitvar, 
			add0=FALSE)
          ordnames <- c(ordnames, names(uniquecol))
        } else {
          ordnames <- c(ordnames, colvar)
        }
        if (!is.null(uniquerow))  {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			row.add0)
          ordnames <- c(names(uniquerow), ordnames)
        } else {
          ordnames <- c(ordnames, rowvar)
        }
      } else if (col.add0) {
        if (!is.null(uniquecol))  {
          unit.grpest[, (unitvar) := paste(get(unitvar), get(rowvar), sep="#")][, 
			(rowvar) := NULL]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, colvar, uniquecol, unitvar, 
			col.add0)
          ordnames <- c(ordnames, names(uniquecol))
        } else {
          ordnames <- c(ordnames, colvar)
        }
        if (!is.null(uniquerow))  {
          unit.grpest[,(rowvar) := sapply(get(unitvar), 
			function(x){strsplit(as.character(x), "#")[[1]][2]})]
          unit.grpest[,(unitvar) := sapply(get(unitvar), 
			function(x){strsplit(as.character(x), "#")[[1]][1]})]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			add0=FALSE)
          ordnames <- c(names(uniquerow), ordnames)
        } else {
          ordnames <- c(rowvar, ordnames)
        }
      } else {
        if (!is.null(uniquecol)) {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, colvar, uniquecol, unitvar, 
			add0=FALSE)
          ordnames <- c(ordnames, names(uniquecol))
        } else {
          ordnames <- c(ordnames, colvar)
        }
        if (!is.null(uniquerow)) {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			add0=FALSE)
          ordnames <- c(names(uniquerow), ordnames)
        } else {
          ordnames <- c(ordnames, rowvar)
        }
      }
      ordnames <- c(unitvar, ordnames)
      setcolorder(unit.grpest, 
		c(ordnames, names(unit.grpest)[!names(unit.grpest) %in% ordnames]))
    }
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

  returnlst <- list(est=est2return)
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

