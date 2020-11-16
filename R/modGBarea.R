modGBarea <- function(cond=NULL, plt=NULL, pltassgn=NULL, dsn=NULL, 
	cuniqueid="PLT_CN", condid="CONDID", puniqueid="CN", pltassgnid="PLT_CN", 
	pjoinid="CN", evalid=NULL, invyrs=NULL, ACI=FALSE, adj="samp", strata=TRUE, 
	plt.nonsamp.filter=NULL, cond.nonsamp.filter=NULL, unitvar=NULL, unitvar2=NULL, 
	unitarea=NULL, areavar="ACRES", unitcombine=FALSE, minplotnum.unit=10, 
	stratalut=NULL, strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", 
	stratcombine=TRUE, landarea="FOREST", plt.filter=NULL, cond.filter=NULL, 
	rowvar=NULL, colvar=NULL, row.FIAname=FALSE, col.FIAname=FALSE, 
	row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, 
	rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, 
	sumunits=TRUE, allin1=FALSE, estround=1, pseround=2, estnull="--", 
	psenull="--", divideby=NULL, savedata=FALSE, rawdata=FALSE, outfolder=NULL, 
	outfn=NULL, outfn.pre=NULL, outfn.date=TRUE, overwrite=TRUE, addtitle=TRUE, 
	returntitle=FALSE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.filter=NULL, GBpopdat=NULL, 
	GBdata=NULL, gui=FALSE){

  ###################################################################################
  ## DESCRIPTION: 
  ## Generates acre estimates by domain (and estimation unit)
  ###################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 || is.null(cond) && is.null(GBpopdat) && is.null(GBdata)) gui <- TRUE 

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL

  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################

  ## If gui.. set variables to NULL
  if (gui) { 
    cond=landarea=strvar=areavar=sumunits=adj=strata=getwt=cuniqueid=ACI=
	puniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL
    #if (!col.FIAname) col.FIAname <- NULL
  }

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  esttype <- "AREA" 
  nonresp <- FALSE
  substrvar <- NULL
  returnGBpopdat <- TRUE 


  ### Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

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
  if (is.null(GBpopdat)) {
    GBpopdat <- modGBpop(cond=cond, plt=plt, dsn=dsn, pltassgn=pltassgn, 
	puniqueid=puniqueid, cuniqueid=cuniqueid, condid=condid, pltassgnid=pltassgnid, 
	pjoinid=pjoinid, evalid=evalid, invyrs=invyrs, ACI=ACI, adj=adj, 
	plt.nonsamp.filter=plt.nonsamp.filter, cond.nonsamp.filter=cond.nonsamp.filter, 
	strata=strata, unitvar=unitvar, unitvar2=unitvar2, unitarea=unitarea, 
	areavar=areavar, unitcombine=unitcombine, minplotnum.unit=minplotnum.unit, 
	stratalut=stratalut, strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
	stratcombine=stratcombine, GBdata=GBdata, gui=gui)
  } else {
    returnGBpopdat <- FALSE
    if (!is.list(GBpopdat))
      stop("GBpopdat must be a list")
    listitems <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "unitarea", "unitvar", "strlut", "strvar",
		"plotsampcnt", "condsampcnt")
    if (!all(listitems %in% names(GBpopdat))) {
      items.miss <- listitems[!listitems %in% names(GBpopdat)]
      stop("invalid GBpopdat... missing items: ", paste(items.miss, collapse=", "))
    }   
  }	
  if (is.null(GBpopdat)) return(NULL)	
  condx <- GBpopdat$condx
  pltcondx <- GBpopdat$pltcondx
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  ACI.filter <- GBpopdat$ACI.filter
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  unitvar <- GBpopdat$unitvar
  strlut <- GBpopdat$strlut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  stratcombinelut <- GBpopdat$stratcombinelut
  if (nonresp) {
    substrvar <- GBpopdat$substrvar
    nonsampplots <- GBpopdat$nonsampplots
  }
  strunitvars <- c(unitvar, strvar)


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 		condid=condid, sumunits=sumunits, landarea=landarea,
 		ACI.filter=ACI.filter, plt.filter=plt.filter, cond.filter=cond.filter, 
		allin1=allin1, estround=estround, pseround=pseround, divideby=divideby,
 		addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
		savedata=savedata, outfolder=outfolder, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
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
  ### Check row and column data
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, condf=pltcondf, 
	cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, row.orderby=row.orderby, col.orderby=col.orderby, 
	row.add0=row.add0, col.add0=col.add0, title.rowvar=title.rowvar, 
	title.colvar=title.colvar, rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
	rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, landarea=landarea)
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

  ## Merge filtered condition data (condf) to all conditions (condx)
  ###################################################################################
  setkeyv(condx, c(cuniqueid, condid))
  setkeyv(condf, c(cuniqueid, condid))

  cdomdat <- condx[condf]


  ###################################################################################
  ### Get titles for output tables
  ###################################################################################
  alltitlelst <- check.titles(dat=cdomdat, esttype=esttype, sumunits=sumunits,
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.unitvar,
	title.filter=title.filter, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
	addtitle=addtitle, rawdata=rawdata, states=states, invyrs=invyrs, landarea=landarea, 
	plt.filter=plt.filter, cond.filter=cond.filter, allin1=allin1, divideby=divideby, 
	outfn=outfn, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) outfn.rawdat <- alltitlelst$outfn.rawdat


  ###################################################################################
  ## GENERATE ESTIMATES
  ###################################################################################
  unit.totest=unit.rowest=unit.colest=unit.grpest=rowunit=totunit=tdomdattot <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(cdomdat[[rowvar]])) > 1, TRUE, FALSE)
  estvar.name <- ifelse(adj == "samp", "CONDPROP_ADJ", "CONDPROP_UNADJ")

#  if (addtotal) {
    ## Get total estimate and merge area
    cdomdat$TOTAL <- 1
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    unit.totest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdattot, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit.totest, unitvar)
    unitarea <- tabs$tab1
    unit.totest <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- FIESTA::getarea(unit.totest, areavar=areavar, esttype=esttype)
#  }

  ## Get row estimate  
  if (rowvar != "TOTAL") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    unit.rowest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain=rowvar)
  }

  ## Get column (and cell) estimate  
  if (colvar != "NONE") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
    unit.colest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain=colvar)

    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
    unit.grpest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain=grpvar)
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit.rowest)) {
    unit.rowest <- add0unit(unit.rowest, rowvar, uniquerow, unitvar, row.add0)
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
  ## Get row and column totals for units if sumunits=FALSE
  ###################################################################################

  ## For sumunits=FALSE, get estimation unit totals
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && !is.null(grpvar))) {

    ## AGGREGATE UNIT strlut FOR ROWVAR and GRAND TOTAL
    strlut2 <- data.table(strlut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    if (is.null(getwtvar) || !getwtvar %in% names(strlut2)) getwtvar <- "strwt"
    strlut2 <- strlut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(getwtvar, "n.strata")]
    strlut2[, strwt:=prop.table(get(getwtvar)), by="ONEUNIT"]
    strlut2[, n.total := sum(n.strata)]
    setkeyv(strlut2, strunitvars2)

    unitacres2 <- data.table(unitarea, ONEUNIT=1)
    unitacres2 <- unitacres2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitacres2, "ONEUNIT")

    cdomdat[, ONEUNIT := 1]

    ## CALCULATE UNIT TOTALS FOR ROWVAR
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, cuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut2, unitvar="ONEUNIT", strvar=strvar, 
		domain=rowvar)
    rowunit <- FIESTA::add0unit(rowunit, rowvar, uniquerow, "ONEUNIT", row.add0)
    tabs <- FIESTA::check.matchclass(unitacres2, rowunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    rowunit <- FIESTA::getarea(rowunit, areavar=areavar, esttype=esttype)
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut2, unitvar="ONEUNIT", strvar=strvar, 
		domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitacres2, totunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitacres2, nomatch=0]
    totunit <- FIESTA::getarea(totunit, areavar=areavar, esttype=esttype)
  }          

  ###################################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################################

  if (rawdata) {
    rawdat <- list()
    rawdat$domdat <- setDF(cdomdat)
  }
 
  estnm <- "est" 
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvar2=unitvar2, unit.totest=unit.totest, 
	unit.rowest=unit.rowest, unit.colest=unit.colest, unit.grpest=unit.grpest,
 	rowvar=rowvar, colvar=colvar, uniquerow=uniquerow, uniquecol=uniquecol,
 	rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
	allin1=allin1, savedata=savedata, addtitle=addtitle, title.ref=title.ref,
 	title.colvar=title.colvar, title.rowvar=title.rowvar, title.rowgrp=title.rowgrp,
 	title.unitvar=title.unitvar, title.estpse=title.estpse, title.est=title.est,
 	title.pse=title.pse, rawdata=rawdata, outfn.estpse=outfn.estpse, 
	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite, estnm=estnm, 
	estround=estround, pseround=pseround, divideby=divideby, rawdat=rawdat, 
	returntitle=returntitle, estnull=estnull, psenull=psenull) 

  est2return <- tabs$tabest
  pse2return <- tabs$tabpse
  if (rawdata) rawdat <- tabs$rawdat
  if (returntitle) titlelst <- tabs$titlelst

  if (savedata) {
    ## Save rawdata
    if (rawdata) {
      rawfolder <- paste(outfolder, "rawdata", sep="/")
      if (!file.exists(rawfolder)) dir.create(rawfolder)

      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- paste(title.est, title.ref, sep="; ")
      }
      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm, ".csv") 
        if (tabnm %in% c("plotsampcnt", "condsampcnt", "stratdat", "stratcombinelut")) {
          write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite)
        } else if (is.data.frame(rawtab)) {
          suppressWarnings(save1tab(tab=rawtab, tab.title=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
        }
      }
    }

    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    if (!is.null(outfn)) 
      outfn.param <- paste(outfn.estpse, "parameters", sep="_")
    outparamfn <- getoutfn(outfn.param, outfolder=outfolder, 
		outfn.date=outfn.date, overwrite=overwrite, ext="txt")

    outfile <- file(outparamfn, "w")
    cat(  "cond = ", as.character(bquote(cond)), "\n",
      "cond = ", as.character(bquote(cond)), "\n",
      "plt = ", as.character(bquote(plt)), "\n",
      "pltassgn = ", as.character(bquote(pltassgn)), "\n",
      "dsn = \"", dsn, "\"", "\n", 
      "cuniqueid = \"", cuniqueid, "\"", "\n",
      "cuniqueid = \"", cuniqueid, "\"", "\n", 
      "condid = \"", condid, "\"", "\n", 
      "puniqueid = \"", puniqueid, "\"", "\n",
      "pltassgnid = \"", pltassgnid, "\"", "\n",
      "ACI = ", ACI, "\n",
      "sumunits = ", sumunits, "\n",
      "adj = \"", adj, "\"", "\n",
      "strata = ", strata, "\n",
      "plt.nonsamp.filter = \"", plt.nonsamp.filter, "\"", "\n",
      "cond.nonsamp.filter = \"", cond.nonsamp.filter, "\"", "\n",
      "unitvar = \"", unitvar2, "\"", "\n",
      "unitvar2 = \"", unitvar, "\"", "\n",
      "unitcombine = ", unitcombine, "\n",
      "unitarea = ", as.character(bquote(unitarea)), "\n",
      "areavar = \"", areavar, "\"", "\n",
      "stratalut = ", as.character(bquote(stratalut)), "\n", 
      "strvar = \"", strvar, "\"", "\n",
      "getwt = ", getwt, "\n",
      "getwtvar = \"", getwtvar, "\"", "\n",
      "stratcombine = ", stratcombine, "\n",
      "landarea = \"", landarea, "\"", "\n",
      "plt.filter = \"", plt.filter, "\"", "\n",
      "cond.filter = \"", cond.filter, "\"", "\n",
      "rowvar = \"", rowvar, "\"", "\n",
      "rowvar.filter = \"", rowvar.filter, "\"", "\n",
      "colvar = \"", colvar, "\"", "\n",
      "colvar.filter = \"", colvar.filter, "\"", "\n",
      "row.FIAname = ", row.FIAname, "\n",
      "col.FIAname = ", col.FIAname, "\n",
      "row.orderby = \"", row.orderby, "\"", "\n",
      "col.orderby = \"", col.orderby, "\"", "\n",
      "row.add0 = ", row.add0, "\n",
      "col.add0 = ", col.add0, "\n",
      "rowlut = ", as.character(bquote(rowlut)), "\n",
      "collut = ", as.character(bquote(collut)), "\n",
      "rowgrp = ", rowgrp, "\n",
      "rowgrpnm = \"", rowgrpnm, "\"", "\n",
      "rowgrpord = \"", rowgrpord, "\"", "\n",
      "allin1 = ", allin1, "\n",
      "estround = ", estround, "\n",
      "pseround = ", pseround, "\n",
      "divideby = \"", divideby, "\"", "\n",
      "savedata = ", savedata, "\n",
      "rawdata = ", rawdata, "\n",
      "outfolder = \"", outfolder, "\"", "\n",
      "outfn = \"", outfn, "\"", "\n",
      "outfn.pre = \"", outfn.pre, "\"", "\n",
      "outfn.date = ", outfn.date, "\n",
      "overwrite = ", overwrite, "\n",
      "addtitle = ", addtitle, "\n",
      "returntitle = ", returntitle, "\n",
      "title.main = \"", title.main, "\"", "\n",
      "title.ref = \"", title.ref, "\"", "\n",
      "title.rowvar = \"", title.rowvar, "\"", "\n",
      "title.colvar = \"", title.colvar, "\"", "\n",
      "title.unitvar = \"", title.unitvar, "\"", "\n",
      "title.filter = \"", title.filter, "\"", "\n",
      "gui = ", gui, "\n","\n",
    file = outfile, sep="")

    cat(  "est <- modGBarea(cond=cond, plt=plt, pltassgn=pltassgn, cuniqueid=cuniqueid, 
	puniqueid=puniqueid,  	pltassgnid=pltassgnid, ACI=ACI, sumunits=sumunits, adj=adj, 
	strata=strata, plt.nonsamp.filter=plt.nonsamp.filter, cond.nonsamp.filter=cond.nonsamp.filter,
 	landarea=landarea, plt.filter=plt.filter, cond.filter=cond.filter, unitvar=unitvar, 
	unitvar2=unitvar2, stratcombine=stratcombine, unitarea=unitarea, areavar=areavar,
	stratalut=stratalut, strvar=strvar, getwt=getwt, getwtvar=getwtvar, rowvar=rowvar,
 	rowvar.filter=rowvar.filter, colvar=colvar, colvar.filter=colvar.filter, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, row.orderby=row.orderby, 
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, rowlut=rowlut, 
	collut=collut, rowgrp=rowgrp, rowgrpnm=NULL, rowgrpord=NULL, allin1=allin1, 
	estround=estround, pseround=pseround, divideby=divideby, savedata=savedata, 
	rawdata=rawdata, outfolder=outfolder, outfn=outfn, outfn.pre=outfn.pre, 
	overwrite=overwrite, outfn.date=outfn.date, addtitle=addtitle, 
	returntitle=returntitle, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.unitvar=title.unitvar,
 	title.filter=title.filter, gui=gui)", file = outfile, sep="")
    close(outfile)

  }
 
  ## GET VALUES TO RETURN
  returnlst <- list(est=est2return)
  if (!is.null(pse2return)) returnlst$pse <- pse2return
  if (rawdata) returnlst$raw <- rawdat
  if (returntitle) returnlst$titlelst <- alltitlelst
  if (returnGBpopdat) returnlst$GBpopdat <- GBpopdat
    
  return(returnlst)
}
