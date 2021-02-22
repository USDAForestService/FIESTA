modGBtree <- function(	estseed="none", landarea="FOREST", pfilter=NULL, 
	cfilter=NULL, estvar=NULL, estvar.filter=NULL, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, 
	rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, sumunits=TRUE, allin1=FALSE, 
	estround=1, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, rawdata=FALSE, rawonly=FALSE, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=TRUE, overwrite=TRUE, addtitle=TRUE, returntitle=FALSE, 
 	title.main=NULL, title.ref=NULL, title.rowvar=NULL, title.colvar=NULL, 
	title.unitvar=NULL, title.estvar=NULL, title.filter=NULL, 
 	GBpopdat=NULL, gui=FALSE, ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modGBtree)),
		names(formals(FIESTA::modGBpop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

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

  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  esttype <- "TREE"
  returnGBpopdat <- TRUE 
  parameters <- FALSE
  returnlst <- list()

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
  if (is.null(GBpopdat)) {
    GBpopdat <- modGBpop(gui=gui, ...)
  } else {
    returnGBpopdat <- FALSE
    list.items <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "unitarea", "unitvar", "stratalut", "strvar",
		"plotsampcnt", "condsampcnt")
    GBpopdat <- FIESTA::pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  }		
  if (is.null(GBpopdat)) return(NULL)
  condx <- GBpopdat$condx
  pltcondx <- GBpopdat$pltcondx
  treex <- GBpopdat$treex
  seedx <- GBpopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for tree estimates")
  }
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  tuniqueid <- GBpopdat$tuniqueid
  ACI.filter <- GBpopdat$ACI.filter
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  unitvar <- GBpopdat$unitvar
  unitvar2 <- GBpopdat$unitvar2
  stratalut <- GBpopdat$stratalut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  stratcombinelut <- GBpopdat$stratcombinelut
  getwtvar <- GBpopdat$getwtvar
  adj <- GBpopdat$adj
  strunitvars <- c(unitvar, strvar)

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
  ### Check row and column data
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf,
	condf=pltcondf, cuniqueid=cuniqueid, tuniqueid=tuniqueid, estseed=estseed,
	rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar, 
	colvar.filter=colvar.filter, row.FIAname=row.FIAname, col.FIAname=col.FIAname,
 	row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0, 
	col.add0=col.add0, title.rowvar=title.rowvar, title.colvar=title.colvar, 
	rowlut=rowlut, collut=collut, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowgrpord=rowgrpord, landarea=landarea)
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
  ### Get estimation data from tree table
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- check.tree(gui=gui, treef=treef, seedf=seedf, estseed=estseed, 
		bycond=TRUE, condf=condf, bytdom=bytdom, tuniqueid=tuniqueid, 
		cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
		estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, 
		tdomvar2=tdomvar2, adjtree=adjtree)
  if (is.null(treedat)) return(NULL) 
  tdomdat <- merge(condx, treedat$tdomdat, by=c(cuniqueid, condid))
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst

  ## remove NA values
  #if (!is.null(tdomvar) && !is.null(tdomvar2)) {
  #  tdomdat <- tdomdat[!is.na(tdomdat[[rowvar]]) & !is.na(tdomdat[[colvar]]),]
  #}
  
  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
	sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.filter=title.filter, title.estvarn=title.estvar,
 	unitvar=unitvar, rowvar=rowvar, colvar=colvar, estvarn=estvar,
 	estvarn.filter=estvar.filter, addtitle=addtitle, returntitle=returntitle, 
	rawdata=rawdata, states=states, invyrs=invyrs, landarea=landarea, 
	pfilter=pfilter, cfilter=cfilter, allin1=allin1, divideby=divideby,
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
  unit.totest=unit.tdomest=unit.domest=unit.rowest=unit.colest=unit.grpest=
	rowunit=totunit=tdomdattot <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlst) && length(tdomvarlst) > 1)), TRUE, FALSE)

  ## Note: tdomdat is the summed response by condition (not domain)
  #if (addtotal) {
    ## Get estimate for total
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    unit.totest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdattot, 
		esttype=esttype, uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, 
		strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit.totest, unitvar)
    unitarea <- tabs$tab1
    unit.totest <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- getarea(unit.totest, areavar=areavar, esttype=esttype)
  #}

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[rowvar]]),]
    unit.rowest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=rowvar)
    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
      tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[colvar]]),]
      unit.colest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=colvar)

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
      unit.grpest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=grpvar)
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
  ## Get row and column totals for units if sumunits=FALSE
  ###################################################################################

  ## For sumunits=FALSE, get estimation unit totals
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && rowvar != "TOTAL")) {
    ## AGGREGATE UNIT stratalut FOR ROWVAR and GRAND TOTAL
    stratalut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    if (is.null(getwtvar) || !getwtvar %in% names(stratalut2)) {
      getwtvar <- "strwt"
    }
    stratalut2 <- stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
		by = strunitvars2, .SDcols=c(getwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(getwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvars2)

    unitacres2 <- data.table(unitarea, ONEUNIT=1)
    unitacres2 <- unitacres2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitacres2, "ONEUNIT")

    tdomdat[, ONEUNIT := 1]

    ## Calculate unit totals for rowvar
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, esttype=esttype, 
			uniqueid=tuniqueid, stratalut=stratalut2, 
			unitvar="ONEUNIT", strvar=strvar, domain=rowvar)

    rowunit <- add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
    tabs <- FIESTA::check.matchclass(unitacres2, rowunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkeyv(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    rowunit <- FIESTA::getarea(rowunit, areavar=areavar, esttype=esttype)
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## Calculate grand total for all units
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=estvar.name]
    totunit <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, esttype=esttype, 
			uniqueid=tuniqueid, stratalut=stratalut2, 
			unitvar="ONEUNIT", strvar=strvar, domain="TOTAL")
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
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste0(outfn.rawdat, "_", tabnm, ".csv") 
        if (tabnm %in% c("plotsampcnt", "condsampcnt", "stratdat", "stratcombinelut")) {
          write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite, outtxt=tabnm)
        } else if (is.data.frame(rawtab)) {
          suppressWarnings(save1tab(tab=rawtab, tab.title=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
        }
      }
    }
  }  

  ## GET VALUES TO RETURN
  if (!is.null(est2return)) returnlst$est <- setDF(est2return)
  if (!is.null(pse2return)) returnlst$pse <- setDF(pse2return) 
  if (rawdata) {
    rawdat$esttype <- "TREE"
    rawdat$estvar <- estvar
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    returnlst$raw <- rawdat
  }
  if(returntitle) returnlst$titlelst <- alltitlelst
  if (returnGBpopdat) returnlst$GBpopdat <- GBpopdat

  return(returnlst)
}
