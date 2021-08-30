modGBarea <- function(GBpopdat=NULL, landarea="FOREST", pcfilter=NULL, 
	rowvar=NULL, colvar=NULL, row.FIAname=FALSE, col.FIAname=FALSE, 
	row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, 
	rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, 
	sumunits=TRUE, allin1=FALSE, metric=FALSE, estround=1, pseround=2,
	estnull="--", psenull="--", divideby=NULL, savedata=FALSE, outfolder=NULL, 
	outfn.pre=NULL, outfn.date=FALSE, addtitle=TRUE, rawdata=FALSE, rawonly=FALSE, 
	raw_fmt="csv", raw_dsn=NULL, overwrite_dsn=FALSE, overwrite_layer=TRUE,
 	append_layer=FALSE, returntitle=FALSE, title.main=NULL, title.ref=NULL,
 	title.rowvar=NULL, title.colvar=NULL, title.unitvar=NULL, title.filter=NULL,
 	gui=FALSE, ...){

  ###################################################################################
  ## DESCRIPTION: 
  ## Generates acre estimates by domain (and estimation unit)
  ###################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modGBarea)),
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
    landarea=strvar=areavar=sumunits=adj=strata=getwt=cuniqueid=ACI=
	puniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL
    #if (!col.FIAname) col.FIAname <- NULL
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter=
	rawfolder <- NULL
  #estvar <- "CONDPROP_ADJ"

  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  esttype <- "AREA" 
  nonresp <- FALSE
  substrvar <- NULL
  returnGBpopdat <- TRUE 
  parameters <- FALSE
  returnlst <- list()


  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(GBpopdat)) {
    GBpopdat <- modGBpop(gui=gui, ...)
  } else {
    returnGBpopdat <- FALSE
    list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "unitarea", "unitvar", "stratalut", "strvar",
		"plotsampcnt", "condsampcnt")
    GBpopdat <- FIESTA::pcheck.object(GBpopdat, "GBpopdat", list.items=list.items)
  }
  if (is.null(GBpopdat)) return(NULL)
  condx <- GBpopdat$condx
  pltcondx <- GBpopdat$pltcondx
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  ACI.filter <- GBpopdat$ACI.filter
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  areaunits <- GBpopdat$areaunits
  unitvar <- GBpopdat$unitvar
  unitvars <- GBpopdat$unitvars
  strata <- GBpopdat$strata
  stratalut <- GBpopdat$stratalut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  estvar.name <- GBpopdat$estvar.area
  stratcombinelut <- GBpopdat$stratcombinelut
  strwtvar <- GBpopdat$strwtvar
  if (nonresp) {
    substrvar <- GBpopdat$substrvar
    nonsampplots <- GBpopdat$nonsampplots
  } 
  strunitvars <- c(unitvar, strvar)


  ########################################
  ## Check area units
  ########################################
  unitchk <- pcheck.areaunits(unitarea=unitarea, areavar=areavar, 
			areaunits=areaunits, metric=metric)
  unitarea <- unitchk$unitarea
  areavar <- unitchk$areavar
  areaunits <- unitchk$outunits


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 	condid=condid, sumunits=sumunits, landarea=landarea, ACI.filter=ACI.filter, 
 	pcfilter=pcfilter, allin1=allin1, estround=estround, pseround=pseround, 
	divideby=divideby, addtitle=addtitle, returntitle=returntitle,
 	rawdata=rawdata, rawonly=rawonly, savedata=savedata, outfolder=outfolder, 
	overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, outfn.pre=outfn.pre,
 	outfn.date=outfn.date, append_layer=append_layer, raw_fmt=raw_fmt, 
	raw_dsn=raw_dsn, gui=gui)
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

#  ## Add a column for totals
#  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
#  if (addtotal) {
#    condf$TOTAL <- 1
#  }

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
	title.filter=title.filter, title.unitsn=areaunits, unitvar=unitvar, rowvar=rowvar,
 	colvar=colvar, addtitle=addtitle, rawdata=rawdata, states=states, invyrs=invyrs,
 	landarea=landarea, pcfilter=pcfilter, allin1=allin1, divideby=divideby, 
	outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
  }

  ###################################################################################
  ## GENERATE ESTIMATES
  ###################################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit=tdomdattot <- NULL
  addtotal <- ifelse(rowvar == "TOTAL" || length(unique(condf[[rowvar]])) > 1, TRUE, FALSE)
  #estvar.name <- estvar 
  stratalut <- setDT(stratalut)

  message("getting estimates using GB...")
#  if (addtotal) {
    ## Get total estimate and merge area
    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
    unit_totest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdattot, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    unit_totest <- FIESTA::getarea(unit_totest, areavar=areavar, esttype=esttype)
#  }

  ## Get row estimate  
  if (rowvar != "TOTAL") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvar.name]
    unit_rowest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=rowvar)
  }

  ## Get column (and cell) estimate  
  if (colvar != "NONE") {
    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
    unit_colest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=colvar)

    cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
    unit_grpest <- GBest.pbar(sumyn=estvar.name, ysum=cdomdatsum, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=grpvar)
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- add0unit(x=unit_rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
    unit_rowest <- FIESTA::getarea(unit_rowest, areavar=areavar, esttype=esttype)
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit_colest)) {
    unit_colest <- add0unit(x=unit_colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_colest, unitvar)
    unitarea <- tabs$tab1
    unit_colest <- tabs$tab2
    setkeyv(unit_colest, unitvar)
    unit_colest <- unit_colest[unitarea, nomatch=0]
    unit_colest <- FIESTA::getarea(unit_colest, areavar=areavar, esttype=esttype)
    setkeyv(unit_colest, c(unitvar, colvar))
  }
  if (!is.null(unit_grpest)) {
    unit_grpest <- add0unit(x=unit_grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_grpest, unitvar)
    unitarea <- tabs$tab1
    unit_grpest <- tabs$tab2
    setkeyv(unit_grpest, unitvar)
    unit_grpest <- unit_grpest[unitarea, nomatch=0]
    unit_grpest <- FIESTA::getarea(unit_grpest, areavar=areavar, esttype=esttype)
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
    stratalut2 <- stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(strwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(strwtvar)), by="ONEUNIT"]
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
		uniqueid=cuniqueid, stratalut=stratalut2, unitvar="ONEUNIT", strvar=strvar, 
		domain=rowvar)
    rowunit <- FIESTA::add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
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
		uniqueid=cuniqueid, stratalut=stratalut2, unitvar="ONEUNIT", strvar=strvar, 
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
  message("getting output...")
  estnm <- "est" 
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, 
	unit_rowest=unit_rowest, unit_colest=unit_colest, unit_grpest=unit_grpest,
 	rowvar=rowvar, colvar=colvar, uniquerow=uniquerow, uniquecol=uniquecol,
 	rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
	allin1=allin1, savedata=savedata, addtitle=addtitle, title.ref=title.ref,
 	title.rowvar=title.rowvar, title.colvar=title.colvar, title.rowgrp=title.rowgrp,
 	title.unitvar=title.unitvar, title.estpse=title.estpse, title.est=title.est,
 	title.pse=title.pse, rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, 
	outfolder=outfolder, outfn.date=outfn.date, overwrite=overwrite_layer, 
	estnm=estnm, estround=estround, pseround=pseround, divideby=divideby, 
	returntitle=returntitle, estnull=estnull, psenull=psenull) 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (!is.null(est2return)) {
    returnlst$est <- setDF(est2return)
  }
  if (!is.null(pse2return)) {
    returnlst$pse <- setDF(pse2return)
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }
 
  if (rawdata) {
    rawdat <- tabs$rawdat
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
          datExportData(rawtab, out_fmt=raw_fmt, outfolder=rawfolder, 
 			out_dsn=raw_dsn, out_layer=out_layer, 
			overwrite_layer=overwrite_layer, add_layer=TRUE, 
			append_layer=append_layer)
        }
      }
    }
    rawdat$esttype <- "AREA"
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    returnlst$raw <- rawdat
  }
  if (returnGBpopdat) {
    returnlst$GBpopdat <- GBpopdat
  }
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }
    
  return(returnlst)
}
