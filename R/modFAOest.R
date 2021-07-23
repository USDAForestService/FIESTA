modFAOest <- function(FAOpopdat=NULL, estseed="none", esttype="RATIO", 
	ratiotype="PERACRE", bcfilter=NULL, estvarn=NULL, estvarn.filter=NULL, 
	estvard=NULL, estvard.filter=NULL, TPA=TRUE, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, 
	rowgrpnm=NULL, rowgrpord=NULL, sumunits=FALSE, allin1=FALSE, metric=FALSE,
	estround=3, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	addtitle=TRUE, rawdata=FALSE, rawonly=FALSE, raw_fmt="csv", raw_dsn=NULL,
 	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE,
 	title.main=NULL, title.ref=NULL, title.rowvar=NULL, title.colvar=NULL,
 	title.unitvar=NULL, title.estvarn=NULL, title.estvard=NULL, title.filter=NULL,
 	gui=FALSE, ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modFAOest)),
		names(formals(FIESTA::modFAOpop))) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(FAOpopdat)) {
    gui <- TRUE
  } 

  ## If gui.. set variables to NULL
  if (gui) { 
    strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
	tuniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
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
  landarea <- "ALL"
  nonresp <- FALSE
  substrvar <- NULL
  returnFAOpopdat <- TRUE 
  returnlst <- list()


  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(FAOpopdat)) {
    FAOpopdat <- modFAOpop(gui=gui, ...)
  } else {
    returnFAOpopdat <- FALSE
    list.items <- c("basex", "clustbasex", "treex", "buniqueid", "baseid", 
		"unitarea", "areavar", "unitlevel1", "unitvars", "stratalut")
    FAOpopdat <- FIESTA::pcheck.object(FAOpopdat, "FAOpopdat", list.items=list.items)
  }	
  if (is.null(FAOpopdat)) return(NULL)
  basex <- FAOpopdat$basex
  clustbasex <- FAOpopdat$clustbasex	
  treex <- FAOpopdat$treex
  seedx <- FAOpopdat$seedx
  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree data for ratio estimates")
  }
  buniqueid <- FAOpopdat$buniqueid
  baseid <- FAOpopdat$baseid
  tuniqueid <- FAOpopdat$tuniqueid
  unitarea <- FAOpopdat$unitarea
  areavar <- FAOpopdat$areavar
  areaunits <- FAOpopdat$areaunits
  unitvar <- FAOpopdat$unitvar
  unitvars <- FAOpopdat$unitvars
  stratalut <- FAOpopdat$stratalut
  strvar <- FAOpopdat$strvar
  expcondtab <- FAOpopdat$expcondtab
  plotsampcnt <- FAOpopdat$plotsampcnt
  condsampcnt <- FAOpopdat$condsampcnt
  states <- FAOpopdat$states
  invyrs <- FAOpopdat$invyrs
  estvar.area <- FAOpopdat$estvar.area
  stratcombinelut <- FAOpopdat$stratcombinelut
  strwtvar <- FAOpopdat$strwtvar
  adj <- FAOpopdat$adj
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
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(FAOpopdat)) {
    FAOpopdat <- modFAOpop(gui=gui, ...)
  } else {
    returnFAOpopdat <- FALSE
    list.items <- c("basex", "clustbasex", "treex", "buniqueid", "baseid", 
		"tuniqueid", "unitarea", "unitlevel1", "stratalut", "strvar",
		"plotsampcnt", "condsampcnt")
    FAOpopdat <- FIESTA::pcheck.object(FAOpopdat, "FAOpopdat", list.items=list.items)
  }		
  if (is.null(FAOpopdat)) return(NULL)
  condx <- FAOpopdat$basex
  pltcondx <- FAOpopdat$clustbasex
  treex <- FAOpopdat$treex
  if (is.null(treex)) stop("must include tree data for tree estimates")
  cuniqueid <- FAOpopdat$buniqueid
  condid <- FAOpopdat$baseid
  tuniqueid <- FAOpopdat$tuniqueid
  unitarea <- FAOpopdat$unitarea
  areavar <- FAOpopdat$areavar
  unitvar <- FAOpopdat$unitlevel1
  unitvars <- FAOpopdat$unitvars
  stratalut <- FAOpopdat$stratalut
  strvar <- FAOpopdat$strvar
  expcondtab <- FAOpopdat$expcondtab
  plotsampcnt <- FAOpopdat$plotsampcnt
  condsampcnt <- FAOpopdat$condsampcnt
  states <- FAOpopdat$states
  invyrs <- FAOpopdat$invyrs
  estvar.area <- FAOpopdat$estvar.area
  stratcombinelut <- FAOpopdat$stratcombinelut
  if (nonresp) {
    substrvar <- FAOpopdat$substrvar
    nonsampplots <- FAOpopdat$nonsampplots
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
 	condid=condid, treex=treex, seedx=seedx, tuniqueid=tuniqueid, estseed=estseed,
	sumunits=sumunits, landarea=landarea, pcfilter=bcfilter, TPA=TPA,
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, rawonly=rawonly, 
	savedata=savedata, outfolder=outfolder, overwrite_dsn=overwrite_dsn, 
	overwrite_layer=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	append_layer=append_layer, raw_fmt=raw_fmt, raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  pltcondf <- estdat$pltcondf
  cuniqueid <- estdat$cuniqueid
  treef <- estdat$treef
  seedf <- estdat$seedf
  tuniqueid <- estdat$tuniqueid
  estseed <- estdat$estseed
  sumunits <- estdat$sumunits
  landarea <- estdat$landarea
  TPA <- estdat$TPA
  allin1 <- estdat$allin1
  estround <- estdat$estround
  pseround <- estdat$pseround
  divideby <- estdat$divideby
  addtitle <- estdat$addtitle
  returntitle <- estdat$returntitle
  rawdata <- estdat$rawdata
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
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, seedf=seedf,
	condf=pltcondf, cuniqueid=cuniqueid, condid=condid, tuniqueid=tuniqueid, 
	estseed=estseed, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.orderby=row.orderby, 
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
  concat <- rowcolinfo$concat
  grpvar <- rowcolinfo$grpvar
  tdomvar2 <- rowcolinfo$tdomvar2
  grpvar <- rowcolinfo$grpvar
  #rm(rowcolinfo)  

  if (rowvar == "TOTAL") rowcol.total <- TRUE

  ## Generate a uniquecol for estimation units
  if (!sumunits & colvar == "NONE") {
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
	cuniqueid=cuniqueid, condid=condid, 
	esttype=esttype, ratiotype=ratiotype, 
	estvarn=estvarn, estvarn.TPA=TPA, estvarn.filter=estvarn.filter,
 	estvarn.name=estvarn.name, 
	estvard=estvard, estvard.TPA=TPA, estvard.filter=estvard.filter,
 	estvard.name=estvard.name, 
	esttotn=TRUE, esttotd=TRUE, tdomvar=tdomvar, adjtree=adjtree, metric=metric)
  if (is.null(treedat)) return(NULL)

  tdomdat <- treedat$tdomdat

  if (rowvar != "TOTAL") {
    if (!row.add0 && any(tdomdat[[rowvar]] == 0)) {
      tdomdat <- tdomdat[tdomdat[[rowvar]] != 0,]
    }
    if (colvar != "NONE") {
      if (!col.add0 && any(tdomdat[[colvar]] == 0)) {
        tdomdat <- tdomdat[tdomdat[[colvar]] != 0,]
      }
    }
  }
  tdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid))
  if (!is.null(tdomvar)) {
    cdomdat <- merge(condx, condf, by=c(cuniqueid, condid))
  }
  estvarn <- treedat$estvarn
  estvarn.name <- treedat$estvarn.name
  estvarn.filter <- treedat$estvarn.filter
  tdomvarlstn <- treedat$tdomvarlstn
  estunitsn <- treedat$estunitsn
  estunitsd <- treedat$estunitsd

  if (ratiotype == "PERTREE") {
    estvard <- treedat$estvard
    estvard.name <- treedat$estvard.name
    tdomvarlstd <- treedat$tdomvarlstd
  } else {
    estvard.name <- estvar.area
    tdomvarlstd <- NULL
    estunitsd <- areaunits
  }  

  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
	ratiotype=ratiotype, sumunits=sumunits, title.main=title.main, title.ref=title.ref,
 	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.filter=title.filter, title.unitsn=estunitsn,
 	title.unitsd=estunitsd, title.estvarn=title.estvarn,
 	unitvar=unitvar, rowvar=rowvar, colvar=colvar,
 	estvarn=estvarn, estvarn.filter=estvarn.filter, estvard=estvard,
 	estvard.filter=estvard.filter, addtitle=addtitle, rawdata=rawdata, states=states,
 	invyrs=invyrs, landarea=landarea, pcfilter=bcfilter, allin1=allin1, 
	divideby=divideby, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if(rawdata) outfn.rawdat <- alltitlelst$outfn.rawdat
   

  ############################################################################
  ## GENERATE ESTIMATES
  ############################################################################
  unit_totest=unit.tdomest=unit_grpest=unit_rowest=unit_colest=unit_grpest=
	rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlstn) && length(tdomvarlstn) > 1)), TRUE, FALSE)
  stratalut$prop.total <- sum(tdomdat[[estvar.area]], na.rm=TRUE) 
  stratalut$propsq.total <- sum(tdomdat[[estvar.area]]^2, na.rm=TRUE) 

  ## Note: tdomdat is the summed response by condition (not domain)
  if (addtotal) {
    ## Get estimate for total
    tdomdat$TOTAL <- 1
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]
    unit_totest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdattot, uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, 
		strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    unit_totest <- FIESTA::getarea(unit_totest, areavar=areavar, esttype=esttype)
  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]
    unit_rowest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain=rowvar)

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=c(estvarn.name, estvard.name)]
      unit_colest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain=colvar)

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=c(estvarn.name, estvard.name)]
      unit_grpest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain=grpvar)

    }
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit_rowest)) {
    unit_rowest <- FIESTA::add0unit(x=unit_rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)
    tabs <- FIESTA::check.matchclass(unitarea, unit_rowest, unitvar)
    unitarea <- tabs$tab1
    unit_rowest <- tabs$tab2
    setkeyv(unit_rowest, unitvar)
    unit_rowest <- unit_rowest[unitarea, nomatch=0]
    unit_rowest <- getarea(unit_rowest, areavar=areavar, esttype=esttype)
    setkeyv(unit_rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit_colest)) {
    unit_colest <- FIESTA::add0unit(x=unit_colest, xvar=colvar, uniquex=uniquecol, 
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
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && rowvar != "TOTAL")) {
    ## AGGREGATE UNIT stratalut FOR ROWVAR and GRAND TOTAL
    strlut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    if (is.null(getwtvar) || !getwtvar %in% names(strlut2)) getwtvar <- "strwt"
    strlut2 <- strlut2[, lapply(.SD, sum, na.rm=TRUE), 
		by = strunitvars2, .SDcols=c(getwtvar, "n.strata")]
    strlut2[, strwt:=prop.table(get(getwtvar)), by="ONEUNIT"]
    strlut2[, n.total := sum(n.strata)]
    setkeyv(strlut2, strunitvars2)

    unitacres2 <- data.table(unitarea, ONEUNIT=1)
    unitacres2 <- unitacres2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
    setkey(unitacres2, "ONEUNIT")

    tdomdat[, ONEUNIT := 1]

    ## Calculate unit totals for rowvar
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=estvarn.name]
    rowunit <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=strlut2, unitvar="ONEUNIT", domain=rowvar)

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
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=estvarn.name]
    totunit <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		stratalut=strlut2, unitvar="ONEUNIT", domain="TOTAL")
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
  estnm <- ifelse(esttype == "RATIO", "rhat", "est")
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, 
	unit_rowest=unit_rowest, unit_colest=unit_colest, unit_grpest=unit_grpest, 
	rowvar=rowvar, colvar=colvar, uniquerow=uniquerow, uniquecol=uniquecol, 
	rowgrp=rowgrp, rowgrpnm=rowgrpnm, rowunit=rowunit, totunit=totunit, 
	allin1=allin1, savedata=savedata, addtitle=addtitle, title.ref=title.ref,
 	title.colvar=title.colvar, title.rowvar=title.rowvar, title.rowgrp=title.rowgrp,
 	title.unitvar=title.unitvar, title.estpse=title.estpse, title.est=title.est,
 	title.pse=title.pse, rawdata=rawdata, rawonly=rawonly, 
	outfn.estpse=outfn.estpse, outfolder=outfolder, overwrite=overwrite_layer,
 	outfn.date=outfn.date, estnm=estnm, estround=estround, pseround=pseround,
 	divideby=divideby, returntitle=returntitle, estnull=estnull, psenull=psenull) 
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
    rawdat$domdat <- setDF(tdomdat) 
    rawdat$estvarn <- estvarn.name
    rawdat$estvarn.filter <- estvarn.filter
    if (ratiotype == "PERACRE") {
      rawdat$estvard <- estvard.name
      rawdat$estvard.filter <- estvard.filter
    }
    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
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

    rawdat$esttype <- "RATIO"
    rawdat$estvarn <- estvarn
    rawdat$estvarn.filter <- estvarn.filter
    if (!is.null(estvard)) rawdat$estvard <- estvard
    if (!is.null(estvard.filter)) rawdat$estvard.filter <- estvard.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    if (ratiotype == "PERACRE") {
      rawdat$areaunits <- areaunits
    }
    rawdat$estunitsn <- estunitsn
    if (ratiotype == "PERTREE") {
      rawdat$estunitsd <- estunitsd
    }
    returnlst$raw <- rawdat
  }
  if (returnFAOpopdat) {
    returnlst$FAOpopdat <- FAOpopdat
  }
    
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }

  return(returnlst)
}
