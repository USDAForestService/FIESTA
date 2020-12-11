modFAOest <- function(tree=NULL, base=NULL, cluster=NULL, clustassgn=NULL, 
	seed=NULL, dsn=NULL, buniqueid="CN", baseid="CONDID", tuniqueid=NULL,
	basewt="CONDPROP_UNADJ", clustid="CN", clustassgnid="PLT_CN", clustjoinid="CN", 	
	invyrs=NULL, adj="none", strata=FALSE, clust.nonsamp.filter=NULL, 
 	base.nonsamp.filter=NULL, unitlevel1=NULL, unitlevel2=NULL, unitarea=NULL, 	
	areavar="AREA", unitcombine=FALSE, minplotnum.unit=10, stratalut=NULL, 
	strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=TRUE, 
	clust.filter=NULL, base.filter=NULL, esttype="RATIO", ratiotype="PERACRE", 
	estvarn=NULL, estvarn.filter=NULL, estvarn.name=NULL, estvard=NULL, 
	estvard.filter=NULL, estvard.name=NULL, TPA=TRUE, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, 
	rowgrpnm=NULL, rowgrpord=NULL, sumunits=FALSE, allin1=FALSE, 
	estround=1, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, rawdata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, 
	outfn.date=TRUE, overwrite=TRUE,  addtitle=TRUE, returntitle=FALSE, 
	title.main=NULL, title.ref=NULL, title.rowvar=NULL, title.colvar=NULL, 
	title.unitvar=NULL, title.estvarn=NULL, title.estvard=NULL, title.filter=NULL, 
	title.units=NULL, FAOpopdat=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 || is.null(tree) && is.null(FAOpopdat)) gui <- TRUE 


  ## If gui.. set variables to NULL
  if (gui) { 
    strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
	tuniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL 
    #if (!col.FIAname) col.FIAname <- NULL 
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=rowvar.filter=colvar.filter <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  landarea <- "ALL"
  nonresp <- FALSE
  substrvar <- NULL
  returnFAOpopdat <- TRUE 


  ## Translation
  #cond <- base
  #plt <- cluster
  #pltassgn <- clustassgn 
  #condid <- baseid
  #puniqueid <- cluniqueid
  #pltassgnid <- classgnid
  #pjoinid <- cljoinid
  #plt.nonsamp.filter <- clust.nonsamp.filter 
  #cond.nonsamp.filter <- base.nonsamp.filter
  #unitvar <- unitlevel1 
  #unitvar2 <- unitlevel2
  #clust.filter <- plt.filter
  #cond.filter <- base.filter 


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
  if (is.null(FAOpopdat)) {
    FAOpopdat <- modFAOpop(tree=tree, base=base, cluster=cluster, dsn=dsn, 
	clustassgn=clustassgn, clustid=clustid, clustassgnid=clustassgnid, 
	clustjoinid=clustjoinid, tuniqueid=tuniqueid, buniqueid=buniqueid, baseid=baseid, 
 	basewt=basewt, invyrs=invyrs, adj=adj, clust.nonsamp.filter=clust.nonsamp.filter, 
	base.nonsamp.filter=base.nonsamp.filter, strata=strata, unitlevel1=unitlevel1, 
	unitlevel2=unitlevel2, unitarea=unitarea, areavar=areavar, unitcombine=unitcombine, 
	minplotnum.unit=minplotnum.unit, stratalut=stratalut, strvar=strvar, 
	getwt=getwt, getwtvar=getwtvar, stratcombine=stratcombine, gui=gui, savedata=FALSE)

  } else {
    returnFAOpopdat <- FALSE
    if (!is.list(FAOpopdat))
      stop("FAOpopdat must be a list")
    listitems <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "unitarea", "unitvar", "strlut", "strvar",
		"plotsampcnt", "condsampcnt")
    if (!all(listitems %in% names(FAOpopdat))) {
      items.miss <- listitems[!listitems %in% names(FAOpopdat)]
      stop("invalid FAOpopdat... missing items: ", paste(items.miss, collapse=", "))
    }   
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
  strlut <- FAOpopdat$strlut
  strvar <- FAOpopdat$strvar
  expcondtab <- FAOpopdat$expcondtab
  plotsampcnt <- FAOpopdat$plotsampcnt
  condsampcnt <- FAOpopdat$condsampcnt
  states <- FAOpopdat$states
  invyrs <- FAOpopdat$invyrs
  stratcombinelut <- FAOpopdat$stratcombinelut
  if (nonresp) {
    substrvar <- FAOpopdat$substrvar
    nonsampplots <- FAOpopdat$nonsampplots
  }
  strunitvars <- c(unitvar, strvar)


  ###################################################################################
  ## Check parameters and apply plot and condition filters
  ###################################################################################
  estdat <- check.estdata(esttype=esttype, pltcondf=pltcondx, cuniqueid=cuniqueid,
 		condid=condid, treex=treex, tuniqueid=tuniqueid, sumunits=sumunits, 
		landarea=landarea, plt.filter=clust.filter, cond.filter=base.filter, 
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
  ### Check row and column data
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, 
	condf=pltcondf, cuniqueid=cuniqueid, condid=condid, tuniqueid=tuniqueid, 
	rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.orderby=row.orderby, 
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
  ### Get estimation data from tree table
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)

  treedat <- check.tree(gui=gui, treef=treef, condf=condf, bytdom=bytdom, 
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, 
	esttype=esttype, ratiotype=ratiotype, 
	estvarn=estvarn, estvarn.TPA=TPA, estvarn.filter=estvarn.filter,
 	estvarn.name=estvarn.name, 
	estvard=estvard, estvard.TPA=TPA, estvard.filter=estvard.filter,
 	estvard.name=estvard.name, 
	esttotn=TRUE, esttotd=TRUE, tdomvar=tdomvar, adjtree=adjtree)
  if (is.null(treedat)) return(NULL)

  tdomdat <- merge(condx, treedat$tdomdat, by=c(cuniqueid, condid))
  estvarn <- treedat$estvarn
  estvarn.name <- treedat$estvarn.name
  estvarn.filter <- treedat$estvarn.filter
  tdomvarlstn <- treedat$tdomvarlstn

  if (ratiotype == "PERTREE") {
    estvard <- treedat$estvard
    estvard.name <- treedat$estvard.name
    tdomvarlstd <- treedat$tdomvarlstd
  } else {
    estvard.name <- basewt
    tdomvarlstd <- NULL
  }

  ## add or separate concatenated columns
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
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, ratiotype=ratiotype,
 	sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.filter=title.filter, title.estvarn=title.estvarn,
 	title.units=title.units, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
	estvarn=estvarn, estvarn.filter=estvarn.filter, estvard=estvard,
 	estvard.filter=estvard.filter, addtitle=addtitle, rawdata=rawdata,
 	states=states, invyrs=invyrs, landarea=landarea, plt.filter=clust.filter, 
	cond.filter=base.filter, allin1=allin1, divideby=divideby, outfn=outfn, 
	outfn.pre=outfn.pre)
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
  unit.totest=unit.tdomest=unit.grpest=unit.rowest=unit.colest=unit.grpest=
	rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlstn) && length(tdomvarlstn) > 1)), TRUE, FALSE)
  strlut$prop.total <- sum(tdomdat[[basewt]], na.rm=TRUE) 
  strlut$propsq.total <- sum(tdomdat[[basewt]]^2, na.rm=TRUE) 

  ## Note: tdomdat is the summed response by condition (not domain)
  if (addtotal) {
    ## Get estimate for total
    tdomdat$TOTAL <- 1
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]
    unit.totest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdattot, uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, 
		strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit.totest, unitvar)
    unitarea <- tabs$tab1
    unit.totest <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- FIESTA::getarea(unit.totest, areavar=areavar, esttype=esttype)
  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]
    unit.rowest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		strlut=strlut, unitvar=unitvar, strvar=strvar, domain=rowvar)

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=c(estvarn.name, estvard.name)]
      unit.colest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		strlut=strlut, unitvar=unitvar, strvar=strvar, domain=colvar)

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=c(estvarn.name, estvard.name)]
      unit.grpest <- Ratio2Size(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, uniqueid=cuniqueid, 
		strlut=strlut, unitvar=unitvar, strvar=strvar, domain=grpvar)

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
    unit.rowest <- getarea(unit.rowest, areavar=areavar, esttype=esttype)
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
    ## AGGREGATE UNIT strlut FOR ROWVAR and GRAND TOTAL
    strlut2 <- data.table(strlut, ONEUNIT=1)
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
		strlut=strlut2, unitvar="ONEUNIT", domain=rowvar)

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
		strlut=strlut2, unitvar="ONEUNIT", domain="TOTAL")
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
    rawdat$estvarn <- estvarn.name
    rawdat$estvarn.filter <- estvarn.filter
  }
 
  estnm <- ifelse(esttype == "RATIO", "rhat", "est")
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvar2=unitlevel2, unit.totest=unit.totest, 
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
  returnlst <- list(est=est2return)
  if (!is.null(pse2return)) returnlst$pse <- pse2return 
  if (rawdata) returnlst$raw <- rawdat
  if(returntitle) returnlst$titlelst <- alltitlelst
  if (returnFAOpopdat) returnlst$FAOpopdat <- FAOpopdat

  return(returnlst)
}
