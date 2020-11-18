modGBtree <- function(tree=NULL, cond=NULL, plt=NULL, pltassgn=NULL, seed=NULL, 
	dsn=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", 
	puniqueid="CN", pltassgnid="PLT_CN", pjoinid="CN", evalid=NULL, invyrs=NULL, 
	ACI=FALSE, adj="samp", strata=TRUE, plt.nonsamp.filter=NULL, 
	cond.nonsamp.filter=NULL, unitvar=NULL, unitvar2=NULL, unitarea=NULL, 
	areavar="ACRES", unitcombine=FALSE, minplotnum.unit=10, stratalut=NULL, 
	strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=TRUE, 
	landarea="FOREST", plt.filter=NULL, cond.filter=NULL, estvar=NULL, 
	estvar.filter=NULL, estvar.name=NULL, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, 
	rowgrpnm=NULL, rowgrpord=NULL, sumunits=TRUE, allin1=FALSE, estround=1, 
	pseround=2, estnull="--", psenull="--", divideby=NULL, savedata=FALSE, 
	rawdata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=TRUE, 
	overwrite=TRUE, addtitle=TRUE, returntitle=FALSE, title.main=NULL, title.ref=NULL, 
	title.rowvar=NULL, title.colvar=NULL, title.unitvar=NULL, title.estvar=NULL, 
	title.filter=NULL, GBpopdat=NULL, GBdata=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 || is.null(tree) && is.null(GBpopdat) && is.null(GBdata)) 
    gui <- TRUE 


  ## If gui.. set variables to NULL
  if (gui) { 
    landarea=strvar=areavar=sumunits=adjplot=strata=getwt=cuniqueid=ACI=
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
  esttype <- "TREE"
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
    GBpopdat <- modGBpop(tree=tree, cond=cond, plt=plt, dsn=dsn, pltassgn=pltassgn, 
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, puniqueid=puniqueid,
 	pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid, invyrs=invyrs, 
	ACI=ACI, adj=adj, plt.nonsamp.filter=plt.nonsamp.filter, 
	cond.nonsamp.filter=cond.nonsamp.filter, strata=strata, unitvar=unitvar, 
	unitvar2=unitvar2, unitarea=unitarea, areavar=areavar, unitcombine=unitcombine, 
	minplotnum.unit=minplotnum.unit, stratalut=stratalut, strvar=strvar, 
	getwt=getwt, getwtvar=getwtvar, stratcombine=stratcombine, GBdata=GBdata, gui=gui)
  } else {
    returnGBpopdat <- FALSE
    if (!is.list(GBpopdat))
      stop("GBpopdat must be a list")
    listitems <- c("condx", "pltcondx", "treex", "cuniqueid", "condid", 
		"tuniqueid", "ACI.filter", "unitarea", "unitvar", "strlut", "strvar",
		"plotsampcnt", "condsampcnt")
    if (!all(listitems %in% names(GBpopdat))) {
      items.miss <- listitems[!listitems %in% names(GBpopdat)]
      stop("invalid GBpopdat... missing items: ", paste(items.miss, collapse=", "))
    }   
  }		
  if (is.null(GBpopdat)) return(NULL)
  condx <- GBpopdat$condx
  pltcondx <- GBpopdat$pltcondx
  treex <- GBpopdat$treex
  if (is.null(treex)) stop("must include tree data for tree estimates")
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  tuniqueid <- GBpopdat$tuniqueid
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
  ### Check row and column data
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
  ### Get estimation data from tree table
  #####################################################################################
  adjtree <- ifelse(adj %in% c("samp", "plot"), TRUE, FALSE)
  treedat <- check.tree(gui=gui, treef=treef, bycond=TRUE, condf=condf, bytdom=bytdom, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, estvarn=estvar, 
		estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, adjtree=adjtree)
  if (is.null(treedat)) return(NULL) 
  tdomdat <- merge(condx, treedat$tdomdat, by=c(cuniqueid, condid))
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst

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
    tdomdat <- tdomdat[!is.na(tdomdat[[rowvar]]) & !is.na(tdomdat[[colvar]]),]
  } 


  #####################################################################################
  ### Get titles for output tables
  #####################################################################################
  alltitlelst <- check.titles(dat=tdomdat, esttype=esttype, sumunits=sumunits, 
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
    tdomdat$TOTAL <- 1
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvar.name]
#print(table(tdomdattot$STRATUMCD))
#print(dim(tdomdattot))
#print(head(tdomdattot))
#print(sum(tdomdattot[[estvar.name]]))
#print(strlut)

#test <- strlut[, c("COUNTYFIPS", "STRATUMCD", "n.strata")]
#test

    unit.totest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdattot, 
		esttype=esttype, uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, 
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
    unit.rowest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain=rowvar)

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=estvar.name]
      unit.colest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain=colvar)
 
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, grpvar), .SDcols=estvar.name]
      unit.grpest <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, 
		uniqueid=cuniqueid, strlut=strlut, unitvar=unitvar, strvar=strvar, 
		domain=grpvar)
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
      unit.grpest <- add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
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
          ordnames <- unique(c(ordnames, names(uniquecol)))
        } else {
          ordnames <- unique(c(ordnames, colvar))
        }
        if (!is.null(uniquerow))  {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			row.add0)
          ordnames <- unique(c(names(uniquerow), ordnames))
        } else {
          ordnames <- unique(c(ordnames, rowvar))
        }
      } else if (col.add0) {
        if (!is.null(uniquecol))  {
          unit.grpest[, (unitvar) := paste(get(unitvar), get(rowvar), sep="#")][, 
			(rowvar) := NULL]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, colvar, uniquecol, unitvar, 
			col.add0)
          ordnames <- unique(c(ordnames, names(uniquecol)))
        } else {
          ordnames <- unique(c(ordnames, colvar))
        }
        if (!is.null(uniquerow))  {
          unit.grpest[,(rowvar) := sapply(get(unitvar), 
			function(x){strsplit(as.character(x), "#")[[1]][2]})]
          unit.grpest[,(unitvar) := sapply(get(unitvar), 
			function(x){strsplit(as.character(x), "#")[[1]][1]})]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			add0=FALSE)
          ordnames <- unique(c(names(uniquerow), ordnames))
        } else {
          ordnames <- unique(c(rowvar, ordnames))
        }
      } else {
        if (!is.null(uniquecol)) {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, colvar, uniquecol, unitvar, 
			add0=FALSE)
          ordnames <- unique(c(ordnames, names(uniquecol)))
        } else {
          ordnames <- unique(c(ordnames, colvar))
        }
        if (!is.null(uniquerow)) {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, rowvar, uniquerow, unitvar, 
			add0=FALSE)
          ordnames <- unique(c(names(uniquerow), ordnames))
        } else {
          ordnames <- unique(c(ordnames, rowvar))
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
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=estvar.name]
    rowunit <- GBest.pbar(sumyn=estvar.name, ysum=tdomdatsum, esttype=esttype, 
			bytdom=bytdom, uniqueid=tuniqueid, strlut=strlut2, 
			unitvar="ONEUNIT", strvar=strvar, domain=rowvar)

    rowunit <- add0unit(rowunit, rowvar, uniquerow, "ONEUNIT", row.add0)
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
			bytdom=bytdom, uniqueid=tuniqueid, strlut=strlut2, 
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
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    if (!is.null(outfn)) 
      outfn.param <- paste(outfn, "parameters", sep="_")
    outparamfn <- getoutfn(outfn.param, outfolder=outfolder, 
		outfn.date=outfn.date, overwrite=overwrite, ext="txt")
  
    outfile <- file(outparamfn, "w")
    cat(  "tree = ", as.character(bquote(tree)), "\n",
      "cond = ", as.character(bquote(cond)), "\n",
      "plt = ", as.character(bquote(plt)), "\n",
      "pltassgn = ", as.character(bquote(pltassgn)), "\n",
      "dsn = \"", dsn, "\"", "\n", 
      "tuniqueid = \"", tuniqueid, "\"", "\n", 
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
      "estvar = \"", estvar, "\"", "\n",
      "estvar.filter = \"", estvar.filter, "\"", "\n",
      "estvar.name = \"", estvar.name, "\"", "\n",
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
      "title.estvar = \"", title.estvar, "\"", "\n",
      "title.filter = \"", title.filter, "\"", "\n",
      "gui = ", gui, "\n",
      "\n",
    file = outfile, sep="")

    cat(  "est <- modGBtree(tree=tree, cond=cond, plt=plt, pltassgn=pltassgn, seed=seed,
	dsn=dsn, tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, puniqueid=puniqueid,
 	pltassgnid=pltassgnid, ACI=ACI, sumunits=sumunits, adj=adj, strata=strata, 
	plt.nonsamp.filter=plt.nonsamp.filter, cond.nonsamp.filter=cond.nonsamp.filter,
 	landarea=landarea, plt.filter=plt.filter, cond.filter=cond.filter, 
	unitvar=unitvar, unitvar2=unitvar2, stratcombine=stratcombine, unitarea=unitarea, 
	areavar=areavar, stratalut=stratalut, strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
	estvar=estvar, estvar.filter=estvar.filter, estvar.name=estvar.name, rowvar=rowvar,
 	rowvar.filter=rowvar.filter, colvar=colvar, colvar.filter=colvar.filter, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, row.orderby=row.orderby, 
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, rowlut=rowlut, 
	collut=collut, rowgrp=rowgrp, rowgrpnm=NULL, rowgrpord=NULL, allin1=allin1,
 	estround=estround, pseround=pseround, estnull=estnull, psenull=psenull, 
	divideby=divideby, savedata=savedata, rawdata=rawdata, outfolder=outfolder, 
	outfn=outfn, outfn.pre=outfn.pre, outfn.date=outfn.date, overwrite=overwrite, 
	addtitle=addtitle, returntitle=returntitle, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.unitvar=title.unitvar,
 	title.estvar=title.estvar, title.filter=title.filter, gui=gui)", 
	file = outfile, sep="")
    close(outfile)

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
