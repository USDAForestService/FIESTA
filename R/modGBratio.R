modGBratio <- function(tree, cond=NULL, pltstrat=NULL, tuniqueid="PLT_CN",
	cuniqueid="PLT_CN", puniqueid="CN", sumunits=TRUE, adjsamp=TRUE, strata=TRUE, 
	ratiotype="PERACRE", landarea="FOREST", ACI=FALSE, nonsamp.filter=NULL, 
	plt.filter=NULL, cond.filter=NULL, unitvar=NULL, unitvar2=NULL, 
	autocombine=TRUE, unitarea=NULL, areavar="ACRES", stratalut=NULL, 
	strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", estvarn=NULL, 
	estvarn.filter=NULL, estvarn.name=NULL, estvard=NULL, estvard.filter=NULL, 
	estvard.name=NULL, rowvar=NULL, rowvar.filter=NULL, colvar=NULL, 
	colvar.filter=NULL, row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, 
	col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, 
	rowgrp=FALSE, allin1=FALSE, estround=3, pseround=3, divideby=NULL, 
	savedata=FALSE, rawdata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, 
	outfn.date=TRUE, overwrite=FALSE, addtitle=TRUE, returntitle=FALSE, 
	title.main=NULL, title.ref=NULL, title.rowvar=NULL, title.colvar=NULL, 
	title.unitvar=NULL, title.estvarn=NULL, title.estvard=NULL, title.filter=NULL, 
	gui=FALSE){

  ##################################################################################
  ## DESCRIPTION: 
  ## Generates per-acre or per-tree estimates by domain using ratio estimators
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 | is.null(estvarn)) gui <- TRUE

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=tdom=estvar.name <- NULL


  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea=strvar=areavar=sumunits=adjsamp=strata=getwt=cuniqueid=ACI=
	tuniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL 
    #if (!col.FIAname) col.FIAname <- NULL  
  }

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  condid <- "CONDID"
  esttype <- "RATIO"
  rowcol.total <- TRUE
  minplotnum <- 10

  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generates table of sampled/nonsampled conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  datcheck <- FIESTA::check.data(gui=gui, esttype="RATIO", tree=tree, cond=cond, 
	plt=pltstrat, tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid,
 	puniqueid=puniqueid, sumunits=sumunits, adjsamp=adjsamp, landarea=landarea, 
	ACI=ACI, unitvar=unitvar, unitvar2=unitvar2, autocombine=autocombine, 
	module="GB", strata=strata, strvar=strvar, nonsamp.filter=nonsamp.filter, 
	plt.filter=plt.filter, cond.filter=cond.filter, allin1=allin1, estround=estround, 
	pseround=pseround, divideby=divideby, savedata=savedata, addtitle=addtitle, 
	returntitle=returntitle, rawdata=rawdata, outfolder=outfolder)
  if (is.null(datcheck)) return(NULL)
  treef <- datcheck$treef
  tuniqueid <- datcheck$tuniqueid
  condx <- datcheck$condx
  condf <- datcheck$condf
  pltstratx <- datcheck$pltx
  cuniqueid <- datcheck$cuniqueid
  condid <- datcheck$condid
  puniqueid <- datcheck$puniqueid
  unitvar <- datcheck$unitvar
  unitvar2 <- datcheck$unitvar2
  auxvars <- datcheck$auxvars
  sumunits <- datcheck$sumunits
  landarea <- datcheck$landarea
  adj <- datcheck$adj
  strata <- datcheck$strata
  strvar <- datcheck$strvar
  plotsampcnt <- datcheck$plotsampcnt
  condsampcnt <- datcheck$condsampcnt
  states <- datcheck$states
  invyrs <- datcheck$invyrs
  allin1 <- datcheck$allin1
  estround <- datcheck$estround
  pseround <- datcheck$pseround
  divideby <- datcheck$divideby
  savedata <- datcheck$savedata
  addtitle <- datcheck$addtitle
  returntitle <- datcheck$returntitle
  rawdata <- datcheck$rawdata
  outfolder <- datcheck$outfolder
  rm(datcheck)


  ###################################################################################
  ## CHECK unitarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and area by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  unitvars <- c(unitvar, unitvar2)
  unitdat <- FIESTA::check.unitarea(unitarea=unitarea, pltx=pltstratx, 
	unitvars=unitvars, areavar=areavar, gui=gui)
  unitarea <- unitdat$unitarea
  areavar <- unitdat$areavar
  if (sumunits && nrow(unitarea) == 1) sumunits <- FALSE 


  ###################################################################################
  ## CHECK STRATA
  ## Check number of plots by strata and estimation unit
  ## Gets strata weights
  ###################################################################################
  ## Returns: 
  ## - condx and pltstratx with strvar, strvar
  ## - strlut with strata weights (strwt)
  ## If strata=FALSE, a variable, ONESTRAT=1 is added to pltstratx 
  ##		and strlut is created with ONESTRAT=1 and strwt=1.
  ###################################################################################
  if (strata) {
    stratcheck <- FIESTA::check.strata(pltstratx=pltstratx, puniqueid=puniqueid, 
		stratalut=stratalut, strvar=strvar, unitarea=unitarea, unitvar=unitvar, 
		unitvar2=unitvar2, unitvars=unitvars, areavar=areavar, getwt=getwt, 
		getwtvar=getwtvar, sumunits=sumunits, autocombine=autocombine, gui=gui,
 		minplotnum=minplotnum)
    if (autocombine) autocombinelut <- stratcheck$unitstrgrplut
  } else {
    stratcheck <- FIESTA::check.nostrata(pltstratx=pltstratx, puniqueid=puniqueid, 
		unitvars=unitvars, autocombine=autocombine, unitarea=unitarea, 
		unitvar=unitvar, areavar=areavar, minplotnum=minplotnum)
  }
  pltstratx <- stratcheck$pltstratx
  unitarea <- stratcheck$unitarea
  strlut <- stratcheck$strlut
  strvars <- stratcheck$strvars
  if (autocombine) autocombinelut <- stratcheck$unitstrgrplut


  ## If more than one unitvar, concatenate into 1 unitvar
  if (length(unitvars) > 1) {
    unitarea[[unitvar]] <- paste(unitarea[[unitvar2]], unitarea[[unitvar]], sep="-")
    strlut[[unitvar]] <- paste(strlut[[unitvar2]], strlut[[unitvar]], sep="-")
    pltstratx[[unitvar]] <- paste(pltstratx[[unitvar2]], pltstratx[[unitvar]], sep="-")
    unitvars <- unitvar
  }

  ## Set key to strlut and unitarea
  strunitvars <- c(unitvar, strvars)
  setkeyv(strlut, strunitvars)
  setkeyv(unitarea, unitvar)


  ###################################################################################
  ## GET ADJUSTMENT FACTORS BY STRATA AND/OR ESTIMATION UNIT FOR NONSAMPLED CONDITIONS
  ## Calculates adjustment factors for area and trees by strata (and estimation unit)
  ##		to account for nonsampled plots and conditions.
  ## Creates an adjusted condition proportion by merging strata-level adjustment
  ##		factors to cond and dividing CONDPROP_UNADJ by adjustment factor.
  ###################################################################################
  ## Returns:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC)  
  ##     by strata and estunit (*PROP_UNADJ_SUM / n.strata)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ###################################################################################
  ## Merge plot strata info to condx
  keycondx <- key(condx)
  condx <- condx[pltstratx[,c(puniqueid, strunitvars), with=FALSE]]
  setkeyv(condx, keycondx)

  if (adj == "samp") {
    adjfacdata <- FIESTA::getadjfactorGB(esttype=esttype, treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, unitlut=strlut, 
		unitvars=unitvars, strvars=strvars, unitarea=unitarea, areavar=areavar, 
		cvars2keep=cvars2keep)
    condx <- adjfacdata$condx
    strlut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    expcondtab <- adjfacdata$expcondtab
    cvars2keep <- adjfacdata$cvars2keep
  } 


  ###################################################################################
  ### GET ROW AND COLUMN INFO
  ###################################################################################
  rowcolinfo <- check.rowcol(gui=gui, esttype=esttype, treef=treef, 
	condf=condf, cuniqueid=cuniqueid, tuniqueid=tuniqueid, rowvar=rowvar, 
	rowvar.filter=rowvar.filter, colvar=colvar, colvar.filter=colvar.filter, 
	row.FIAname=row.FIAname, col.FIAname=col.FIAname, 
	row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0, 
	col.add0=col.add0, title.rowvar=title.rowvar, title.colvar=title.colvar, 
	rowlut=rowlut, collut=collut, rowgrp=rowgrp, landarea=landarea)
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
  domain <- rowcolinfo$domain
  dom.orderby <- rowcolinfo$dom.orderby
  tdomvar <- rowcolinfo$tdomvar
  rm(rowcolinfo)

  if (rowvar == "TOTAL") rowcol.total <- TRUE

  ## Generate a uniquecol for estimation units
  if (!sumunits & colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }


  ## Merge filtered condition data (condf) to all conditions (condx)
  #####################################################################################
  condall <- condx[condf]

  #####################################################################################
  ### GET ESTIMATION DATA FROM TREE TABLE (NUMERATOR/DENOMINATOR)
  #####################################################################################
  treedat <- check.tree(gui=gui, treef=treef, condall=condall, 
	bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype,
	ratiotype=ratiotype, estvarn=estvarn, estvarn.filter=estvarn.filter,
	estvarn.name=estvarn.name, estvard=estvard, estvard.filter=estvard.filter,
 	estvard.name=estvard.name, esttotn=TRUE, esttotd=TRUE, tdomvar=tdomvar, 
	adjtree=adjsamp)
  if (is.null(treedat)) return(NULL)

  tdomdat <- setDT(treedat$tdomdat)
  estvarn <- treedat$estvarn
  estvarn.name <- treedat$estvarn.name
  estvarn.filter <- treedat$estvarn.filter
  tdomvarlstn <- treedat$tdomvarlstn

  if (ratiotype == "PERTREE") {
    estvard <- treedat$estvard
    estvard.name <- treedat$estvard.name
    tdomvarlstd <- treedat$tdomvarlstd
  } else {
    estvard.name <- "CONDPROP_ADJ"
    tdomvarlstd <- NULL
  }

  if (rowcol.total) {
    if (bytdom && rowvar %in% names(treef) && colvar %in% names(treef)) {
      row.tdomvar <- strsplit(tdomvar, "#")[[1]][1]
      row.treedat <- check.tree(gui=gui, treef=treef, condall=condall, 
		bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, 			
		ratiotype=ratiotype, estvarn=estvarn, estvarn.filter=estvarn.filter,
 		estvard=estvard, estvard.filter=estvard.filter, tdomvar=row.tdomvar, 
 		esttotn=TRUE, esttotd=TRUE, adjtree=adjsamp)
      row.tdomdat <- setDT(row.treedat$tdomdat)
      row.tdomvarlstn <- row.treedat$tdomvarlstn
      if (ratiotype == "PERTREE") {
        row.tdomvarlstd <- row.treedat$tdomvarlstd
      } else {
        row.tdomvarlstd <- NULL
      }


      col.tdomvar <- strsplit(tdomvar, "#")[[1]][2]
      col.treedat <- check.tree(gui=gui, treef=treef, condall=condall, 
		bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype,
 		ratiotype=ratiotype, estvarn=estvarn, estvarn.filter=estvarn.filter,
 		estvard=estvard, estvard.filter=estvard.filter, tdomvar=col.tdomvar,
 		esttotn=TRUE, esttotd=TRUE, adjtree=adjsamp)
      col.tdomdat <- setDT(col.treedat$tdomdat)
      col.tdomvarlstn <- col.treedat$tdomvarlstn
      if (ratiotype == "PERTREE") {
        col.tdomvarlstd <- col.treedat$tdomvarlstd
      } else {
        col.tdomvarlstd <- NULL
      }
    }
  }


  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=condall, esttype=esttype, ratiotype=ratiotype,
 	sumunits=sumunits, title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.unitvar,
 	title.filter=title.filter, title.estvarn=title.estvarn, unitvar=unitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvarn, estvarn.filter=estvarn.filter, 
	estvard=estvard, estvard.filter=estvard.filter, addtitle=addtitle, rawdata=rawdata,
 	states=states, invyrs=invyrs, landarea=landarea, plt.filter=plt.filter, 
	cond.filter=cond.filter, allin1=allin1, divideby=divideby, outfn=outfn, 
	outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if(rawdata) outfn.rawdat <- alltitlelst$outfn.rawdat


  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit.totest=unit.tdomest=unit.grpest=unit.rowest=unit.colest=unit.grpest=
	rowunit=totunit <- NULL
  if (is.null(domain)) domain <- "TOTAL" 


  ## Get estimates for tree domain by domain and total tree domain
  domvar2 <- NULL
  grpvar <- NULL

  ## Note: tdomdat is the summed response by condition (not domain)
  if (rowcol.total) {
    ## Get estimate for total

    ## STRATA/PLOT/DOMAIN LEVEL: Aggregate response by strata/plot/domain
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]
    unit.totest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, ysum=tdomdattot, 
		esttype=esttype, ratiotype=ratiotype, bytdom=bytdom, strlut=strlut, 
		uniqueid=cuniqueid, strunitvars=strunitvars, unitvars=unitvars, 
		unitvar=unitvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unit.totest, unitarea, unitvar)
    unit.totest <- tabs$tab1
    unitarea <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- FIESTA::getarea(unit.totest, areavar=areavar, esttype=esttype)
  }

  ## Get row, column, cell estimate and merge area if row or column in tree table 
  if (bytdom) { 
    tdomvarlstn2 <- c(sort(tdomvarlstn), estvarn.name)
    tdomvarlstd2 <- c(sort(tdomvarlstd), estvard.name)

    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, domain), .SDcols=c(tdomvarlstn2, tdomvarlstd2)]
    unit.tdomest <- do.call(rbind, lapply(tdomvarlstn2, GBest.pbar, 
		ysum=tdomdatsum, sumyd=tdomvarlstd2, esttype=esttype, ratiotype=ratiotype, 
		bytdom=bytdom, uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, 
		unitvars=unitvars, unitvar=unitvar, domain=domain))
    if ("TOTAL" %in% names(unit.tdomest)) unit.tdomest[, TOTAL := NULL]

    if (domain != "TOTAL") {
      tdomdattotn <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=c(tdomvarlstn2, tdomvarlstd2)]

      if (domain == rowvar) {
        unit.rowest <- unit.tdomest[tdom == estvarn.name,]

        unit.colest <- do.call(rbind, lapply(tdomvarlstn, GBest.pbar, 
		ysum=tdomdattotn, sumyd=tdomvarlstd2, esttype=esttype, ratiotype=ratiotype,
		bytdom=bytdom, uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, 
		unitvars=unitvars, unitvar=unitvar, domain="TOTAL"))
        setnames(unit.colest, "tdom", colvar)

        unit.grpest <- unit.tdomest[unit.tdomest$tdom != estvarn.name,] 
        setnames(unit.grpest, "tdom", colvar)
        tdomvar <- colvar

      } else {
        unit.colest <- unit.tdomest[tdom == estvarn.name,]

        unit.rowest <- do.call(rbind, lapply(tdomvarlstn, GBest.pbar, 
		ysum=tdomdattotn, sumyd=tdomvarlstd2, esttype=esttype, ratiotype=ratiotype, 
		bytdom=bytdom, uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, 
		unitvars=unitvars, unitvar=unitvar, domain="TOTAL"))
        setnames(unit.rowest, 'tdom', rowvar)

        unit.grpest <- subset(unit.tdomest, tdom != estvarn.name) 
        setnames(unit.grpest, 'tdom', rowvar)
        tdomvar <- rowvar
      }
    } else {  ## domain == "TOTAL"  
      if (colvar != "NONE") {
        row.tdomvarlstn2 <- c(sort(row.tdomvarlstn), estvarn.name)
        row.tdomvarlstd2 <- c(sort(row.tdomvarlstd), estvard.name)
        col.tdomvarlstn2 <- c(sort(col.tdomvarlstn), estvarn.name)
        col.tdomvarlstd2 <- c(sort(col.tdomvarlstd), estvard.name)

        row.tdomdatsum <- row.tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, domain), .SDcols=c(row.tdomvarlstn2, row.tdomvarlstd2)]
        col.tdomdatsum <- col.tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, domain), .SDcols=c(col.tdomvarlstn2, col.tdomvarlstd2)]

        if (rowcol.total) {
          unit.rowest <- do.call(rbind, lapply(row.tdomvarlstn, GBest.pbar, 
			ysum=row.tdomdatsum, sumyd=row.tdomvarlstd2, esttype=esttype, ratiotype=ratiotype,
			bytdom=bytdom, uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, 
			unitvars=unitvars, unitvar=unitvar, domain=domain))
          setnames(unit.rowest, "tdom", rowvar)

          unit.colest <- do.call(rbind, lapply(col.tdomvarlstn, GBest.pbar, 
			ysum=col.tdomdatsum, sumyd=col.tdomvarlstd2, esttype=esttype, ratiotype=ratiotype,
			bytdom=bytdom, uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, 
			unitvars=unitvars, unitvar=unitvar, domain=domain))
          setnames(unit.colest, "tdom", colvar)
        }

        unit.grpest <- unit.tdomest
        setnames(unit.grpest, "tdom", tdomvar)
        domvar2 <- colvar
        grpvar <- tdomvar

      } else {
        
        unit.rowest <- subset(unit.tdomest, tdom != estvarn.name) 
        setnames(unit.rowest, 'tdom', rowvar)
      }
    } 
  
  } else {  ## bytdom=FALSE
    ## Get row, column, cell estimate and merge area if row or column in cond table 
    if (domain != "TOTAL") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, domain), .SDcols=c(estvarn.name, estvard.name)]

      unit.domest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, esttype=esttype, ratiotype=ratiotype, bytdom=bytdom, 
		uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, unitvars=unitvars,
 		unitvar=unitvar, domain=domain)

      if (colvar != "NONE") {
        unit.grpest <- unit.domest

        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]
        unit.rowest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, esttype=esttype, ratiotype=ratiotype, bytdom=bytdom, 
		uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, unitvars=unitvars,
 		unitvar=unitvar, domain=rowvar)

        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=c(estvarn.name, estvard.name)]
        unit.colest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, esttype=esttype, ratiotype=ratiotype, bytdom=bytdom, 
		uniqueid=cuniqueid, strlut=strlut, strunitvars=strunitvars, unitvars=unitvars,
 		unitvar=unitvar, domain=colvar)

        domvar2 <- colvar
        grpvar <- domain

      } else {
        unit.rowest <- unit.domest
      }
    }
  }

  ###################################################################################
  ## Check add0 and Add area
  ###################################################################################
  if (!sumunits && nrow(unitarea) > 1) col.add0 <- TRUE
  if (!is.null(unit.rowest)) {
    unit.rowest <- FIESTA::add0unit(unit.rowest, rowvar, uniquerow, unitvar, row.add0)
    tabs <- FIESTA::check.matchclass(unit.rowest, unitarea, unitvar)
    unit.rowest <- tabs$tab1
    unitarea <- tabs$tab2
    setkeyv(unit.rowest, unitvar)
    unit.rowest <- unit.rowest[unitarea, nomatch=0]
    unit.rowest <- FIESTA::getarea(unit.rowest, areavar=areavar, esttype=esttype)
    setkeyv(unit.rowest, c(unitvar, rowvar))
  }

  if (!is.null(unit.colest)) {
    unit.colest <- FIESTA::add0unit(x=unit.colest, colvar, uniquecol, unitvar, col.add0)
    tabs <- FIESTA::check.matchclass(unit.colest, unitarea, unitvar)
    unit.colest <- tabs$tab1
    unitarea <- tabs$tab2
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
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=colvar, 
			uniquex=uniquecol, unitvar=unitvar, add0=FALSE)
          ordnames <- c(ordnames, names(uniquecol))
        } else {
          ordnames <- c(ordnames, colvar)
        }
        if (!is.null(uniquerow))  {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=rowvar, 
			uniquex=uniquerow, unitvar=unitvar, row.add0)
          ordnames <- c(names(uniquerow), ordnames)
        } else {
          ordnames <- c(ordnames, rowvar)
        }
      } else if (col.add0) {
        if (!is.null(uniquecol))  {
          unit.grpest[, (unitvar) := paste(get(unitvar), get(rowvar), sep="#")][, 
			(rowvar) := NULL]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=colvar, uniquex=uniquecol, 
			unitvar=unitvar, add0=col.add0)
          ordnames <- c(ordnames, names(uniquecol))
        } else {
          ordnames <- c(ordnames, colvar)
        }
        if (!is.null(uniquerow))  {
          unit.grpest[, c(unitvar, rowvar) := tstrsplit(get(unitvar), "#", fixed=TRUE)]
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=rowvar, 
			uniquex=uniquerow, unitvar=unitvar, add0=FALSE)
          ordnames <- c(names(uniquerow), ordnames)
        } else {
          ordnames <- c(rowvar, ordnames)
        }
      } else {
        if (!is.null(uniquecol)) {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=colvar, uniquex=uniquecol, 
			unitvar=unitvar, add0=FALSE)
          ordnames <- c(ordnames, names(uniquecol))
        } else {
          ordnames <- c(ordnames, colvar)
        }
        if (!is.null(uniquerow)) {
          unit.grpest <- FIESTA::add0unit(x=unit.grpest, xvar=rowvar, uniquex=uniquerow, 
			unitvar=unitvar, add0=FALSE)
          ordnames <- c(names(uniquerow), ordnames)
        } else {
          ordnames <- c(ordnames, rowvar)
        }
      }
      ordnames <- c(unitvar, ordnames)
      setcolorder(unit.grpest, 
		c(ordnames, names(unit.grpest)[!names(unit.grpest) %in% ordnames]))
    }
    tabs <- FIESTA::check.matchclass(unit.grpest, unitarea, unitvar)
    unit.grpest <- tabs$tab1
    unitarea <- tabs$tab2
    setkeyv(unit.grpest, unitvar)
    unit.grpest <- unit.grpest[unitarea, nomatch=0]
    unit.grpest <- FIESTA::getarea(unit.grpest, areavar=areavar, esttype=esttype)
    setkeyv(unit.grpest, c(unitvar, rowvar, colvar))
  }

print("TEST1")
  if (rowcol.total) {

  ## For sumunits=FALSE, get estimation unit totals
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && (!bytdom && domain != "TOTAL" || bytdom))) {
    ## AGGREGATE UNIT strlut FOR ROWVAR and GRAND TOTAL
    strlut2 <- data.table(strlut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvars)
    if (!getwtvar %in% names(strlut2)) getwtvar <- "strwt"

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

    ## CALCULATE UNIT TOTALS FOR ROWVAR
    if (bytdom) {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, domain), .SDcols=c(tdomvarlstn2, tdomvarlstd2)]

      rowunit <- do.call(rbind, lapply(tdomvarlstn, GBest.pbar, 
		sumyd=estvard.name, ysum=tdomdatsum, esttype=esttype, bytdom=bytdom, 
		uniqueid=tuniqueid, strlut=strlut2, strunitvars=strunitvars2, 
		unitvars="ONEUNIT", unitvar="ONEUNIT", domain="TOTAL"))
      setnames(rowunit, "tdom", rowvar)
    } else {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]

      rowunit <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, ysum=tdomdatsum, 
		esttype=esttype, bytdom=bytdom, uniqueid=tuniqueid, strlut=strlut2, 
		strunitvars=strunitvars2, unitvars="ONEUNIT", unitvar="ONEUNIT", 
		domain=rowvar)
    }
    rowunit <- FIESTA::add0unit(rowunit, rowvar, uniquerow, "ONEUNIT", row.add0)
    tabs <- FIESTA::check.matchclass(rowunit, unitacres2, "ONEUNIT")
    rowunit <- tabs$tab1
    unitacres2 <- tabs$tab2
    setkey(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    rowunit <- FIESTA::getarea(rowunit, areavar=areavar, esttype=esttype)
    setkeyv(rowunit, c("ONEUNIT", rowvar))
print("TEST2")

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]

    totunit <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, ysum=tdomdatsum, 
		esttype=esttype, bytdom=bytdom, uniqueid=tuniqueid, strlut=strlut2, 
		strunitvars=strunitvars2, unitvars="ONEUNIT", unitvar="ONEUNIT", 
		domain="TOTAL")
    tabs <- FIESTA::check.matchclass(totunit, unitacres2, "ONEUNIT")
    totunit <- tabs$tab1
    unitacres2 <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitacres2, nomatch=0]
    totunit <- FIESTA::getarea(totunit, areavar=areavar, esttype=esttype)
  }          

  }
print("TEST3")

  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################

  if (rawdata) {
    rawdat <- list()
    if (!is.null(plotsampcnt)) rawdat$plotsampcnt <- plotsampcnt
    if (!is.null(condsampcnt)) rawdat$condsampcnt <- setDF(condsampcnt)
    rawdat$unitarea <- unitarea
    if (!is.null(strlut)) rawdat$stratdat <- setDF(strlut)
    if (autocombine && !is.null(autocombinelut)) rawdat$autocombinelut <- autocombinelut
    if (adjsamp && !is.null(expcondtab)) rawdat$expcondtab <- setDF(expcondtab) 
    rawdat$tdomdat <- setDF(tdomdat) 
  }

  estnm <- "estn"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, unitvar=unitvar, 
	unitvar2=unitvar2, unit.totest=unit.totest, unit.rowest=unit.rowest, 
	unit.colest=unit.colest, unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	overwrite=overwrite, outfn.date=outfn.date, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, rawdat=rawdat, returntitle=returntitle) 

  est2return <- tabs$tabest
  pse2return <- tabs$tabpse
  rawdat <- tabs$rawdat
  titlelst <- tabs$titlelst


  if (savedata) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    if(is.null(outfn)){
      outparamfnbase <- paste(outfn.param, format(Sys.time(), "%Y%m%d"), sep="_")
    }else{
      outparamfnbase <- paste0(outfn, "_parameters_", format(Sys.time(), "%Y%m%d"))
    }
    outparamfn <- fileexistsnm(outfolder, outparamfnbase, "txt")

    outfile <- file(paste(outfolder, "/", outparamfn, ".txt", sep=""), "w")
    cat(  "tree = ", as.character(bquote(tree)), "\n",
      "cond = ", as.character(bquote(cond)), "\n",
      "pltstrat = ", as.character(bquote(pltstrat)), "\n",
      "seed = ", as.character(bquote(seed)), "\n",
      "tuniqueid = \"", tuniqueid, "\"", "\n", 
      "cuniqueid = \"", cuniqueid, "\"", "\n", 
      "puniqueid = \"", puniqueid, "\"", "\n",
      "sumunits = ", sumunits, "\n",
      "adjsamp = ", adjsamp, "\n",
      "strata = ", strata, "\n",
      "ratiotype = ", ratiotype, "\n",
      "landarea = \"", landarea, "\"", "\n",
      "ACI = ", ACI, "\n",
      "nonsamp.filter = \"", nonsamp.filter, "\"", "\n",
      "plt.filter = \"", plt.filter, "\"", "\n",
      "cond.filter = \"", cond.filter, "\"", "\n",
      "unitvar = \"", unitvar, "\"", "\n",
      "unitarea = ", as.character(bquote(unitarea)), "\n",
      "areavar = \"", areavar, "\"", "\n",
      "autocombine = ", autocombine, "\n",
      "stratalut = ", as.character(bquote(stratalut)), "\n", 
      "strvar = \"", strvar, "\"", "\n",
      "getwt = ", getwt, "\n",
      "getwtvar = \"", getwtvar, "\"", "\n",
      "estvarn = \"", estvarn, "\"", "\n",
      "estvarn.filter = \"", estvarn.filter, "\"", "\n",
      "estvarn.name = \"", estvarn.name, "\"", "\n",
      "estvard = \"", estvard, "\"", "\n",
      "estvard.filter = \"", estvard.filter, "\"", "\n",
      "estvard.name = \"", estvard.name, "\"", "\n",
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
      "returntitle= ", returntitle, "\n",
      "title.main = \"", title.main, "\"", "\n",
      "title.ref = \"", title.ref, "\"", "\n",
      "title.rowvar = \"", title.rowvar, "\"", "\n",
      "title.colvar = \"", title.colvar, "\"", "\n",
      "title.unitvar = \"", title.unitvar, "\"", "\n",
      "title.estvarn = \"", title.estvarn, "\"", "\n",
      "title.estvard = \"", title.estvard, "\"", "\n",
      "title.filter = \"", title.filter, "\"", "\n",
      "gui = ", gui, "\n",
      "\n",
    file = outfile, sep="")

    cat(  "est <- modGBratio(tree=tree, cond=cond, pltstrat=pltstrat, seed=seed,
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, sumunits=sumunits, ACI=ACI,
	adjsamp=adjsamp, strata=strata, ratiotype=ratiotype, landarea=landarea, ACI=ACI,
	nonsamp.filter=nonsamp.filter, plt.filter=plt.filter, cond.filter=cond.filter, 
	unitvar=unitvar, unitcares=unitarea, areavar=areavar, autocombine=autocombine,
 	stratalut=stratalut, strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
	estvarn=estvarn, estvarn.filter=estvarn.filter, estvarn.name=estvarn.name, 
	estvard=estvard, estvard.filter=estvard.filter, estvard.name=estvard.name, 
	rowvar=rowvar, rowvar.filter=rowvar.filter, colvar=colvar, 
	colvar.filter=colvar.filter, row.FIAname=row.FIAname, col.FIAname=col.FIAname,
 	row.orderby=row.orderby, col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0,
	rowlut=rowlut, collut=collut, rowgrp=rowgrp, allin1=allin1, estround=estround, 
	pseround=pseround, divideby=divideby, savedata=savedata, rawdata=rawdata, 
	outfolder=outfolder, outfn=outfn, outfn.pre=outfn.pre, outfn.date=outfn.date, 
	overwrite=overwrite, addtitle=addtitle, returntitle=returntitle, title.main=title.main,
 	title.ref=title.ref, title.rowvar=title.rowvar, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.estvar=title.estvar, title.filter=title.filter, 
	gui=gui)",
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
        outfn.rawtab <- paste(outfn.rawdat, tabnm, sep="_") 
        if (tabnm %in% c("plotsampcnt", "condsampcnt", "stratdat", "autocombinelut")) {
          write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite)
        } else {
          suppressWarnings(save1tab(estpse=rawtab, title.estpse=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn.estpse=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
        }
      }
    }
  }  


  ## GET VALUES TO RETURN
#  if (is.null(pse2return) & !rawdata & !returntitle) { 
#    return(est2return)

#  } else {
    returnlst <- list(est=est2return)
    if (!is.null(pse2return)) returnlst$pse <- pse2return
    if (rawdata) returnlst$raw <- rawdat
    if(returntitle) returnlst$titlelst <- titlelst    
#  }
    
  return(returnlst)
}
