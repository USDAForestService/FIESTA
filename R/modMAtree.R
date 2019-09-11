modMAtree <- function(tree, cond=NULL, pltmodel=NULL, seed=NULL, tuniqueid="PLT_CN", 
	cuniqueid="PLT_CN", puniqueid="CN", sumunits=FALSE, adj="samp", MAmethod="GREG", 
	landarea="ALL", ACI=FALSE, nonsamp.filter=NULL, plt.filter=NULL, cond.filter=NULL, 
	unitvar=NULL, unitvar2=NULL, autocombine=FALSE, unitarea=NULL, areavar="ACRES", 
	unitlut=NULL, npixelvar=NULL, prednames=NULL, predfac=NULL, PSstrvar=NULL, 
	estvar=NULL, estvar.filter=NULL, estvar.name=NULL, rowvar=NULL, rowvar.filter=NULL, 
	colvar=NULL, colvar.filter=NULL, row.FIAname=FALSE, col.FIAname=FALSE, 
	row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, rowlut=NULL, 
	collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, allin1=FALSE, 
	estround=1, pseround=2, estnull=0, psenull="--", divideby=NULL, savedata=FALSE,
 	rawdata=FALSE, outfolder=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=TRUE,
 	overwrite=FALSE, addtitle=TRUE, returntitle=FALSE, title.main=NULL, title.ref=NULL, 
	title.rowvar=NULL, title.colvar=NULL, title.unitvar=NULL, title.estvar=NULL,
 	title.filter=NULL, gui=FALSE){

  ########################################################################################
  ## DESCRIPTION: 
  ## Generates model-assisted estimates by domain (and estimation unit)
  ######################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 | is.null(cond)) gui <- TRUE 

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=char.width <- NULL
 

  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################
 
  ## If gui.. set variables to NULL
  if (gui) { 
    tree=landarea=PSstrvar=areavar <- NULL
    if (!row.FIAname) row.FIAname <- NULL
    if (!col.FIAname) col.FIAname <- NULL
  }

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  condid <- "CONDID"
  esttype <- "TREE"
  minplotnum <- 10
  title.rowgrp=NULL

  if (!"mase" %in% rownames(installed.packages()))
    stop("modMAtree function requires package mase")
  

  ## Check adjustment for nonsampled conditions
  ########################################################
  adjlst <- c("samp", "plot")
  adjsamp=adjplot <- FALSE
  adj <- FIESTA::pcheck.varchar(var2check=adj, varnm="adj", gui=gui, 
		checklst=adjlst, caption="Adjustment?", stopifnull=TRUE)
  if (adj == "samp") {
    adjsamp <- TRUE
  } else if (adj == "plot") {
    adjplot <- TRUE
  } 

  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generates table of sampled/nonsampled conditions
  ## Remove nonsampled conditions (e.g., COND_STATUS_CD = 5)
  ###################################################################################
  datcheck <- check.data(gui=gui, esttype="TREE", module="MA", method=MAmethod, 
	tree=tree, cond=cond, plt=pltmodel, tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
	condid=condid, puniqueid=puniqueid, unitvar=unitvar, unitvar2=unitvar2, 
	autocombine=autocombine, prednames=prednames, predfac=predfac, sumunits=sumunits,
 	adjplot=adjplot, adjsamp=adjsamp, strvar=PSstrvar, landarea=landarea, ACI=ACI, 
	nonsamp.filter=nonsamp.filter, plt.filter=plt.filter, cond.filter=cond.filter, 
	allin1=allin1, estround=estround, pseround=pseround, divideby=divideby, 
	savedata=savedata, addtitle=addtitle, returntitle=returntitle, rawdata=rawdata,
 	outfolder=outfolder)
  treef <- datcheck$treef
  tuniqueid <- datcheck$tuniqueid
  condx <- datcheck$condx
  condf <- datcheck$condf
  pltmodelx <- datcheck$pltx
  cuniqueid <- datcheck$cuniqueid
  condid <- datcheck$condid
  puniqueid <- datcheck$puniqueid
  unitvar <- datcheck$unitvar
  unitvar2 <- datcheck$unitvar2
  prednames <- datcheck$prednames
  predfac <- datcheck$predfac
  adj <- datcheck$adj
  strata <- datcheck$strata
  PSstrvar <- datcheck$strvar
  plotsampcnt <- datcheck$plotsampcnt
  condsampcnt <- datcheck$condsampcnt
  invyrs <- datcheck$invyrs
  allin1 <- datcheck$allin1
  estround <- datcheck$estround
  pseround <- datcheck$pseround
  divideby <- datcheck$divideby
  savedata <- datcheck$savedata
  addtitle <- datcheck$addtitle
  returntitle <- datcheck$returntitle
  add0 <- datcheck$add0
  rawdata <- datcheck$rawdata
  outfolder <- datcheck$outfolder
  MAmethod <- datcheck$method
  #rm(datcheck)


  ###################################################################################
  ## CHECK unitarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and acres by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  unitvars <- c(unitvar, unitvar2)
  unitdat <- check.unitarea(unitarea=unitarea, pltx=pltmodelx, 
		unitvars=unitvars, areavar=areavar, gui=gui)
  unitarea <- unitdat$unitarea
  areavar <- unitdat$areavar


  ###################################################################################
  ## Check number of plots by estimation unit
  ##  (including partially sampled plots - COND_STATUS_CD=5) 
  ###################################################################################
  ## If < 2 plots, an error occurs, must collapse plots.
  ## If 2-10 plots, a warning is displayed, suggesting to collapse plots. 
  ## Returns:
  ## - unitlut including total number of plots by estimation unit (n.total)
  ###################################################################################

  ## If more than one unitvar, concatenate into 1 unitvar
  if (length(unitvars) > 1) {
    unitarea[[unitvar]] <- paste(unitarea[[unitvar2]], unitarea[[unitvar]], sep="-")
    pltmodelx[[unitvar]] <- paste(pltmodelx[[unitvar2]], pltmodelx[[unitvar]], sep="-")
    unitvars <- unitvar
  }
  setkeyv(unitarea, unitvar)

  unitcheck <- check.auxiliary(pltmodelx, puniqueid, auxlut=unitlut, 
			prednames=prednames, PSstrvar=PSstrvar, predfac=predfac, 
			unitvars=unitvars) 
  pltmodelx <- unitcheck$pltmodelx
  unitlut <- unitcheck$auxlut
  npixels <- unitcheck$npixels

  ###################################################################################
  ## Calculate adjustment factors by estimation unit for area and trees to
  ##		account for nonsampled conditions.
  ## Returns:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC)  
  ##     by unitvar(s) (*PROP_UNADJ_SUM / n.total)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ###################################################################################
  adjtree <- FALSE
  if (adj == "samp") {
    bycond <- TRUE
    adjtree <- TRUE

    ## Merge plot strata info to condx
    condx <- condx[pltmodelx[,c(puniqueid, unitvars, PSstrvar), with=FALSE]]

    adjfacdata <- getadjfactorGB(esttype=esttype, treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, unitlut=unitlut, 
		unitvars=unitvars, strvars=PSstrvar, unitarea=unitarea, areavar=areavar,
 		cvars2keep=cvars2keep)
    condx <- adjfacdata$condx
    unitlut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    expcondtab <- adjfacdata$expcondtab
    cvars2keep <- adjfacdata$cvars2keep
  } else if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE

    ## Merge plot strata info to condx
    condx <- condx[pltmodelx[,c(puniqueid, unitvars), with=FALSE]]

    adjfacdata <- FIESTA::getadjfactorPLOT(esttype=esttype, treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
  }


  ###################################################################################
  ### GET ROW AND COLUMN INFO FROM condf
  ###################################################################################
  if (!sumunits) col.add0 <- TRUE
  rowcolinfo <- FIESTA::check.rowcol(gui=gui, esttype=esttype, treef=treef, 
	condf=condf, cuniqueid=cuniqueid, rowvar=rowvar, rowvar.filter=rowvar.filter, 
	colvar=colvar, colvar.filter=colvar.filter, row.FIAname=row.FIAname, 
	col.FIAname=col.FIAname, row.orderby=row.orderby, col.orderby=col.orderby,
 	row.add0=row.add0, col.add0=col.add0, title.rowvar=title.rowvar, 
	title.colvar=title.colvar, rowlut=rowlut, collut=collut, rowgrp=rowgrp, 
	rowgrpnm=rowgrpnm, rowgrpord=rowgrpord, landarea=landarea) 
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
  bytdom <- rowcolinfo$bytdom
  domain <- rowcolinfo$domain
  dom.orderby <- rowcolinfo$dom.orderby
  tdomvar <- rowcolinfo$tdomvar
  rm(rowcolinfo)  

  ## Generate a uniquecol for estimation units
  if (!sumunits && colvar == "NONE") {
    uniquecol <- data.table(unitarea[[unitvar]])
    setnames(uniquecol, unitvar)
    uniquecol[[unitvar]] <- factor(uniquecol[[unitvar]])
  }
  

  ## Merge filtered condition data (condf) to all conditions (condx)
  #####################################################################################
  condall <- condx[condf]

  #####################################################################################
  ### GET ESTIMATION DATA FROM TREE TABLE
  #####################################################################################
  treedat <- check.tree(gui=gui, treef=treef, bycond=bycond, condall=condall, 
	plt=pltmodelx, bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
	puniqueid=puniqueid, esttype=esttype, estvarn=estvar, 
	estvarn.filter=estvar.filter, esttotn=TRUE, tdomvar=tdomvar, adjtree=adjtree)
  if (is.null(treedat)) return(NULL)
 
  tdomdat <- setDT(treedat$tdomdat)
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst

  if (bytdom && rowvar %in% names(treef) && colvar %in% names(treef)) {
    row.tdomvar <- strsplit(tdomvar, "#")[[1]][1]
    row.treedat <- FIESTA::check.tree(gui=gui, treef=treef, condall=condall, 
		bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, 
		estvarn=estvar, estvarn.filter=estvar.filter, esttotn=FALSE, 
		tdomvar=row.tdomvar, adjsamp=adjplot)
    row.tdomdat <- setDT(row.treedat$tdomdat)
    row.tdomvarlst <- row.treedat$tdomvarlst

    col.tdomvar <- strsplit(tdomvar, "#")[[1]][2]
    col.treedat <- FIESTA::check.tree(gui=gui, treef=treef, condall=condall, 
		bytdom=bytdom, tuniqueid=tuniqueid, cuniqueid=cuniqueid, esttype=esttype, 
		estvarn=estvar, estvarn.filter=estvar.filter, esttotn=FALSE, 
		tdomvar=col.tdomvar, adjsamp=adjplot)
    col.tdomdat <- setDT(col.treedat$tdomdat)
    col.tdomvarlst <- col.treedat$tdomvarlst
  }

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=condall, esttype=esttype, sumunits=sumunits, 
 	title.main=title.main, title.ref=title.ref, title.rowvar=title.rowvar,
 	title.rowgrp=title.rowgrp, title.colvar=title.colvar, title.unitvar=title.unitvar,
	title.filter=title.filter, title.estvarn=title.estvar, unitvar=unitvar, 
	rowvar=rowvar, colvar=colvar, estvarn=estvar, estvarn.filter=estvar.filter, 
	addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, invyrs=invyrs, 
	landarea=landarea, plt.filter=plt.filter, cond.filter=cond.filter, 
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
	rowunit=totunit=domvar2=grpvar <- NULL
  if (is.null(domain)) domain <- "TOTAL"
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(condall[[rowvar]])) > 1) ||
		(!is.null(tdomvarlst) && length(tdomvarlst) > 1)), TRUE, FALSE)
  response <- estvar.name
  estunits <- sort(unique(pltmodelx[[unitvar]]))

  ## Set up factor data in unitlut
  ##########################################################################
  if ("GREG" %in% MAmethod && !is.null(predfac)) {
    for (fac in predfac) {
      ## Set as factor if not already
      if (!is.factor(pltmodelx[[fac]])) {
        fac.levels <- sort(unique(pltmodelx[[fac]]))
        pltmodelx[[fac]] <- factor(pltmodelx[[fac]], levels=fac.levels)
      }
      ## Get factor levels
      fac.levels <- sort(unique(pltmodelx[[fac]]))

      ## Set factor levels to keep and delete from unitlut.GREG
      fac.unitcol.keep <- paste(fac, fac.levels[-1], sep=".")
      fac.unitcol.del <- paste(fac, fac.levels[1], sep=".")
      unitlut[[fac.unitcol.del]] <- NULL
  
      ## Rename factor variables and add names to predictor list
      facs <- paste0(fac, fac.levels[-1])
      names(unitlut)[names(unitlut) %in% fac.unitcol.keep] <- facs
      unitpreds <- c(prednames[prednames != fac], facs)

      ## Reformat factor predictor variables in plt data table
      #facnames <- paste0(fac, levels(pltmodel[[fac]])[-1])

      ## Create dummy variables for factor levels - 1
      dtfac <- pltmodelx[, as.data.table(model.matrix(~., 
				data=pltmodelx[, fac, with=FALSE]))][,-1]
      pltmodelx <- cbind(pltmodelx, dtfac)
      pltmodelx[, (fac) := NULL]

      ## Remove old name and add new names to predictor list
      prednames <- unique(c(prednames[prednames != fac], facs))
    }
  }

  if (addtotal) {
    ## Get total estimate and merge area
    unit.totest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdat, cuniqueid=cuniqueid, pltmodelx=pltmodelx, puniqueid=puniqueid, 
		unitlut=unitlut, unitvar=unitvar, esttype=esttype, MAmethod=MAmethod, 
		PSstrvar=PSstrvar, prednames=prednames, domain="TOTAL", response=response,
		npixels=npixels))
    tabs <- FIESTA::check.matchclass(unit.totest, unitarea, unitvar)
    unit.totest <- tabs$tab1
    unitarea <- tabs$tab2
    setkeyv(unit.totest, unitvar)
    unit.totest <- unit.totest[unitarea, nomatch=0]
    unit.totest <- getarea(unit.totest, areavar=areavar, esttype=esttype)
  }


  if (bytdom) {

    tdomvarlst <- c(sort(tdomvarlst), estvar.name)
    unit.tdomest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdat, cuniqueid=cuniqueid, pltmodelx=pltmodelx, 
		puniqueid=puniqueid, unitlut=unitlut, unitvar=unitvar, esttype=esttype,
		MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, domain=domain,
 		response=response, bytdom=TRUE, tdomvarlst=tdomvarlst, npixels=npixels))
    if ("TOTAL" %in% names(unit.tdomest)) unit.tdomest[, TOTAL := NULL]

  } else {

    ## Get row, column, cell estimate and merge area if row or column in cond table 
    if (domain != "TOTAL") {
      unit.domest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdat, cuniqueid=cuniqueid, pltmodelx=pltmodelx, 
		puniqueid=puniqueid, unitlut=unitlut, unitvar=unitvar, esttype=esttype,
		MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, domain=domain,
 		response=response, npixels=npixels))

      if (colvar != "NONE") {
        unit.grpest <- unit.domest

        unit.rowest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdat, cuniqueid=cuniqueid, pltmodelx=pltmodelx, 
		puniqueid=puniqueid, unitlut=unitlut, unitvar=unitvar, esttype=esttype,
		MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, domain=rowvar,
 		response=response, npixels=npixels))

        unit.colest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdat, cuniqueid=cuniqueid, pltmodelx=pltmodelx, 
		puniqueid=puniqueid, unitlut=unitlut, unitvar=unitvar, esttype=esttype,
		MAmethod=MAmethod, PSstrvar=PSstrvar, prednames=prednames, domain=colvar,
 		response=response, npixels=npixels))
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
    tabs <- FIESTA::check.matchclass(unit.grpest, unitarea, unitvar)
    unit.grpest <- tabs$tab1
    unitarea <- tabs$tab2
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
    if (!is.null(plotsampcnt)) rawdat$plotsampcnt <- plotsampcnt
    if (!is.null(condsampcnt)) rawdat$condsampcnt <- setDF(condsampcnt)
    rawdat$unitlut <- unitlut
    rawdat$unitarea <- unitarea
    if (MAmethod != "HT") rawdat$pltmodel <- pltmodelx
    rawdat$dat <- tdomdat
    rawdat$estvar <- response
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
	estnull=estnull, psenull=psenull, char.width=char.width) 

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
          outfn.rawtab <- paste(outfn.rawdat, tabnm, sep="_") 
          if (tabnm %in% c("plotsampcnt", "condsampcnt", "unitlut", "unitarea")) {
            write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite)
          } else  { 
            suppressWarnings(save1tab(estpse=rawtab, title.estpse=title.raw, 
			outfolder=rawfolder, allin1=allin1, coltitlerow=FALSE, 
			rowtotal=FALSE, outfn.estpse=outfn.rawtab, addtitle=FALSE,
			addformat=FALSE, outfn.date=outfn.date, overwrite=overwrite))
          }
        }
      }
    }
  }
  

  returnlst <- list(est=est2return)
  if (!is.null(pse2return)) returnlst$pse <- pse2return 
  if (rawdata) returnlst$raw <- rawdat
  if(returntitle) returnlst$titlelst <- titlelst
    
  return(returnlst)
}

