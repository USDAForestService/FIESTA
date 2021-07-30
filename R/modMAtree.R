modMAtree <- function(MApopdat=NULL, MAmethod, FIA=TRUE, prednames=NULL, 
	estseed="none", landarea="FOREST", pcfilter=NULL, estvar=NULL, 
	estvar.filter=NULL, rowvar=NULL, colvar=NULL, row.FIAname=FALSE, 
	col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, row.add0=FALSE, 
	col.add0=FALSE, rowlut=NULL, collut=NULL, rowgrp=FALSE, rowgrpnm=NULL, 
	rowgrpord=NULL, sumunits=FALSE, allin1=FALSE, metric=FALSE, estround=1, 
	pseround=2, estnull="--", psenull="--", divideby=NULL, savedata=FALSE, 
	outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, addtitle=TRUE, 
	rawdata=FALSE, rawonly=FALSE, raw_fmt="csv", raw_dsn=NULL, overwrite_dsn=FALSE,
 	overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE, title.main=NULL, 
	title.ref=NULL, title.rowvar=NULL, title.colvar=NULL, title.unitvar=NULL, 
	title.estvar=NULL, title.filter=NULL, gui=FALSE, ...){

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
    tree=landarea=strvar=areavar <- NULL
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
  MAmethodlst <- c("HT", "PS", "greg", "gregEN", "ratio")
  MAmethod <- FIESTA::pcheck.varchar(var2check=MAmethod, varnm="MAmethod", gui=gui, 
		checklst=MAmethodlst, caption="MAmethod", multiple=FALSE, stopifnull=TRUE)


  ###################################################################################
  ## Check data and generate population information 
  ###################################################################################
  if (is.null(MApopdat)) {
    MApopdat <- modMApop(gui=gui, prednames=prednames, ...)
  } else {
    returnMApopdat <- FALSE
    list.items <- c("condx", "pltcondx", "cuniqueid", "condid", 
		"ACI.filter", "unitarea", "unitvar", "unitlut", "npixels",
		"npixelvar", "expcondtab", "plotsampcnt", "condsampcnt", "MAmethod")
    #if (MAmethod == "PS") {
    #  list.items <- c(list.items, "strvar")
    #}
    #if (MAmethod == "greg") {
    #  list.items <- c(list.items, "prednames")
    #}
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
  areaunits <- MApopdat$areaunits
  unitvar <- MApopdat$unitvar
  unitlut <- MApopdat$unitlut
  unitvars <- MApopdat$unitvars
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
  strvar <- MApopdat$strvar
  adj <- MApopdat$adj

  if (MAmethod %in% c("greg", "gregEN", "ratio")) {
    if (is.null(prednames)) {
      prednames <- MApopdat$prednames
    } else {
      if (!all(prednames %in% MApopdat$prednames))
        stop("invalid prednames: ", 
			toString(prednames[!prednames %in% MApopdat$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  } 


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
 	condid=condid, treex=treex, seedx=seedx, estseed=estseed, sumunits=sumunits, 
	landarea=landarea, ACI.filter=ACI.filter, pcfilter=pcfilter, 
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
	tdomvar2=tdomvar2, adjtree=adjtree, metric=metric)
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

  tdomdat <- merge(condx, tdomdat, by=c(cuniqueid, condid), all.x=TRUE)
  estvar <- treedat$estvar
  estvar.name <- treedat$estvar.name
  estvar.filter <- treedat$estvar.filter
  tdomvarlst <- treedat$tdomvarlst
  estunits <- treedat$estunits

  #####################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  #####################################################################################
  alltitlelst <- FIESTA::check.titles(dat=tdomdat, esttype=esttype, estseed=estseed, 
	sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.colvar=title.colvar,
 	title.unitvar=title.unitvar, title.filter=title.filter, title.unitsn=estunits, 
	title.estvarn=title.estvar, unitvar=unitvar, rowvar=rowvar, colvar=colvar, 
	estvarn=estvar, estvarn.filter=estvar.filter, addtitle=addtitle, 
	returntitle=returntitle, rawdata=rawdata, states=states, invyrs=invyrs, 
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
    outfn.rawdat <- paste0(outfn.rawdat, "_modMA_mase", "_", MAmethod) 
  } 
  ## Append name of package and method to outfile name
  outfn.estpse <- paste0(outfn.estpse, "_modMA_mase", "_", MAmethod) 

  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit_totest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlst) && length(tdomvarlst) > 1)), TRUE, FALSE)
  response <- estvar.name
  estunits <- sort(unique(tdomdat[[unitvar]]))

  masemethod <- ifelse(MAmethod == "PS", "postStrat", 
	ifelse(MAmethod == "greg", "greg", 
	ifelse(MAmethod == "gregEN", "gregElasticNet", 
	ifelse(MAmethod == "ratio", "ratioEstimator", "horvitzThompson"))))
  message("generating estimates using mase::", masemethod, " function...\n")
  if (!MAmethod %in% c("HT", "PS")) {
    message("using the following predictors...", toString(prednames))
  } 

#  if (addtotal) {
    ## Get total estimate and merge area
    tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, "TOTAL", strvar, prednames), .SDcols=response]
    unit_totest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdattot, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
		domain="TOTAL", response=response, npixels=npixels, FIA=FIA))
    tabs <- FIESTA::check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    unit_totest <- getarea(unit_totest, areavar=areavar, esttype=esttype)
#  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, rowvar, strvar, prednames), .SDcols=response]
    unit_rowest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar,
		esttype=esttype, MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
		domain=rowvar, response=response, npixels=npixels, FIA=FIA))
    #unit_rowest <- unit_rowest[!is.na(unit_rowest[[rowvar]]), ]

    if (colvar != "NONE") {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, colvar, strvar, prednames), .SDcols=response]
      unit_colest <- do.call(rbind, lapply(estunits, MAest.unit, 
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
		domain=colvar, response=response, npixels=npixels, FIA=FIA))
      #unit_colest <- unit_colest[!is.na(unit_colest[[colvar]]), ]

      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, cuniqueid, grpvar, strvar, prednames), .SDcols=response]
      tdomdatsum[, grpvar := do.call(paste, c(.SD, sep="#")), .SDcols=grpvar]

      unit_grpest <- do.call(rbind, lapply(estunits, MAest.unit,
		dat=tdomdatsum, cuniqueid=cuniqueid, unitlut=unitlut, unitvar=unitvar, 
		esttype=esttype, MAmethod=MAmethod, strvar=strvar, prednames=prednames, 
		domain="grpvar", response=response, npixels=npixels, FIA=FIA))
      unit_grpest[, c(rowvar, colvar) := tstrsplit(grpvar, "#", fixed=TRUE)]
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
    unit_rowest <- FIESTA::getarea(unit_rowest, areavar=areavar, esttype=esttype)
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
  ## GENERATE OUTPUT TABLES
  ###################################################################################
  message("getting output...")
  estnm <- "est"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, unit_rowest=unit_rowest, 
	unit_colest=unit_colest, unit_grpest=unit_grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, outfn.estpse=outfn.estpse, outfolder=outfolder, 
	outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, estround=estround, 
	pseround=pseround, divideby=divideby, returntitle=returntitle,
	estnull=estnull, psenull=psenull) 

  est2return <- tabs$tabest
  pse2return <- tabs$tabpse

  if (!is.null(est2return)) {
    returnlst$est <- est2return 
  }
  if (!is.null(pse2return)) {
    returnlst$pse <- pse2return 
  }
  if (returntitle) {
    returnlst$titlelst <- alltitlelst
  }

  if (rawdata) {
    rawdat <- tabs$rawdat
    rawdat$domdat <- setDF(tdomdat)
    rawdat$estvar <- estvar.name
    rawdat$estvar.filter <- estvar.filter
  
    if (savedata) {
      if (!is.null(title.estpse)) {
        title.raw <- paste(title.estpse, title.ref)
      } else {
        title.raw <- title.est
      }

      for (i in 1:length(rawdat)) {
        tabnm <- names(rawdat[i])
        if (!tabnm %in% c(estvar, prednames)) {
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
    }
    rawdat$esttype <- "TREE"
    rawdat$MAmethod <- MAmethod
    rawdat$estvar <- estvar
    rawdat$estvar.filter <- estvar.filter
    if (!is.null(rowvar)) rawdat$rowvar <- rowvar
    if (!is.null(colvar)) rawdat$colvar <- colvar
    rawdat$areaunits <- areaunits
    rawdat$estunits <- estunits
    returnlst$raw <- rawdat
  }
  if (returnMApopdat) {
    returnlst$MApopdat <- MApopdat
  }
  if ("STATECD" %in% names(pltcondf)) {
    returnlst$statecd <- sort(unique(pltcondf$STATECD))
  }
  if ("INVYR" %in% names(pltcondf)) {
    returnlst$invyr <- sort(unique(pltcondf$INVYR))
  }
    
  return(returnlst)
}

