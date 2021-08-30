modGBratio <- function(GBpopdat=NULL, estseed="none", ratiotype="PERACRE", 
	landarea="FOREST", pcfilter=NULL, estvarn=NULL, estvarn.filter=NULL, 
	estvard=NULL, estvard.filter=NULL, rowvar=NULL, colvar=NULL, 
	row.FIAname=FALSE, col.FIAname=FALSE, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, 
	rowgrp=FALSE, rowgrpnm=NULL, rowgrpord=NULL, sumunits=TRUE, allin1=FALSE, 
	metric=FALSE, estround=3, pseround=2, estnull="--", psenull="--", divideby=NULL, 
	savedata=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
 	addtitle=TRUE, rawdata=FALSE, rawonly=FALSE, raw_fmt="csv", raw_dsn=NULL,
 	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, returntitle=FALSE,
 	title.main=NULL, title.ref=NULL, title.rowvar=NULL, title.colvar=NULL,
 	title.unitvar=NULL, title.estvarn=NULL, title.estvard=NULL, title.filter=NULL,
 	gui=FALSE, ...){

  ##################################################################################
  ## DESCRIPTION: 
  ## Generates per-acre or per-tree estimates by domain using ratio estimators
  ##################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::modGBratio)),
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
    tree=landarea=strvar=areavar=sumunits=adj=strata=getwt=cuniqueid=ACI=
	tuniqueid=savedata=addtitle=returntitle=rawdata=unitvar <- NULL
    #if (!row.FIAname) row.FIAname <- NULL 
    #if (!col.FIAname) col.FIAname <- NULL  
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=tdom=estvar.name=rowvar.filter=colvar.filter=
	variable <- NULL

  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  rowcol.total <- TRUE
  esttype <- "RATIO"
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
    stop("must include tree data for ratio estimates")
  }
  cuniqueid <- GBpopdat$cuniqueid
  condid <- GBpopdat$condid
  tuniqueid <- GBpopdat$tuniqueid
  ACI.filter <- GBpopdat$ACI.filter
  unitarea <- GBpopdat$unitarea
  areavar <- GBpopdat$areavar
  areaunits <- GBpopdat$areaunits
  unitvar <- GBpopdat$unitvar
  unitvars <- GBpopdat$unitvars
  stratalut <- GBpopdat$stratalut
  strvar <- GBpopdat$strvar
  expcondtab <- GBpopdat$expcondtab
  plotsampcnt <- GBpopdat$plotsampcnt
  condsampcnt <- GBpopdat$condsampcnt
  states <- GBpopdat$states
  invyrs <- GBpopdat$invyrs
  estvar.area <- GBpopdat$estvar.area
  stratcombinelut <- GBpopdat$stratcombinelut
  strwtvar <- GBpopdat$strwtvar
  adj <- GBpopdat$adj
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
	cuniqueid=cuniqueid, esttype=esttype, ratiotype=ratiotype, 
	estvarn=estvarn, estvarn.filter=estvarn.filter, estvard=estvard,
 	estvard.filter=estvard.filter, esttotn=TRUE, esttotd=TRUE, 
	tdomvar=tdomvar, tdomvar2=tdomvar2, adjtree=adjtree, metric=metric)
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
 	invyrs=invyrs, landarea=landarea, pcfilter=pcfilter, allin1=allin1, 
	divideby=divideby, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if(rawdata) {
    outfn.rawdat <- alltitlelst$outfn.rawdat
  }

  #####################################################################################
  ## GENERATE ESTIMATES
  #####################################################################################
  unit_totest=unit_grpest=unit_rowest=unit_colest=unit_grpest=rowunit=totunit <- NULL
  addtotal <- ifelse(((rowvar == "TOTAL" || length(unique(tdomdat[[rowvar]])) > 1) ||
		(!is.null(tdomvarlstn) && length(tdomvarlstn) > 1)), TRUE, FALSE)
  stratalut <- setDT(stratalut)

  ## Transpose rows if tdomvar2 is not NULL
  if (!is.null(tdomvar2)) {
    ddomvar <- "TOTAL"
    tdomdat <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, ddomvar), .SDcols=tdomvarlstn]
    tdomdat <- transpose2row(tdomdat, uniqueid=c(strunitvars, cuniqueid, ddomvar),
 		tvars=tdomvarlstn)
    setnames(tdomdat, "value", estvarn.name)
    suppressWarnings(tdomdat[, (grpvar) := tstrsplit(variable, "#")])[, variable := NULL]

    cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvard.name]   
    tdomdat <- merge(tdomdat, cdomdattot, by=c(strunitvars, cuniqueid, "TOTAL"))
  }
 
  ## Note: tdomdat is the summed response by condition (not domain)
  if (addtotal) {
    ## Get estimate for total
    if (!is.null(tdomvar)) {
      tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvarn.name]
      cdomdattot <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=estvard.name]   
      tdomdattot <- merge(tdomdattot, cdomdattot, by=c(strunitvars, cuniqueid, "TOTAL"))
    } else {
      tdomdattot <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]
    }
#saveRDS(tdomdattot, "E:/workspace/FIESTA/FIESTA_MA/data_v1/tdomdattot_volcf_ndead_dlive.rds")
#saveRDS(stratalut, "E:/workspace/FIESTA/FIESTA_MA/data_v1/stratalut.rds")

    unit_totest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, ysum=tdomdattot, 
		esttype=esttype, ratiotype=ratiotype, uniqueid=cuniqueid,
		stratalut=stratalut, unitvar=unitvar, strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitarea, unit_totest, unitvar)
    unitarea <- tabs$tab1
    unit_totest <- tabs$tab2
    setkeyv(unit_totest, unitvar)
    unit_totest <- unit_totest[unitarea, nomatch=0]
    unit_totest <- FIESTA::getarea(unit_totest, areavar=areavar, esttype=esttype)
  }

  ## Get row, column, cell estimate and merge area if row or column in cond table 
  if (rowvar != "TOTAL") {
    if (!is.null(tdomvar)) {
      if (!is.null(tdomvar2)) {
        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvarn.name]    
      } else {
        if (tdomvar == rowvar) {
          tdomdatsum <- transpose2row(tdomdat, uniqueid=c(strunitvars, cuniqueid),
 			tvars=tdomvarlstn)
          setnames(tdomdatsum, c("variable", "value"), c(rowvar, estvarn.name))
          tdomdatsum <- tdomdatsum[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvarn.name]
        } else {  
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvarn.name]
        }
      }
      if (rowvar %in% names(cdomdat)) {
        cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, rowvar), .SDcols=estvard.name]
        tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(strunitvars, cuniqueid, rowvar))
      } else {
        cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid), .SDcols=estvard.name]
        tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(strunitvars, cuniqueid))
      }
    } else {
      tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]
    }

    #tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[rowvar]]),]
    unit_rowest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, esttype=esttype, ratiotype=ratiotype, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=rowvar)
 
    if (colvar != "NONE") {
      if (!is.null(tdomvar)) {
        if (!is.null(tdomvar2)) {
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, colvar), .SDcols=estvarn.name]    
        } else {
          if (tdomvar == colvar) {
            tdomdatsum <- transpose2row(tdomdat, uniqueid=c(strunitvars, cuniqueid),
 			tvars=tdomvarlstn)
            setnames(tdomdatsum, c("variable", "value"), c(colvar, estvarn.name))
            tdomdatsum <- tdomdatsum[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, colvar), .SDcols=estvarn.name]
          } else {     
            tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, colvar), .SDcols=estvarn.name]
          }
        }
        if (colvar %in% names(cdomdat)) {
          cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, colvar), .SDcols=estvard.name]
          tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(strunitvars, cuniqueid, colvar))
        } else {
          cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid), .SDcols=estvard.name]
          tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(strunitvars, cuniqueid))
        }
      } else {
        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars, cuniqueid, colvar), .SDcols=c(estvarn.name, estvard.name)]
      }
      #tdomdatsum <- tdomdatsum[!is.na(tdomdatsum[[colvar]]),]
      unit_colest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, esttype=esttype, ratiotype=ratiotype, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=colvar)

      if (!is.null(tdomvar)) {
        if (!is.null(tdomvar2)) {
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, grpvar), .SDcols=c(estvarn.name, estvard.name)]
        } else {
          ddomvar <- grpvar[grpvar != tdomvar]
          tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, ddomvar), .SDcols=tdomvarlstn]
          tdomdatsum <- transpose2row(tdomdatsum, uniqueid=c(strunitvars, cuniqueid, ddomvar),
 			tvars=tdomvarlstn)
          setnames(tdomdatsum, c("variable", "value"), c(tdomvar, estvarn.name))      

          if (any(grpvar %in% names(cdomdat))) {
            mergevar <- grpvar[grpvar %in% names(cdomdat)]
            cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid, mergevar), .SDcols=estvard.name]
            tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(strunitvars, cuniqueid, mergevar))
          } else {
            cdomdatsum <- cdomdat[, lapply(.SD, sum, na.rm=TRUE), 
			by=c(strunitvars, cuniqueid), .SDcols=estvard.name]
            tdomdatsum <- merge(tdomdatsum, cdomdatsum, by=c(strunitvars, cuniqueid))
          }
        }
      } else {
        tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
	 	by=c(strunitvars, cuniqueid, grpvar), .SDcols=c(estvarn.name, estvard.name)]
      }
      unit_grpest <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, 
		ysum=tdomdatsum, esttype=esttype, ratiotype=ratiotype, 
		uniqueid=cuniqueid, stratalut=stratalut, unitvar=unitvar, strvar=strvar, 
		domain=grpvar)
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

  ## For sumunits=FALSE, get estimation unit totals
  if (!sumunits && (length(unique(unitarea[[unitvar]])) > 1 && rowvar != "TOTAL")) {
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

    tdomdat[, ONEUNIT := 1]

    ## Calculate unit totals for rowvar
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, rowvar), .SDcols=c(estvarn.name, estvard.name)]

    rowunit <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, ysum=tdomdatsum, 
		esttype=esttype, uniqueid=tuniqueid, stratalut=stratalut2, 
		unitvar="ONEUNIT", strvar=strvar, domain=rowvar)
    
    rowunit <- FIESTA::add0unit(rowunit, rowvar, uniquerow, "ONEUNIT", row.add0)
    tabs <- FIESTA::check.matchclass(unitacres2, rowunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    rowunit <- tabs$tab2
    setkey(rowunit, "ONEUNIT")
    rowunit <- rowunit[unitacres2, nomatch=0]
    rowunit <- FIESTA::getarea(rowunit, areavar=areavar, esttype=esttype)
    setkeyv(rowunit, c("ONEUNIT", rowvar))

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    tdomdatsum <- tdomdat[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(strunitvars2, tuniqueid, "TOTAL"), .SDcols=c(estvarn.name, estvard.name)]

    totunit <- GBest.pbar(sumyn=estvarn.name, sumyd=estvard.name, ysum=tdomdatsum, 
		esttype=esttype, uniqueid=tuniqueid, stratalut=stratalut2, 
		unitvar="ONEUNIT", strvar=strvar, domain="TOTAL")
    tabs <- FIESTA::check.matchclass(unitacres2, totunit, "ONEUNIT")
    unitacres2 <- tabs$tab1
    totunit <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    totunit <- totunit[unitacres2, nomatch=0]
    totunit <- FIESTA::getarea(totunit, areavar=areavar, esttype=esttype)
  }          

  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################
  message("getting output...")
  estnm <- "estn"
  tabs <- est.outtabs(esttype=esttype, sumunits=sumunits, areavar=areavar, 
	unitvar=unitvar, unitvars=unitvars, unit_totest=unit_totest, unit_rowest=unit_rowest, 
	unit_colest=unit_colest, unit_grpest=unit_grpest, rowvar=rowvar, colvar=colvar, 
	uniquerow=uniquerow, uniquecol=uniquecol, rowgrp=rowgrp, rowgrpnm=rowgrpnm, 
	rowunit=rowunit, totunit=totunit, allin1=allin1, savedata=savedata, 
	addtitle=addtitle, title.ref=title.ref, title.colvar=title.colvar, 
	title.rowvar=title.rowvar, title.rowgrp=title.rowgrp, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse, 
	rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder,
 	overwrite=overwrite_layer, outfn.date=outfn.date, estnm=estnm, estround=estround,
 	pseround=pseround, divideby=divideby, returntitle=returntitle, 
	estnull=estnull, psenull=psenull) 
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
