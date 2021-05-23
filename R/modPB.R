modPB <- function(PBpopdat=NULL, tabtype="PCT", sumunits=FALSE, strata=FALSE,
	strtype="POST", ratio=FALSE, landarea="ALL", landarea.filter=NULL,
 	nonsamp.pntfilter=NULL, pntfilter=NULL, pfilter=NULL, 
	rowvar=NULL, colvar=NULL, row.orderby=NULL, col.orderby=NULL, 
	row.add0=FALSE, col.add0=FALSE, rowlut=NULL, collut=NULL, 
	domlut=NULL, domvarlst=NULL, ratioden="ROWVAR", allin1=FALSE, 
	estround=3, pseround=3, estnull=0, psenull="--", divideby=NULL, 
	savedata=FALSE, outfolder=NULL, outfn.pre=NULL, outfn.date=TRUE, 
	addtitle=TRUE, rawdata=FALSE, rawonly=FALSE, raw_fmt="csv", raw_dsn=NULL,
 	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, 
	returntitle=FALSE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.filter=NULL, 
	title.units="acres", gainloss=FALSE, gainloss.vals=NULL, gui=FALSE, ...){
 

  ###################################################################################
  ## DESCRIPTION: 
  ## Generates percent or acre estimates by domain (and estimation unit)
  ###################################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
#  formallst <- names(formals(FIESTA::modPB))
#  if (!all(input.params %in% formallst)) {
#    miss <- input.params[!input.params %in% formallst]
#    stop("invalid parameter: ", toString(miss))
#  }

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0 && is.null(PBpopdat)) {
    gui <- TRUE
  } 

  ## Set global variables
  TOTAL=ONEUNIT=n.total=n.strata=strwt=NBRPNTS=psq.pltdom=
	  uniqueid=p.pltdom.n=nbrpts.pltdom.n=PtsPerPlot=nbrpts.pltdom=
	  value=p.pltdom=PBvars2keep=title.est=title.pse=title.estpse=
	  outfn.estpse <- NULL


  ## If gui.. set variables to NULL
  if (gui) {
    pntid=plotid=puniqueid=landarea=strvar=areavar=PBvars2keep <- NULL
  }

  ###################################################################################
  ## INITIALIZE SETTINGS
  ###################################################################################
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  minplotnum <- 10
  returnPBpopdat <- TRUE 
  parameters <- FALSE
  returnlst <- list()

  ##################################################################
  ## Check population data
  ########################################################
  if (is.null(PBpopdat)) {
    PBpopdat <- modPBpop(strata=strata, sumunits=sumunits, gui=gui, ...)
  } else {
    returnPBpopdat <- FALSE
    if (!is.list(PBpopdat))
      stop("PBpopdat must be a list")
    listitems <- c("PBx", "plotid", "pntid", "getprop")
    if (!all(listitems %in% names(PBpopdat))) {
      items.miss <- listitems[!listitems %in% names(PBpopdat)]
      stop("invalid PBpopdat... missing items: ", paste(items.miss, collapse=", "))
    }   
  }	
  if (is.null(PBpopdat)) return(NULL)	
  PBx <- PBpopdat$PBx
  pltassgnx <- PBpopdat$pltassgnx
  plotid <- PBpopdat$plotid
  pntid <- PBpopdat$pntid
  pltassgnid <- PBpopdat$pltassgnid
  unitvar <- PBpopdat$unitvar
  unitvar2 <- PBpopdat$unitvar2
  unitvars <- PBpopdat$unitvars
  unitarea <- PBpopdat$unitarea
  areavar <- PBpopdat$areavar
  areaunits <- PBpopdat$areaunits
  strata <- PBpopdat$strata
  stratalut <- PBpopdat$stratalut
  strvar <- PBpopdat$strvar
  strwtvar <- PBpopdat$strwtvar
  plotsampcnt <- PBpopdat$plotsampcnt
  stratcombinelut <- PBpopdat$stratcombinelut
  getprop <- PBpopdat$getprop
  if (!getprop) {
    rowvar <- PBpopdat$rowvar
    PBvars2keep <- "p.pltdom"
  }
  unitvars <- c(unitvar, unitvar2)
  if (strata) {
    strtype <- PBpopdat$strtype
  }
  strunitvars <- c(unitvar, strvar)

  ###################################################################################
  ## Check parameters and apply plot and pnt filters
  ###################################################################################
  estdat <- check.estdataPB(PBx=PBx, plotid=plotid, pntid=pntid, tabtype=tabtype,
	ratio=ratio, pfilter=pfilter, nonsamp.pntfilter=nonsamp.pntfilter,
	landarea=landarea, landarea.filter=landarea.filter, pntfilter=pntfilter, 
	sumunits=sumunits, allin1=allin1, estround=estround, pseround=pseround, 
	divideby=divideby, addtitle=addtitle, returntitle=returntitle, 
	rawdata=rawdata, rawonly=rawonly, savedata=savedata, outfolder=outfolder, 
	overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, outfn.pre=outfn.pre,
 	outfn.date=outfn.date, append_layer=append_layer, raw_fmt=raw_fmt, 
	raw_dsn=raw_dsn, gui=gui)
  if (is.null(estdat)) return(NULL)
  PBx <- estdat$PBf	
  plotid <- estdat$plotid
  pntid <- estdat$pntid
  filterids <- estdat$filterids
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
  overwrite_layer <- estdat$overwrite_layer
  raw_fmt <- estdat$raw_fmt
  raw_dsn <- estdat$raw_dsn
  rawfolder <- estdat$rawfolder
  tabtype <- estdat$tabtype

  if (tabtype == "AREA" && is.null(unitarea)) {
    stop("must include unitarea in population data for area table")
  }

  #################################################################################
  ### GET ROW AND COLUMN INFO
  #################################################################################
  #if (!is.null(domlut)) domlut <- setDF(domlut) 
  rowcolinfo <- check.rowcolPB(gui=gui, ratio=ratio, PBx=PBx, plotid=plotid, 
	pntid=pntid, rowvar=rowvar, colvar=colvar, row.orderby=row.orderby, 
	col.orderby=col.orderby, domvarlst=domvarlst, domlut=domlut, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, filterids=filterids, 
	row.add0=row.add0, col.add0=col.add0, rowlut=rowlut, collut=collut, 
	PBvars2keep=PBvars2keep)
  PBx <- rowcolinfo$PBx
  setkeyv(PBx, c(plotid, pntid))
  uniquerow <- rowcolinfo$uniquerow
  uniquecol <- rowcolinfo$uniquecol
  domainlst <- rowcolinfo$domainlst
  rowvar <- rowcolinfo$rowvar
  colvar <- rowcolinfo$colvar
  row.orderby <- rowcolinfo$row.orderby
  col.orderby <- rowcolinfo$col.orderby
  title.rowvar <- rowcolinfo$title.rowvar
  title.colvar <- rowcolinfo$title.colvar 
  row.add0 <- rowcolinfo$row.add0
  col.add0 <- rowcolinfo$col.add0
  if (ratio) {
    PBx.d <- rowcolinfo$PBx.d
  }

  ###################################################################################
  ## MERGE FILTERED DATA TO ALL PLOTS
  ###################################################################################
  tabs <- FIESTA::check.matchclass(PBx, pltassgnx, plotid, pltassgnid,
			tab1txt="pnt", tab2txt="pltassgn")
  PBx <- tabs$tab1
  pltassgnx <- tabs$tab2
  PBall <- merge(PBx, pltassgnx, by.x=plotid, by.y=pltassgnid, all.x=TRUE)

  if (ratio) {
    tabs <- FIESTA::check.matchclass(PBx.d, pltassgnx, plotid, pltassgnid,
			tab1txt="pnt.d", tab2txt="pltassgn")
    PBx.d <- tabs$tab1
    pltassgnx <- tabs$tab2
    PBall.d <- merge(PBx.d, pltassgnx, by.x=plotid, by.y=pltassgnid, all.x=TRUE)
  }

  ###################################################################################
  ### GET TITLES FOR OUTPUT TABLES
  ###################################################################################
  phototype <- ifelse(ratio, tolower(paste("ratio", tabtype, sep="_")), 
		tolower(paste("nratio", tabtype, sep="_")))
  pcfilter <- NULL
  if (!is.null(pfilter)) {
    pcfilter <- pfilter
    if (!is.null(pntfilter)) {
      pcfilter <- paste(pcfilter, "and", pntfilter)
    } 
  } else if (!is.null(pntfilter)) {
    pcfilter <- pntfilter
  }
  alltitlelst <- check.titles(dat=PBall, esttype="PHOTO", phototype=phototype, 
	tabtype=tabtype, sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.unitvar=title.unitvar, 
	title.filter=title.filter, title.unitsn=areaunits, unitvar=unitvar, rowvar=rowvar,
 	colvar=colvar, addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
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

  ###########################################################
  ## DO WORK
  ###########################################################

  ## Define columns
  estnm <- "est"
  psenm <- "pse"

  ####################################################################### 
  ## Get proportion of points by domain and plot and number of points
  #######################################################################
  pltdom.tot=pltdom.row=pltdom.col=pltdom.grp <- NULL
  totvar <- "TOTAL"

  if (getprop) {

    if (!ratio) {
      if (totvar %in% names(PBall)) {
        pltdom.tot <- getpltdom.prop(PBall, uniqueid=plotid, domain="TOTAL", strunitvars)

        totest.pntcnt <- pltdom.tot[get(totvar) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, totvar)]
      }
      if (rowvar != "Total") {
        pltdom.row <- getpltdom.prop(PBall, uniqueid=plotid, domain=rowvar, strunitvars)
        rowest.pntcnt <- pltdom.row[get(rowvar) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, rowvar)]
        setkeyv(rowest.pntcnt, c(unitvar, rowvar))

        ## for kelly
        #tpltdom.row <- FIESTA::transpose2col(pltdom.row, plotid, tvar=rowvar,
        #		   value.var="p.pltdom")

      } 
 
      if (colvar != "NONE") {
        grpvar <- c(rowvar, colvar)
        pltdom.col <- getpltdom.prop(PBall, uniqueid=plotid, domain=colvar, strunitvars)
        pltdom.grp <- getpltdom.prop(PBall, uniqueid=plotid, domain=grpvar, strunitvars)

        colest.pntcnt <- pltdom.col[get(colvar) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, colvar)]
        setkeyv(colest.pntcnt, c(unitvar, colvar)) 

        grpest.pntcnt <- pltdom.grp[get(grpvar) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom)), by=c(unitvar, grpvar)]
        setkeyv(grpest.pntcnt, c(unitvar, grpvar)) 
      }

    } else {

      domain <- ifelse(ratioden == "ROWVAR", rowvar, colvar)
      attribute <- ifelse(ratioden == "ROWVAR", colvar, rowvar)
      grpvar <- c(domain, attribute)

      ## Get proportion of points for domain (denominator) by plot
      chgnames <- c("nbrpts.pltdom", "p.pltdom")
      pltdom.d <- getpltdom.prop(PBall.d, uniqueid=plotid, domain=domain, strunitvars)
      setnames(pltdom.d, chgnames, paste0(chgnames, ".d")) 
      setkeyv(pltdom.d, c(strunitvars, plotid))

      ## Get proportion of points for domain and attribute (numerator) by plot
      chgnames <- c("nbrpts.pltdom", "p.pltdom")
      pltdom.n <- getpltdom.prop(PBall, uniqueid=plotid, domain=c(domain, attribute),
 			strunitvars)
      setnames(pltdom.n, chgnames, paste0(chgnames, ".n")) 
      setkeyv(pltdom.n, c(strunitvars, plotid))

      ## Check if class of join columns in pltdom.n matches pltdom.d
      tabs <- FIESTA::check.matchclass(pltdom.d, pltdom.n, 
			c(strunitvars, plotid, domain))
      pltdom.d <- tabs$tab1
      pltdom.n <- tabs$tab2

      grpest.pntcnt <- pltdom.n[get(attribute) != "NOTinDOMAIN", 
			list(NBRPNTS=sum(nbrpts.pltdom.n)), by=c(unitvar, domain, attribute)]
      setkeyv(grpest.pntcnt, c(unitvar, grpvar)) 
    }

  } else {
    pltdom.row <- PBall
  }

  ####################################################################### 
  ## GENERATE ESTIMATES
  ####################################################################### 
  unit.totest=unit.rowest=unit.colest=unit.grpest=rowunit=totunit=
	unit.totest.str=unit.rowest.str=unit.colest.str=unit.grpest.str <- NULL

  if (!ratio) {
    phatcol <- "phat"
    phatcol.var <- "phat.var"

    if (!is.null(pltdom.tot)) {
      ## Get estimate for TOTAL
      #######################################################################
      pbar.totest <- PBest.pbar(dom.prop=pltdom.tot, uniqueid=plotid, domain=totvar, 
		strtype="post", stratalut=stratalut, strunitvars=strunitvars, unitvars=unitvar,
		strvar=strvar)
      unit.totest <- pbar.totest$est.unit
      if (rawdata) unit.totest.str <- pbar.totest$ybardat
      setkeyv(unit.totest, c(unitvar, totvar))

      ## Merge NBRPNTS
      tabs <- FIESTA::check.matchclass(unit.totest, totest.pntcnt, c(unitvar, totvar))
      unit.totest <- tabs$tab1
      totest.pntcnt <- tabs$tab2
      unit.totest <- unit.totest[totest.pntcnt, nomatch=0]
    
      ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
      if (tabtype == "AREA" || sumunits) {
        tabs <- FIESTA::check.matchclass(unitarea, unit.totest, unitvar)
        unitarea <- tabs$tab1
        unit.totest <- tabs$tab2
        setkeyv(unit.totest, unitvar)
        unit.totest <- unit.totest[unitarea, nomatch=0]
        unit.totest <- PBgetest(unit.totest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      } else {
        unit.totest <- PBgetest(unit.totest, phatcol=phatcol, phatcol.var=phatcol.var)
      }

      ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
      ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
      tot.filterval <- ifelse (is.numeric(unit.totest[[totvar]]), 9999, "NOTinDOMAIN")
      unit.totest <- unit.totest[get(eval(totvar)) != tot.filterval,]
    }

    ## Get estimate for row and columns
    #######################################################################
    if (rowvar != "TOTAL") {
      pbar.rowest <- PBest.pbar(dom.prop=pltdom.row, uniqueid=plotid, domain=rowvar, 
		strtype="post", stratalut=stratalut, strunitvars=strunitvars, unitvars=unitvar,
		strvar=strvar)
      unit.rowest <- pbar.rowest$est.unit
      if (rawdata) {
        unit.rowest.str <- pbar.rowest$ybardat
      }
      setkeyv(unit.rowest, c(unitvar, rowvar))

      ## Get filter value for row and column
      row.filterval <- ifelse (is.numeric(unit.rowest[[rowvar]]), 9999, "NOTinDOMAIN")
    }
   
    ## Get column (and cell) estimate  
    if (colvar != "NONE") {
      pbar.colest <- PBest.pbar(dom.prop=pltdom.col, uniqueid=puniqueid, 
		domain=colvar, strtype="post", stratalut=stratalut, strunitvars=strunitvars,
 		unitvars=unitvar, strvar=strvar)
      unit.colest <- pbar.colest$est.unit
      if (rawdata) { 
        unit.colest.str <- pbar.colest$ybardat
      }
      setkeyv(unit.colest, c(unitvar, colvar))

      ## Get filter value for column
      col.filterval <- ifelse (is.numeric(unit.colest[[colvar]]), 9999, "NOTinDOMAIN")

      pbar.grpest <- PBest.pbar(dom.prop=pltdom.grp, uniqueid=puniqueid,
 		domain=grpvar, strtype="post", stratalut=stratalut, strunitvars=strunitvars,
 		unitvars=unitvar, strvar=strvar)
      unit.grpest <- pbar.grpest$est.unit
      if (rawdata) {
        unit.grpest.str <- pbar.grpest$ybardat
      }
      setkeyv(unit.grpest, c(unitvar, grpvar))
    }
  } else {  ## ratio=TRUE

    phatcol <- "rhat"
    phatcol.var <- "rhat.var"

    unit.grpest <- PBest.pbarRatio(dom.prop.n=pltdom.n, dom.prop.d=pltdom.d, 
		uniqueid=plotid, domain=domain, attribute=attribute, stratalut=stratalut, 
		strunitvars=strunitvars, unitvars=unitvar, strvar=strvar)
    setkeyv(unit.grpest, c(unitvar, grpvar))

    if (tabtype == "AREA") {
      pltdom <- pltdom.n
      names(pltdom) <- sub("\\.n", "", names(pltdom.n))
      unit.grpest.domtot <- PBest.pbar(dom.prop=pltdom, uniqueid=plotid, 
		domain=domain, stratalut=stratalut, strunitvars=strunitvars, unitvars=unitvar,
		strvar=strvar, strtype="post")$est.unit
      setkeyv(unit.grpest.domtot, c(unitvar, domain))
    }

    ## Get filter value for row and column
    row.filterval <- ifelse (is.numeric(unit.rowest[[rowvar]]), 9999, "NOTinDOMAIN")
    col.filterval <- ifelse (is.numeric(unit.colest[[colvar]]), 9999, "NOTinDOMAIN")
  }
 
  ###################################################################################
  ## Check add0 and Add acres
  ###################################################################################
  if (!is.null(unit.rowest)) {
    if (getprop) {
      ## Merge number of points
      tabs <- FIESTA::check.matchclass(unit.rowest, rowest.pntcnt, c(unitvar, rowvar))
      unit.rowest <- tabs$tab1
      rowest.pntcnt <- tabs$tab2
      unit.rowest <- unit.rowest[rowest.pntcnt, nomatch=0]
    }

    ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
    unit.rowest <- unit.rowest[get(eval(rowvar)) != row.filterval,]

    ## Merge uniquerow
    unit.rowest <- FIESTA::add0unit(x=unit.rowest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0)

    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    if (!is.null(unit.rowest) && !ratio) {
      if (tabtype == "AREA" || sumunits) {
        tabs <- FIESTA::check.matchclass(unitarea, unit.rowest, unitvar)
        unitarea <- tabs$tab1
        unit.rowest <- tabs$tab2
        setkeyv(unit.rowest, unitvar)
        unit.rowest <- unit.rowest[unitarea, nomatch=0]
        unit.rowest <- PBgetest(unit.rowest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
        setkeyv(unit.rowest, c(unitvar, rowvar))
      } else {
        unit.rowest <- PBgetest(xdat=copy(unit.rowest), phatcol=phatcol, phatcol.var=phatcol.var)
      }
    }
  }

  if (!is.null(unit.colest) && !ratio) {
    ## Merge number of points
    tabs <- FIESTA::check.matchclass(unit.colest, colest.pntcnt, c(unitvar, colvar))
    unit.colest <- tabs$tab1
    colest.pntcnt <- tabs$tab2
    unit.colest <- unit.colest[colest.pntcnt, nomatch=0]

    ## Remove rows that are filtered out (NOTinDOMAIN or 9999)
    unit.colest <- unit.colest[get(eval(colvar)) != col.filterval,]

    ## Merge uniquecol
    unit.colest <- FIESTA::add0unit(x=unit.colest, xvar=colvar, uniquex=uniquecol, 
		unitvar=unitvar, xvar.add0=col.add0)

    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    if (tabtype == "AREA" || sumunits) {
      tabs <- FIESTA::check.matchclass(unitarea, unit.colest, unitvar)
      unitarea <- tabs$tab1
      unit.colest <- tabs$tab2
      setkeyv(unit.colest, unitvar)
      unit.colest <- unit.colest[unitarea, nomatch=0]
      unit.colest <- PBgetest(unit.colest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      setkeyv(unit.colest, c(unitvar, colvar))
    } else {
      unit.colest <- PBgetest(xdat=copy(unit.colest), phatcol=phatcol, phatcol.var=phatcol.var)
    }
  }
  if (!is.null(unit.grpest)) {
    unit.grpest <- add0unit(x=unit.grpest, xvar=rowvar, uniquex=uniquerow, 
		unitvar=unitvar, xvar.add0=row.add0, xvar2=colvar, uniquex2=uniquecol,
		xvar2.add0=col.add0)

    if (tabtype == "AREA" || sumunits) {
      tabs <- FIESTA::check.matchclass(unitarea, unit.grpest, unitvar)
      unitarea <- tabs$tab1
      unit.grpest <- tabs$tab2
      setkeyv(unit.grpest, unitvar)
      unit.grpest <- unit.grpest[unitarea, nomatch=0]
      unit.colest <- PBgetest(unit.grpest, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      setkeyv(unit.grpest, c(unitvar, rowvar, colvar))
    } else {
      unit.grpest <- PBgetest(xdat=copy(unit.grpest), phatcol=phatcol, phatcol.var=phatcol.var)
    }
  }
        
  if (!sumunits && length(unique(stratalut[[unitvar]])) > 1 && !ratio && tabtype != "PCT") {
    ## AGGREGATE UNIT stratalut FOR ROWVAR and GRAND TOTAL
    stratalut2 <- data.table(stratalut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    stratalut2 <- stratalut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(strwtvar, "n.strata")]
    stratalut2[, strwt:=prop.table(get(strwtvar)), by="ONEUNIT"]
    stratalut2[, n.total := sum(n.strata)]
    setkeyv(stratalut2, strunitvars2)

    if (!is.null(unitarea)) {
      unitarea2 <- data.table(unitarea, ONEUNIT=1)
      unitarea2 <- unitarea2[, lapply(.SD, sum, na.rm=TRUE), by="ONEUNIT", 
		.SDcols=areavar]
      setkey(unitarea2, "ONEUNIT")
    }

    PBall[, ONEUNIT := 1]

    ## CALCULATE UNIT TOTALS FOR ROWVAR
    pltdom.prop <- getpltdom.prop(PBall, uniqueid=plotid, domain=rowvar, strunitvars2)
    rowunit <- FIESTA::PBest.pbar(dom.prop=pltdom.prop, uniqueid=plotid, 
		domain=rowvar, strtype="post", stratalut=stratalut2, strunitvars=strunitvars2,
 		unitvars="ONEUNIT", strvar=strvar)$est.unit
    rowunit <- FIESTA::add0unit(x=rowunit, xvar=rowvar, uniquex=uniquerow, 
		unitvar="ONEUNIT", xvar.add0=row.add0)
    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    if (tabtype == "AREA") {
      tabs <- FIESTA::check.matchclass(rowunit, unitarea2, "ONEUNIT")
      rowunit <- tabs$tab1
      unitarea2 <- tabs$tab2
      setkeyv(rowunit, "ONEUNIT")
      rowunit <- rowunit[unitarea2, nomatch=0]
      rowunit <- PBgetest(rowunit, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
      setkeyv(rowunit, c("ONEUNIT", rowvar))

    } else {
      rowunit <- PBgetest(rowunit, phatcol=phatcol, phatcol.var=phatcol.var)
    }  

    ## CALCULATE GRAND TOTAL FOR ALL UNITS
    pltdom.prop <- FIESTA::getpltdom.prop(PBall, uniqueid=plotid, domain="TOTAL", 
		strunitvars2)
    totunit <- FIESTA::PBest.pbar(dom.prop=pltdom.prop, uniqueid=plotid, 
		domain="TOTAL", strtype="post", stratalut=stratalut2, strunitvars=strunitvars2,
 		unitvars="ONEUNIT", strvar=strvar)$est.unit

    ## Add acres (tabtype="AREA") or round values (tabtype="PCT")
    tabs <- FIESTA::check.matchclass(totunit, unitarea2, "ONEUNIT")
    totunit <- tabs$tab1
    unitarea2 <- tabs$tab2
    setkeyv(totunit, "ONEUNIT")
    if (tabtype == "AREA") {
      totunit <- totunit[unitarea2, nomatch=0]
      totunit <- PBgetest(totunit, areavar, phatcol=phatcol, phatcol.var=phatcol.var)
    } else {
      PBgetest(totunit, phatcol=phatcol, phatcol.var=phatcol.var)
    }
   # totunit[, TOTAL:= "Total"]
   # setnames(totunit, names(totunit), names(rowunit))
  }          
 
  ###################################################################
  ## GENERATE OUTPUT TABLES
  ###################################################################
  CI <- TRUE
  estnm <- "est"
  tabs <- est.outtabs(esttype="PHOTO", phototype=tabtype, photoratio=ratio, 
	sumunits=sumunits, areavar=areavar, unitvar=unitvar, unitvars=unitvars,
 	unit.totest=unit.totest, unit.rowest=unit.rowest, unit.colest=unit.colest,
 	unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, uniquerow=uniquerow,
 	uniquecol=uniquecol, rowunit=rowunit, totunit=totunit, allin1=allin1, 
	savedata=savedata, addtitle=addtitle, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse,
 	rawdata=rawdata, rawonly=rawonly, outfn.estpse=outfn.estpse, outfolder=outfolder,
 	outfn.date=outfn.date, overwrite=overwrite_layer, estnm=estnm, estround=estround,
 	pseround=pseround, estnull=estnull, psenull=psenull, divideby=divideby, CI=CI) 
 
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
    if (!is.null(rowvar) && rowvar != "NONE") {
      rawdat$rowvar <- rowvar
      rawdat$pltdom.row <- pltdom.row
    }
    if (!is.null(colvar) && colvar != "NONE") {
      rawdat$colvar <- colvar 
      rawdat$pltdom.col <- pltdom.col
      rawdat$pltdom.grp <- pltdom.grp
    }
    ## Generate sample counts by attribute
#    if (is.null(sampcnt)) {
#      byvars <- c(unitvar, rowvar)
#      if (colvar != "NONE") byvars <- c(byvars, colvar)
#      sampcnt <- PBall[, .N, by=byvars]
#      setnames(sampcnt, "N", "NBRPNTS")
#      setorderv(sampcnt, byvars)
#    }

    if (!is.null(plotsampcnt)) {

      if (!is.null(row.orderby)) {
        mergevar.row <- ifelse (rowvar %in% names(plotsampcnt), rowvar, row.orderby)
      } else {
        mergevar.row <- rowvar
      }
      if (colvar != "NONE") {
        if (col.orderby != "NONE") {
          mergevar.col <- ifelse (colvar %in% names(plotsampcnt), colvar, col.orderby)
        } else {
          mergevar.col <- colvar
        }

        if (ncol(uniquerow) > 1 && ncol(uniquecol) > 1) {
          if (unitvar %in% names(plotsampcnt)) {
            plotsampcnt <- FIESTA::add0unit(x=plotsampcnt, xvar=mergevar.row, uniquex=uniquerow, 
			unitvar=unitvar, xvar2=mergevar.col, uniquex2=uniquecol, xvar.add0=row.add0)
          } else {
            plotsampcnt <- FIESTA::add0unit(x=plotsampcnt, xvar=mergevar.row, uniquex=uniquerow, 
			xvar2=mergevar.col, uniquex2=uniquecol, xvar.add0=row.add0)
          }
        } else if (ncol(uniquerow) > 1) {
          plotsampcnt <- FIESTA::add0unit(x=plotsampcnt, xvar=mergevar.row, uniquex=uniquerow, 
			xvar.add0=row.add0)
        } else if (ncol(uniquecol) > 1) {
          plotsampcnt <- FIESTA::add0unit(x=plotsampcnt, xvar=mergevar.col, uniquex=uniquecol, 
			xvar.add0=col.add0)
        }

      } else {
 
        if (!is.null(uniquerow) && ncol(uniquerow) > 1) {
          xvar <- ifelse (rowvar %in% names(plotsampcnt), rowvar, row.orderby)
          plotsampcnt <- FIESTA::add0unit(x=plotsampcnt, xvar=xvar, uniquex=uniquerow, 
			xvar.add0=row.add0)
        }
      }
      rawdat$plotsampcnt <- setDF(plotsampcnt)
    }

    if (!is.null(pltdom.row)) { 
      if ("psq.pltdom" %in% names(pltdom.row)) {
        pltdom.row[, psq.pltdom := NULL]
        rawdat$pltdom.row <- pltdom.row 
      }
    }
    if (!is.null(pltdom.col)) { 
      if ("psq.pltdom" %in% names(pltdom.col)) {
        pltdom.col[, psq.pltdom := NULL]
        rawdat$pltdom.col <- pltdom.col 
      }
    }
    if (!is.null(pltdom.grp)) { 
      if ("psq.pltdom" %in% names(pltdom.grp)) {
        pltdom.grp[, psq.pltdom := NULL]
        rawdat$pltdom.grp <- pltdom.grp 
      }
    }

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
 			out_dsn=raw_dsn, out_layer=out_layer, overwrite_layer=overwrite_layer, 
			append_layer=append_layer)
        }
      }
    }    
    if (!ratio) {
      if (!is.null(unit.rowest.str)) {
        rawdat$unit.rowest.str <- unit.rowest.str
      }
      if (!is.null(unit.colest.str)) {
        rawdat$unit.colest.str <- unit.colest.str
      }
      if (!is.null(unit.grpest.str)) {
        rawdat$unit.grpest.str <- unit.grpest.str
      }
    }
  }

  ## GAIN/LOSS
  if (gainloss) {

    if (is.null(rowvar) || is.null(colvar)) {
      stop("must have rowvar and colvar to calculate gain/loss") 
    }
    ## Check
    rowcolvals <- unique(c(pltdom.grp[[rowvar]], pltdom.grp[[colvar]]))

    if (is.null(gainloss.vals)) {
      gainloss.vals <- rowcolvals
    } else {
      if (any(!gainloss.vals %in% rowcolvals)) {
       valsnotin <- gainloss.vals[which(!gainloss.vals %in% rowcolvals)]
       stop(paste("invalid gainloss.vals.. ", paste(valsnotin, collapse=", "), 
		"not in data"))
      }
    }
 
    numvars <- c("gain.est", "gain.se", "loss.est", "loss.se", "diff.est", "diff.se")
    charvars <- c(unitvars, "gain.val", "loss.val")

    if (length(rowcolvals) == 2) {
      est.gainloss <- data.frame(t(sapply(gainloss.vals, getgainloss, 
		pltdom.grp, plotid, rowvar, colvar, stratalut, unitvars, strvar,
		tabtype, areavar, unitarea, sumunits)))
    } else {
      est.gainloss <- data.frame(t(sapply(gainloss.vals, FIESTA::getgainloss, 
		pltdom.grp, plotid, rowvar, colvar, stratalut, unitvars, strvar,
		tabtype, areavar, unitarea, sumunits)))
    }
    est.gainloss[, numvars] <- lapply(est.gainloss[, numvars], as.numeric)
    est.gainloss[, charvars] <- lapply(est.gainloss[, charvars], as.character)

    ## Add 95 and 68% confidence intervals for gain.est, loss.est, diff.est
    CInames <- c("CI95left", "CI95right", "CI68left", "CI68right")

    est.gainloss <- FIESTA::addCI(est.gainloss, estnm="gain.est", 
		senm="gain.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("gain.", CInames))

    est.gainloss <- FIESTA::addCI(est.gainloss, estnm="loss.est", 
		senm="loss.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("loss.", CInames))

    est.gainloss <- FIESTA::addCI(est.gainloss, estnm="diff.est", 
		senm="diff.se", gainloss=gainloss)
    setnames(est.gainloss, CInames, paste0("diff.", CInames))

    ## Add to return list
    rawdat$est.gainloss <- est.gainloss

    if (savedata) {
      out_layer <- paste(outfn.rawdat, "gainloss", sep="_")
      datExportData(est.gainloss, out_fmt=raw_fmt, outfolder=rawfolder, 
 		out_dsn=raw_dsn, out_layer=out_layer, overwrite_layer=overwrite_layer, 
		append_layer=append_layer)
    }          
  }
  
  if (rawdata) {
    returnlst$raw <- rawdat
  }
  return(returnlst)
}

