modPB <- function(pnt=NULL, pltpct=NULL, plotid="plot_id", pntid=NULL, 
	pltpctvars=NULL, plt=NULL, pltassgn=NULL, puniqueid="CN", pltassgnid="CN",
	plt.nonsamp.filter=NULL, tabtype="PCT", strata=FALSE, sumunits=FALSE,   
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", 
	unitcombine=FALSE, stratalut=NULL, strvar="STRATUMCD", getwt=TRUE, 
	getwtvar="P1POINTCNT", stratcombine=TRUE, ratio=FALSE, landarea="ALL", 
	landarea.filter=NULL, pnt.nonsamp.filter=NULL, pnt.filter=NULL, 
	plt.filter=NULL, rowvar=NULL, colvar=NULL, row.orderby=NULL, 
	col.orderby=NULL, row.add0=FALSE, col.add0=FALSE, rowlut=NULL, 
	collut=NULL, domlut=NULL, domvarlst=NULL, ratioden="ROWVAR", 
	allin1=FALSE, estround=3, pseround=3, estnull=0, psenull="--", 
	divideby=NULL, savedata=FALSE, rawdata=FALSE, outfolder=NULL, outfn=NULL, 
	outfn.pre=NULL, outfn.date=TRUE, overwrite=FALSE, addtitle=TRUE, 
	returntitle=FALSE, title.main=NULL, title.ref=NULL, title.rowvar=NULL, 
	title.colvar=NULL, title.unitvar=NULL, title.filter=NULL, 
	title.units="acres", gainloss=FALSE, gainloss.vals=NULL, 
	PBpopdat=NULL, gui=FALSE){
 

  ##################################################################
  ## INITIALIZE SETTINGS
  ##################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables
  TOTAL=ONEUNIT=n.total=n.strata=strwt=NBRPNTS=psq.pltdom=
	  uniqueid=p.pltdom.n=nbrpts.pltdom.n=PtsPerPlot=nbrpts.pltdom=
	  value=p.pltdom=PBvars2keep=title.est=title.pse=title.estpse=
	  outfn.estpse <- NULL

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(FIESTA::modPB))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## If gui.. set variables to NULL
  if (gui) pnt=pntid=plotid=puniqueid=landarea=strvar=areavar=PBvars2keep <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  minplotnum <- 10
  returnPBpopdat <- TRUE 
  parameters <- FALSE

  ##################################################################
  ## Check population data
  ########################################################
  if (is.null(PBpopdat)) {
    PBpopdat <- modPBpop(pnt=pnt, pltpct=pltpct, plotid=plotid, pntid=pntid,
 	pltpctvars=pltpctvars, plt=plt, pltassgn=pltassgn, puniqueid=puniqueid, 
	pltassgnid=pltassgnid, plt.nonsamp.filter=plt.nonsamp.filter, tabtype=tabtype,
	sumunits=sumunits, strata=strata, unitarea=unitarea, unitvar=unitvar, 
	unitvar2=unitvar2, unitcombine=unitcombine, stratalut=stratalut, 
	strvar=strvar, getwt=getwt, getwtvar=getwtvar, stratcombine=stratcombine, 
	gui=gui)
  } else {
    returnPBpopdat <- FALSE
    if (!is.list(PBpopdat))
      stop("PBpopdat must be a list")
    listitems <- c("PBx", "plotid", "pntid", "tabtype", "getprop")
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
  unitarea <- PBpopdat$unitarea
  areavar <- PBpopdat$areavar
  strlut <- PBpopdat$strlut
  strvar <- PBpopdat$strvar
  plotsampcnt <- PBpopdat$plotsampcnt
  stratcombinelut <- PBpopdat$stratcombinelut
  getprop <- PBpopdat$getprop
  if (!getprop) {
    rowvar <- PBpopdat$rowvar
    PBvars2keep <- "p.pltdom"
  }
  unitvars <- c(unitvar, unitvar2)
  strunitvars <- c(unitvar, strvar)

  ## Check tabtype
  tabtype <- FIESTA::pcheck.varchar(var2check=tabtype, varnm="tabtype", gui=gui, 
		checklst=c("PCT", "AREA"), caption="Table output type", 
		warn="invalid tabtype")
  ## Check unitcombine 
  unitcombine <- FIESTA::pcheck.logical(unitcombine, varnm="unitcombine", 
		title="Combine estimation units?", first="YES", gui=gui, stopifnull=TRUE)
  if ((tabtype == "AREA" || unitcombine) && is.null(unitarea)) {
    if (sumunits) {
      stop("need unitarea to combine estimation units")
    } else {
      stop("need unitarea to return acre estimates")
    }
  }

  ###################################################################################
  ## Check parameters and apply plot and pnt filters
  ###################################################################################
  estdat <- check.estdataPB(gui=gui, PBx=PBx, plotid=plotid, pntid=pntid, tabtype=tabtype,
	ratio=ratio, plt.filter=plt.filter, pnt.nonsamp.filter=pnt.nonsamp.filter,
	landarea=landarea, landarea.filter=landarea.filter, pnt.filter=pnt.filter, 
	sumunits=sumunits, allin1=allin1, estround=estround, pseround=pseround, 
	divideby=divideby, savedata=savedata, addtitle=addtitle, returntitle=returntitle, 
	rawdata=rawdata, outfolder=outfolder) 
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
  savedata <- estdat$savedata
  outfolder <- estdat$outfolder


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
  if (ratio)
    PBx.d <- rowcolinfo$PBx.d

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
  alltitlelst <- check.titles(dat=PBall, esttype="PHOTO", phototype=phototype, 
	tabtype=tabtype, sumunits=sumunits, title.main=title.main, title.ref=title.ref, 
	title.rowvar=title.rowvar, title.colvar=title.colvar, title.unitvar=title.unitvar, 
	title.filter=title.filter, title.units=title.units, unitvar=unitvar, rowvar=rowvar,
 	colvar=colvar, addtitle=addtitle, returntitle=returntitle, rawdata=rawdata, 
	landarea=landarea, plt.filter=plt.filter, cond.filter=pnt.filter, 
	allin1=allin1, outfn=outfn, outfn.pre=outfn.pre)
  title.unitvar <- alltitlelst$title.unitvar
  title.est <- alltitlelst$title.est
  title.pse <- alltitlelst$title.pse
  title.estpse <- alltitlelst$title.estpse
  title.ref <- alltitlelst$title.ref
  outfn.estpse <- alltitlelst$outfn.estpse
  outfn.param <- alltitlelst$outfn.param
  if (rawdata) outfn.rawdat <- alltitlelst$outfn.rawdat


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
		strattype="post", strlut=strlut, strunitvars=strunitvars, unitvars=unitvar,
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
		strattype="post", strlut=strlut, strunitvars=strunitvars, unitvars=unitvar,
		strvar=strvar)
      unit.rowest <- pbar.rowest$est.unit
      if (rawdata) unit.rowest.str <- pbar.rowest$ybardat
      setkeyv(unit.rowest, c(unitvar, rowvar))

      ## Get filter value for row and column
      row.filterval <- ifelse (is.numeric(unit.rowest[[rowvar]]), 9999, "NOTinDOMAIN")
    }
   
    ## Get column (and cell) estimate  
    if (colvar != "NONE") {
      pbar.colest <- PBest.pbar(dom.prop=pltdom.col, uniqueid=puniqueid, 
		domain=colvar, strattype="post", strlut=strlut, strunitvars=strunitvars,
 		unitvars=unitvar, strvar=strvar)
      unit.colest <- pbar.colest$est.unit
      if (rawdata) unit.colest.str <- pbar.colest$ybardat
      setkeyv(unit.colest, c(unitvar, colvar))

      ## Get filter value for column
      col.filterval <- ifelse (is.numeric(unit.colest[[colvar]]), 9999, "NOTinDOMAIN")

      pbar.grpest <- PBest.pbar(dom.prop=pltdom.grp, uniqueid=puniqueid,
 		domain=grpvar, strattype="post", strlut=strlut, strunitvars=strunitvars,
 		unitvars=unitvar, strvar=strvar)
      unit.grpest <- pbar.grpest$est.unit
      if (rawdata) unit.grpest.str <- pbar.grpest$ybardat
      setkeyv(unit.grpest, c(unitvar, grpvar))
    }
  } else {  ## ratio=TRUE

    phatcol <- "rhat"
    phatcol.var <- "rhat.var"

    unit.grpest <- PBest.pbarRatio(dom.prop.n=pltdom.n, dom.prop.d=pltdom.d, 
		uniqueid=plotid, domain=domain, attribute=attribute, strlut=strlut, 
		strunitvars=strunitvars, unitvars=unitvar, strvar=strvar)
    setkeyv(unit.grpest, c(unitvar, grpvar))

    if (tabtype == "AREA") {
      pltdom <- pltdom.n
      names(pltdom) <- sub("\\.n", "", names(pltdom.n))
      unit.grpest.domtot <- PBest.pbar(dom.prop=pltdom, uniqueid=plotid, 
		domain=domain, strlut=strlut, strunitvars=strunitvars, unitvars=unitvar,
		strvar=strvar, strattype="post")$est.unit
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
        unit.rowest <- PBgetest(unit.rowest, phatcol=phatcol, phatcol.var=phatcol.var)
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
      unit.colest <- PBgetest(unit.colest, phatcol=phatcol, phatcol.var=phatcol.var)
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
      unit.grpest <- PBgetest(unit.grpest, phatcol=phatcol, phatcol.var=phatcol.var)
    }
  }
        
  if (!sumunits && length(unique(strlut[[unitvar]])) > 1 && !ratio && tabtype != "PCT") {
    ## AGGREGATE UNIT strlut FOR ROWVAR and GRAND TOTAL
    strlut2 <- data.table(strlut, ONEUNIT=1)
    strunitvars2 <- c("ONEUNIT", strvar)
    if (is.null(getwtvar) || !getwtvar %in% names(strlut2)) getwtvar <- "strwt"

    strlut2 <- strlut2[, lapply(.SD, sum, na.rm=TRUE), 
		by=strunitvars2, .SDcols=c(getwtvar, "n.strata")]
    strlut2[, strwt:=prop.table(get(getwtvar)), by="ONEUNIT"]
    strlut2[, n.total := sum(n.strata)]
    setkeyv(strlut2, strunitvars2)

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
		domain=rowvar, strattype="post", strlut=strlut2, strunitvars=strunitvars2,
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
		domain="TOTAL", strattype="post", strlut=strlut2, strunitvars=strunitvars2,
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

  if (rawdata) {
    rawdat <- list(unitvars=unitvars, strvar=strvar, stratdat=setDF(strlut))
    if (!is.null(rowvar) && rowvar != "NONE") {
      rawdat$rowvar <- rowvar
      rawdat$pltdom.row <- pltdom.row
    }
    if (!is.null(colvar) && colvar != "NONE") {
      rawdat$colvar <- colvar 
      rawdat$pltdom.col <- pltdom.col
      rawdat$pltdom.grp <- pltdom.grp
    }
    if (stratcombine && !is.null(stratcombinelut)) 
      rawdat$stratcombinelut <- stratcombinelut

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
  }
 
  CI <- TRUE
  estnm <- "est"
  tabs <- est.outtabs(esttype="PHOTO", phototype=tabtype, photoratio=ratio, 
	sumunits=sumunits, areavar=areavar, unitvar=unitvar, unitvar2=unitvar2,
 	unit.totest=unit.totest, unit.rowest=unit.rowest, unit.colest=unit.colest,
 	unit.grpest=unit.grpest, rowvar=rowvar, colvar=colvar, uniquerow=uniquerow,
 	uniquecol=uniquecol, rowunit=rowunit, totunit=totunit, allin1=allin1, 
	savedata=savedata, addtitle=addtitle, returntitle=returntitle, title.ref=title.ref, 
	title.colvar=title.colvar, title.rowvar=title.rowvar, title.unitvar=title.unitvar,
 	title.estpse=title.estpse, title.est=title.est, title.pse=title.pse,
 	outfn.estpse=outfn.estpse, outfolder=outfolder, outfn.date=outfn.date, 
	overwrite=overwrite, estnm=estnm, estround=estround, pseround=pseround, 
	estnull=estnull, psenull=psenull, divideby=NULL, rawdata=rawdata, CI=CI, 
	rawdat=rawdat) 
 
  est2return <- tabs$tabest
  pse2return <- tabs$tabpse
  if (rawdata) rawdat <- tabs$rawdat
  if (returntitle) titlelst <- tabs$titlelst


  ## GAIN/LOSS
  if (gainloss) {

    if (is.null(rowvar) || is.null(colvar))
      stop("must have rowvar and colvar to calculate gain/loss") 
 
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
		pltdom.grp, plotid, rowvar, colvar, strlut, unitvars, strvar,
		tabtype, areavar, unitarea, sumunits)))
    } else {
      est.gainloss <- data.frame(t(sapply(gainloss.vals, FIESTA::getgainloss, 
		pltdom.grp, plotid, rowvar, colvar, strlut, unitvars, strvar,
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
      outfn.gainloss <- paste(outfn.rawdat, "gainloss", sep="_")
      write2csv(est.gainloss, outfolder=paste(outfolder, "rawdata", sep="/"), 
		outfilenm=outfn.gainloss, outfn.date=outfn.date, overwrite=overwrite)
    }          
  }

 
  if (parameters) {
    ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
    ###########################################################
    if(is.null(outfn)) {
      outparamfnbase <- paste(outfn.param, format(Sys.time(), "%Y%m%d"), sep="_")
    } else {
      outparamfnbase <- paste0(outfn, "_parameters_", format(Sys.time(), "%Y%m%d"))
    }
    outparamfn <- fileexistsnm(outfolder, outparamfnbase, "txt")

    outfile <- file(paste(outfolder, "/", outparamfn, ".txt", sep=""), "w")
    cat(  "pnt = ", as.character(bquote(pnt)), "\n",
      "pltassgn = ", as.character(bquote(pltassgn)), "\n",
      "pntid = \"", pntid, "\"", "\n",
      "plotid = \"", plotid, "\"", "\n",
      "puniqueid = \"", puniqueid, "\"", "\n",
      "tabtype = \"", tabtype, "\"", "\n",
      "sumunits = \"", sumunits, "\"", "\n",
      "strata = \"", strata, "\"", "\n",
      "ratio = \"", ratio, "\"", "\n",
      "landarea = \"", landarea, "\"", "\n",
      "landarea.filter = \"", landarea.filter, "\"", "\n",
      "pnt.nonsamp.filter = \"", pnt.nonsamp.filter, "\"", "\n",
      "pnt.filter = \"", pnt.filter, "\"", "\n",
      "plt.filter = \"", plt.filter, "\"", "\n",
      "unitvar = \"", unitvar, "\"", "\n",
      "unitarea = \"", as.character(bquote(unitarea)), "\n",
      "areavar = \"", areavar, "\"", "\n",
      "unitcombine = \"", unitcombine, "\"", "\n",
      "stratalut = ", as.character(bquote(stratalut)), "\n",
      "strvar = \"", strvar, "\"", "\n",
      "stratcombine = \"", stratcombine, "\"", "\n",
      "getwt = \"", getwt, "\"", "\n",
      "getwtvar = \"", getwtvar, "\"", "\n",
      "rowvar = \"", rowvar, "\"", "\n",
      "colvar = \"", colvar, "\"", "\n",
      "row.orderby = \"", row.orderby, "\"", "\n",
      "col.orderby = \"", col.orderby, "\"", "\n",
      "row.add0 = \"", row.add0, "\"", "\n",
      "col.add0 = \"", col.add0, "\"", "\n",
      "rowlut = ", as.character(bquote(rowlut)), "\n",
      "collut = ", as.character(bquote(collut)), "\n",
      "domlut <- ", as.character(bquote(domlut)), "\n",
      "domvarlst <- ", as.character(bquote(domvarlst)), "\n",
      "ratioden = \"", ratioden, "\"", "\n",
      "allin1 = ", allin1, "\n",
      "savedata = ", savedata, "\n",
      "outfolder = \"", outfolder, "\"", "\n",
      "outfn = \"", outfn, "\"", "\n",
      "outfn.pre = \"", outfn.pre, "\"", "\n",
      "outfn.date = ", outfn.date, "\n",
      "overwrite = ", overwrite, "\n",
      "addtitle = ", addtitle, "\n",
      "title.main = \"", title.main, "\"", "\n",
      "title.ref = \"", title.ref, "\"", "\n",
      "title.rowvar = \"", title.rowvar, "\"", "\n",
      "title.colvar = \"", title.colvar, "\"", "\n",
      "title.unitvar = \"", title.unitvar, "\"", "\n",
      "title.filter = \"", title.filter, "\"", "\n",
      "returntitle = ", returntitle, "\n",
      "estround = \"", estround, "\"", "\n",
      "pseround = \"", pseround, "\"", "\n",
      "rawdata = ", rawdata, "\n", "\n",
      "gui = ", gui, "\n", "\n",
    file = outfile, sep="")

    cat(  "est <- modPB(pnt=pnt, pltassgn=pltassgn, pntid=pntid, plotid=plotid,
	Npts=Npts, tabtype=tabtype, sumunits=sumunits, strata=strata, ratio=ratio, 
	landarea=landarea, landarea.filter=landarea.filter, nonsamp.filter=nonsamp.filter,
 	pnt.filter=pnt.filter, plt.filter=plt.filter, unitvar=unitvar, unitarea=unitarea,
 	areavar=areavar, stratcombine=stratcombine, stratalut=stratalut, strvar=strvar, 
	getwt=getwt, getwtvar=getwtvar, rowvar=rowvar, colvar=colvar, row.orderby=row.orderby,
	col.orderby=col.orderby, row.add0=row.add0, col.add0=col.add0, rowlut=rowlut,
 	collut=collut, domlut=domlut, domvarlst=domvarlst, ratioden=ratioden, 
	allin1=allin1, savedata=savedata, outfolder=outfolder, outfn=outfn, 
	outfn.pre=outfn.pre, outfn.date=outfn.date, overwrite=overwrite, addtitle=addtitle,
 	title.main=NULL, title.ref=title.ref, title.rowvar=title.rowvar, title.colvar=title.colvar, 
	title.unitvar=title.unitvar, title.filter=title.filter, returntitle=returntitle,
 	estround=estround, pseround=pseround, rawdata=rawdata, gui=gui)", 
    file = outfile, sep="")
    close(outfile)
  }

  outfn.rawtab <- NULL
  if (rawdata) {

    rawfolder <- paste(outfolder, "rawdata", sep="/")
    if (!dir.exists(rawfolder)) dir.create(rawfolder)

    title.estpse <- paste(title.estpse, title.ref)
    for (i in 1:length(rawdat)) {
      outfile <- NULL
      tabnm <- names(rawdat[i])
 
      if (!tabnm %in% c("unitvars", "strvar", "rowvar", "colvar", "est.gainloss")) {
        rawtab <- rawdat[[i]]
        outfn.rawtab <- paste(outfn.rawdat, tabnm, sep="_") 
        if (tabnm %in% c("plotsampcnt", "pntsampcnt")) {
           write2csv(rawtab, outfolder=rawfolder, outfilenm=outfn.rawtab, 
			outfn.date=outfn.date, overwrite=overwrite)
        } else {
            #if ("NBRPNTS" %in% names(rawtable)) {
            #  charvars <- c("NBRPNTS", 
		 #		names(rawtable)[which(sapply(rawtable, class) %in% 
	       #				c("character", "factor"))])
            #} else {
            #  charvars <- NULL
            #}
            #suppressWarnings(save1tab(estpse=rawtab, title.estpse=title, 
		 #	outfolder=outfolder, allin1=allin1, coltitlerow=FALSE, 
		 #	charvars=charvars, rowtotal=FALSE, outfn.estpse=outfn.rawtab, 
		 #	addtitle=FALSE))
            suppressWarnings(save1tab(tab=rawtab, tab.title=title, 
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
  if (returntitle) returnlst$titlelst <- titlelst

  if (rawdata && !ratio) {
    if (!is.null(unit.rowest.str)) returnlst$raw$unit.rowest.str <- unit.rowest.str
    if (!is.null(unit.colest.str)) returnlst$raw$unit.colest.str <- unit.colest.str
    if (!is.null(unit.grpest.str)) returnlst$raw$unit.grpest.str <- unit.grpest.str
  }

#    if (savedata) {
#      returnlst$outfn.estpse <- outfn.estpse
#     if (rawdata) returnlst$outfn.rawdat <- outfn.rawdat
#    }

  return(returnlst)
}

