modGBpop <- function(popType="VOL", cond=NULL, plt=NULL, tree=NULL, seed=NULL, 
	vsubpspp=NULL, vsubpstr=NULL, subplot=NULL, subp_cond=NULL, lulc=NULL, 
	pltassgn=NULL, dsn=NULL, puniqueid="CN", pltassgnid="PLT_CN", pjoinid="CN", 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", areawt="CONDPROP_UNADJ", 
	adj="samp", evalid=NULL, invyrs=NULL, intensity=NULL, ACI=FALSE, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", areaunits="acres",
	unitcombine=FALSE, minplotnum.unit=10, strata=TRUE, stratalut=NULL, 
	strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", strwtvar="strwt",
	stratcombine=TRUE, minplotnum.strat=2, saveobj=FALSE, objnm="GBpopdat", 
	savedata=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL,
 	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, GBdata=NULL, 
	pltdat=NULL, GBstratdat=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data 'on-the-fly', including strata weights, number
  ## of plots by strata and estimation unit, strata-level expansion factors,
  ## and sample-based area adjustment factors.
  ## - checks input parameters and data tables (see check.popdata for details)
  ## - checks unitarea data
  ## - checks auxiliary data (i.e., stratification data)
  ## - calculates adjustment factors for nonresponse
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE

  ## If gui.. set variables to NULL
  if (gui) {
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  }
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modGBpop)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=expcondtab=V1=SUBPCOND_PROP <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  adjtree <- FALSE
  nonresp=FALSE
  substrvar=nonsamp.pfilter=nonsamp.cfilter <- NULL
  returnlst <- list()

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check saveobj 
  saveobj <- FIESTA::pcheck.logical(saveobj, varnm="saveobj", 
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)
 
  ## Check output
  ########################################################
  if (savedata || saveobj) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
  }

  if (!is.null(GBdata)) {
    list.items <- c("bnd", "plt", "cond", "unitarea", "unitvar")
    GBdata <- FIESTA::pcheck.object(GBdata, "GBdata", list.items=list.items)
    #bnd <- GBdata$bnd
    plt <- GBdata$plt
    pltassgn <- GBdata$pltassgn
    cond <- GBdata$cond
    tree <- GBdata$tree
    seed <- GBdata$seed
    unitarea <- GBdata$unitarea
    areavar <- GBdata$areavar
    stratalut <- GBdata$stratalut
    strvar <- GBdata$strvar
    puniqueid <- GBdata$puniqueid
    pjoinid <- GBdata$pjoinid
    pltassgnid <- GBdata$pltassgnid 

    if (is.null(unitvar)) {
      unitvar <- GBdata$unitvar
      unitvar2 <- GBdata$unitvar2
    } 

  } else {
    if (!is.null(pltdat)) {
      list.items <- c("bndx", "tabs", "xypltx")
      if (popType == "LULC") {
        list.items <- c(list.items, "lulcx")
      }
      pltdat <- FIESTA::pcheck.object(pltdat, "pltdat", list.items=list.items)

      ## Extract list objects
      puniqueid <- pltdat$puniqueid
      pjoinid <- pltdat$pjoinid
      plt <- pltdat$tabs$pltx
      cond <- pltdat$tabs$condx
      tree <- pltdat$tabs$treex
      seed <- pltdat$tabs$seedx
      if (popType == "LULC") {
        lulc <- pltdat$tabs$lulcx
      }
    }
    if (!is.null(GBstratdat)) {
      list.items <- c("pltassgn", "unitarea", "unitvar", "stratalut", "strvar")
      GBstratdat <- FIESTA::pcheck.object(GBstratdat, "GBstratdat", list.items=list.items)
      pltassgn <- GBstratdat$pltassgn
      pltassgnid <- GBstratdat$pltassgnid
      unitarea <- GBstratdat$unitarea
      areavar <- GBstratdat$areavar
      stratalut <- GBstratdat$stratalut
      strvar <- GBstratdat$strvar
      getwt <- GBstratdat$getwt
      getwtvar <- GBstratdat$getwtvar
      strwtvar <- GBstratdat$strwtvar

      if (is.null(unitvar)) {
        unitvar <- GBstratdat$unitvar
        unitvar2 <- GBstratdat$unitvar2
      } 
    }
  } 
 
  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="GB", popType=popType, 
	tree=tree, cond=cond, plt=plt, seed=seed, vsubpspp=vsubpspp, vsubpstr=vsubpstr, 
	subplot=subplot, subp_cond=subp_cond, lulc=lulc, pltassgn=pltassgn, dsn=dsn, 
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, areawt=areawt, 
	puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid, 
	invyrs=invyrs, adj=adj, intensity=intensity, ACI=ACI, 
	nonsamp.pfilter=nonsamp.pfilter, nonsamp.cfilter=nonsamp.cfilter,
 	unitarea=unitarea, unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
	areaunits=areaunits, unitcombine=unitcombine, strata=strata, 
	stratalut=stratalut, strvar=strvar, stratcombine=stratcombine)
  if (is.null(popcheck)) return(NULL)
  condx <- popcheck$condx
  pltcondx <- popcheck$pltcondx
  treef <- popcheck$treef
  seedf <- popcheck$seedf
  pltassgnx <- popcheck$pltassgnx
  cuniqueid <- popcheck$cuniqueid
  condid <- popcheck$condid
  tuniqueid <- popcheck$tuniqueid
  vuniqueid <- popcheck$vuniqueid
  pltassgnid <- popcheck$pltassgnid
  ACI.filter <- popcheck$ACI.filter
  adj <- popcheck$adj
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  unitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  areaunits <- popcheck$areaunits
  unitcombine <- popcheck$unitcombine
  stratcombine <- popcheck$stratcombine
  strata <- popcheck$strata
  strvar <- popcheck$strvar
  nonresp <- popcheck$nonresp
  P2POINTCNT <- popcheck$P2POINTCNT 
  plotsampcnt <- popcheck$plotsampcnt
  condsampcnt <- popcheck$condsampcnt
  states <- popcheck$states
  invyrs <- popcheck$invyrs
  cvars2keep <- popcheck$cvars2keep
  pvars2keep <- popcheck$pvars2keep
  areawt <- popcheck$areawt
  if (strata) {
    stratalut <- popcheck$stratalut
  }
  if (nonresp) {
    substrvar <- popcheck$substrvar
  } 
  #rm(popcheck)
  if ("P2VEG" %in% popType) {
    pltassgn.P2VEG <- popcheck$pltassgn.P2VEG
    subp_condf <- popcheck$subp_condf
    vsubpsppf <- popcheck$vsubpsppf
    vsubpstrf <- popcheck$vsubpsppf
  }
 
  ###################################################################################
  ## CHECK STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unitcombine=TRUE, combines estimation units to reach minplotnum.unit.
  ## If unitvar and unitvar2, concatenates variables to 1 unitvar
  ###################################################################################
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, strata=strata,
		auxlut=stratalut, PSstrvar=strvar, nonresp=nonresp, substrvar=substrvar, 
		stratcombine=stratcombine, unitcombine=unitcombine, unitarea=unitarea, 
		unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
		minplotnum.unit=minplotnum.unit, minplotnum.strat=minplotnum.strat, 
		getwt=getwt, getwtvar=getwtvar, strwtvar=strwtvar, P2POINTCNT=P2POINTCNT)  
  pltassgnx <- auxdat$pltx
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  stratalut <- auxdat$auxlut
  strvar <- auxdat$PSstrvar
  stratcombinelut <- auxdat$unitstrgrplut
  if (nonresp) nonsampplots <- auxdat$nonsampplots
  strunitvars <- c(unitvar, strvar)
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)

  if ("P2VEG" %in% popType) {
    auxdatv <- check.auxiliary(pltx=pltassgn.P2VEG, puniqueid=pltassgnid, 
		strata=strata, auxlut=stratalut, PSstrvar=strvar, nonresp=nonresp,
 		substrvar=substrvar, stratcombine=stratcombine, unitcombine=unitcombine,
 		unitarea=unitarea, unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
		minplotnum.unit=minplotnum.unit, getwt=getwt, getwtvar=getwtvar, 
		P2POINTCNT=P2POINTCNT)  
    pltassgnv <- auxdatv$pltx
    stratalutv <- auxdatv$auxlut
    stratcombinelutv <- auxdatv$unitstrgrplut
    if (nonresp) nonsampplotsv <- auxdatv$nonsampplots
    #strunitvars <- c(unitvar, strvar)
    if (is.null(key(pltassgnv))) setkeyv(pltassgnv, pltassgnid)
    nveg.names <- c("nveg.strata", "nveg.total")
    setnames(stratalutv, c("n.strata", "n.total"), nveg.names)
    stratalut <- merge(stratalut, stratalutv[, c(key(stratalutv), nveg.names), with=FALSE],
 		by=key(stratalutv))

  }
 
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
  if (is.null(key(condx))) setkeyv(condx, c(cuniqueid, condid))
  condx <- condx[pltassgnx[,c(pltassgnid, strunitvars), with=FALSE]]

  if (adj == "samp") {
    adjtree <- TRUE
    adjfacdata <- getadjfactorGB(treex=treef, seedx=seedf, condx=condx, 
		cuniqueid=cuniqueid, condid=condid, tuniqueid=tuniqueid, 
		unitlut=stratalut, unitvars=unitvar, strvars=strvar, 
		unitarea=unitarea, areavar=areavar, areawt=areawt)
    condx <- adjfacdata$condx
    stratalut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab

    if (any(popType == "P2VEG")) {
      ## Merge plot strata info to condx
      if (is.null(key(subp_condf))) setkeyv(subp_condf, c(cuniqueid, condid))
      subp_condfx <- subp_condf[pltassgnx[,c(pltassgnid, strunitvars), with=FALSE]]

      subp_adj <- subp_condfx[, sum(SUBPCOND_PROP)/.N, by=c("PLT_CN", strunitvars)][, 
		sum(V1, na.rm=TRUE), by=strunitvars]
      setnames(subp_adj, "V1", "SUBPCOND_PROP_SUB")
      setkeyv(subp_adj, strunitvars)
      stratalut <- stratalut[subp_adj]
      stratalut$ADJ_FACTOR_P2VEG_SUBP <- stratalut$P2POINTCNT / subp_adj$SUBPCOND_PROP_SUB
    }
  } 

  setkeyv(stratalut, strunitvars)
  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- append(returnlst, list(popType=popType, 
	condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, condid=condid, 
	ACI.filter=ACI.filter, unitarea=unitarea, areavar=areavar, 
	areaunits=areaunits, unitvar=unitvar, unitvars=unitvars, 
	stratalut=stratalut, strvar=strvar, expcondtab=expcondtab, 
	plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, states=states, 
	invyrs=invyrs, estvar.area=estvar.area, adj=adj))

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }
  if (!is.null(seedf)) {
    returnlst$seedx <- seedf
  }

  if (!is.null(stratcombinelut)) 
    returnlst$stratcombinelut <- stratcombinelut
  if (!is.null(evalid)) {
    returnlst$evalid <- evalid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(condx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="condx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(pltcondx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltcondx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)

    if (!is.null(treef)) {
      datExportData(treef, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="treex", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }
    if (!is.null(seedf)) {
      datExportData(seedf, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="seedx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    }

    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }

  return(returnlst)
}
