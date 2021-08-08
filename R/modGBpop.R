modGBpop <- function(popType="VOL", cond=NULL, plt=NULL, tree=NULL, seed=NULL, 
	vsubpspp=NULL, vsubpstr=NULL, subplot=NULL, subp_cond=NULL, lulc=NULL, 
	pltassgn=NULL, dsn=NULL, puniqueid="CN", pltassgnid="PLT_CN", pjoinid="CN", 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", areawt="CONDPROP_UNADJ", 
	adj="samp", evalid=NULL, invyrs=NULL, intensity=NULL, ACI=FALSE, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", areaunits="acres",
	minplotnum.unit=10, unit.action="keep", strata=TRUE, stratalut=NULL, 
	strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", strwtvar="strwt",
	stratcombine=TRUE, minplotnum.strat=2, saveobj=FALSE, objnm="GBpopdat", 
	savedata=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL,
 	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE,
	GBdata=NULL, pltdat=NULL, GBstratdat=NULL, nonsamp.vfilter.fixed=FALSE, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data 'on-the-fly', including strata weights, number
  ## of plots by strata and estimation unit, strata-level expansion factors,
  ## and sample-based area adjustment factors.
  ## - checks input parameters and data tables, including removing nonsampled
  ##   plots and conditions (see check.popdata for details).
  ## - checks auxiliary data (i.e., stratification data).
  ## - calculates adjustment factors for nonresponse and appends an adjustment
  ##   variable to condition and tree data.
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
  ONEUNIT=n.total=n.strata=strwt=expcondtab=V1=SUBPCOND_PROP=SUBPCOND_PROP_UNADJ=
	treef=seedf=vcondsppf=vcondstrf=bndx <- NULL

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
		overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer, 
		append_layer=append_layer, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    if (out_fmt != "csv") {
      outfn.date <- FALSE
    }
  }


  ###################################################################################
  ## Load data
  ###################################################################################
  if (!is.null(GBdata)) {
    list.items <- c("cond", "unitarea", "unitvar")
    GBdata <- FIESTA::pcheck.object(GBdata, "GBdata", list.items=list.items)
    #bnd <- GBdata$bnd
    plt <- GBdata$plt
    cond <- GBdata$cond
    tree <- GBdata$tree
    seed <- GBdata$seed
    pltassgn <- GBdata$pltassgn
    pltassgnid <- GBdata$pltassgnid 
    unitarea <- GBdata$unitarea
    areavar <- GBdata$areavar
    stratalut <- GBdata$stratalut
    strvar <- GBdata$strvar
    strwtvar <- GBdata$strwtvar
    puniqueid <- GBdata$puniqueid
    pjoinid <- GBdata$pjoinid

    if (is.null(unitvar)) {
      unitvar <- GBdata$unitvar
      unitvar2 <- GBdata$unitvar2
    } 

  } else {
    if (!is.null(pltdat)) {
      if ("tabs" %in% names(pltdat)) {
        list.items <- c("tabs", "xypltx")
      } else {
        list.items <- c("states", "plt", "cond")
      }
      if (popType == "LULC") {
        list.items <- c(list.items, "lulcx")
      }
      if (popType == "P2VEG") {
        list.items <- c(list.items, "vsubpspp", "vsubpstr", "subplot", "subp_cond")
      }
      pltdat <- FIESTA::pcheck.object(pltdat, "pltdat", list.items=list.items)

      ## Extract list objects
      puniqueid <- pltdat$puniqueid
      if ("tabs" %in% names(pltdat)) {
        pjoinid <- pltdat$pjoinid
        plt <- pltdat$tabs$pltx
        cond <- pltdat$tabs$condx
        tree <- pltdat$tabs$treex
        seed <- pltdat$tabs$seedx
        if (popType == "LULC") {
          lulc <- pltdat$tabs$lulcx
        } else if (popType == "P2VEG") {
          vsubpspp <- pltdat$tabs$vsubpspp
          vsubpstr <- pltdat$tabs$vsubpstr
          subplot <- pltdat$tabs$subplot
          subp_cond <- pltdat$tabs$subp_cond
        }
      } else {
        pjoinid <- puniqueid
        plt <- pltdat$plt
        cond <- pltdat$cond
        tree <- pltdat$tree
        seed <- pltdat$seed
        if (popType == "LULC") {
          lulc <- pltdat$lulc
        } else if (popType == "P2VEG") {
          vsubpspp <- pltdat$vsubpspp
          vsubpstr <- pltdat$vsubpstr
          subplot <- pltdat$subplot
          subp_cond <- pltdat$subp_cond
        }
      }
    }
    if (!is.null(GBstratdat)) {
      list.items <- c("pltassgn", "unitarea", "unitvar", "stratalut", "strvar")
      GBstratdat <- FIESTA::pcheck.object(GBstratdat, "GBstratdat", list.items=list.items)
      bndx <- GBstratdat$bndx
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

  if (strwtvar != "strwt") {
    names(stratalut)[names(stratalut) == strwtvar] <- "strwt"
    strwtvar <- "strwt"
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
	nonsamp.vfilter.fixed=nonsamp.vfilter.fixed,
 	unitarea=unitarea, unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
	areaunits=areaunits, unit.action=unit.action, strata=strata, 
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
  unit.action <- popcheck$unit.action
  stratcombine <- popcheck$stratcombine
  strata <- popcheck$strata
  stratalut <- popcheck$stratalut
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
  tpropvars <- popcheck$tpropvars
  if (nonresp) {
    substrvar <- popcheck$substrvar
  } 
  #rm(popcheck)
  if (popType == "P2VEG") {
    vcondsppf <- popcheck$vcondsppf
    vcondstrf <- popcheck$vcondstrf
    areawt <- "SUBP_CONDPROP_UNADJ"
  }
 
  ###################################################################################
  ## CHECK STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unit.action='combine', combines estimation units to reach minplotnum.unit.
  ## If unitvar and unitvar2, concatenates variables to 1 unitvar
  ###################################################################################
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, unitvar=unitvar, 
	unitvar2=unitvar2, unitarea=unitarea, areavar=areavar, 
	minplotnum.unit=minplotnum.unit, unit.action=unit.action,
	strata=strata, auxlut=stratalut, strvar=strvar,  
	nonresp=nonresp, substrvar=substrvar, stratcombine=stratcombine, 
	minplotnum.strat=minplotnum.strat, removeifnostrata=TRUE, getwt=getwt, 				
	getwtvar=getwtvar, strwtvar=strwtvar, P2POINTCNT=P2POINTCNT)
  pltassgnx <- auxdat$pltx
  unitarea <- auxdat$unitarea
  stratalut <- auxdat$auxlut
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  strvar <- auxdat$strvar
  strwtvar <- auxdat$strwtvar
  stratcombinelut <- auxdat$unitstrgrplut
  if (nonresp) nonsampplots <- auxdat$nonsampplots
  strunitvars <- c(unitvar, strvar)
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid) 


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

  ## If more than one unitvar, 
  ## split the concatenated unitvar variable to keep original columns
  if (!is.null(unitvar2)) {
    condx[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
  }
  if (adj == "samp") {
    adjfacdata <- getadjfactorGB(condx=condx, treex=treef, seedx=seedf,
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, 
		vcondsppx=vcondsppf, vcondstrx=vcondstrf, vuniqueid=vuniqueid, 
		unitlut=stratalut, unitvars=unitvar, strvars=strvar, 
		unitarea=unitarea, areavar=areavar, areawt=areawt, tpropvars=tpropvars)
    condx <- adjfacdata$condx
    stratalut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab
    vcondsppf <- adjfacdata$vcondsppx
    vcondstrf <- adjfacdata$vcondstrx
    setorderv(stratalut, c(unitvars, strvar))

  } else if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE
    adjfacdata <- FIESTA::getadjfactorPLOT(treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condx
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
  } else {
    setkeyv(condx, c(cuniqueid, condid))
  }

  ###################################################################################
  ## Return population data objects
  ###################################################################################
  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- append(returnlst, list(popType=popType, bndx=bndx,
	condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, condid=condid, 
	ACI.filter=ACI.filter, unitarea=unitarea, areavar=areavar, 
	areaunits=areaunits, unitvar=unitvar, unitvars=unitvars, 
	strata=strata, stratalut=stratalut, strvar=strvar, strwtvar=strwtvar, 
	expcondtab=expcondtab, plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
	states=states, invyrs=invyrs, estvar.area=estvar.area, adj=adj))

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }
  if (!is.null(seedf)) {
    returnlst$seedx <- seedf
  }

  if (strata) {
    if (!is.null(stratcombinelut)) {
      returnlst$stratcombinelut <- stratcombinelut
    }
  }
  if (!is.null(evalid)) {
    returnlst$evalid <- evalid
  }
  if ("P2VEG" %in% popType) {
    returnlst$vcondsppx <- vcondsppf
    returnlst$vcondstrx <- vcondstrf
  }


  ###################################################################################
  ## Save population data objects
  ###################################################################################
  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(condx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="condx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(pltcondx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltcondx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)

    if (!is.null(treef)) {
      datExportData(treef, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="treex", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    }
    if (!is.null(seedf)) {
      datExportData(seedf, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="seedx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    }
    if (!is.null(vcondstrf)) {
      datExportData(vcondstrf, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vcondstrx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    }
    if (!is.null(vcondsppf)) {
      datExportData(vcondsppf, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vcondsppx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    }

    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
  }

  return(returnlst)
}
