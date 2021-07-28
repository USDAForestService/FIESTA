modMApop <- function(cond=NULL, plt=NULL, tree=NULL, seed=NULL, 
	pltassgn=NULL, dsn=NULL, puniqueid="CN", pltassgnid="CN", pjoinid="CN", 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", 
	areawt="CONDPROP_UNADJ", evalid=NULL, invyrs=NULL, intensity=NULL, 
	ACI=FALSE, adj="samp", unitvar=NULL, unitvar2=NULL, unitarea=NULL, 
	areavar="ACRES", areaunits="acres", unitlut=NULL, minplotnum.unit=10, 
	unit.action="keep", npixelvar="npixels", prednames=NULL, predfac=NULL, 
	strata=FALSE, strvar=NULL, stratcombine=TRUE, minplotnum.strat=2, 
	saveobj=FALSE, savedata=FALSE, outfolder=NULL, out_fmt="csv", 
	out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, append_layer=FALSE, MAdata=NULL, pltdat=NULL, 
	MAmodeldat=NULL, gui=FALSE){

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
    areavar=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  }

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modMApop)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=expcondtab=strwtvar <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  adjtree <- FALSE
  popType <- "VOL"
  nonsamp.pfilter=nonsamp.cfilter <- NULL
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
  if (!is.null(MAdata)) {
    list.items <- c("bnd", "plt", "cond", "unitarea", "unitvar")
    MAdata <- FIESTA::pcheck.object(MAdata, "MAdata", list.items=list.items)
    #bnd <- MAdata$bnd
    plt <- MAdata$plt
    cond <- MAdata$cond
    tree <- MAdata$tree
    seed <- MAdata$seed
    pltassgn <- MAdata$pltassgn
    pltassgnid <- MAdata$pltassgnid
    unitarea <- MAdata$unitarea
    areavar <- MAdata$areavar
    unitlut <- MAdata$unitlut
    #npixelvar <- MAdata$npixelvar
    puniqueid <- MAdata$puniqueid
    pjoinid <- MAdata$pjoinid

    if (is.null(unitvar)) {
      unitvar <- MAdata$unitvar
      unitvar2 <- MAdata$unitvar2
    } 
    if (is.null(npixelvar)) {
      npixelvar <- MAmodeldat$npixelvar
    }
    if (is.null(prednames)) {
      prednames <- MAdata$prednames
    } else {
      if (!all(prednames %in% MAdata$prednames))
        stop("invalid prednames: ", 
	 	toString(prednames[!prednames %in% MAdata$prednames]))
    }

    #if (is.null(prednames)) {
    #  stop("must include prednames")
    #}
    if (is.null(predfac)) {
      predfac <- MAdata$predfac
    }
    predfac <- predfac[predfac %in% prednames]
    
    if (strata) {
      if (is.null(strvar)) {
        strvar <- MAdata$strvar
      }
      if (is.null(strvar)) {
        stop("must include strvar if strata=TRUE")
      }
      if (!strvar %in% predfac) {
        predfac <- c(predfac, strvar)
      }
      stratalut <- MAdata$stratalut
      strwtvar <- MAdata$strwtvar
      strunitvars <- c(unitvar, unitvar2, strvar)
      if ("strwt" %in% names(stratalut)) {
        setnames(stratalut, "strwt", "Prop")
      }
    }

  } else {
    if (!is.null(pltdat)) {
      list.items <- c("bndx", "tabs", "xypltx")
      pltdat <- FIESTA::pcheck.object(pltdat, "pltdat", list.items=list.items)

      ## Extract list objects
      puniqueid <- pltdat$puniqueid
      if ("tabs" %in% names(pltdat)) {
        pjoinid <- pltdat$pjoinid
        plt <- pltdat$tabs$pltx
        cond <- pltdat$tabs$condx
        tree <- pltdat$tabs$treex
        seed <- pltdat$tabs$seedx
      } else {
        pjoinid <- puniqueid
        plt <- pltdat$plt
        cond <- pltdat$cond
        tree <- pltdat$tree
        seed <- pltdat$seed
      }
    }
    if (!is.null(MAmodeldat)) {
      list.items <- c("pltassgn", "dunitlut", "dunitvar", "predfac", "npixelvar", 
		"pltassgnid", "dunitarea", "areavar")
      MAmodeldat <- FIESTA::pcheck.object(MAmodeldat, "MAmodeldat", list.items=list.items)
      pltassgn <- MAmodeldat$pltassgn
      pltassgnid <- MAmodeldat$pltassgnid
      unitlut <- MAmodeldat$dunitlut
      unitvar <- MAmodeldat$dunitvar
      unitvar2 <- MAmodeldat$dunitvar2
      unitarea <- MAmodeldat$dunitarea
      areavar <- MAmodeldat$areavar
      #npixelvar <- MAmodeldat$npixelvar

      if (is.null(npixelvar)) {
        npixelvar <- MAmodeldat$npixelvar
      }
      if (is.null(prednames)) {
        prednames <- MAmodeldat$prednames
      } else {
        if (!all(prednames %in% MAmodeldat$prednames))
          stop("invalid prednames: ", 
	 	toString(prednames[!prednames %in% MAmodeldat$prednames]))
      }

      #if (is.null(prednames)) {
      #  message("no prednames included")
      #}

      if (is.null(predfac)) {
        predfac <- MAmodeldat$predfac
      }
      predfac <- predfac[predfac %in% prednames]
      if (strata) {
        if (is.null(strvar)) {
          strvar <- MAmodeldat$strvar
        }
        if (is.null(strvar)) {
          stop("must include strvar if strata=TRUE")
        }
        if (!strvar %in% predfac) {
          predfac <- c(predfac, strvar)
        }
        stratalut <- MAmodeldat$stratalut
        strwtvar <- MAmodeldat$strwtvar
        strunitvars <- c(unitvar, unitvar2, strvar)
        if ("strwt" %in% names(stratalut)) {
          setnames(stratalut, "strwt", "Prop")
        }
      }
    }
  } 

  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="MA", popType=popType, strata=strata,
 	tree=tree, cond=cond, plt=plt, seed=seed, pltassgn=pltassgn, dsn=dsn,
 	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, areawt=areawt,
 	puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid,
 	adj=adj, invyrs=invyrs, intensity=intensity, ACI=ACI, 
	nonsamp.pfilter=nonsamp.pfilter, nonsamp.cfilter=nonsamp.cfilter, 
	unitarea=unitarea, unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
	areaunits=areaunits, unit.action=unit.action, stratalut=stratalut, 
	strvar=strvar, stratcombine=stratcombine, prednames=prednames, predfac=predfac)
  if (is.null(popcheck)) return(NULL)
  condx <- popcheck$condx
  pltcondx <- popcheck$pltcondx
  treef <- popcheck$treef
  seedf <- popcheck$seedf
  pltassgnx <- popcheck$pltassgnx
  cuniqueid <- popcheck$cuniqueid
  condid <- popcheck$condid
  tuniqueid <- popcheck$tuniqueid
  pltassgnid <- popcheck$pltassgnid
  ACI.filter <- popcheck$ACI.filter
  adj <- popcheck$adj
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  unitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  areaunits <- popcheck$areaunits
  unit.action <- popcheck$unit.action
  strata <- popcheck$strata
  stratalut <- popcheck$stratalut
  stratcombine <- popcheck$stratcombine
  strvar <- popcheck$strvar
  prednames <- popcheck$prednames
  predfac <- popcheck$predfac
  P2POINTCNT <- popcheck$P2POINTCNT 
  plotsampcnt <- popcheck$plotsampcnt
  condsampcnt <- popcheck$condsampcnt
  states <- popcheck$states
  invyrs <- popcheck$invyrs
  cvars2keep <- popcheck$cvars2keep
  pvars2keep <- popcheck$pvars2keep

  ###################################################################################
  ## CHECK ESTIMATION UNIT or STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unit.action='combine', combines estimation units to reach minplotnum.unit.
  ###################################################################################
  if (strata) {
      auxlut <- stratalut
      strwtvar <- "Prop"
      makedummy <- FALSE
  } else {
      auxlut <- unitlut
      makedummy <- TRUE
  }
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, 
        	module="MA", strata=strata, unitvar=unitvar, unitvar2=unitvar2, 
		unitarea=unitarea, areavar=areavar, minplotnum.unit=minplotnum.unit,
 		unit.action=unit.action, auxlut=auxlut, prednames=prednames, 
		strvar=strvar, predfac=predfac, makedummy=makedummy, npixelvar=npixelvar, 
		strwtvar="Prop", stratcombine=stratcombine, minplotnum.strat=minplotnum.strat, 
		removeifnostrata=TRUE)
  pltassgnx <- auxdat$pltx
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  unitlut <- auxdat$auxlut
  prednames <- auxdat$prednames
  predfac <- auxdat$predfac
  npixels <- auxdat$npixels
  stratcombinelut <- auxdat$unitstrgrplut
  strvar <- auxdat$strvar
  unitNA <- auxdat$unitNA
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
  condx <- condx[pltassgnx[,c(pltassgnid, unitvar, strvar, prednames), with=FALSE]]

  ## If more than one unitvar, 
  ## split the concatenated unitvar variable to keep original columns
  if (!is.null(unitvar2)) {
    condx[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
  }
  if (adj == "samp") {
    bycond <- TRUE
    adjtree <- TRUE
    adjfacdata <- getadjfactorGB(treex=treef, seedx=seedf, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, unitlut=unitlut, 
		unitvars=unitvar, strvars=strvar, unitarea=unitarea, areavar=areavar, 
		cvars2keep=cvars2keep)
    condx <- adjfacdata$condx
    unitlut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab
 
  } else if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE
    adjfacdata <- FIESTA::getadjfactorPLOT(treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
    seedf <- adjfacdata$seedx

  } else {
    setkeyv(condx, c(cuniqueid, condid))
  }

  ###################################################################################
  ## Return population data objects
  ###################################################################################
  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- list(condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, 
	condid=condid, ACI.filter=ACI.filter, unitarea=unitarea, areavar=areavar,
	areaunits=areaunits, unitvar=unitvar, unitvars=unitvars, unitlut=unitlut, 
	npixels=npixels, npixelvar=npixelvar, prednames=prednames, 
	expcondtab=expcondtab, plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
	states=states, invyrs=invyrs, estvar.area=estvar.area, strata=strata, adj=adj)

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }
  if (!is.null(seedf)) {
    returnlst$seedx <- seedf
  }
  returnlst$prednames <- prednames
  returnlst$predfac <- predfac

  if (strata) {
    setkeyv(unitlut, c(unitvar, unitvar2, strvar))
    returnlst$unitlut <- unitlut
    returnlst$strvar <- strvar
    if (!is.null(stratcombinelut)) {
      returnlst$stratcombinelut <- stratcombinelut
    }
  }

    returnlst$prednames <- prednames
    returnlst$predfac <- predfac


  ###################################################################################
  ## Save population data objects
  ###################################################################################

  if (saveobj) {
    objfn <- getoutfn(outfn="MApopdat", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=outfn.date, ext="rda")
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

    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(unitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
  }

  return(returnlst)
}

