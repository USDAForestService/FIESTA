modMApop <- function(MAmethod, cond, plt=NULL, tree=NULL, seed=NULL, 
	pltassgn=NULL, dsn=NULL, puniqueid="CN", pltassgnid="CN", pjoinid="CN", 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", 
	areawt="CONDPROP_UNADJ", evalid=NULL, invyrs=NULL, ACI=FALSE, 
	adj="samp", unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", 
	areaunits="acres", unitcombine=FALSE, minplotnum.unit=10, unitlut=NULL, 
	npixelvar="npixels", prednames=NULL, predfac=NULL, PSstrvar=NULL, 
	stratcombine=TRUE, saveobj=FALSE, savedata=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite_dsn=FALSE, overwrite_layer=TRUE, MAdata=NULL, pltdat=NULL, 
	MAmodeldat=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates estimates of trees by domain using non-ratio estimators.
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
#  if (nargs() == 0 | is.null(estvar)) gui <- TRUE
  if (nargs() == 0) gui <- TRUE

  ## If gui.. set variables to NULL
  if (gui)  
    areavar=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
 
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(FIESTA::modMApop)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=expcondtab <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  adjtree <- FALSE
  popType <- "VOL"
  nonsamp.pfilter=nonsamp.cfilter <- NULL
  returnlst <- list()


  ## Check popType
  popTypelst <- c("VOL")
  popType <- FIESTA::pcheck.varchar(var2check=popType, varnm="popType", gui=gui, 
		checklst=popTypelst, caption="population type", stopifnull=TRUE,
		warn="only VOL is currently available")

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
  } 


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
    unitlut <- MAdata$unitlut
    unitvar <- MAdata$unitvar
    unitarea <- MAdata$unitarea
    areavar <- MAdata$areavar
    npixelvar <- MAdata$npixelvar
    predfac <- MAdata$predfac

    if (any(MAmethod %in% c("greg", "gregEN", "ratio"))) {
      if (is.null(prednames)) {
        prednames <- MAdata$prednames
      } else {
        if (!all(prednames %in% MAdata$prednames))
          stop("invalid prednames: ", 
			toString(prednames[!prednames %in% MAdata$prednames]))
        predfac <- predfac[predfac %in% prednames]
      }
    } else if (any(MAmethod == "PS")) {
      if (is.null(PSstrvar) && is.null(MAdata$PSstrvar)) {
        stop("must include PSstrvar if MAmethod = 'PS'")
      } else if (is.null(PSstrvar)) {
        PSstrvar <- MAdata$PSstrvar
      }
    }
  } else {
    if (!is.null(pltdat)) {
      list.items <- c("bndx", "tabs", "xypltx")
      pltdat <- FIESTA::pcheck.object(pltdat, "pltdat", list.items=list.items)

      ## Extract list objects
      puniqueid <- pltdat$puniqueid
      pjoinid <- pltdat$pjoinid
      plt <- pltdat$tabs$pltx
      cond <- pltdat$tabs$condx
      tree <- pltdat$tabs$treex
      seed <- pltdat$tabs$seedx
    }
    if (!is.null(MAmodeldat)) {
      list.items <- c("pltassgn", "domzonal", "domvar", "predfac", "npixelvar", 
		"pltassgnid", "domarea", "areavar")
      MAmodeldat <- FIESTA::pcheck.object(MAmodeldat, "MAmodeldat", list.items=list.items)
      pltassgn <- MAmodeldat$pltassgn
      pltassgnid <- MAmodeldat$pltassgnid
      unitlut <- MAmodeldat$domzonal
      unitvar <- MAmodeldat$domvar
      unitvar2 <- MAmodeldat$domvar2
      unitarea <- MAmodeldat$domarea
      areavar <- MAmodeldat$areavar
      npixelvar <- MAmodeldat$npixelvar
      predfac <- MAmodeldat$predfac

      if (any(MAmethod %in% c("greg", "gregEN", "ratio"))) {
        if (is.null(prednames)) {
          prednames <- MAmodeldat$prednames
        } else {
          if (!all(prednames %in% MAmodeldat$prednames))
            stop("invalid prednames: ", 
			toString(prednames[!prednames %in% MAmodeldat$prednames]))
          predfac <- predfac[predfac %in% prednames]
        }
      } else if (any(MAmethod == "PS")) {
        if (is.null(PSstrvar) && is.null(MAmodeldat$PSstrvar)) {
          stop("must include PSstrvar if MAmethod = 'PS'")
        } else if (is.null(PSstrvar)) {
          PSstrvar <- MAmodeldat$PSstrvar
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
  popcheck <- check.popdata(gui=gui, module="MA", method=MAmethod, tree=tree, 
	cond=cond, plt=plt, seed=seed, pltassgn=pltassgn, dsn=dsn,
 	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, areawt=areawt,
 	puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid,
 	invyrs=invyrs, ACI=ACI, adj=adj, nonsamp.pfilter=nonsamp.pfilter, 
	nonsamp.cfilter=nonsamp.cfilter, unitarea=unitarea, unitvar=unitvar, 
	unitvar2=unitvar2, areavar=areavar, areaunits=areaunits, unitcombine=unitcombine, 
	stratcombine=stratcombine, strvar=PSstrvar, prednames=prednames, predfac=predfac)
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
  unitcombine <- popcheck$unitcombine
  stratcombine <- popcheck$stratcombine
  PSstrvar <- popcheck$strvar
  prednames <- popcheck$prednames
  predfac <- popcheck$predfac
  P2POINTCNT <- popcheck$P2POINTCNT 
  plotsampcnt <- popcheck$plotsampcnt
  condsampcnt <- popcheck$condsampcnt
  states <- popcheck$states
  invyrs <- popcheck$invyrs
  cvars2keep <- popcheck$cvars2keep
  pvars2keep <- popcheck$pvars2keep
  MAmethod <- popcheck$method


  ###################################################################################
  ## CHECK unitarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and area by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
#  unitdat <- check.unitarea(unitarea=unitarea, pltx=pltassgnx, 
#	unitvars=c(unitvar, unitvar2), areavar=areavar, gui=gui)
#  unitarea <- unitdat$unitarea
#  areavar <- unitdat$areavar

  ###################################################################################
  ## CHECK STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unitcombine=TRUE, combines estimation units to reach minplotnum.unit.
  ###################################################################################
  strata <- ifelse(MAmethod == "PS", TRUE, FALSE)
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, module="MA",
		MAmethod=MAmethod, unitvar=unitvar, unitvar2=unitvar2, unitarea=unitarea,
		areavar=areavar, unitcombine=unitcombine, auxlut=unitlut, strata=strata, 
		PSstrvar=PSstrvar, prednames=prednames, predfac=predfac, npixelvar=npixelvar, 
		stratcombine=stratcombine, removeifnostrata=TRUE)
  pltassgnx <- auxdat$pltx
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  unitlut <- auxdat$auxlut
  prednames <- auxdat$prednames
  predfac <- auxdat$predfac
  npixels <- auxdat$npixels
  stratcombinelut <- auxdat$unitstrgrplut
  PSstrvar <- auxdat$PSstrvar

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
  if (is.null(key(condx))) setkeyv(condx, c(cuniqueid, condid))
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)

  if (adj == "samp") {
    bycond <- TRUE
    adjtree <- TRUE

    ## Merge plot strata info to condx
    condx <- condx[pltassgnx[,c(pltassgnid, unitvar, PSstrvar, prednames), with=FALSE]]
 
    adjfacdata <- getadjfactorGB(treex=treef, seedx=seedf, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, unitlut=unitlut, 
		unitvars=unitvar, strvars=PSstrvar, unitarea=unitarea, areavar=areavar, 
		cvars2keep=cvars2keep)
    condx <- adjfacdata$condx
    unitlut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab
  } else if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE

    ## Merge plot strata info to condx
    condx <- condx[pltassgnx[,c(pltassgnid, unitvar, PSstrvar, prednames), with=FALSE]]

    adjfacdata <- FIESTA::getadjfactorPLOT(treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
    seedf <- adjfacdata$seedx
  } else {

    ## Merge plot strata info to condx
    condx <- condx[pltassgnx[,c(puniqueid, unitvar, prednames), with=FALSE]]
    setkeyv(condx, c(cuniqueid, condid))
  }

  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- list(condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, 
	condid=condid, ACI.filter=ACI.filter, unitarea=unitarea, areavar=areavar,
	areaunits=areaunits, unitvar=unitvar, unitvars=unitvars, unitlut=unitlut, 
	npixels=npixels, npixelvar=npixelvar, prednames=prednames, 
	expcondtab=expcondtab, plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
	states=states, invyrs=invyrs, MAmethod=MAmethod, estvar.area=estvar.area, 
	adj=adj)

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }
  if (!is.null(seedf)) {
    returnlst$seedx <- seedf
  }
  if (strata) {
    returnlst$PSstrvar <- PSstrvar
    if (!is.null(stratcombinelut)) {
      returnlst$stratcombinelut <- stratcombinelut
    }
  }

  if (any(MAmethod != "HT")) {
    returnlst$prednames <- prednames
    returnlst$predfac <- predfac
  }

  if (saveobj) {
    objfn <- getoutfn(outfn="MApopdat", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.date=outfn.date, ext="rda")
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(unitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }

  return(returnlst)
}

