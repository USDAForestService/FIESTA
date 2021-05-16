modSApop <- function(SAdoms=NULL, cond=NULL, tree=NULL, seed=NULL, plt=NULL, 
	pltassgn=NULL, dsn=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", 
	puniqueid="CN", pltassgnid="CN", pjoinid="CN", measCur=FALSE, measEndyr=NULL, 
	measEndyr.filter=NULL, invyrs=NULL, ACI=FALSE, adj="plot", dunitvar="DOMAIN", 
	dunitvar2=NULL, dunitarea=NULL, areavar="ACRES", areaunits="acres", 
	unitcombine=FALSE, dunitlut=NULL, prednames=NULL, predfac=NULL, 
	pvars2keep=NULL, cvars2keep=NULL, saveobj=FALSE, savedata=FALSE, 
	outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, SAdata=NULL, 
	SAmodeldat=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data for small area estimation
  ## - check population data
  ## - check dunitarea data
  ## - calculate plot-level adjustment factors by dividing 1 by summed proportions in plot
  ## VALUE:
  ## - return all data needed for input to modSAest() function
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
#  if (nargs() == 0 | is.null(estvar)) gui <- TRUE
  if (nargs() == 0) gui <- TRUE


  ## If gui.. set variables to NULL
  if (gui)  
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(modSApop)))) {
    miss <- input.params[!input.params %in% formals(modSApop)]
    stop("invalid parameter: ", toString(miss))
  }


  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=stratcombinelut <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  adjtree <- FALSE
  returnSApopdat <- FALSE
  nonsamp.pfilter=nonsamp.cfilter <- NULL 
  returnlst <- list()

# dunitvar2=NULL
# pvars2keep=NULL
# cvars2keep=NULL
# adj="plot"
# ACI=FALSE
# gui <- FALSE 


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


  ## Check SAdata
  #########################################################
  if (!is.null(SAdata)) {
    SAdata.names <- c("SAdoms", "cond", "plt",
		"pltassgn", "puniqueid", "pltassgnid", "pjoinid", "dunitarea",
		"dunitvar", "areavar", "dunitlut")
    if (!all(SAdata.names %in% names(SAdata))) 
      stop("missing components in SAdata list: ", 
		toString(SAdata.names[!SAdata.names %in% names(SAdata)])) 

    SAdoms <- SAdata$SAdoms
    tree <- SAdata$tree
    cond <- SAdata$cond
    plt <- SAdata$plt
    puniqueid <- SAdata$puniqueid
    pjoinid <- SAdata$pjoinid
    pltassgn <- SAdata$pltassgn
    pltassgnid <- SAdata$pltassgnid
    dunitarea <- SAdata$dunitarea
    dunitvar <- SAdata$dunitvar
    areavar <- SAdata$areavar
    dunitlut <- SAdata$dunitlut

    if (is.null(prednames)) {
      prednames <- SAdata$prednames
    } else {
      if (!all(prednames %in% SAdata$prednames))
        stop("invalid prednames: ", 
		toString(prednames[!prednames %in% SAdata$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  } else if (!is.null(SAmodeldat)) {
    list.items <- c("pltassgn", "domzonal", "domvar", "prednames", "domarea")
    SAmodeldat <- FIESTA::pcheck.object(SAmodeldat, "SAmodeldat", list.items=list.items)
    pltassgn <- SAmodeldat$pltassgn
    pltassgnid <- SAmodeldat$pltassgnid
    dunitarea <- SAmodeldat$domarea
    dunitvar <- SAmodeldat$domvar
    areavar <- SAmodeldat$areavar
    dunitlut <- SAmodeldat$domzonal

    if (is.null(prednames)) {
      prednames <- SAmodeldat$prednames
    } else {
      if (!all(prednames %in% SAmodeldat$prednames))
        stop("invalid prednames: ", 
		toString(prednames[!prednames %in% SAmodeldat$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  } 


  ## Check SAdoms
  if (!is.null(SAdoms) && !"sf" %in% class(SAdoms)) {
    stop("invalid SAdoms")
  }

  ###################################################################################
  ## Check population parameters 
  ## Note: currently removes units that do not have plots
  ###################################################################################

  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="SA", tree=tree, cond=cond, plt=plt, 
	seed=seed, pltassgn=pltassgn, dsn=dsn, tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
	condid=condid, puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid,
	measCur=measCur, measEndyr=measEndyr, measEndyr.filter, invyrs=invyrs, 
	ACI=ACI, adj=adj, nonsamp.pfilter=nonsamp.pfilter, 
	nonsamp.cfilter=nonsamp.cfilter, unitarea=dunitarea, areavar=areavar, 
	areaunits=areaunits, unitvar=dunitvar, unitvar2=dunitvar2, prednames=prednames, 
	predfac=predfac, pvars2keep=pvars2keep, cvars2keep=cvars2keep, removeunits=TRUE,
 	removetext="dunitarea")
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
  dunitvar <- popcheck$unitvar
  dunitvar2 <- popcheck$unitvar2
  dunitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  areaunits <- popcheck$areaunits
  prednames <- popcheck$prednames
  predfac <- popcheck$predfac
  plotsampcnt <- popcheck$plotsampcnt
  condsampcnt <- popcheck$condsampcnt
  states <- popcheck$states
  invyrs <- popcheck$invyrs
  cvars2keep <- popcheck$cvars2keep
  areawt <- popcheck$areawt

  if (is.null(treef) && is.null(seedf)) {
    stop("must include tree data")
  }


  ###################################################################################
  ## CHECK STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unitcombine=TRUE, combines estimation units to reach minplotnum.unit.
  ###################################################################################
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, module="SA",
		auxlut=dunitlut, prednames=prednames, predfac=predfac, 
		unitcombine=unitcombine, unitarea=dunitarea, unitvar=dunitvar, 
		areavar=areavar, minplotnum.strat=0, minplotnum.unit=0,
		auxtext="dunitlut", removetext="dunitarea")  
  pltassgnx <- auxdat$pltx
  dunitvar <- auxdat$unitvar
  dunitlut <- auxdat$auxlut
  prednames <- auxdat$prednames
  predfac <- auxdat$predfac


  ###################################################################################
  ## CALCULATE ADJUSTMENT FACTORS FOR NONSAMPLED CONDITIONS
  ## If adj="samp", calculate adjustment factors by strata and estimation unit
  ## If adj="plot", calculate adjustment factors by plot
  ## adjfac = 1 / summed condition proportions (by plot-size), for area and trees
  ###################################################################################
  ## Returns:
  ##  Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ###################################################################################
  if (is.null(key(condx))) setkeyv(condx, c(cuniqueid, condid))
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)

  ## Merge plot strata info to condx
  condx <- condx[pltassgnx[, c(pltassgnid, dunitvar, prednames), with=FALSE]]
  setkeyv(condx, c(cuniqueid, condid))

  if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE

    adjfacdata <- getadjfactorPLOT(condx=condx, treex=treef, seedx=seedf, 
				cuniqueid=cuniqueid, tuniqueid=tuniqueid, )
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
    seedf <- adjfacdata$seedx
  }
 
  if (!is.null(SAdoms)) {
    returnlst$SAdomsdf <- sf::st_drop_geometry(SAdoms)
  }

  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- append(returnlst, list(condx=condx, pltcondx=pltcondx,
		cuniqueid=cuniqueid, condid=condid, ACI.filter=ACI.filter, 
		dunitarea=dunitarea, areavar=areavar, areaunits=areaunits, 
		dunitvar=dunitvar, dunitlut=dunitlut, prednames=prednames, 
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

  if (saveobj) {
    objfn <- getoutfn(outfn="SApopdat", ext="rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
  }

  return(returnlst)
}
