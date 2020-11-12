modSApop <- function(SAdoms=NULL, cond=NULL, tree=NULL, plt=NULL, pltassgn=NULL, 
	dsn=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", 
	puniqueid="CN", pltassgnid="CN", pjoinid="CN", measCur=FALSE, measEndyr=NULL, 
	measEndyr.filter=NULL, invyrs=NULL, ACI=FALSE, adj="plot", plt.nonsamp.filter=NULL, 
	cond.nonsamp.filter=NULL, dunitvar=NULL, dunitvar2=NULL, dunitarea=NULL, 
	areavar="ACRES", unitcombine=FALSE, dunitlut=NULL, prednames=NULL, predfac=NULL, 
	pvars2keep=NULL, cvars2keep=NULL, saveobj=FALSE, outfolder=NULL, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite=FALSE, SAdata=NULL, gui=FALSE){

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
  

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=stratcombinelut <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  adjtree <- FALSE
  returnSApopdat <- FALSE

# dunitvar2=NULL
# pvars2keep=NULL
# cvars2keep=NULL
# adj="plot"
# ACI=FALSE
# plt.nonsamp.filter=NULL
# cond.nonsamp.filter=NULL
# gui <- FALSE 


  ### Check saveobj 
  saveobj <- FIESTA::pcheck.logical(saveobj, varnm="saveobj", 
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check outfolder 
  ########################################################
  if (saveobj) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to file name?", first="NO", gui=gui) 

    objfn <- getoutfn(outfn="SApopdat.rda", outfolder=outfolder, 
		overwrite=overwrite, outfn.date=outfn.date)
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
    pltassgn <- SAdata$pltassgn
    puniqueid <- SAdata$puniqueid
    pltassgnid <- SAdata$pltassgnid
    pjoinid <- SAdata$pjoinid
    dunitarea <- SAdata$dunitarea
    dunitvar <- SAdata$dunitvar
    areavar <- SAdata$areavar
    dunitlut <- SAdata$dunitlut
    prednames <- SAdata$prednames
    predfac <- SAdata$predfac

    if (is.null(prednames)) {
      prednames <- SAdata$prednames
    } else {
      if (!all(prednames %in% SAdata$prednames))
        stop("invalid prednames: ", 
		toString(prednames[!prednames %in% SAdata$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  }

  ## Check SAdoms
  if (!"sf" %in% class(SAdoms)) stop("invalid SAdoms")

  ###################################################################################
  ## Check population parameters 
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="SA", tree=tree, cond=cond, plt=plt, 
	pltassgn=pltassgn, dsn=dsn, tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
	condid=condid, puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid,
	measCur=measCur, measEndyr=measEndyr, measEndyr.filter, invyrs=invyrs, 
	ACI=ACI, adj=adj, plt.nonsamp.filter=plt.nonsamp.filter, 
	cond.nonsamp.filter=cond.nonsamp.filter, unitvar=dunitvar, unitvar2=dunitvar2, 
	prednames=prednames, predfac=predfac, pvars2keep=pvars2keep, cvars2keep=cvars2keep)
  condx <- popcheck$condx	
  pltcondx <- popcheck$pltcondx
  treef <- popcheck$treef
  pltassgnx <- popcheck$pltassgnx
  cuniqueid <- popcheck$cuniqueid
  condid <- popcheck$condid
  tuniqueid <- popcheck$tuniqueid
  pltassgnid <- popcheck$pltassgnid
  ACI.filter <- popcheck$ACI.filter
  adj <- popcheck$adj
  dunitvar <- popcheck$unitvar
  dunitvar2 <- popcheck$unitvar2
  prednames <- popcheck$prednames
  predfac <- popcheck$predfac
  plotsampcnt <- popcheck$plotsampcnt
  condsampcnt <- popcheck$condsampcnt
  states <- popcheck$states
  invyrs <- popcheck$invyrs
  cvars2keep <- popcheck$cvars2keep

  if (is.null(treef)) {
    stop("must include tree data")
  }


  ###################################################################################
  ## CHECK domarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and acres by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  dunitdat <- check.unitarea(unitarea=dunitarea, pltx=pltassgnx, 
		unitvars=c(dunitvar, dunitvar2), areavar=areavar, gui=gui)
  dunitarea <- dunitdat$unitarea
  areavar <- dunitdat$areavar


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
		areavar=areavar, minplotnum.strat=0, minplotnum.unit=0)  
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

    adjfacdata <- getadjfactorPLOT(condx=condx, treex=treef, 
				cuniqueid=cuniqueid, tuniqueid=tuniqueid, )
    condx <- adjfacdata$condadj
    treef <- adjfacdata$treeadj
  }
 
  returnlst <- list(SAdomsdf=sf::st_drop_geometry(SAdoms),
		condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, 
		condid=condid, tuniqueid=tuniqueid, ACI.filter=ACI.filter, 
		dunitarea=dunitarea, areavar=areavar, dunitvar=dunitvar, 
		dunitlut=dunitlut, prednames=prednames, plotsampcnt=plotsampcnt,
 		condsampcnt=condsampcnt, states=states, invyrs=invyrs)

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$adjtree <- adjtree
  }

  if (saveobj) save(returnlst, file=objfn)

  return(returnlst)
}
