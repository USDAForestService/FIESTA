modMApop <- function(MAmethod, cond, plt=NULL, tree=NULL, pltassgn=NULL, 
	dsn=NULL, tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", 
	puniqueid="CN", pltassgnid="CN", ACI=FALSE, adj="samp", 
	plt.nonsamp.filter=NULL, cond.nonsamp.filter=NULL, unitvar=NULL, 
	unitvar2=NULL, unitarea=NULL, areavar="ACRES", unitcombine=FALSE, 
	unitlut=NULL, npixelvar="npixels", prednames=NULL, predfac=NULL, 
	PSstrvar=NULL, stratcombine=TRUE, gui=FALSE){

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
  

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=expcondtab <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  adjtree <- FALSE
 

  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="MA", method=MAmethod, 
	tree=tree, cond=cond, plt=plt, pltassgn=pltassgn, dsn=dsn, tuniqueid=tuniqueid, 
	cuniqueid=cuniqueid, condid=condid, puniqueid=puniqueid, pltassgnid=pltassgnid, 
	ACI=ACI, adj=adj, plt.nonsamp.filter=plt.nonsamp.filter, 
	cond.nonsamp.filter=cond.nonsamp.filter, unitvar=unitvar, unitvar2=unitvar2,
 	strvar=PSstrvar, prednames=prednames, predfac=predfac, stratcombine=stratcombine, 
	pvars2keep=pvars2keep, cvars2keep=cvars2keep)
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
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  unitcombine <- popcheck$unitcombine
  PSstrvar <- popcheck$strvar
  stratcombine <- popcheck$stratcombine
  prednames <- popcheck$prednames
  predfac <- popcheck$predfac
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
  unitdat <- check.unitarea(unitarea=unitarea, pltx=pltassgnx, 
	unitvars=c(unitvar, unitvar2), areavar=areavar, gui=gui)
  unitarea <- unitdat$unitarea
  areavar <- unitdat$areavar

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
  unitlut <- auxdat$auxlut
  PSstrvar <- auxdat$PSstrvar
  prednames <- auxdat$prednames
  predfac <- auxdat$predfac
  npixels <- auxdat$npixels
  stratcombinelut <- auxdat$unitstrgrplut



  if ("GREG" %in% MAmethod && !is.null(predfac)) {
    for (fac in predfac) {
      ## Get factor levels
      fac.levels <- sort(unique(pltassgnx[[fac]]))

      ## Set factor levels to keep and delete from unitlut.GREG
      fac.unitcol.keep <- paste(fac, fac.levels[-1], sep=".")
      fac.unitcol.del <- paste(fac, fac.levels[1], sep=".")
      unitlut[[fac.unitcol.del]] <- NULL
  
      ## Rename factor variables and add names to predictor list
      facs <- paste0(fac, fac.levels[-1])
      names(unitlut)[names(unitlut) %in% fac.unitcol.keep] <- facs
      unitpreds <- c(prednames[prednames != fac], facs)

      ## Create dummy variables for factor levels - 1
      dtfac <- pltassgnx[, as.data.table(model.matrix(~., 
				data=pltassgnx[, fac, with=FALSE]))][,-1]
      pltassgnx <- cbind(pltassgnx, dtfac)
      pltassgnx[, (fac) := NULL]

      ## Remove old name and add new names to predictor list
      prednames <- unique(c(prednames[prednames != fac], facs))
    }
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
  if (is.null(key(condx))) setkeyv(condx, c(cuniqueid, condid))
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)
  if (adj == "samp") {
    bycond <- TRUE
    adjtree <- TRUE

    ## Merge plot strata info to condx
    condx <- condx[pltassgnx[,c(pltassgnid, unitvar, PSstrvar, prednames), with=FALSE]]
 
    adjfacdata <- getadjfactorGB(treex=treef, condx=condx, tuniqueid=tuniqueid, 
		cuniqueid=cuniqueid, condid=condid, unitlut=unitlut, 
		unitvars=unitvar, strvars=PSstrvar, unitarea=unitarea, areavar=areavar, 
		cvars2keep=cvars2keep)
    condx <- adjfacdata$condx
    unitlut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
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
  } else {

    ## Merge plot strata info to condx
    condx <- condx[pltassgnx[,c(puniqueid, unitvar, prednames), with=FALSE]]
    setkeyv(condx, c(cuniqueid, condid))
  }


  returnlst <- list(condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, condid=condid,
 		ACI.filter=ACI.filter, unitarea=unitarea, areavar=areavar,
		unitvar=unitvar, unitlut=unitlut, npixels=npixels, npixelvar=npixelvar, 
		PSstrvar=PSstrvar, prednames=prednames, expcondtab=expcondtab, 
		plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, states=states, invyrs=invyrs,
 		MAmethod=MAmethod)

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }

  if (!is.null(stratcombinelut)) 
    returnlst$stratcombinelut <- stratcombinelut

  return(returnlst)
}

