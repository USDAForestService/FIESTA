modFAOpop <- function(FAOdata=NULL, base=NULL, cluster=NULL, tree=NULL, 
	seed=NULL, clustassgn=NULL, dsn=NULL, clustid="CN", clustassgnid="PLT_CN",
	clustjoinid="CN", buniqueid="PLT_CN", baseid="CONDID", tuniqueid=NULL,
 	basewt="CONDPROP_UNADJ", invyrs=NULL, adj="none", diavar="DIA",
	MICRO_BREAKPOINT_DIA=5, MACRO_BREAKPOINT_DIA=NULL, areawt_micr="MICRPROP_UNADJ", 
	areawt_subp="SUBPPROP_UNADJ", areawt_macr="MACRPROP_UNADJ",
	strata=TRUE, nonsamp.clustfilter=NULL, nonsamp.basefilter=NULL, 
	unitlevel1=NULL, unitlevel2=NULL, unitarea=NULL, areavar="ACRES", 
	unitcombine=FALSE, minplotnum.unit=10, stratalut=NULL, 
	strvar="STRATUMCD", strwtvar="strwt", getwt=TRUE, getwtvar="P1POINTCNT", 
	stratcombine=TRUE, minplotnum.strat=2, saveobj=FALSE, objnm="FAOpopdat", 
	savedata=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL,
 	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, 
	append_layer=FALSE, gui=FALSE){

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
  formallst <- names(formals(modFAOpop)) 
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
  nonresp <- FALSE
  substrvar <- NULL
  if (is.null(tuniqueid)) tuniqueid <- buniqueid
  FAOdata <- NULL
  returnlst <- list()


  ## Translation
  #cond <- base
  #plt <- cluster
  #pltassgn <- clustassgn 
  #condid <- baseid
  #puniqueid <- cluniqueid
  #pltassgnid <- classgnid
  #pjoinid <- cljoinid
  #plt.nonsamp.filter <- nonsamp.clustfilter 
  #cond.nonsamp.filter <- nonsamp.basefilter
  #unitvar <- unitlevel1 
  #unitvar2 <- unitlevel2
  #bcfilter <- pcfilter

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

  if (!is.null(FAOdata)) {
    if (!is.list(FAOdata))
      stop("FAOdata must be a list")
    listitems <- c("bnd", "plt", "base", "unitarea", "unitvar")
    if (!all(listitems %in% names(FAOdata))) {
      items.miss <- listitems[!listitems %in% names(FAOdata)]
      stop("invalid FAOdata... missing items: ", paste(items.miss, collapse=", "))
    } 
    bnd <- FAOdata$bnd
    cluster <- FAOdata$cluster
    clustassgn <- FAOdata$clustassgn
    base <- FAOdata$base
    tree <- FAOdata$tree
    unitarea <- FAOdata$unitarea
    areavar <- FAOdata$areavar
    stratalut <- FAOdata$stratalut
    strvar <- FAOdata$strvar
    clustid <- FAOdata$clustid
    clustjoinid <- FAOdata$clustjoinid
    clustassgnid <- FAOdata$clustassgnid 

    if (is.null(unitvar)) {
      unitvar <- FAOdata$unitvar
      unitvar2 <- FAOdata$unitvar2
    }  
  } 
 
  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="GB", tree=tree, cond=base, plt=cluster, 
	pltassgn=clustassgn, dsn=dsn, tuniqueid=tuniqueid, cuniqueid=buniqueid, 
	condid=baseid, areawt=basewt, puniqueid=clustid, pltassgnid=clustassgnid, 
	pjoinid=clustjoinid, invyrs=invyrs, adj=adj, diavar=diavar,
	MICRO_BREAKPOINT_DIA=MICRO_BREAKPOINT_DIA, MACRO_BREAKPOINT_DIA=MACRO_BREAKPOINT_DIA, 	areawt_micr=areawt_micr, areawt_subp=areawt_subp, areawt_macr=areawt_macr,
 	nonsamp.pfilter=nonsamp.clustfilter, nonsamp.cfilter=nonsamp.basefilter,
 	unitarea=unitarea, unitvar=unitlevel1, unitvar2=unitlevel2, 
	unitcombine=unitcombine, strata=strata, stratalut=stratalut, strvar=strvar,
 	stratcombine=stratcombine)
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
  adj <- popcheck$adj
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  unitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  areaunits <- popcheck$areaunits
  unitcombine <- popcheck$unitcombine
  strata <- popcheck$strata
  strvar <- popcheck$strvar
  stratcombine <- popcheck$stratcombine
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
  if (strata) {
    stratalut <- popcheck$stratalut
  }
  if (nonresp) {
    substrvar <- popcheck$substrvar 
    nonsampplots <- popcheck$nonsampplots
  }
  #rm(popcheck)

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
  ##		factors to base and dividing CONDPROP_UNADJ by adjustment factor.
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
    adjtree <- TRUE
    adjfacdata <- getadjfactorGB(condx=condx, treex=treef, seedx=seedf, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, 
		unitlut=stratalut, unitvars=unitvar, strvars=strvar,
		unitarea=unitarea, areavar=areavar, areawt=areawt, tpropvars=tpropvars)
    condx <- adjfacdata$condx
    stratalut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab
  } 
 
  setkeyv(stratalut, strunitvars)
  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- append(returnlst, list(basex=condx, clustbasex=pltcondx, 
		buniqueid=cuniqueid, baseid=condid,
 		unitarea=unitarea, areavar=areavar, 
		areaunits=areaunits, unitlevel1=unitvar, unitvars=unitvars,
 		stratalut=stratalut, strvar=strvar, strwtvar=strwtvar, 
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

  if (!is.null(stratcombinelut)) {
    returnlst$stratcombinelut <- stratcombinelut
  }

  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(condx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="basex", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(pltcondx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="clustbasex", 
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
  }

  return(returnlst)
}
