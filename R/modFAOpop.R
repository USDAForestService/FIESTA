modFAOpop <- function(FAOdata=NULL, base=NULL, cluster=NULL, tree=NULL, 
	clustassgn=NULL, dsn=NULL, clustid="CN", clustassgnid="PLT_CN",
	clustjoinid="CN", buniqueid="PLT_CN", baseid="CONDID", tuniqueid=NULL,
 	basewt="CONDPROP_UNADJ", invyrs=NULL, adj="none", 
	strata=TRUE, nonsamp.clustfilter=NULL, nonsamp.basefilter=NULL, 
	unitlevel1=NULL, unitlevel2=NULL, unitarea=NULL, areavar="ACRES", 
	unitcombine=FALSE, minplotnum.unit=10, stratalut=NULL, 
	strvar="STRATUMCD", strwtvar="strwt", getwt=TRUE, getwtvar="P1POINTCNT", 
	stratcombine=TRUE, saveobj=FALSE, savedata=FALSE, outfolder=NULL, 
	outfn=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=TRUE, gui=FALSE){

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
  if (gui)  
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  

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

  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check saveobj 
  saveobj <- FIESTA::pcheck.logical(saveobj, varnm="saveobj", 
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)


  ## Check overwrite, outfolder, outfn 
  ########################################################
  if (savedata || saveobj) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="NO", gui=gui) 

    ## If outfn.pre is not null, create a folder within the outfolder, named outfn.pre
    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }

    if (savedata) {

      out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb", "shp")
      out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 
      if (out_fmt == "shp") out_fmt <- "csv"
      if (out_fmt != "csv" && is.null(out_dsn))
        out_dsn <- paste0("SAdata.", out_fmt)

      if (out_fmt == "gdb") {
        out_dsn <- DBtestESRIgdb(gdbfn=out_dsn, outfolder=outfolder, overwrite=FALSE, 
			showlist=FALSE, returnpath=FALSE)
      }	else if (out_fmt %in% c("sqlite", "gpkg")) {
        gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
        out_dsn <- DBcreateSQLite(SQLitefn=out_dsn, gpkg=gpkg, outfolder=outfolder, 
			overwrite=FALSE, returnpath=FALSE)
      }
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
    unitvar <- FAOdata$unitvar
    areavar <- FAOdata$areavar
    stratalut <- FAOdata$stratalut
    strvar <- FAOdata$strvar
    clustid <- FAOdata$clustid
    clustjoinid <- FAOdata$clustjoinid
    clustassgnid <- FAOdata$clustassgnid  
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
	pjoinid=clustjoinid, invyrs=invyrs, adj=adj,
 	nonsamp.pfilter=nonsamp.clustfilter, nonsamp.cfilter=nonsamp.basefilter,
 	unitvar=unitlevel1, unitvar2=unitlevel2, unitcombine=unitcombine, 
	stratcombine=stratcombine, strata=strata, strvar=strvar)
  if (is.null(popcheck)) return(NULL)
  condx <- popcheck$condx
  pltcondx <- popcheck$pltcondx
  treef <- popcheck$treef
  pltassgnx <- popcheck$pltassgnx
  cuniqueid <- popcheck$cuniqueid
  condid <- popcheck$condid
  tuniqueid <- popcheck$tuniqueid
  pltassgnid <- popcheck$pltassgnid
  adj <- popcheck$adj
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
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
  if (nonresp) {
    substrvar <- popcheck$substrvar 
    nonsampplots <- popcheck$nonsampplots
  }
  #rm(popcheck)


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
  ## If unitvar and unitvar2, concatenates variables to 1 unitvar
  ###################################################################################
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, strata=strata,
		auxlut=stratalut, PSstrvar=strvar, nonresp=nonresp, substrvar=substrvar, 
		stratcombine=stratcombine, unitcombine=unitcombine, unitarea=unitarea, 
		unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
		minplotnum.unit=minplotnum.unit, getwt=getwt, getwtvar=getwtvar, 
		P2POINTCNT=P2POINTCNT)  
  pltassgnx <- auxdat$pltx
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  strlut <- auxdat$auxlut
  strvar <- auxdat$PSstrvar
  stratcombinelut <- auxdat$unitstrgrplut
  if (nonresp) nonsampplots <- auxdat$nonsampplots
  strunitvars <- c(unitvar, strvar)
 

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
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)
  condx <- condx[pltassgnx[,c(pltassgnid, strunitvars), with=FALSE]]

  if (adj == "samp") {
    adjtree <- TRUE
    adjfacdata <- getadjfactorGB(treex=treef, condx=condx, cuniqueid=cuniqueid, 
		tuniqueid=tuniqueid, condid=condid, unitlut=strlut, unitvars=unitvar, 
		strvars=strvar, unitarea=unitarea, areavar=areavar)
    condx <- adjfacdata$condx
    strlut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    expcondtab <- adjfacdata$expcondtab
  } 
 
  returnlst <- list(basex=condx, clustbasex=pltcondx, buniqueid=cuniqueid, baseid=condid,
 		unitarea=unitarea, areavar=areavar, unitlevel1=unitvar, unitvars=unitvars,
 		strlut=strlut, strvar=strvar, expcondtab=expcondtab,
 		plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, states=states, invyrs=invyrs)

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }

  if (!is.null(stratcombinelut)) 
    returnlst$stratcombinelut <- stratcombinelut

  if (saveobj) {
    objfn <- getoutfn(outfn="FAOpopdat.rda", outfolder=outfolder, 
		overwrite=overwrite, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="clustassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(strlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="strlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }

  return(returnlst)
}
