modGBpop <- function(popType="VOL", cond=NULL, plt=NULL, tree=NULL, seed=NULL, 
	vspspp=NULL, subplot=NULL, subp_cond=NULL, pltassgn=NULL, dsn=NULL, 
	puniqueid="CN", pltassgnid="PLT_CN", pjoinid="CN", tuniqueid="PLT_CN", 
	cuniqueid="PLT_CN", condid="CONDID", areawt="CONDPROP_UNADJ", adj="samp", 
	evalid=NULL, invyrs=NULL, intensity=NULL, ACI=FALSE, plt.nonsamp.filter=NULL, 
	cond.nonsamp.filter=NULL, unitvar=NULL, unitvar2=NULL, unitarea=NULL,
	areavar="ACRES", unitcombine=FALSE, minplotnum.unit=10, strata=TRUE, 
	stratalut=NULL, strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", 
	stratcombine=TRUE, saveobj=FALSE, savedata=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=TRUE, GBdata=NULL, GBstratdat=NULL, gui=FALSE){

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
  

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modGBpop)) 
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
  nonresp=FALSE
  substrvar=NULL

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

  ## Check overwrite, outfolder, outfn 
  ########################################################
  if (savedata || saveobj) {
    if (savedata) {
      out_fmtlst <- c("sqlite", "gpkg", "csv", "gdb", "shp")
      out_fmt <- FIESTA::pcheck.varchar(var2check=out_fmt, varnm="out_fmt", 
		checklst=out_fmtlst, gui=gui, caption="Output format?") 
      if (out_fmt == "shp") out_fmt <- "csv"
      if (out_fmt != "csv") {
        if (is.null(out_dsn)) {
          out_dsn <- paste0("SAdata.", out_fmt)
        } else {
          if (!file.exists(checkfilenm(out_dsn, outfolder=outfolder)) || overwrite) {
            if (out_fmt == "gdb") {
              out_dsn <- DBtestESRIgdb(gdbfn=out_dsn, outfolder=outfolder, 
				overwrite=overwrite, showlist=FALSE, outfn.date=outfn.date, 
				returnpath=FALSE)
            } else if (out_fmt %in% c("sqlite", "gpkg")) {
              gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
              out_dsn <- DBcreateSQLite(SQLitefn=out_dsn, gpkg=gpkg, outfolder=outfolder, 
				overwrite=overwrite, outfn.date=outfn.date, returnpath=FALSE)
            }
          }
        }
      } else {
        outfolder <- pcheck.outfolder(outfolder, gui)
        overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
			title="Overwrite?", first="NO", gui=gui)  
        outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
			title="Add date to outfiles?", first="NO", gui=gui) 

        ## If outfn.pre is not null, create a folder within the outfolder, named outfn.pre
        if (!is.null(outfn.pre)) {
          outfolder <- file.path(outfolder, outfn.pre)
          if (!dir.exists(outfolder)) dir.create(outfolder)
        }
      }
    }
  } 

  if (!is.null(GBdata)) {
    list.items <- c("bnd", "plt", "cond", "unitarea", "unitvar")
    GBdata <- FIESTA::pcheck.object(GBdata, "GBdata", list.items=list.items)
    bnd <- GBdata$bnd
    plt <- GBdata$plt
    pltassgn <- GBdata$pltassgn
    cond <- GBdata$cond
    tree <- GBdata$tree
    seed <- GBdata$seed
    unitarea <- GBdata$unitarea
    unitvar <- GBdata$unitvar
    areavar <- GBdata$areavar
    stratalut <- GBdata$stratalut
    strvar <- GBdata$strvar
    puniqueid <- GBdata$puniqueid
    pjoinid <- GBdata$pjoinid
    pltassgnid <- GBdata$pltassgnid  

  } else if (!is.null(GBstratdat)) {
    list.items <- c("pltassgn", "unitarea", "unitvar", "stratalut", "strvar")
    GBstratdat <- FIESTA::pcheck.object(GBstratdat, "GBstratdat", list.items=list.items)
    pltassgn <- GBstratdat$pltassgn
    pltassgnid <- GBstratdat$pltassgnid
    unitarea <- GBstratdat$unitarea
    unitvar <- GBstratdat$unitvar
    unitvar2 <- GBstratdat$unitvar2
    areavar <- GBstratdat$areavar
    stratalut <- GBstratdat$stratalut
    strvar <- GBstratdat$strvar
    getwt <- GBstratdat$getwt
    getwtvar <- GBstratdat$getwtvar
  } 

  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="GB", popType=popType, 
	tree=tree, cond=cond, plt=plt, seed=seed, vspspp=vspspp, subplot=subplot, 
	subp_cond=subp_cond, pltassgn=pltassgn, dsn=dsn, tuniqueid=tuniqueid, 
	cuniqueid=cuniqueid, condid=condid, areawt=areawt, puniqueid=puniqueid, 
 	pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid, invyrs=invyrs,
 	intensity=intensity, adj=adj, ACI=ACI, plt.nonsamp.filter=plt.nonsamp.filter, 
	cond.nonsamp.filter=cond.nonsamp.filter, unitvar=unitvar, unitvar2=unitvar2,
 	unitcombine=unitcombine, stratcombine=stratcombine, strata=strata, strvar=strvar,
	nonresp=nonresp, substrvar=substrvar)
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
  if (nonresp) 
    substrvar <- popcheck$substrvar 
  #rm(popcheck)
  if ("P2VEG" %in% popType) {
    pltassgn.P2VEG <- popcheck$pltassgn.P2VEG
    subp_condf <- popcheck$subp_condf
    vspsppf <- popcheck$vspsppf
  }

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
		unitarea=unitarea, areavar=areavar)
    condx <- adjfacdata$condx
    stratalut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab
  } 

  estvar.name <- ifelse(adj == "samp", "CONDPROP_ADJ", "CONDPROP_UNADJ")
  returnlst <- list(condx=condx, pltcondx=pltcondx, cuniqueid=cuniqueid, condid=condid,
 		ACI.filter=ACI.filter, unitarea=unitarea, areavar=areavar,
 		unitvar=unitvar, stratalut=stratalut, strvar=strvar, expcondtab=expcondtab,
 		plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, states=states, invyrs=invyrs,
		estvar.name=estvar.name, adj=adj)

  if (!is.null(treef)) {
    returnlst$treex <- treef
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }
  if (!is.null(seedf))
    returnlst$seedx <- seedf
  

  if (!is.null(stratcombinelut)) 
    returnlst$stratcombinelut <- stratcombinelut

  if (saveobj) {
    objfn <- getoutfn(outfn="GBpopdat", outfolder=outfolder, ext="rda",
		overwrite=overwrite, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }

  return(returnlst)
}
