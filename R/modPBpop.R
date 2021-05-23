modPBpop <- function(pntdat=NULL, pltpct=NULL, plotid="plot_id", pntid=NULL, 
	pltpctvars=NULL, plt=NULL, pltassgn=NULL, puniqueid="CN", pltassgnid="CN",
 	nonsamp.pfilter=NULL, strata=FALSE, sumunits=FALSE, unitvar=NULL, unitvar2=NULL, 
	unitarea=NULL, areavar="ACRES", areaunits="acres", unitcombine=FALSE, 
 	minplotnum.unit=10, stratalut=NULL, strvar="STRATUMCD", getwt=TRUE, 
	getwtvar="P1POINTCNT", strwtvar="strwt", stratcombine=TRUE, minplotnum.strat=2, 
	pvars2keep=NULL, saveobj=FALSE, objnm="PBpopdat", savedata=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite_dsn=FALSE, 
	overwrite_layer=TRUE, PBstratdat=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data 'on-the-fly', including strata weights, number
  ## of plots by strata and estimation unit, strata-level expansion factors,
  ## and condition-level adjustment factors.
  ## - checks input parameters and data tables
  ## - checks auxiliary data
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
#  if (nargs() == 0 | is.null(estvar)) gui <- TRUE
  if (nargs() == 0) gui <- TRUE

  ## If gui.. set variables to NULL
  if (gui) { 
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  }
  ## Check input parameters
#  input.params <- names(as.list(match.call()))[-1]
#  formallst <- names(formals(FIESTA::modPBpop))
#  if (!all(input.params %in% formallst)) {
#    miss <- input.params[!input.params %in% formallst]
#    stop("invalid parameter: ", toString(miss))
#  }
 
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  auxvars <- NULL

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

    if (!is.null(PBstratdat)) {
      list.items <- c("pltassgn", "unitarea", "unitvar", "stratalut", "strvar")
      PBstratdat <- FIESTA::pcheck.object(PBstratdat, "PBstratdat", list.items=list.items)
      pltassgn <- PBstratdat$pltassgn
      pltassgnid <- PBstratdat$pltassgnid
      unitarea <- PBstratdat$unitarea
      areavar <- PBstratdat$areavar
      stratalut <- PBstratdat$stratalut
      strvar <- PBstratdat$strvar
      getwt <- PBstratdat$getwt
      getwtvar <- PBstratdat$getwtvar
      strwtvar <- PBstratdat$strwtvar

      if (is.null(unitvar)) {
        unitvar <- PBstratdat$unitvar
        unitvar2 <- PBstratdat$unitvar2
      } 
    }

  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdataPB(gui=gui, pnt=pntdat, pltpct=pltpct, pltpctvars=pltpctvars, 
	plt=plt, pltassgn=pltassgn, plotid=plotid, pntid=pntid, puniqueid=puniqueid, 
	pltassgnid=pltassgnid, nonsamp.pfilter=nonsamp.pfilter, unitvar=unitvar, 
	unitvar2=unitvar2, unitarea=unitarea, areavar=areavar, areaunits=areaunits, 	unitcombine=unitcombine, auxvars=auxvars, strata=strata, strvar=strvar, 
	stratcombine=stratcombine, pvars2keep=pvars2keep, sumunits=sumunits)
  PBx <- popcheck$PBx
  pltassgnx <- popcheck$pltassgnx
  plotid <- popcheck$plotid
  pntid <- popcheck$pntid
  pltassgnid <- popcheck$pltassgnid
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  areaunits <- popcheck$areaunits
  unitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  unitcombine <- popcheck$unitcombine
  strata <- popcheck$strata
  strvar <- popcheck$strvar
  stratcombine <- popcheck$stratcombine
  plotsampcnt <- popcheck$plotsampcnt
  getprop <- popcheck$getprop

  if (!getprop) {
    rowvar <- popcheck$rowvar
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
  stratcheck <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, module="PB",
		strata=strata, auxlut=stratalut, PSstrvar=strvar, stratcombine=stratcombine, 
		unitcombine=unitcombine, unitarea=unitarea, unitvar=unitvar, 
		unitvar2=unitvar2, areavar=areavar, minplotnum.unit=minplotnum.unit, 
		minplotnum.strat=minplotnum.strat, getwt=getwt, getwtvar=getwtvar,
		strwtvar=strwtvar)  
  pltassgnx <- stratcheck$pltx
  unitarea <- stratcheck$unitarea
  unitvar <- stratcheck$unitvar
  unitvars <- stratcheck$unitvars
  stratalut <- stratcheck$auxlut
  strvar <- stratcheck$PSstrvar
  strwtvar <- stratcheck$strwtvar
  stratcombinelut <- stratcheck$unitstrgrplut
  strunitvars <- c(unitvar, strvar)

  returnlst <- list(PBx=PBx, pltassgnx=pltassgnx, plotid=plotid, pntid=pntid, 
		pltassgnid=pltassgnid, sumunits=sumunits, strata=strata, strwtvar=strwtvar,
		unitvar=unitvar, unitvars=unitvars, unitarea=unitarea, stratalut=stratalut,
 		strvar=strvar, plotsampcnt=plotsampcnt, getprop=getprop)

  if (!is.null(unitarea)) {
    returnlst$unitarea <- stratcheck$unitarea
    returnlst$areavar <- areavar
    returnlst$areaunits <- areaunits
  }
  if (!getprop) {
    returnlst$rowvar <- rowvar
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
    datExportData(PBx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="PBx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
 
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
