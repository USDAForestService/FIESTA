modPBpop <- function(pnt=NULL, pltpct=NULL, plotid="plot_id", pntid=NULL, 
	pltpctvars=NULL, plt=NULL, pltassgn=NULL, puniqueid="CN", pltassgnid="CN",
 	plt.nonsamp.filter=NULL, tabtype="PCT", strata=FALSE, sumunits=FALSE,
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", unitcombine=FALSE, 
	stratalut=NULL, strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", 
	stratcombine=TRUE, pvars2keep=NULL, saveobj=FALSE, savedata=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, outfn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite=TRUE, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data 'on-the-fly', including strata weights, number
  ## of plots by strata and estimation unit, strata-level expansion factors,
  ## and condition-level adjustment factors.
  ## - checks input parameters and data tables
  ## - checks auxiliary data
  ## - calculates adjustment factors for nonresponse
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
#  if (nargs() == 0 | is.null(estvar)) gui <- TRUE
  if (nargs() == 0) gui <- TRUE


  ## If gui.. set variables to NULL
  if (gui)  
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
 
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(FIESTA::modPBpop))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  auxvars <- NULL
 
  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdataPB(gui=gui, pnt=pnt, pltpct=pltpct, pltpctvars=pltpctvars, 
	plt=plt, pltassgn=pltassgn, plotid=plotid, pntid=pntid, puniqueid=puniqueid, 
	pltassgnid=pltassgnid, plt.nonsamp.filter=plt.nonsamp.filter, unitvar=unitvar, 
	unitvar2=unitvar2, unitcombine=unitcombine, auxvars=auxvars, strata=strata, 
	strvar=strvar, stratcombine=stratcombine, pvars2keep=pvars2keep, tabtype=tabtype)
  PBx <- popcheck$PBx
  pltassgnx <- popcheck$pltassgnx
  plotid <- popcheck$plotid
  pntid <- popcheck$pntid
  pltassgnid <- popcheck$pltassgnid
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  unitcombine <- popcheck$unitcombine
  tabtype <- popcheck$tabtype
  strata <- popcheck$strata
  strvar <- popcheck$strvar
  stratcombine <- popcheck$stratcombine
  plotsampcnt <- popcheck$plotsampcnt
  getprop <- popcheck$getprop

  if (!getprop)
    rowvar <- popcheck$rowvar
 
  ###################################################################################
  ## CHECK unitarea BY ESTIMATION UNIT
  ## Returns: data table with unitvar and area by estimation unit (unitvar)
  ##	 and areavar (default="ACRES")
  ###################################################################################
  if (is.null(unitarea) || unitarea == 0) {   
    if (tabtype == "AREA" || sumunits) {
      if (sumunits) {
        stop("need unitarea to combine estimation units")
      } else {
        stop("need unitarea to return acre estimates")
      }
    }
    unitarea <- NULL
  } else {
    unitvars <- c(unitvar, unitvar2)
    unitdat <- check.unitarea(unitarea=unitarea, pltx=pltassgnx, 
		unitvars=unitvars, areavar=areavar, gui=gui)
    unitarea <- unitdat$unitarea
    areavar <- unitdat$areavar
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
		unitvar2=unitvar2, areavar=areavar, getwt=getwt, getwtvar=getwtvar)  
  pltassgnx <- stratcheck$pltx
  unitarea <- stratcheck$unitarea
  unitvar <- stratcheck$unitvar
  strlut <- stratcheck$auxlut
  strvar <- stratcheck$PSstrvar
  stratcombinelut <- stratcheck$unitstrgrplut
  strunitvars <- c(unitvar, strvar)

  returnlst <- list(PBx=PBx, pltassgnx=pltassgnx, plotid=plotid, pntid=pntid, 
		pltassgnid=pltassgnid, tabtype=tabtype, sumunits=sumunits, 
		unitvar=unitvar, strlut=strlut, strvar=strvar, 
		plotsampcnt=plotsampcnt, getprop=getprop)

  if (!is.null(unitarea)) {
    returnlst$unitarea <- stratcheck$unitarea
    returnlst$areavar <- areavar
  }
  if (!getprop) 
    returnlst$rowvar <- rowvar

  if (!is.null(stratcombinelut)) 
    returnlst$stratcombinelut <- stratcombinelut


  if (saveobj) {
    objfn <- getoutfn(outfn="PBpopdat", outfolder=outfolder, 
		overwrite=overwrite, outfn.date=outfn.date, ext="rda")
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    if (!is.null(unitarea)) {
      datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
    }
    datExportData(strlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="strlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite)
  }

  return(returnlst)
}
