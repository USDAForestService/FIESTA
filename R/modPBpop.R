modPBpop <- function(pnt=NULL, pltpct=NULL, plotid="plot_id", pntid=NULL, 
	pltpctvars=NULL, plt=NULL, pltassgn=NULL, puniqueid="CN", pltassgnid="CN",
 	plt.nonsamp.filter=NULL, tabtype="PCT", strata=FALSE, sumunits=FALSE,
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", unitcombine=FALSE, 
	stratalut=NULL, strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", 
	stratcombine=TRUE, pvars2keep=NULL, gui=FALSE){

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
  if (tabtype == "AREA" || sumunits) {
    if (is.null(unitarea) || unitarea == 0) {
      if (sumunits) {
        stop("need unitarea to combine estimation units")
      } else {
        stop("need unitarea to return acre estimates")
      }
    }
    unitvars <- c(unitvar, unitvar2)
    unitdat <- check.unitarea(unitarea=unitarea, pltx=pltassgnx, 
		unitvars=unitvars, areavar=areavar, gui=gui)
    unitarea <- unitdat$unitarea
    areavar <- unitdat$areavar
  } else {
    unitarea <- NULL
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

  if (tabtype == "AREA") {
    returnlst$unitarea <- stratcheck$unitarea
    returnlst$areavar <- areavar
  }
  if (!getprop) 
    returnlst$rowvar <- rowvar

  if (!is.null(stratcombinelut)) 
    returnlst$stratcombinelut <- stratcombinelut

  return(returnlst)
}
