#' Photo-Based module - Generate population data for PB module.
#' 
#' Generates population data for generating photo-based estimation.  Plots that
#' are totally nonsampled are excluded from estimation dataset. Next, an
#' adjustment factor is calculated by strata to adjust for nonsampled
#' (nonresponse) conditions that have proportion less than 1. Attributes
#' adjusted to a per-acre value are summed by plot, divided by the adjustment
#' factor, and averaged by stratum.  Strata means are combined using the strata
#' weights and then expanded to using the total land area in the population.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot, to link to pltassgn (ex. PLT_CN).\cr \tab
#' \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr \tab \tab TPA_UNADJ
#' \tab Number of trees per acre each sample tree represents (ex. DESIGNCD=1:
#' TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for trees on
#' microplot).\cr \tab cond \tab cuniqueid \tab Unique identifier for each
#' plot, to link to pltassgn (ex. PLT_CN).\cr \tab \tab CONDID \tab Unique
#' identifier of each condition on plot.  Set CONDID=1, if only 1 condition per
#' plot.\cr \tab \tab CONDPROP_UNADJ \tab Unadjusted proportion of condition on
#' each plot.  Set CONDPROP_UNADJ=1, if only 1 condition per plot.\cr \tab \tab
#' COND_STATUS_CD \tab Status of each forested condition on plot (i.e.
#' accessible forest, nonforest, water, etc.)\cr \tab \tab NF_COND_STATUS_CD
#' \tab If ACI=TRUE. Status of each nonforest condition on plot (i.e.
#' accessible nonforest, nonsampled nonforest)\cr \tab \tab SITECLCD \tab If
#' landarea=TIMBERLAND. Measure of site productivity.\cr \tab \tab RESERVCD
#' \tab If landarea=TIMBERLAND. Reserved status.\cr \tab \tab SUBPROP_UNADJ
#' \tab Unadjusted proportion of subplot conditions on each plot.  Set
#' SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr \tab \tab
#' MICRPROP_UNADJ \tab If microplot tree attributes. Unadjusted proportion of
#' microplot conditions on each plot. Set MICRPROP_UNADJ=1, if only 1 condition
#' per microplot.\cr \tab \tab MACRPROP_UNADJ \tab If macroplot tree
#' attributes. Unadjusted proportion of macroplot conditions on each plot. Set
#' MACRPROP_UNADJ=1, if only 1 condition per macroplot.\cr \tab pltassgn \tab
#' puniqueid \tab Unique identifier for each plot, to link to cond (ex. CN).\cr
#' \tab \tab STATECD \tab Identifies state each plot is located in.\cr \tab
#' \tab INVYR \tab Identifies inventory year of each plot.\cr \tab \tab
#' PLOT_STATUS_CD \tab Status of each plot (i.e. sampled, nonsampled).  If not
#' included, all plots are assumed as sampled.\cr }
#' 
#' For available reference tables: sort(unique(FIESTAutils::ref_codes$VARIABLE)) \cr
#' 
#' @param pntdat DF/DT or comma-delimited file (*.csv). Point-level table with
#' one record per point. If NULL, aggregated point counts must be in pntcnt.
#' @param pltpct DF/DT or comma-delimited file (*.csv). Plot-domain-level table
#' with percent observed by domain per plot.
#' @param plotid String. Unique identifier of plot in pnt. All values must
#' match puniqueid values, if pltassgn is not NULL.
#' @param pntid String. Unique identifier of points in pnt.
#' @param pltpctvars String vector. Variables in pltpct for estimation. If
#' NULL, all variables are used except plotid in pltpct.
#' @param plt DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
#' Can also be a shapefile(*.shp) with one record per plot, a spatial layer in
#' dsn, or a sf R object. Plot-level variables. If nonsampled plots are
#' included, PLOT_STATUS_CD variable must be in table. Optional.
#' @param pltassgn DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or
#' shapefile(*.shp). The plot-level data with one record per plot, including
#' estimation unit and/or strata information. Optional.
#' @param puniqueid String. Unique identifier of plot.
#' @param pltassgnid String. Name of unique identifier of plot in pltassgn with
#' All values must match plotid values if pnt is not NULL.
#' @param nonsamp.pfilter String. An expression for filtering nonsampled plots.
#' Must be R syntax.
#' @param sumunits Logical. If TRUE, estimation units are combined to one table
#' for output. Note: only available if tabtype="AREA". Acres
#' @param unitvar String. Name of the estimation unit variable in cond or
#' pltassgn with estimation unit assignment for each plot (e.g., 'ESTN_UNIT').
#' If one estimation unit, set unitvar=NULL.
#' @param unitarea Numeric or DF. Total area by estimation unit. If only 1
#' estimation unit, include number of total acreage for the area of interest or
#' a data frame with areavar. If more than one estimation unit, provide a data
#' frame of total area by estimation unit, including unitvar and areavar.
#' @param areavar String. Name of acre variable in unitarea. Default="ACRES".
#' @param strata Logical. If TRUE, add data information for stratification.
#' @param strtype String. If strata=TRUE, the type of strata ('POST', 'PRE').
#' Note: the variance equations are slightly different.
#' @param stratalut DF/DT. If strata=TRUE, look-up table with strata
#' proportions ('strwt') by strata (and estimation unit). To calculate 'strwt',
#' set getwt=TRUE and getwtvar= name of variable with information to calculate
#' weights from (e.g., pixel counts)
#' @param strvar String. If strata=TRUE, name of the strata variable in
#' stratalut and cond or pltassgn data frame with stratum assignment for each
#' plot (Default = 'STRATUMCD').
#' @param pvars2keep String vector. Additional plot variables to keep in
#' dataset.
#' @param saveobj Logical. If TRUE, saves SApopdat object to outfolder.
#' @param objnm String. Name of *.rda object.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param unit_opts List. See help(unit_options()) for a list of options.
#' @param strata_opts List. See help(strata_options()) for a list of options.
#' Only used when strata = TRUE. 
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param PBstratdat R List object. Output data list components from
#' FIESTA::DBgetStrata().
#' @param gui Logical. If gui, user is prompted for parameters.
#' @return A list with population data for Green-Book estimates.
#' 
#' \item{condx}{ Data frame. Condition-level data including plot-level
#' assignment of estimation unit and stratum (if strata=TRUE) and adjusted
#' condition proportion. } \item{pltcondx}{ Data frame. Condition-level data,
#' merged with plot data. } \item{cuniqueid}{ String. Unique identifier of plot
#' in condx and pltcondx. } \item{condid}{ String. Unique identifier of
#' condition in condx and pltcondx. } \item{treex}{ Data frame. If
#' esttype='TREE', tree-level data, including sample adjustment factor. }
#' \item{tuniqueid}{ String. If esttype='TREE', unique identifier of plot in
#' treex. } \item{ACI.filter}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD
#' == 1" . } \item{unitarea}{ String. Returned table of area by estimation
#' unit. } \item{unitvar}{ String. Variable name for estimation unit. }
#' \item{strlut}{ String. Strata-level table with pixel counts by strata
#' (P1POINTCNT), strata weights (strwt), number of plots by strata (n.strata),
#' total number of plots in estimation unit (n.total), sum of condition
#' proportions (*_UNADJ_SUM), area adjustments (*_ADJFAC), total area, and area
#' expansion by strata (EXPNS). } \item{strvar}{ String. Variable name for
#' strata. If strata=FALSE, strvar="ONESTRAT". } \item{expcondtab}{ String. If
#' ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . } \item{plotsampcnt}{ Data
#' frame. Number of plots by PLOT_STATUS_CD. } \item{condsampcnt}{ Data frame.
#' Number of conditions by COND_STATUS_CD. } \item{states}{ String. State names
#' in dataset. } \item{invyrs}{ String. Range of inventory years in dataset. }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description}\cr \tab unitvar
#' \tab estimation unit \cr \tab strvar \tab stratum value \cr \tab strwtvar
#' \tab number of pixels by strata and estimation unit \cr \tab n.strata \tab
#' number of plots in strata (after totally nonsampled plots removed) \cr \tab
#' n.total \tab number of plots for estimation unit \cr \tab strwt \tab
#' proportion of area (or plots) by strata and estimation unit (i.e., strata
#' weight) \cr \tab CONDPROP_UNADJ_SUM \tab summed condition proportion by
#' strata and estimation unit \cr \tab CONDPROP_ADJFAC \tab adjusted condition
#' proportion by strata after nonsampled plots removed \cr \tab AREA_USED \tab
#' total area of estimation unit \cr \tab expfac \tab strata-level expansion
#' factor after nonsampled plots and conditions removed (AREA_USED/n.strata)
#' \cr \tab EXPNS \tab strata-level area expansions (expfac * strwt)\cr }
#' 
#' Table(s) are also written to outfolder.
#' @note
#' 
#' ADJUSTMENT FACTOR:\cr The adjustment factor is necessary to account for
#' nonsampled conditions. It is calculated for each estimation unit by strata.
#' by summing the unadjusted proportions of the subplot, microplot, and
#' macroplot (i.e. *PROP_UNADJ) and dividing by the number of plots in the
#' strata/estimation unit).
#' 
#' An adjustment factor is determined for each tree based on the size of the
#' plot it was measured on. This is identified using TPA_UNADJ as follows:
#' 
#' \tabular{llr}{ \tab \bold{PLOT SIZE} \tab \bold{TPA_UNADJ} \cr \tab SUBPLOT
#' \tab 6.018046 \cr \tab MICROPLOT \tab 74.965282 \cr \tab MACROPLOT \tab
#' 0.999188 \cr }
#' 
#' If ACI=FALSE, only nonsampled forest conditions are accounted for in the
#' adjustment factor. \cr If ACI=TRUE, the nonsampled nonforest conditions are
#' removed as well and accounted for in adjustment factor.  This is if you are
#' interested in estimates for all lands or nonforest lands in the
#' All-Condition-Inventory.
#' 
#' unitcombine:\cr If TRUE and less than 2 plots in any one estimation unit,
#' all estimation units with 10 or less plots are combined. The current method
#' for combining is to group the estimation unit with less than 10 plots with
#' the estimation unit following in consecutive order (numeric or
#' alphabetical), restrained by survey unit (UNITCD) if included in dataset,
#' and continuing until the number of plots equals 10. If there are no
#' estimation units following in order, it is combined with the estimation unit
#' previous in order.
#' 
#' stratcombine:\cr If TRUE and less than 2 plots in any one strata class
#' within an estimation unit, all strata classes with 2 or less plots are
#' combined. The current method for combining is to group the strata with less
#' than 2 plots with the strata class following in consecutive order (numeric
#' or alphabetical), restrained by estimation unit (if unitcombine=FALSE), and
#' continuing until the number of plots equals 10. If there are no strata
#' classes following in order, it is combined with the estimation unit previous
#' in order.
#' @author Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
#' @references Scott, Charles T.; Bechtold, William A.; Reams, Gregory A.;
#' Smith, William D.; Westfall, James A.; Hansen, Mark H.; Moisen, Gretchen G.
#' 2005. Sample-based estimators used by the Forest Inventory and Analysis
#' national information management system. Gen. Tech. Rep. SRS-80. Asheville,
#' NC: U.S. Department of Agriculture, Forest Service, Southern Research
#' Station, p.53-77.
#' @keywords data
#' @examples 
#' # Load necessary data from FIESTA
#' ## Point data
#' icepntfn <- system.file("extdata",
#'                         "PB_data/icepnt_utco1135.csv",
#'                          package = "FIESTA")
#' icepnt <- read.csv(icepntfn)
#' 
#' ## Plot data
#' icepltfn <- system.file("extdata",
#'                         "PB_data/icepltassgn_utco1135.csv",
#'                          package = "FIESTA")
#' iceplt <- read.csv(icepltfn)
#' 
#' # Percent land cover at Time 1 (2011) for all land in Davis and Salt Lake
#' # Counties, UT
#' PBpopdat <- modPBpop(pnt = icepnt, 
#'                      pltassgn = iceplt,
#'                      pltassgnid = "plot_id",
#'                      pntid = "dot_cnt")
#' 
#' str(PBpopdat, max.level = 1)
#' 
#' # We can also create population data for estimates by estimation unit
#' ## Read in data for multiple estimation units
#' unitareafn <- system.file("extdata", 
#'                           "PB_data/unitarea_utco1135.csv",
#'                            package = "FIESTA")
#' unitarea <- read.csv(unitareafn)
#' 
#' ## Run modPBpop
#' PBpopunit <- modPBpop(pnt = icepnt, 
#'                       pltassgn = iceplt, 
#'                       pltassgnid = "plot_id", 
#'                       pntid = "dot_cnt",
#'                       unitarea = unitarea, 
#'                       unitvar = "ESTN_UNIT")
#' @export modPBpop
modPBpop <- function(pntdat = NULL, 
                     pltpct = NULL, 
                     plotid = "plot_id", 
                     pntid = NULL, 
                     pltpctvars = NULL, 
                     plt = NULL, 
                     pltassgn = NULL, 
                     puniqueid = "CN", 
                     pltassgnid = "CN", 
                     nonsamp.pfilter = NULL, 
                     sumunits = FALSE, 
                     unitvar = NULL, 
                     unitarea = NULL, 
                     areavar = "ACRES",
                     strata = FALSE, 
                     strtype = "POST",
                     stratalut = NULL, 
                     strvar = "STRATUMCD", 
                     pvars2keep = NULL, 
                     saveobj = FALSE, 
                     objnm = "PBpopdat", 
                     savedata = FALSE, 
                     unit_opts = NULL,
                     strata_opts = NULL,
                     savedata_opts = NULL,
                     PBstratdat = NULL, 
                     gui = FALSE){

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

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt <- NULL

  ## Set parameters
  auxvars <- NULL

  
  # Check input parameters
   input.params <- names(as.list(match.call()))[-1]
   formallst <- names(formals(modPBpop))
   if (!all(input.params %in% formallst)) {
     miss <- input.params[!input.params %in% formallst]
     stop("invalid parameter: ", toString(miss))
   }

  ## Check parameter lists
  pcheck.params(input.params, strata_opts=strata_opts, unit_opts=unit_opts, 
                savedata_opts=savedata_opts)
  
  
  ## Set unit defaults
  unit_defaults_list <- formals(unit_options)[-length(formals(unit_options))]
  
  for (i in 1:length(unit_defaults_list)) {
    assign(names(unit_defaults_list)[[i]], unit_defaults_list[[i]])
  }
  
  ## Set user-supplied unit values
  if (length(unit_opts) > 0) {
    for (i in 1:length(unit_opts)) {
      if (names(unit_opts)[[i]] %in% names(unit_defaults_list)) {
        assign(names(unit_opts)[[i]], unit_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(unit_opts)[[i]]))
      }
    }
  }
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ## Set strata defaults
  strata_defaults_list <- formals(strata_options)[-length(formals(strata_options))]
  
  for (i in 1:length(strata_defaults_list)) {
    assign(names(strata_defaults_list)[[i]], strata_defaults_list[[i]])
  }
  
  ## Set user-supplied strata values
  if (length(strata_opts) > 0) {
    for (i in 1:length(strata_opts)) {
      if (names(strata_opts)[[i]] %in% names(strata_defaults_list)) {
        assign(names(strata_opts)[[i]], strata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(strata_opts)[[i]]))
      }
    }
  }
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check saveobj 
  saveobj <- pcheck.logical(saveobj, varnm="saveobj", 
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)
 
 
  ## Check output
  ########################################################
  if (savedata || saveobj) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
            out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
            overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
            add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
  } 

    if (!is.null(PBstratdat)) {
      list.items <- c("pltassgn", "unitarea", "unitvar", "stratalut", "strvar")
      PBstratdat <- pcheck.object(PBstratdat, "PBstratdat", list.items=list.items)
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
  unitcombine <- ifelse(unit.action == "combine", TRUE, FALSE)
  popcheck <- check.popdataPB(gui=gui, pnt=pntdat, pltpct=pltpct, pltpctvars=pltpctvars, 
                      plt=plt, pltassgn=pltassgn, plotid=plotid, pntid=pntid, 
                      puniqueid=puniqueid, pltassgnid=pltassgnid, 
                      nonsamp.pfilter=nonsamp.pfilter, 
                      unitvar=unitvar, unitvar2=unitvar2, 
                      unitarea=unitarea, areavar=areavar, areaunits=areaunits, 
                      unit.action=unit.action, auxvars=auxvars, 
                      strata=strata, strvar=strvar, stratcombine=stratcombine, 
                      pvars2keep=pvars2keep, sumunits=sumunits)
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
  areaunits <- popcheck$areaunits
  unit.action <- popcheck$unit.action
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
		strata=strata, auxlut=stratalut, strvar=strvar, stratcombine=stratcombine, 
		unit.action=unit.action, unitarea=unitarea, unitvar=unitvar, 
		unitvar2=unitvar2, areavar=areavar, minplotnum.unit=minplotnum.unit, 
		minplotnum.strat=minplotnum.strat, getwt=getwt, getwtvar=getwtvar,
		strwtvar=strwtvar)  
  pltassgnx <- stratcheck$pltx
  unitarea <- stratcheck$unitarea
  unitvar <- stratcheck$unitvar
  unitvars <- stratcheck$unitvars
  stratalut <- stratcheck$auxlut
  strvar <- stratcheck$strvar
  strwtvar <- stratcheck$strwtvar
  stratcombinelut <- stratcheck$unitstrgrplut
  strunitvars <- c(unitvar, strvar)

  returnlst <- list(PBx=PBx, pltassgnx=pltassgnx, plotid=plotid, pntid=pntid, 
		pltassgnid=pltassgnid, sumunits=sumunits, 
		unitvar=unitvar, unitvars=unitvars, 
		strata=strata, strtype=strtype, stratalut=stratalut, 
		strvar=strvar, strwtvar=strwtvar, 
 		plotsampcnt=plotsampcnt, getprop=getprop)
  
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
                  overwrite=overwrite_layer, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(PBx, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="PBx",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
   
    datExportData(pltassgnx, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="pltassgn",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    if (!is.null(unitarea)) {
      datExportData(unitarea, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="unitarea",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    }
    datExportData(stratalut, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="stratalut",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
  }

  return(returnlst)
}
