#' Green-Book module - Generate population data for GB module.
#' 
#' Generates population data for generating 'green-book' estimates (Scott et
#' al. 2005).  Plots that are totally nonsampled are excluded from estimation
#' dataset. Next, an adjustment factor is calculated by strata to adjust for
#' nonsampled (nonresponse) conditions that have proportion less than 1.
#' Attributes adjusted to a per-acre value are summed by plot, divided by the
#' adjustment factor, and averaged by stratum.  Strata means are combined using
#' the strata weights and then expanded to using the total land area in the
#' population. 
#' 
#' Population types \cr \tabular{lll}{ \tab \bold{popType}
#' \bold{Description}\cr 
#' \tab ALL \tab Population data, including nonsampled plots.\cr 
#' \tab CURR \tab Population data for area estimates, excluding nonsampled 
#' plots.\cr 
#' \tab VOL \tab Population data for area/tree estimates, excluding 
#' nonsampled plots.\cr 
#' \tab LULC \tab Population data for land use/land cover transitional estimates, 
#' including only plots with previous measurements and excluding nonsampled 
#' plots.\cr }
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr 
#' \tab tree \tab tuniqueid \tab Unique identifier for each plot in tree 
#' table.\cr \tab \tab CONDID \tab Unique identifier of each condition on plot.  
#' Set CONDID=1, if only 1 condition per plot.\cr 
#' \tab \tab TPA_UNADJ \tab Number of trees per acre each sample tree represents 
#' (e.g., DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot; 74.965282 for 
#' trees on microplot).\cr 
#' \tab cond \tab cuniqueid \tab Unique identifier for each plot in cond 
#' table.\cr 
#' \tab \tab CONDID \tab Unique identifier of each condition on plot. Set 
#' CONDID=1, if only 1 condition per plot.\cr 
#' \tab \tab CONDPROP_UNADJ \tab Unadjusted proportion of condition on each 
#' plot. Set CONDPROP_UNADJ=1, if only 1 condition per plot.\cr 
#' \tab \tab COND_STATUS_CD \tab Status of each forested condition on
#' plot (i.e. accessible forest, nonforest, water, etc.)\cr 
#' \tab \tab NF_COND_STATUS_CD \tab If ACI=TRUE. Status of each nonforest 
#' condition on plot (i.e. accessible nonforest, nonsampled nonforest)\cr 
#' \tab \tab SITECLCD \tab If landarea=TIMBERLAND. Measure of site 
#' productivity.\cr 
#' \tab \tab RESERVCD \tab If landarea=TIMBERLAND. Reserved status.\cr 
#' \tab \tab SUBPROP_UNADJ \tab Unadjusted proportion of subplot conditions 
#' on each plot. Set SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr 
#' \tab \tab MICRPROP_UNADJ \tab If microplot tree attributes. Unadjusted 
#' proportion of microplot conditions on each plot. Set MICRPROP_UNADJ=1, 
#' if only 1 condition per microplot.\cr 
#' \tab \tab MACRPROP_UNADJ \tab If macroplot tree attributes. Unadjusted 
#' proportion of macroplot conditions on each plot. Set MACRPROP_UNADJ=1, 
#' if only 1 condition per macroplot.\cr 
#' \tab pltassgn \tab pltassgnid \tab Unique identifier for each plot in 
#' pltassgn.\cr 
#' \tab \tab STATECD \tab Identifies state each plot is located in.\cr 
#' \tab \tab INVYR \tab Identifies inventory year of each plot.\cr 
#' \tab \tab PLOT_STATUS_CD \tab Status of each plot (i.e. sampled, nonsampled).  
#' If not included, all plots are assumed as sampled.\cr }
#' 
#' For available reference tables: sort(unique(FIESTAutils::ref_codes$VARIABLE)) \cr
#' 
#' @param popType String. Type of evaluation(s) to include in population data.
#' Note: currently only c('CURR', 'VOL', 'LULC', 'DWM') 
#' are available. See details below for descriptions of each.
#' @param popTabs List of population tables the user would like returned.
#'  See help(popTables) for a list of options.
#' @param popTabIDs List of unique IDs corresponding to the population tables
#' that the user has requested. See help(popTableIDs) for a list of
#' options.
#' @param popFilter List of population filters. See help(popFilters) for a 
#' list of options. 
#' @param pltassgn DF/DT, Optional. R object, sf R object, comma-delimited
#' file(.csv), layer or spatial layer in dsn, or shapefile(.shp). Plot-level
#' assignment of estimation unit and/or strata, with one record for each plot.
#' @param pltassgnid String.
#' @param dsn String. Name of database where tree, cond, and plot-level tables
#' reside.  The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html).
#' @param pjoinid String. Join variable in plot to match pltassgnid. Does not
#' need to be uniqueid. If using most current XY coordinates for plot
#' assignments, use identifier for plot (e.g., PLOT_ID).
#' @param areawt String. Name of variable in cond for summarizing area 
#' weights (e.g., CONDPROP_UNADJ).
#' @param areawt2 String. An equation to multiply to areawt for estimation. 
#' All variables in equation must be in cond.
#' weights (e.g., CONDPROP_UNADJ).
#' @param adj String. How to calculate adjustment factors for nonsampled
#' (nonresponse) conditions based on summed proportions for by plot ('samp',
#' 'plot').  'samp' - adjustments are calculated at strata/estimation unit
#' level; 'plot' - adjustments are calculated at plot-level. Adjustments are
#' only calculated for annual inventory plots (DESIGNCD=1).
#' @param defaultVars Logical. If TRUE, a set of default variables are selected.
#' @param unitvar String. Name of the estimation unit variable in unitarea and
#' cond or pltassgn data frame with estimation unit assignment for each plot
#' (e.g., 'ESTN_UNIT'). Optional if only one estimation unit.
#' @param unitarea Numeric or DF. Total area by estimation unit. If only 1
#' estimation unit, include number of total acreage for the area of interest or
#' a data frame with area and estimation unit. If more than one estimation
#' unit, provide a data frame of total area by estimation unit, including
#' unitvar and areavar.
#' @param areavar String. Name of area variable in unitarea. Default="ACRES".
#' @param strata Logical. If TRUE, include information for post-stratification.
#' @param stratalut DF/DT. If strata=TRUE, look-up table with pixel counts or
#' area by strata or proportion or area ('strwt') by strata (and estimation
#' unit).  If 'strwt' is not included, set getwt=TRUE and getwtvar as the name
#' of variable to calculate weights from (e.g., pixel counts).
#' @param strvar String. If strata=TRUE, name of the strata variable in
#' stratalut and cond or pltassgn data frame with stratum assignment for each
#' plot (Default = 'STRATUMCD').
#' @param returndata Logical. If TRUE, returns data objects.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param saveobj Logical. If TRUE, saves returned list object to outfolder.
#' @param objnm String. Name of *.rds object.
#' @param unit_opts List. See help(unit_options()) for a list of options.
#' @param strata_opts List. See help(strata_options()) for a list of options.
#' Only used when strata = TRUE. 
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param GBdata R List object. Output data list components from
#' FIESTA::anGBdata().
#' @param pltdat R List object. Output data list components from
#' FIESTA::spGetPlots().
#' @param stratdat R List object. Output data list components from
#' FIESTA::spGetStrata().
#' @param auxdat R List object. Output data list components from
#' FIESTA::spGetAuxiliary().
#' @param gui Logical. If gui, user is prompted for parameters.
#' @param ... For extendibility.
#' @return A list with population data for Green-Book estimates.
#' 
#' \item{condx}{ Data frame. Condition-level data including plot-level
#' assignment of estimation unit and stratum (if strata=TRUE), condition
#' proportion adjustment factor (cadjfac), and adjusted condition proportions
#' (CONDPROP_ADJ). } 
#' \item{cuniqueid}{ String. Unique identifier of plot in condx and pltcondx. } 
#' \item{condid}{ String. Unique identifier of condition in condx and pltcondx. } 
#' \item{treex}{ Data frame. Tree data within population, used for estimation, 
#' including trees per acre adjustment factor (tadjfac), and adjusted trees per 
#' acre (TPA_ADJ) (if treef is included). }
#' \item{tuniqueid}{ String. Unique identifier of plot in treex (if treef is
#' included). } 
#' \item{ACI.filter}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1". } 
#' \item{unitarea}{ String. Returned table of area by estimation unit. } 
#' \item{unitvar}{ String. Variable name for estimation unit. } 
#' \item{strlut}{ String. Strata-level table with pixel counts by strata 
#' (P1POINTCNT), strata weights (strwt), number of plots by strata (n.strata), 
#' total number of plots in estimation unit (n.total), sum of condition proportions 
#' (_UNADJ_SUM), area adjustments (*_ADJFAC), total area, and area expansion 
#' by strata (EXPNS). } 
#' \item{strvar}{ String. Variable name for strata. If strata=FALSE, 
#' strvar="ONESTRAT". }
#' \item{expcondtab}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1".} 
#' \item{plotsampcnt}{ Data frame. Number of plots by PLOT_STATUS_CD. }
#' \item{condsampcnt}{ Data frame. Number of conditions by COND_STATUS_CD. }
#' \item{states}{ String. State names in dataset. } 
#' \item{invyrs}{ String. Range of inventory years in dataset. }
#' 
#' \item{stratdat}{ Data frame. Strata information by estimation unit. }
#' \tabular{lll}{ \tab \bold{Variable} \tab \bold{Description}\cr 
#' \tab unitvar \tab estimation unit \cr 
#' \tab strvar \tab stratum value \cr \tab strwtvar \tab number of pixels by 
#' strata and estimation unit \cr 
#' \tab n.strata \tab number of plots in strata (after totally nonsampled 
#' plots removed) \cr 
#' \tab n.total \tab number of plots for estimation unit \cr 
#' \tab strwt \tab proportion of area (or plots) by strata and estimation 
#' unit (i.e., strata weight) \cr 
#' \tab CONDPROP_UNADJ_SUM \tab summed conditionproportion by strata and 
#' estimation unit \cr 
#' \tab CONDPROP_ADJFAC \tab adjusted condition proportion by strata after 
#' nonsampled plots removed \cr 
#' \tab AREA_USED \tab total area of estimation unit \cr 
#' \tab expfac \tab strata-level expansion factor after nonsampled plots and 
#' conditions removed (AREA_USED/n.strata) \cr 
#' \tab EXPNS \tab strata-level area expansions (expfac * strwt)\cr }
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
#' \donttest{
#' GBpopdat <- modGBpop(
#' popTabs = list(cond = FIESTA::WYcond,  
#'                tree = FIESTA::WYtree,        
#'                seed = FIESTA::WYseed),      
#' popTabIDs = list(cond = "PLT_CN"),            
#' pltassgn = FIESTA::WYpltassgn,  
#' pltassgnid = "CN",        
#' pjoinid = "PLT_CN",         
#' unitarea = FIESTA::WYunitarea,
#' unitvar = "ESTN_UNIT",        
#' strata = TRUE,           
#' stratalut = WYstratalut,    
#' strata_opts = strata_options(getwt = TRUE)   
#' )
#' 
#' str(GBpopdat, max.level = 1)
#' }
#' @export modGBpop
modGBpop <- function(popType = "VOL",
                     popTabs = popTables(),
                     popTabIDs = popTableIDs(), 
                     popFilter = popFilters(),
                     pltassgn = NULL,
                     pltassgnid = "PLT_CN",
                     dsn = NULL, 
                     pjoinid = "CN", 
                     areawt = "CONDPROP_UNADJ", 
                     areawt2 = NULL,
                     adj = "samp", 
                     defaultVars = TRUE, 
                     unitvar = NULL, 
                     unitarea = NULL, 
                     areavar = "ACRES", 
                     strata = TRUE, 
                     stratalut = NULL, 
                     strvar = "STRATUMCD",
                     returndata = TRUE, 
                     savedata = FALSE,
                     saveobj = FALSE, 
                     objnm = "GBpopdat",
                     unit_opts = NULL,
                     strata_opts = NULL, 
                     savedata_opts = NULL, 
                     GBdata = NULL, 
                     pltdat = NULL, 
                     stratdat = NULL, 
                     auxdat = NULL, 
                     gui = FALSE, 
                     ...){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data 'on-the-fly', including strata weights, number
  ## of plots by strata and estimation unit, strata-level expansion factors,
  ## and sample-based area adjustment factors.
  ## - checks input parameters and data tables, including removing nonsampled
  ##   plots and conditions (see check.popdata for details).
  ## - checks auxiliary data (i.e., stratification data).
  ## - calculates adjustment factors for nonresponse and appends an adjustment
  ##   variable to condition and tree data.
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE

  ## If gui.. set variables to NULL
  if (gui) {
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  }

  ## Set parameters
  adjtree <- FALSE
  nonsamp.pfilter=nonsamp.cfilter <- NULL
  #nonsamp.vfilter.fixed <- FALSE
  poponly <- FALSE
  if (returndata) {
    returnlst <- list(module = "GB")
  }
 
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=expcondtab=V1=SUBPCOND_PROP=SUBPCOND_PROP_UNADJ=
    	treef=seedf=grmf=vcondsppf=vcondstrf=cond_dwm_calcf=bndx=RHGlut=
	sccmx=cond_pcondx=lulcx=popevalid <- NULL
  condid <- "CONDID"
  pvars2keep <- NULL

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modGBpop)) 
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

  ## Set popFilters defaults
  popFilters_defaults_list <- formals(popFilters)[-length(formals(popFilters))]
  
  for (i in 1:length(popFilters_defaults_list)) {
    assign(names(popFilters_defaults_list)[[i]], popFilters_defaults_list[[i]])
  }
  
  ## Set user-supplied popFilters values
  popFilter2 <- popFilters_defaults_list
  if (length(popFilter) > 0) {
    for (i in 1:length(popFilter)) {
      if (names(popFilter)[[i]] %in% names(popFilters_defaults_list)) {
		popFilter2[[names(popFilter)[[i]]]] <- popFilter[[i]]
      } else {
        stop(paste("Invalid parameter: ", names(popFilter)[[i]]))
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

  ## Set popTables defaults
  popTables_defaults_list <- formals(popTables)[-length(formals(popTables))]
  
  for (i in 1:length(popTables_defaults_list)) {
    assign(names(popTables_defaults_list)[[i]], popTables_defaults_list[[i]])
  }  
  
  ## Set popTabIDs defaults
  popTableIDs_defaults_list <- formals(popTableIDs)[-length(formals(popTableIDs))]
  
  for (i in 1:length(popTableIDs_defaults_list)) {
    if (names(popTableIDs_defaults_list)[[i]] == "cond") {
      assign("cuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "plt") {
      assign("puniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "tree") {
      assign("tuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "seed") {
      assign("suniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "vsubpspp") {
      assign("vsppuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "vsubpstr") {
      assign("vstruniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "invsubp") {
      assign("invuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "subplot") {
      assign("subpuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "subp_cond") {
      assign("subcuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "cond_dwm_calc") {
      assign("dwmuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "grm") {
      assign("grmuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "plot_pplot") {
      assign("pplotuniqueid", popTableIDs_defaults_list[[i]])
    }
    if (names(popTableIDs_defaults_list)[[i]] == "cond_pcond") {
      assign("pconduniqueid", popTableIDs_defaults_list[[i]])
    }
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
  if (savedata) {
    if (out_fmt == "sqlite" && is.null(out_dsn)) {
	  out_dsn <- "GBpopdat.db"
	}
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

  if (saveobj) {
    outobj_fmtlst <- c('rds', 'rda')
    outobj_fmt <- pcheck.varchar(var2check=outobj_fmt, varnm="outobj_fmt", 
           gui=gui, checklst=outobj_fmtlst, caption="outobj_fmt", 
           multiple=FALSE, stopifnull=TRUE)

    if (is.null(objnm)) {
      objnm <- "GBpopdat"
    }
    #if (append_layer) overwrite_layer <- FALSE
    if (append_layer) message("currently cannot append to object lists")
    objfn <- getoutfn(outfn=objnm, ext=outobj_fmt, outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
  }

  ## Check popType
  ########################################################
  #evalTyplst <- c("ALL", "CURR", "VOL", "LULC", "P2VEG", "INV", "GRM", "DWM")
  DWM_types <- c("CWD", "FWD_SM", "FWD_LG", "DUFF")
  evalTyplst <- c("ALL", "CURR", "VOL", "LULC", "P2VEG", "INV", "DWM", 
                  "CHNG", "GRM", "GROW", "MORT", "REMV")
  popType <- pcheck.varchar(var2check=popType, varnm="popType", gui=gui,
		checklst=evalTyplst, caption="popType", multiple=FALSE, stopifnull=TRUE)
  popevalid <- as.character(popFilter2$evalid)
  if (!is.null(popevalid)) {
    substr(popevalid, nchar(popevalid)-1, nchar(popevalid)) <- 
		formatC(FIESTAutils::ref_popType[FIESTAutils::ref_popType$popType %in% popType, "EVAL_TYP_CD"], 
		width=2, flag="0")
    #evalid <- as.character(evalid)
    #substr(evalid, nchar(evalid)-1, nchar(evalid)) <- "01"
  } 
  if (popType %in% c("GROW", "MORT", "REMV")) {
    popType <- "GRM"
  }
 
  ###################################################################################
  ## Load data
  ###################################################################################
  if (!is.null(GBdata)) {
    list.items <- c("tabs", "unitarea")
    GBdata <- pcheck.object(GBdata, "GBdata", list.items=list.items)
    #bnd <- GBdata$bnd
    popTabs <- GBdata$tabs
    popTabIDs <- GBdata$tabIDs
    pltassgn <- GBdata$pltassgn
    pltassgnid <- GBdata$pltassgnid 
    unitarea <- GBdata$unitarea
    areavar <- GBdata$areavar
    unitzonal <- GBdata$unitzonal
    stratalut <- GBdata$stratalut
    predfac <- GBdata$predfac
    puniqueid <- GBdata$puniqueid
    pjoinid <- GBdata$pjoinid

    if (is.null(unitvar)) {
      unitvar <- GBdata$unitvar
      unitvar2 <- GBdata$unitvar2
    }    
    if (strata) { 
      if (is.null(strvar)) {
        if (!is.null(GBdata$strvar)) {
          strvar <- GBdata$strvar
        }		  
        if (!is.null(predfac) && length(predfac) == 1) {
          strvar <- predfac
        } else {
          stop("must include strvar if strata=TRUE")
        }
      } 
      strwtvar <- "strwt" 
      if (!is.null(unitzonal) && is.null(stratalut)) {
	    byunitvars <- c(unitvar, unitvar2)
		if ("AOI" %in% names(unitzonal)) {
		  byunitvars <- c(byunitvars, "AOI")
		}
        stratalut <- strat.pivot(unitzonal, unitvars=byunitvars, 
                      strvar, strwtvar=strwtvar)
		pivot <- FALSE
      }
    }
  } else {
    ## Extract list objects
    if (!is.null(pltdat)) {
	  tabnames <- if (sum(names(pltdat$tabs) %in% names(popTables())) == 0) {
	    stop("no tables exist in pltdat")
      }			  
      popTabs <- pltdat$tabs[names(pltdat$tabs) %in% names(popTables())]
      popTabIDs <- pltdat$tabIDs[names(pltdat$tabIDs) %in% names(popTableIDs())]
      pjoinid <- pltdat$pjoinid
    }
    if (!is.null(stratdat)) {
      list.items <- c("pltassgn", "unitarea", "unitvar")
      stratdat <- pcheck.object(stratdat, "stratdat", list.items=list.items)
      bndx <- stratdat$bndx
      pltassgn <- stratdat$pltassgn
      pltassgnid <- stratdat$pltassgnid
      unitarea <- stratdat$unitarea
      areavar <- stratdat$areavar
      stratalut <- stratdat$stratalut
      strvar <- stratdat$strvar
      getwt <- stratdat$getwt
      getwtvar <- stratdat$getwtvar
      strwtvar <- stratdat$strwtvar

      if (is.null(unitvar)) {
        unitvar <- stratdat$unitvar
        unitvar2 <- stratdat$unitvar2
      } 

#      if (strata) {
#        if (is.null(strwtvar)) {
#          stop("missing strwtvar")
#        }
#        if (strwtvar != "strwt") {
#          names(stratalut)[names(stratalut) == strwtvar] <- "strwt"
#          strwtvar <- "strwt"
#        }
#      }
    } else if (!is.null(auxdat)) {
      list.items <- c("pltassgn", "unitzonal", "unitvar", "predfac", 
		"pltassgnid", "unitarea", "areavar")
      auxdat <- pcheck.object(auxdat, "auxdat", list.items=list.items)
      pltassgn <- auxdat$pltassgn
      pltassgnid <- auxdat$pltassgnid
      stratalut <- auxdat$unitzonal
      unitvar <- auxdat$unitvar
      unitvar2 <- auxdat$unitvar2
      unitarea <- auxdat$unitarea
      areavar <- auxdat$areavar
      predfac <- auxdat$predfac

      if (strata) {
        if (is.null(strvar) || !strvar %in% auxdat$prednames) {    
          if (!is.null(predfac) && length(predfac) == 1) {
            strvar <- predfac
          } else {
            stop("must include strvar if strata=TRUE")
          }
        } 
        pivot <- TRUE
      }
    }
  } 

  ## Set user-supplied popTable values 
  popTables_defaults_list <- formals(popTables)[-length(formals(popTables))]
  if (length(popTabs) > 0) {
    for (i in 1:length(popTabs)) {
      if (names(popTabs)[[i]] %in% names(popTables_defaults_list)) {
        assign(names(popTabs)[[i]], popTabs[[i]])
      } else {
        message(paste("Invalid parameter: ", names(popTabs)[[i]]))
      }
    }
  } else {
    stop("need to include popTabs")
  }
  list.items <- c("cond")
  if (popType == "VOL") {
    list.items <- c(list.items, "tree")
  }
  if (popType == "P2VEG") {
    list.items <- c(list.items, "vsubpstr", "subplot", "subp_cond")
  }
  if (popType == "DWM") {
    list.items <- c(list.items, "cond_dwm_calc")
  }
  if (popType == "CHNG") {
    list.items <- c(list.items, "sccm")
  }
  
  ## Check popTabs
  popTabs <- pcheck.object(popTabs, "popTabs", list.items=list.items, stopifnull=TRUE)


  ## Set user-supplied popTabIDs values
  ### Check for invalid parameters first
  popTableIDs_defaults_list <- formals(popTableIDs)[-length(formals(popTableIDs))]
  for (i in 1:length(popTabIDs)) {
    if (!(names(popTabIDs)[[i]] %in% names(popTableIDs_defaults_list))) {
      message(paste("Invalid parameter: ", names(popTabIDs)[[i]]))
    }
  }
  ### Then actually set the values
  for (nm in names(popTabs)) {
    if (!any(names(popTabIDs) == nm)) {
      popTabIDs[[nm]] <- popTableIDs_defaults_list[[nm]]
    }
  }

  ###################################################################################
  ## CHECK PLOT PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots (if nonsamp.pfilter != "NONE")
  ## Applies plot filters
  ###################################################################################
  pltcheck <- check.popdataPLT(dsn=dsn, tabs=popTabs, tabIDs=popTabIDs, 
      pltassgn=pltassgn, pltassgnid=pltassgnid, pjoinid=pjoinid, 
      module="GB", popType=popType, popevalid=popevalid, adj=adj, 
	  popFilter=popFilter2, nonsamp.pfilter=nonsamp.pfilter, 
      unitarea=unitarea, areavar=areavar, unitvar=unitvar, 
      unitvar2=unitvar2, areaunits=areaunits, unit.action=unit.action, 
      strata=strata, stratalut=stratalut, strvar=strvar, pivot=pivot,
	  pvars2keep=pvars2keep, defaultVars=defaultVars)
  if (is.null(pltcheck)) return(NULL)
  pltassgnx <- pltcheck$pltassgnx
  pltassgnid <- pltcheck$pltassgnid
  pfromqry <- pltcheck$pfromqry
  palias <- pltcheck$palias
  pjoinid <- pltcheck$pjoinid
  whereqry <- pltcheck$whereqry
  ACI <- pltcheck$ACI
  pltx <- pltcheck$pltx
  puniqueid <- pltcheck$puniqueid
  unitvar <- pltcheck$unitvar
  unitvar2 <- pltcheck$unitvar2
  unitarea <- pltcheck$unitarea
  areavar <- pltcheck$areavar
  areaunits <- pltcheck$areaunits
  unit.action <- pltcheck$unit.action
  stratcombine <- pltcheck$stratcombine
  strata <- pltcheck$strata
  stratalut <- pltcheck$stratalut
  strvar <- pltcheck$strvar
  P2POINTCNT <- pltcheck$P2POINTCNT 
  plotsampcnt <- pltcheck$plotsampcnt
  states <- pltcheck$states
  invyrs <- pltcheck$invyrs
  dbconn <- pltcheck$dbconn

  if (ACI) {
    nfplotsampcnt <- pltcheck$nfplotsampcnt
  }

  if (popType %in% c("ALL", "CURR", "VOL")) {
    ###################################################################################
    ## Check parameters and data for popType AREA/VOL
    ###################################################################################
    popcheck <- check.popdataVOL(gui=gui, 
          tabs=popTabs, tabIDs=popTabIDs, pltassgnx=pltassgnx, 
          pfromqry=pfromqry, palias=palias, pjoinid=pjoinid, 
          whereqry=whereqry, adj=adj, ACI=ACI, 
          pltx=pltx, puniqueid=puniqueid, dsn=dsn, dbconn=dbconn,
          condid="CONDID", nonsamp.cfilter=nonsamp.cfilter, 
          areawt=areawt, areawt2=areawt2, pvars2keep=pvars2keep,
		  defaultVars = defaultVars)
    if (is.null(popcheck)) return(NULL)
    condx <- popcheck$condx
    pltcondx <- popcheck$pltcondx
    treef <- popcheck$treef
    seedf <- popcheck$seedf
    cuniqueid <- popcheck$cuniqueid
    condid <- popcheck$condid
    tuniqueid <- popcheck$tuniqueid
    ACI.filter <- popcheck$ACI.filter
    condsampcnt <- popcheck$condsampcnt
    areawt <- popcheck$areawt
    areawt2 <- popcheck$areawt2
    tpropvars <- popcheck$tpropvars
  }

  if (popType %in% c("CHNG", "GRM")) {
    ###################################################################################
    ## Check parameters and data for popType AREA/VOL
    ###################################################################################
    popcheck <- check.popdataCHNG(gui=gui, popType=popType,
          tabs=popTabs, tabIDs=popTabIDs, pltassgnx=pltassgnx, 
          pfromqry=pfromqry, palias=palias, pjoinid=pjoinid, 
          whereqry=whereqry, adj=adj, ACI=ACI, 
          pltx=pltx, puniqueid=puniqueid, dsn=dsn, dbconn=dbconn,
          condid="CONDID", nonsamp.cfilter=nonsamp.cfilter, 
          cvars2keep="REMPER", pvars2keep=pvars2keep)
    if (is.null(popcheck)) return(NULL)
    condx <- popcheck$sccm_condx
    sccmx <- popcheck$sccmx
    treef <- popcheck$treef
    grmf <- popcheck$grmf
    beginf <- popcheck$beginf
    midptf <- popcheck$midptf
    pltcondx <- popcheck$pltcondx
    cuniqueid <- popcheck$cuniqueid
    condid <- popcheck$condid
    tuniqueid <- popcheck$tuniqueid
    ACI.filter <- popcheck$ACI.filter
    condsampcnt <- popcheck$condsampcnt
    areawt <- popcheck$areawt
    tpropvars <- popcheck$tpropvars
  }

  if (popType == "P2VEG") {
    popcheck <- check.popdataP2VEG(gui=gui, 
          tabs=popTabs, tabIDs=popTabIDs, pltassgnx=pltassgnx,
          pfromqry=pfromqry, palias=palias, pjoinid=pjoinid, 
          whereqry=whereqry, adj=adj, ACI=ACI, 
          pltx=pltx, puniqueid=puniqueid, dsn=dsn, dbconn=dbconn, 
          condid="CONDID", nonsamp.cfilter=nonsamp.cfilter, 
		  pvars2keep=pvars2keep)
    pltcondx <- popcheck$pltcondx
    pltassgnx <- popcheck$pltassgnx
    pltassgnid <- popcheck$pltassgnid
    condx <- popcheck$condx
    vcondx <- popcheck$vcondx
    vcondsppf <- popcheck$vcondsppf
    vcondstrf <- popcheck$vcondstrf
    ACI.filter <- popcheck$ACI.filter
    condsampcnt <- popcheck$condsampcnt
    areawt <- popcheck$areawt
    vareawt <- popcheck$vareawt
    vuniqueid <- popcheck$vcondstrid
  }

  if (popType == "DWM") {
    popcheck <- check.popdataDWM(gui=gui, 
          tabs=popTabs, tabIDs=popTabIDs, pltassgnx=pltassgnx, 
          pfromqry=pfromqry, palias=palias, pjoinid=pjoinid, 
          whereqry=whereqry, adj=adj, ACI=ACI, 
          pltx=pltx, puniqueid=puniqueid, dsn=dsn, dbconn=dbconn,
          condid="CONDID", nonsamp.cfilter=nonsamp.cfilter, 
		  pvars2keep=pvars2keep)
    condx <- popcheck$condx
    pltcondx <- popcheck$pltcondx
    cuniqueid <- popcheck$cuniqueid
    condid <- popcheck$condid
    tuniqueid <- popcheck$tuniqueid
    ACI.filter <- popcheck$ACI.filter
    condsampcnt <- popcheck$condsampcnt
    areawt <- popcheck$areawt
    dwmpropvars <- popcheck$dwmpropvars
  }

#  if (popType %in% c("GRM", "CHNG", "LULC")) {
#    sccmx <- popcheck$sccmx
#    condx <- popcheck$sccm_condx
#    cond_pcondx <- popcheck$cond_pcondx
#    tpropvars <- list(SUBP="SUBPPROP_UNADJ", MICR="MICRPROP_UNADJ", MACR="MACRPROP_UNADJ")
#  }

#  if (popType == "LULC") {
#    lulcx <- popcheck$lulcx
#  }

  ###################################################################################
  ## CHECK STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unit.action='combine', combines estimation units to reach minplotnum.unit.
  ## If unitvar and unitvar2, concatenates variables to 1 unitvar
  ###################################################################################
  auxdat <- check.auxiliary(pltx = pltassgnx, 
                            puniqueid = pltassgnid,
							unitvar = unitvar, 
							unitvar2 = unitvar2,
							unitarea = unitarea, 
							areavar = areavar,
							minplotnum.unit = minplotnum.unit, 
							unit.action = unit.action,
							strata = strata, 
							auxlut = stratalut, 
							strvar = strvar,
							stratcombine = stratcombine, 
							minplotnum.strat = minplotnum.strat,
							removeifnostrata = TRUE, 
							getwt = getwt,
							getwtvar = getwtvar, 
							strwtvar = strwtvar, 
							P2POINTCNT = P2POINTCNT,
							auxtext = "stratalut",
							AOI = popFilter$AOIonly)
  pltassgnx <- setDT(auxdat$pltx)
  unitarea <- auxdat$unitarea
  stratalut <- auxdat$auxlut
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  strvar <- auxdat$strvar
  strwtvar <- auxdat$strwtvar
  stratcombinelut <- auxdat$stratcombinelut
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid) 
  strunitvars <- c(unitvar, strvar)


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

  ## If more than one unitvar, 
  ## split the concatenated unitvar variable to keep original columns
  if (!is.null(unitvar2)) {
    condx[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
  }
 
  if (adj == "none") {
    setkeyv(condx, c(cuniqueid, condid))
    areawtnm <- areawt

  } else {
    if (adj == "samp") {
      message("calculating adjustment factors...")
    }      
	
    if (popType %in% c("ALL", "VOL", "CURR")) {
      adjfacdata <- getadjfactorVOL(adj=adj, 
                        condx = condx, 
                        treex = treef, 
                        seedx = seedf, 
                        cuniqueid = cuniqueid, 
                        condid = condid,
                        unitlut = stratalut, 
                        unitvars = unitvar,
                        strvars = strvar,
                        unitarea = unitarea,
                        areavar = areavar, 
                        areawt = areawt
                        )
      condx <- adjfacdata$condx
      treef <- adjfacdata$treex
      seedf <- adjfacdata$seedx
      areaadj <- adjfacdata$areaadj
      varadjlst <- adjfacdata$varadjlst
      areawtnm <- adjfacdata$areawtnm
      stratalut <- adjfacdata$unitlut
      expcondtab <- adjfacdata$expcondtab
    }

    if (popType %in% c("CHNG", "GRM")) {
      adjfacdata <- getadjfactorVOL(adj=adj, 
                        condx = condx, 
                        #treex = treef, 
                        #seedx = seedf, 
                        cuniqueid = cuniqueid, 
                        condid = condid,
                        unitlut = stratalut, 
                        unitvars = unitvar,
                        strvars = strvar,
                        unitarea = unitarea,
                        areavar = areavar, 
                        areawt = areawt
                        )
      condx <- adjfacdata$condx
      areaadj <- adjfacdata$areaadj
      varadjlst <- adjfacdata$varadjlst
      areawtnm <- adjfacdata$areawtnm
      stratalut <- adjfacdata$unitlut
      expcondtab <- adjfacdata$expcondtab

      strunitvars <- key(stratalut)
      if (is.null(key(sccmx))) setkeyv(sccmx, c(cuniqueid, condid))
      sccmx <- sccmx[pltassgnx[,c(pltassgnid, strunitvars), with=FALSE]]
      setkeyv(sccmx, strunitvars)
      sccmx <- sccmx[stratalut[,c(strunitvars, areaadj), with=FALSE]]
      sccmx$SUBPTYP_PROP_ADJ <- sccmx$SUBPTYP_PROP_CHNG * sccmx[[areaadj]]
      areawtnm <- "SUBPTYP_PROP_ADJ"
    }

    if (popType == "DWM") {
      adjfacdata <- getadjfactorDWM(adj=adj, 
                        condx = condx, 
                        cuniqueid = cuniqueid, 
                        condid = condid,
                        unitlut = stratalut, 
                        unitvars = unitvar,
                        strvars = strvar,
                        unitarea = unitarea,
                        areavar = areavar, 
                        areawt = areawt,
                        dwmpropvars = dwmpropvars
                        )
      condx <- adjfacdata$condx
      varadjlst <- adjfacdata$varadjlst
      areawtnm <- adjfacdata$areawtnm
      stratalut <- adjfacdata$unitlut
      expcondtab <- adjfacdata$expcondtab
    }
  

    if (popType == "P2VEG") {
      if (is.null(key(vcondx))) setkeyv(vcondx, c(cuniqueid, condid))
      vcondx <- vcondx[pltassgnx[,c(pltassgnid, strunitvars), with=FALSE]]

      ## If more than one unitvar, 
      ## split the concatenated unitvar variable to keep original columns
      if (!is.null(unitvar2)) {
        vcondx[, (unitvars) := tstrsplit(get(unitvar), "-", fixed=TRUE)]
      }
      adjfacdata <- getadjfactorVOL(adj=adj, 
                        condx = condx, 
                        cuniqueid = cuniqueid, 
                        condid = condid,
                        unitlut = stratalut, 
                        unitvars = unitvar,
                        strvars = strvar,
                        unitarea = unitarea,
                        areavar = areavar, 
                        areawt = areawt
                        )
      condx <- adjfacdata$condx
      stratalut1 <- adjfacdata$unitlut
      areawtnm <- adjfacdata$areawtnm
      varadjlst1 <- adjfacdata$varadjlst

      adjfacdataP2VEG <- getadjfactorP2VEG(adj=adj, 
                        condx = vcondx, 
                        cuniqueid = cuniqueid, 
                        condid = condid,
                        vcondsppx = vcondsppf,
                        vcondstrx = vcondstrf, 
                        vuniqueid = vuniqueid, 
                        unitlut = stratalut, 
                        unitvars = unitvar,
                        strvars = strvar,
                        unitarea = unitarea,
                        areavar = areavar, 
                        areawt = vareawt
                        )
      stratalut2 <- adjfacdataP2VEG$unitlut
      vcondsppf <- adjfacdataP2VEG$vcondsppx
      vcondstrf <- adjfacdataP2VEG$vcondstrx
      varadjP2VEG <- adjfacdataP2VEG$varadjlst

      stratalut <- merge(stratalut1, stratalut2[, c(key(stratalut2), varadjP2VEG), with=FALSE])
    }
  }

  ###################################################################################
  ## Return population data objects
  ###################################################################################

  if (!is.null(areawt2)) {
    if (!is.numeric(condx[[areawt2]])) {
      stop("areawt2 is invalid")
    }
    condx$areawt <- condx[[areawtnm]] * condx[[areawt2]]
    areawtnm <- "areawt"
  } 

  if (is.null(key(unitarea))) {
    setkeyv(unitarea, unitvars)
  }
  setorderv(stratalut, c(unitvars, strvar))

  if (returndata) {
    returnlst$popType <- popType
    if (!is.null(bndx)) {
      returnlst$bndx <- bndx
    }
 
    returnlst <- append(returnlst, list(condx=condx, pltcondx=pltcondx, 
            cuniqueid=cuniqueid, condid=condid, ACI.filter=ACI.filter, 
            unitarea=unitarea, areavar=areavar, 
            areaunits=areaunits, unitvar=unitvar, unitvars=unitvars, 
            strata=strata, stratalut=data.table(stratalut), 
            strvar=strvar, strwtvar=strwtvar, expcondtab=expcondtab, 
            plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
            states=states, invyrs=invyrs, estvar.area=areawtnm, 
            adj=adj, P2POINTCNT=P2POINTCNT))

    if (popType == "VOL") {
      if (!is.null(treef)) {
        returnlst$treex <- treef
        returnlst$tuniqueid <- tuniqueid
        returnlst$adjtree <- adjtree
      }
      if (!is.null(seedf)) {
        returnlst$seedx <- seedf
      }
    }

    if (strata) {
      if (!is.null(stratcombinelut)) {
        returnlst$stratcombinelut <- stratcombinelut
      }
    }
    if (!is.null(evalid)) {
      returnlst$evalid <- evalid
    }
    if (popType == "P2VEG") {
      returnlst$vcondsppx <- vcondsppf
      returnlst$vcondstrx <- vcondstrf
      returnlst$varadjP2VEG <- varadjP2VEG
    }
    if (popType %in% c("CHNG")) {
      returnlst$sccmx <- sccmx
    }
    if (popType %in% c("GRM")) {
      returnlst$treex <- popcheck$treef
      returnlst$grmx <- popcheck$grmf
      returnlst$beginx <- popcheck$beginf
      returnlst$midptx <- popcheck$midptf
    }
  }


  ## Save data frames
  ##################################################################
  if (savedata) {
#    if (out_fmt == "sqlite") {
#      returnlst$pop_fmt <- "sqlite"
#      returnlst$pop_dsn <- file.path(outfolder, out_dsn)
#    }
    message("saving condx...")
    datExportData(condx, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "condx",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
 
    message("saving pltcondx...")
    datExportData(pltcondx, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "pltcondx",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
    rm(condx)
    rm(pltcondx)
    # gc()

    if (popType %in% c("CHNG") && !is.null(sccmx)) {
      message("saving sccmx...")
      datExportData(sccmx, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "sccmx",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
      rm(sccmx)
      # gc()
    }

    if (!is.null(treef)) {
      message("saving treex...")
      datExportData(treef, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "treex",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
      rm(treef)
      # gc()
    }
    if (!is.null(seedf)) {
      message("saving seedx...")
      datExportData(seedf, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer ="seedx",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
      rm(seedf)
      # gc()
    }
    if (!is.null(vcondsppf)) {
      message("saving vcondsppx...")
      datExportData(vcondsppf, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "vcondsppx",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
      rm(vcondsppf)
      # gc()
    }
    if (!is.null(vcondstrf)) {
      message("saving vcondstrx...")
      datExportData(vcondstrf, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "vcondstrx",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
      rm(vcondstrf)
      # gc()
    }
    if (popType == "GRM" && !is.null(grmf) && !poponly) {
      message("saving grmx...")
      if (!is.null(popcheck$grmf)) {
        datExportData(popcheck$grmf, 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
		                   out_dsn = out_dsn, 
		                   out_layer = "grmx",
		                   outfn.pre = outfn.pre, 
		                   outfn.date = outfn.date, 
		                   overwrite_layer = overwrite_layer,
		                   append_layer = append_layer,
		                   add_layer = TRUE))
        rm(grmf)
        # gc()
      }
      if (!is.null(popcheck$beginf)) {
        message("saving beginx...")
        datExportData(popcheck$beginf, 
            savedata_opts=list(outfolder = outfolder, 
                              out_fmt = out_fmt, 
		                  out_dsn = out_dsn, 
		                  out_layer = "beginx",
		                  outfn.pre = outfn.pre, 
		                  outfn.date = outfn.date, 
		                  overwrite_layer = overwrite_layer,
		                  append_layer = append_layer,
		                  add_layer = TRUE))
        rm(beginf)
        # gc()
      }
      if (!is.null(popcheck$midptf)) {
        message("saving midptx...")
        datExportData(popcheck$midptf, 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
		                   out_dsn = out_dsn, 
		                   out_layer = "midptx",
		                   outfn.pre = outfn.pre, 
		                   outfn.date = outfn.date, 
		                   overwrite_layer = overwrite_layer,
		                   append_layer = append_layer,
		                   add_layer = TRUE))
        rm(midptf)
        # gc()
      }
    }

    message("saving pltassgnx...")
    datExportData(pltassgnx, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "pltassgn",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
    message("saving unitarea...")
    datExportData(unitarea, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "unitarea",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
    message("saving stratalut...")
    datExportData(stratalut, 
          savedata_opts=list(outfolder = outfolder, 
                             out_fmt = out_fmt, 
		                 out_dsn = out_dsn, 
		                 out_layer = "stratalut",
		                 outfn.pre = outfn.pre, 
		                 outfn.date = outfn.date, 
		                 overwrite_layer = overwrite_layer,
		                 append_layer = append_layer,
		                 add_layer = TRUE))
    rm(pltassgnx)
    rm(unitarea)
    rm(stratalut)
    # gc()
  }


  ## Save list object
  ##################################################################
  if (saveobj) {
    if (getext(objfn) == "rds") {
      message("saving list object to: ", objfn)
      saveRDS(returnlst, objfn)
    } else if (getext(objfn) == "rda") {
      message("saving list object to: ", objfn)
      save(returnlst, objfn)
    } else {
      message("invalid object name... must end in: ", toString(c("rds", "rda")))
    } 
  } 

  rm(popcheck)
  # gc()

  if (!is.null(dbconn)) {
    DBI::dbDisconnect(dbconn)
  }

  if (returndata) {
    return(returnlst)
  } else {
    return(invisible())
  }
}
