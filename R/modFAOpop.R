#' FAO module - Generate population data for FAO module.
#' 
#' Generates population data for generating FAO estimates.
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
#' identfier of each condition on plot.  Set CONDID=1, if only 1 condition per
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
#' For available reference tables: sort(unique(FIESTA::ref_codes$VARIABLE)) \cr
#' 
#' @param FAOdata R List object. Output data list components from FAOdata.
#' @param base DF/DT, comma-separated values (CSV) file (*.csv), or layer in
#' dsn.  The base-level data (e.g., COND, SUBP). Cluster variables and
#' strata/estimation unit variable(s) may be included if cluster and
#' clustassgn=NULL. See details for necessary variables to include.
#' @param cluster DF/DT, comma-separated values (CSV) file(*.csv), or layer in
#' dsn, Can also be a shapefile(*.shp) with one record per cluster, a spatial
#' layer in dsn, or a sf R object. Cluster-level data. Optional.
#' @param tree DF/DT, comma-delimited file(*.csv), or layer in dsn. If
#' esttype="TREE", tree-level variables to aggregate to condition-level. See
#' details for necessary variables to include.
#' @param seed DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Seedling data with one record for each seedling count.
#' @param clustassgn DF/DT, comma-separated values (CSV) file(*.csv), or layer
#' in dsn, Can also be a shapefile(*.shp) with one record per cluster, a
#' spatial layer in dsn, or a sf R object. Cluster-level assignment of
#' estimation unit and/or strata. Optional.
#' @param dsn String. Name of database where tree, cond, and pltassgn tables
#' reside.  The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html).
#' @param clustid String. Unique identifier of cluster.
#' @param clustassgnid String. Unique identifier of cluster.
#' @param clustjoinid String. Join variable in cluster to match clustassgnid.
#' Does not need to be uniqueid. For example, if using most current XY
#' coordinates for plot assignments.
#' @param buniqueid String. Unique identifier of cluster.
#' @param baseid String. Unique identifier of base unit records (e.g., CONDID,
#' SUBPID).
#' @param tuniqueid String. Unique identifier of cluster.
#' @param basewt String. The variable in base to calculate area weights (e.g,
#' CONDPROP_UNADJ).  If NULL, the basewt is assumed to be equal to 1.
#' @param invyrs Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)).
#' @param adj String. How to calculate adjustment factors for nonsampled
#' (nonresponse) conditions based on summed proportions for by plot ('none',
#' 'samp', 'plot').  'samp' - adjustments are calculated at strata/estimation
#' unit level; 'plot' - adjustments are calculated at plot-level. Adjustments
#' are only calculated for annual inventory plots (designcd=1).
#' @param diavar String. Name of variable with tree diameters (e.g., DIA). Used
#' for to define different plot sizes (SUPB, MICR, MACR).
#' @param MICRO_BREAKPOINT_DIA Number. If a different plot size for measuring
#' seedlings/saplings, identify diameter breakpoint to include trees less than
#' (e.g., 5).
#' @param MACRO_BREAKPOINT_DIA Number. If a different plot size for measuring
#' large trees, identify diameter breakpoint to include trees greater than or
#' equal to.
#' @param areawt_micr String. Name of variable to identify area for microplot
#' (i.e., seedling/saplings).  If areawt_micr is NULL or not in dataset, areawt
#' is used.
#' @param areawt_subp String. Name of variable to identify area for subplot.
#' If areawt_subp is NULL or not in dataset, areawt is used.
#' @param areawt_macr String. Name of variable to identify area for macroplot.
#' If areawt_macr is NULL or not in dataset, areawt is used.
#' @param strata Logical. If TRUE, add data information for stratification.
#' @param nonsamp.clustfilter String. A logical expression for filtering
#' nonsampled clusters.  Must be R syntax (e.g., 'PLOT_STATUS_CD != 3'.
#' @param nonsamp.basefilter String. A logical expression for filtering
#' nonsampled conditions.  Must be R syntax (e.g., 'COND_STATUS_CD != 5'.
#' @param unitlevel1 String. Name of the estimation unit variable in base or
#' clustassgn with estimation unit assignment for each plot (e.g.,
#' 'ESTN_UNIT').  If one estimation unit, set unitvar=NULL.
#' @param unitlevel2 String. Name of a second estimation unit variable in base
#' or clustassgn with assignment for each plot (e.g., 'STATECD').
#' @param unitarea Numeric or DF. Total area by estimation unit. If only 1
#' estimation unit, include number of total acreage for the area of interest or
#' a data frame with areavar. If more than one estimation unit, provide a data
#' frame of total area by estimation unit, including unitvar and areavar.
#' @param areavar String. Name of acre variable in unitarea. Default="ACRES".
#' @param areaunits String. Units of areavar in unitarea ('acres', 'hectares').
#' @param minplotnum.unit Integer. Minimum number of plots for estimation unit.
#' @param unit.action String. What to do if number of plots in an estimation
#' unit is less than minplotnum.unit ('keep', 'remove' 'combine'). If
#' unit.action='combine', combines estimation unit to the following estimation
#' unit in unitlut.
#' @param stratalut DF/DT. If strata=TRUE, look-up table with strata
#' proportions ('strwt') by strata (and estimation unit). To calculate 'strwt',
#' set getwt=TRUE and getwtvar= name of variable with information to calculate
#' weights from (e.g., pixel counts)
#' @param strvar String. If strata=TRUE, name of the strata variable in
#' stratalut and cond or pltassgn data frame with stratum assignment for each
#' plot (Default = 'STRATUMCD').
#' @param getwt Logical. If TRUE, calculates strata weights from stratatlut
#' getwtvar.  If FALSE, strwtvar variable must be in stratalut.
#' @param getwtvar String. If getwt=TRUE, name of variable in stratalut to
#' calculate weights (Default = 'P1POINTCNT').
#' @param strwtvar String. If getwt=FALSE, name of variable in stratalut with
#' calculated weights (Default = 'strwt').
#' @param stratcombine Logical. If TRUE, automatically combines estimation
#' units if less than 2 plots in any one estimation unit. See notes for more
#' info.
#' @param minplotnum.strat Integer. Minimum number of plots for a stratum
#' within an estimation unit.
#' @param saveobj Logical. If TRUE, saves SApopdat object to outfolder.
#' @param objnm String. Name of *.rda object.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param outfolder String. The outfolder to write files to. If NULL, files are
#' written to working directory, or if gui, a window to browse.
#' @param out_fmt String. Format for output tables ('csv', 'sqlite', 'gpkg').
#' @param out_dsn String. Name of database if out_fmt = c('sqlite', 'gpkg').
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param overwrite_dsn Logical. If TRUE, overwrites the out_dsn, if exists.
#' @param overwrite_layer Logical. If TRUE, overwrites the out_layer, if
#' exists.
#' @param append_layer Logical. If TRUE, appends layers to existing out_dsn or
#' files if out_fmt = 'csv'. Note: currently cannot append layers if out_fmt =
#' "gdb".
#' @param gui Logical. If gui, user is prompted for parameters.
#' @return A list with population data for Green-Book estimates.
#' 
#' \item{condx}{ Data frame. Condition-level data including plot-level
#' assignment of estimation unit and stratum (if strata=TRUE), condition
#' proportion adjustment factor (cadjfac), and adjusted condition proportions
#' (CONDPROP_ADJ). } \item{cuniqueid}{ String. Unique identifier of plot in
#' condx and pltcondx. } \item{condid}{ String. Unique identifier of condition
#' in condx and pltcondx. } \item{treex}{ Data frame. Tree data within
#' population, used for estimation, including trees per acre adjustment factor
#' (tadjfac), and adjusted trees per acre (TPA_ADJ) (if treef is included). }
#' \item{tuniqueid}{ String. Unique identifier of plot in treex (if treef is
#' included). } \item{ACI.filter}{ String. If ACI=FALSE,
#' ACI.filter="COND_STATUS_CD == 1" . } \item{unitarea}{ String. Returned table
#' of area by estimation unit. } \item{unitvar}{ String. Variable name for
#' estimation unit. } \item{strlut}{ String. Strata-level table with pixel
#' counts by strata (P1POINTCNT), strata weights (strwt), number of plots by
#' strata (n.strata), total number of plots in estimation unit (n.total), sum
#' of condition proportions (*_UNADJ_SUM), area adjustments (*_ADJFAC), total
#' area, and area expansion by strata (EXPNS). } \item{strvar}{ String.
#' Variable name for strata. If strata=FALSE, strvar="ONESTRAT". }
#' \item{expcondtab}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" .
#' } \item{plotsampcnt}{ Data frame. Number of plots by PLOT_STATUS_CD. }
#' \item{condsampcnt}{ Data frame. Number of conditions by COND_STATUS_CD. }
#' \item{states}{ String. State names in dataset. } \item{invyrs}{ String.
#' Range of inventory years in dataset. }
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
#' within an esimation unit, all strata classes with 2 or less plots are
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
#' @export modFAOpop
modFAOpop <- function(FAOdata=NULL, base=NULL, cluster=NULL, tree=NULL, 
	seed=NULL, clustassgn=NULL, dsn=NULL, clustid="CN", clustassgnid="PLT_CN",
	clustjoinid="CN", buniqueid="PLT_CN", baseid="CONDID", tuniqueid=NULL,
 	basewt="CONDPROP_UNADJ", invyrs=NULL, adj="none", diavar="DIA",
	MICRO_BREAKPOINT_DIA=5, MACRO_BREAKPOINT_DIA=NULL, areawt_micr="MICRPROP_UNADJ", 
	areawt_subp="SUBPPROP_UNADJ", areawt_macr="MACRPROP_UNADJ",
	strata=TRUE, nonsamp.clustfilter=NULL, nonsamp.basefilter=NULL, 
	unitlevel1=NULL, unitlevel2=NULL, unitarea=NULL, areavar="ACRES", 
	areaunits="acres", minplotnum.unit=10, unit.action="keep", stratalut=NULL, 
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
	areaunits=areaunits, unit.action=unit.action, strata=strata, 
	stratalut=stratalut, strvar=strvar, stratcombine=stratcombine)
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
  unit.action <- popcheck$unit.action
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
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, 
		strata=strata, unitvar=unitvar, unitvar2=unitvar2,
		unitarea=unitarea, areavar=areavar, minplotnum.unit=minplotnum.unit,
		unit.action=unit.action, auxlut=stratalut, strvar=strvar, 
		nonresp=nonresp, substrvar=substrvar, stratcombine=stratcombine, 
		minplotnum.strat=minplotnum.strat, getwt=getwt, getwtvar=getwtvar, 
		strwtvar=strwtvar, P2POINTCNT=P2POINTCNT)  
  pltassgnx <- auxdat$pltx
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
  stratalut <- auxdat$auxlut
  strvar <- auxdat$strvar
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
