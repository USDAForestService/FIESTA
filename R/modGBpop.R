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
#' \bold{Description}\cr \tab ALL \tab Population data, inluding nonsampled
#' plots.\cr \tab CURR \tab Population data for area estimates, excluding
#' nonsampled plots.\cr \tab VOL \tab Population data for area/tree estimates,
#' excluding nonsampled plots.\cr \tab LULC \tab Population data for land
#' use/land cover transitional estimates, including only plots with previous
#' measurements and excluding nonsampled plots.\cr }
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr \tab tree \tab tuniqueid \tab
#' Unique identifier for each plot in tree table.\cr \tab \tab CONDID \tab
#' Unique identifier of each condition on plot.  Set CONDID=1, if only 1
#' condition per plot.\cr \tab \tab TPA_UNADJ \tab Number of trees per acre
#' each sample tree represents (e.g., DESIGNCD=1: TPA_UNADJ=6.018046 for trees
#' on subplot; 74.965282 for trees on microplot).\cr \tab cond \tab cuniqueid
#' \tab Unique identifier for each plot in cond table.\cr \tab \tab CONDID \tab
#' Unique identfier of each condition on plot.  Set CONDID=1, if only 1
#' condition per plot.\cr \tab \tab CONDPROP_UNADJ \tab Unadjusted proportion
#' of condition on each plot.  Set CONDPROP_UNADJ=1, if only 1 condition per
#' plot.\cr \tab \tab COND_STATUS_CD \tab Status of each forested condition on
#' plot (i.e. accessible forest, nonforest, water, etc.)\cr \tab \tab
#' NF_COND_STATUS_CD \tab If ACI=TRUE. Status of each nonforest condition on
#' plot (i.e. accessible nonforest, nonsampled nonforest)\cr \tab \tab SITECLCD
#' \tab If landarea=TIMBERLAND. Measure of site productivity.\cr \tab \tab
#' RESERVCD \tab If landarea=TIMBERLAND. Reserved status.\cr \tab \tab
#' SUBPROP_UNADJ \tab Unadjusted proportion of subplot conditions on each plot.
#' Set SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr \tab \tab
#' MICRPROP_UNADJ \tab If microplot tree attributes. Unadjusted proportion of
#' microplot conditions on each plot. Set MICRPROP_UNADJ=1, if only 1 condition
#' per microplot.\cr \tab \tab MACRPROP_UNADJ \tab If macroplot tree
#' attributes. Unadjusted proportion of macroplot conditions on each plot. Set
#' MACRPROP_UNADJ=1, if only 1 condition per macroplot.\cr \tab pltassgn \tab
#' pltassgnid \tab Unique identifier for each plot in pltassgn.\cr \tab \tab
#' STATECD \tab Identifies state each plot is located in.\cr \tab \tab INVYR
#' \tab Identifies inventory year of each plot.\cr \tab \tab PLOT_STATUS_CD
#' \tab Status of each plot (i.e. sampled, nonsampled).  If not included, all
#' plots are assumed as sampled.\cr }
#' 
#' For available reference tables: sort(unique(FIESTA::ref_codes$VARIABLE)) \cr
#' 
#' @param popType String. Type of evaluation(s) to include in population data.
#' Note: currently only c('CURR', 'VOL', 'LULC') are available. See details
#' below for descriptions of each.
#' @param cond DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Condition-level data with one record for each condition, including or
#' excluding nonsampled conditions. Plot variables and strata/estimation unit
#' variable(s) may be included if plt and pltassgn=NULL. See details for
#' necessary variables to include.
#' @param plt DF/DT, Optional. R object, sf R object, comma-delimited
#' file(*.csv), layer or spatial layer in dsn, or shapefile(*.shp).  Plot-level
#' data with one record for each plot, including or excluding nonsampled
#' conditions. If nonsampled plots are included, PLOT_STATUS_CD variable must
#' be in table or a filter defined in plt.nonsamp.filter.
#' @param tree DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Tree-level data with one record for each tree. Tree data are aggregated to
#' condition-level. See details for necessary variables to include.
#' @param seed DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Seedling data with one record for each seedling count.
#' @param vsubpspp DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Vegetation species-level data with one record for each species
#' (P2VEG_SUBPLOT_SPP).
#' @param vsubpstr DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Vegetation species-structure data with one record for each species
#' (P2VEG_SUBP_STRUCTURE).
#' @param subplot DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Subplot-level data with one record for each species (SUBPLOT).
#' @param subp_cond DF/DT, R object, comma-delimited file(*.csv), or layer in
#' dsn.  Subplot condition-level data with one record for each species
#' (SUBP_COND).
#' @param lulc DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Land use/Land cover data with current and previous observations.
#' @param pltassgn DF/DT, Optional. R object, sf R object, comma-delimited
#' file(*.csv), layer or spatial layer in dsn, or shapefile(*.shp). Plot-level
#' assignment of estimation unit and/or strata, with one record for each plot.
#' @param dsn String. Name of database where tree, cond, and plot-level tables
#' reside.  The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html).
#' @param puniqueid String. Unique identifier of plot in plt.
#' @param pltassgnid String. Unique identifier of plot in pltassgn.
#' @param pjoinid String. Join variable in plot to match pltassgnid. Does not
#' need to be uniqueid. If using most current XY coordinates for plot
#' assignments, use identifier for plot (e.g., PLOT_ID).
#' @param tuniqueid String. Unique identifier of plot in tree and seed.
#' @param cuniqueid String. Unique identifier of plot in cond.
#' @param condid String. Unique identifier of plot conditions (e.g., CONDID).
#' If no condid in cond, the data are assumed to have 1 condition per plot.  A
#' CONDID=1 is automatically added.
#' @param areawt String. Name of variable for summarizing area weights (e.g.,
#' CONDPROP_UNADJ).
#' @param evalid Numeric. FIA Evaluation identifier for subsetting plots for
#' population.
#' @param invyrs Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)).
#' @param intensity Integer code. Code(s) indicating intensity to use for
#' population.
#' @param ACI Logical. If TRUE, including All Condition Inventory (ACI) plots.
#' Removes nonsampled nonforest lands (NF_COND_STATUS_CD = 5). Tree data must
#' be included.
#' @param adj String. How to calculate adjustment factors for nonsampled
#' (nonresponse) conditions based on summed proportions for by plot ('samp',
#' 'plot').  'samp' - adjustments are calculated at strata/estimation unit
#' level; 'plot' - adjustments are calculated at plot-level. Adjustments are
#' only calculated for annual inventory plots (DESIGNCD=1).
#' @param unitvar String. Name of the estimation unit variable in unitarea and
#' cond or pltassgn data frame with estimation unit assignment for each plot
#' (e.g., 'ESTN_UNIT'). Optional if only one estimation unit.
#' @param unitvar2 String. Name of a second level estimation unit variable in
#' unitarea and cond or pltassgn with assignment for each plot (e.g.,
#' 'STATECD').
#' @param unitarea Numeric or DF. Total area by estimation unit. If only 1
#' estimation unit, include number of total acreage for the area of interest or
#' a data frame with area and estimation unit. If more than one estimation
#' unit, provide a data frame of total area by estimation unit, including
#' unitvar and areavar.
#' @param areavar String. Name of area variable in unitarea. Default="ACRES".
#' @param areaunits String. Units of areavar in unitarea ('acres', 'hectares').
#' @param minplotnum.unit Integer. Minimum number of plots for estimation unit.
#' @param unit.action String. What to do if number of plots in an estimation
#' unit is less than minplotnum.unit ('keep', 'remove' 'combine'). If
#' unit.action='combine', combines estimation unit to the following estimation
#' unit in unitlut.
#' @param strata Logical. If TRUE, include information for post-stratification.
#' @param stratalut DF/DT. If strata=TRUE, look-up table with pixel counts or
#' area by strata or proportion or area ('strwt') by strata (and estimation
#' unit).  If 'strwt' is not included, set getwt=TRUE and getwtvar as the name
#' of variable to calculate weights from (e.g., pixel counts).
#' @param strvar String. If strata=TRUE, name of the strata variable in
#' stratalut and cond or pltassgn data frame with stratum assignment for each
#' plot (Default = 'STRATUMCD').
#' @param getwt Logical. If TRUE, calculates strata weights from stratatlut
#' getwtvar.  If FALSE, strwtvar variable must be in stratalut.
#' @param getwtvar String. If getwt=TRUE, name of variable in stratalut to
#' calculate weights (Default = 'P1POINTCNT').
#' @param strwtvar String. If getwt=FALSE, name of variable in stratalut with
#' calculated weights (Default = 'strwt').
#' @param stratcombine Logical. If TRUE, and strata=TRUE, automatically combines
#' strata categories if less than minplotnum.strat plots in any one stratum. 
#' See notes for more info.
#' @param minplotnum.strat Integer. Minimum number of plots for a stratum
#' within an estimation unit.
#' @param saveobj Logical. If TRUE, saves GBpopdat object to outfolder.
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
#' @param GBdata R List object. Output data list components from
#' FIESTA::anGBdata().
#' @param pltdat R List object. Output data list components from
#' FIESTA::spGetPlots().
#' @param GBstratdat R List object. Output data list components from
#' FIESTA::DBgetStrata().
#' @param nonsamp.vfilter.fixed Logical. If TRUE and popType="P2VEG", the
#' nonsample filter is fixed in database.
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
#' @export modGBpop
modGBpop <- function(popType="VOL", cond=NULL, plt=NULL, tree=NULL, seed=NULL, 
	vsubpspp=NULL, vsubpstr=NULL, subplot=NULL, subp_cond=NULL, lulc=NULL, 
	pltassgn=NULL, dsn=NULL, puniqueid="CN", pltassgnid="PLT_CN", pjoinid="CN", 
	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", areawt="CONDPROP_UNADJ", 
	adj="samp", evalid=NULL, invyrs=NULL, intensity=NULL, ACI=FALSE, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", areaunits="acres",
	minplotnum.unit=10, unit.action="keep", strata=TRUE, stratalut=NULL, 
	strvar="STRATUMCD", getwt=TRUE, getwtvar="P1POINTCNT", strwtvar="strwt",
	stratcombine=TRUE, minplotnum.strat=2, saveobj=FALSE, objnm="GBpopdat", 
	savedata=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, outfn.pre=NULL,
 	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE,
	GBdata=NULL, pltdat=NULL, GBstratdat=NULL, nonsamp.vfilter.fixed=FALSE, gui=FALSE){

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

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modGBpop)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=expcondtab=V1=SUBPCOND_PROP=SUBPCOND_PROP_UNADJ=
	treef=seedf=vcondsppf=vcondstrf=bndx <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 
  adjtree <- FALSE
  nonresp=FALSE
  substrvar=nonsamp.pfilter=nonsamp.cfilter <- NULL
  returnlst <- list()

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


  ###################################################################################
  ## Load data
  ###################################################################################
  if (!is.null(GBdata)) {
    list.items <- c("cond", "unitarea", "unitvar")
    GBdata <- FIESTA::pcheck.object(GBdata, "GBdata", list.items=list.items)
    #bnd <- GBdata$bnd
    plt <- GBdata$plt
    cond <- GBdata$cond
    tree <- GBdata$tree
    seed <- GBdata$seed
    pltassgn <- GBdata$pltassgn
    pltassgnid <- GBdata$pltassgnid 
    unitarea <- GBdata$unitarea
    areavar <- GBdata$areavar
    stratalut <- GBdata$stratalut
    strvar <- GBdata$strvar
    strwtvar <- GBdata$strwtvar
    puniqueid <- GBdata$puniqueid
    pjoinid <- GBdata$pjoinid

    if (is.null(unitvar)) {
      unitvar <- GBdata$unitvar
      unitvar2 <- GBdata$unitvar2
    } 

  } else {
    if (!is.null(pltdat)) {
      if ("tabs" %in% names(pltdat)) {
        list.items <- c("tabs", "xypltx")
      } else {
        list.items <- c("states", "plt", "cond")
      }
      if (popType == "LULC") {
        list.items <- c(list.items, "lulcx")
      }
      if (popType == "P2VEG") {
        list.items <- c(list.items, "vsubpspp", "vsubpstr", "subplot", "subp_cond")
      }
      pltdat <- FIESTA::pcheck.object(pltdat, "pltdat", list.items=list.items)

      ## Extract list objects
      puniqueid <- pltdat$puniqueid
      if ("tabs" %in% names(pltdat)) {
        pjoinid <- pltdat$pjoinid
        plt <- pltdat$tabs$pltx
        cond <- pltdat$tabs$condx
        tree <- pltdat$tabs$treex
        seed <- pltdat$tabs$seedx
        if (popType == "LULC") {
          lulc <- pltdat$tabs$lulcx
        } else if (popType == "P2VEG") {
          vsubpspp <- pltdat$tabs$vsubpspp
          vsubpstr <- pltdat$tabs$vsubpstr
          subplot <- pltdat$tabs$subplot
          subp_cond <- pltdat$tabs$subp_cond
        }
      } else {
        pjoinid <- puniqueid
        plt <- pltdat$plt
        cond <- pltdat$cond
        tree <- pltdat$tree
        seed <- pltdat$seed
        if (popType == "LULC") {
          lulc <- pltdat$lulc
        } else if (popType == "P2VEG") {
          vsubpspp <- pltdat$vsubpspp
          vsubpstr <- pltdat$vsubpstr
          subplot <- pltdat$subplot
          subp_cond <- pltdat$subp_cond
        }
      }
    }
    if (!is.null(GBstratdat)) {
      list.items <- c("pltassgn", "unitarea", "unitvar")
      GBstratdat <- FIESTA::pcheck.object(GBstratdat, "GBstratdat", list.items=list.items)
      bndx <- GBstratdat$bndx
      pltassgn <- GBstratdat$pltassgn
      pltassgnid <- GBstratdat$pltassgnid
      unitarea <- GBstratdat$unitarea
      areavar <- GBstratdat$areavar
      stratalut <- GBstratdat$stratalut
      strvar <- GBstratdat$strvar
      getwt <- GBstratdat$getwt
      getwtvar <- GBstratdat$getwtvar
      strwtvar <- GBstratdat$strwtvar

      if (is.null(unitvar)) {
        unitvar <- GBstratdat$unitvar
        unitvar2 <- GBstratdat$unitvar2
      } 
    }
  } 

  if (strata) {
    if (is.null(strwtvar)) {
      stop("missing strwtvar")
    }
    if (strwtvar != "strwt") {
      names(stratalut)[names(stratalut) == strwtvar] <- "strwt"
      strwtvar <- "strwt"
    }
  }
 
  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="GB", popType=popType, 
	tree=tree, cond=cond, plt=plt, seed=seed, vsubpspp=vsubpspp, vsubpstr=vsubpstr, 
	subplot=subplot, subp_cond=subp_cond, lulc=lulc, pltassgn=pltassgn, dsn=dsn, 
	tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, areawt=areawt, 
	puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid, evalid=evalid, 
	invyrs=invyrs, adj=adj, intensity=intensity, ACI=ACI, 
	nonsamp.pfilter=nonsamp.pfilter, nonsamp.cfilter=nonsamp.cfilter,
	nonsamp.vfilter.fixed=nonsamp.vfilter.fixed,
 	unitarea=unitarea, unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
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
  vuniqueid <- popcheck$vuniqueid
  pltassgnid <- popcheck$pltassgnid
  ACI.filter <- popcheck$ACI.filter
  adj <- popcheck$adj
  unitvar <- popcheck$unitvar
  unitvar2 <- popcheck$unitvar2
  unitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  areaunits <- popcheck$areaunits
  unit.action <- popcheck$unit.action
  stratcombine <- popcheck$stratcombine
  strata <- popcheck$strata
  stratalut <- popcheck$stratalut
  strvar <- popcheck$strvar
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
  if (nonresp) {
    substrvar <- popcheck$substrvar
  } 
  #rm(popcheck)
  if (popType == "P2VEG") {
    vcondsppf <- popcheck$vcondsppf
    vcondstrf <- popcheck$vcondstrf
    areawt <- "SUBP_CONDPROP_UNADJ"
  }

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
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, unitvar=unitvar, 
	unitvar2=unitvar2, unitarea=unitarea, areavar=areavar, 
	minplotnum.unit=minplotnum.unit, unit.action=unit.action,
	strata=strata, auxlut=stratalut, strvar=strvar,  
	nonresp=nonresp, substrvar=substrvar, stratcombine=stratcombine, 
	minplotnum.strat=minplotnum.strat, removeifnostrata=TRUE, getwt=getwt, 				
	getwtvar=getwtvar, strwtvar=strwtvar, P2POINTCNT=P2POINTCNT)
  pltassgnx <- setDT(auxdat$pltx)
  unitarea <- auxdat$unitarea
  stratalut <- auxdat$auxlut
  unitarea <- auxdat$unitarea
  unitvar <- auxdat$unitvar
  unitvars <- auxdat$unitvars
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
  if (adj == "samp") {
    adjfacdata <- getadjfactorGB(condx=condx, treex=treef, seedx=seedf,
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, condid=condid, 
		vcondsppx=vcondsppf, vcondstrx=vcondstrf, vuniqueid=vuniqueid, 
		unitlut=stratalut, unitvars=unitvar, strvars=strvar, 
		unitarea=unitarea, areavar=areavar, areawt=areawt, tpropvars=tpropvars)
    condx <- adjfacdata$condx
    stratalut <- adjfacdata$unitlut
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
    expcondtab <- adjfacdata$expcondtab
    vcondsppf <- adjfacdata$vcondsppx
    vcondstrf <- adjfacdata$vcondstrx
    setorderv(stratalut, c(unitvars, strvar))

  } else if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE
    adjfacdata <- FIESTA::getadjfactorPLOT(treex=treef, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
    condx <- adjfacdata$condx
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
  } else {
    setkeyv(condx, c(cuniqueid, condid))
  }

  ###################################################################################
  ## Return population data objects
  ###################################################################################
  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst$popType <- popType
  if (!is.null(bndx)) {
    returnlst$bndx <- bndx
  }
  returnlst <- append(returnlst, list(condx=condx, pltcondx=pltcondx, 
	cuniqueid=cuniqueid, condid=condid, ACI.filter=ACI.filter, 
	unitarea=unitarea, areavar=areavar, areaunits=areaunits, 
	unitvar=unitvar, unitvars=unitvars, 
	strata=strata, stratalut=stratalut, strvar=strvar, strwtvar=strwtvar, 
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

  if (strata) {
    if (!is.null(stratcombinelut)) {
      returnlst$stratcombinelut <- stratcombinelut
    }
  }
  if (!is.null(evalid)) {
    returnlst$evalid <- evalid
  }
  if ("P2VEG" %in% popType) {
    returnlst$vcondsppx <- vcondsppf
    returnlst$vcondstrx <- vcondstrf
  }


  ###################################################################################
  ## Save population data objects
  ###################################################################################
  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  } 

  if (savedata) {
    datExportData(condx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="condx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(pltcondx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltcondx", 
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
    if (!is.null(vcondstrf)) {
      datExportData(vcondstrf, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vcondstrx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    }
    if (!is.null(vcondsppf)) {
      datExportData(vcondsppf, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="vcondsppx", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    }

    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(unitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(stratalut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="stratalut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
  }

  return(returnlst)
}
