#' Small-Area module - Compile population data for SA module.
#' 
#' Compile population data for input to the modSA* modules.
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
#' @param SAdoms sf object. SA domains with attributes for joining.
#' @param smallbnd sf object. small bound.
#' @param smallbnd.domain String. Name of attribute defining domain attribute.
#' @param cond DF/DT, comma-separated values (CSV) file (*.csv), or layer in
#' dsn.  The condition-level variables with one record per condition, including
#' or excluding nonsampled conditions. Plot variables and strata/estimation
#' unit variable(s) may be included if plt and pltassgn=NULL. See details for
#' necessary variables to include.
#' @param plt DF/DT, comma-separated values (CSV) file(*.csv), or layer in dsn,
#' Can also be a shapefile(*.shp) with one record per plot, a spatial layer in
#' dsn, or a sf R object. Plot-level variables. If nonsampled plots are
#' included, PLOT_STATUS_CD variable must be in table. Optional.
#' @param tree DF/DT, comma-delimited file(*.csv), or layer in dsn. If
#' esttype="TREE", tree-level variables to aggregate to condition-level. See
#' details for necessary variables to include.
#' @param seed DF/DT, R object, comma-delimited file(*.csv), or layer in dsn.
#' Seedling data with one record for each seedling count.
#' @param pltassgn DF/DT, comma-separated values (CSV) file(*.csv), or layer in
#' dsn, Can also be a shapefile(*.shp) with one record per plot, a spatial
#' layer in dsn, or a sf R object. Plot-level assignment of estimation unit
#' and/or strata. Optional.
#' @param dsn String. Name of database where tree, cond, and pltassgn tables
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
#' @param invyrs Integer vector. Inventory year(s) (e.g., c(2000, 2001, 2002)).
#' @param intensity Integer code. Code(s) indicating intensity to use for
#' population.
#' @param measCur Logical. If TRUE, extract plots with most current measurement
#' for state(s).
#' @param measEndyr Logical. If TRUE, extract plots with most current
#' measurement for state(s) for years measured before measEndyr.
#' @param measEndyr.filter Filter for extracting plots using measEndyr. Must be
#' in R syntax (e.g., 'AOI == 1').
#' @param ACI Logical. If TRUE, including All Condition Inventory (ACI) plots.
#' Removes nonsampled nonforest lands (NF_COND_STATUS_CD = 5). Tree data must
#' be included.
#' @param adj String. How to calculate adjustment factors for nonsampled
#' (nonresponse) conditions based on summed proportions for by plot ('samp',
#' 'plot').  'samp' - adjustments are calculated at strata/estimation unit
#' (i.e., domain unit) level; 'plot' - adjustments are calculated at
#' plot-level. Adjustments are only calculated for annual inventory plots
#' (designcd=1).
#' @param dunitvar String. Name of the domain unit variable in cond, plt, or
#' pltassgn with domain unit assignment for each plot.
#' @param dunitvar2 String. Name of a second domain unit variable in cond, plt,
#' or pltassgn with assignment for each plot (e.g., 'STATECD').
#' @param dunitarea Numeric or DF. Total area by domain unit.
#' @param areavar String. Name of acre variable in unitarea. Default="ACRES".
#' @param areaunits String. Units of areavar in unitarea ('acres', 'hectares').
#' @param minplotnum.unit Integer. Minimum number of plots for estimation unit.
#' @param unit.action String. What to do if number of plots in an estimation
#' unit is less than minplotnum.unit ('keep', 'remove' 'combine'). If
#' unit.action='combine', combines estimation unit to the following estimation
#' unit in unitlut.
#' @param dunitzonal DF/DT. Data frame with zonal auxiliary information by
#' domain unit. For continuous data, means by domain unit; for categorical
#' data, proportion of class by domain unit.
#' @param prednames String vector. Name(s) of predictor variables to use in
#' model.
#' @param predfac String vector. Name(s) of factor predictor variables to use
#' in model.
#' @param pvars2keep String vector. Additional plot variables to keep in
#' dataset.
#' @param cvars2keep String vector. Additional condition variables to keep in
#' dataset.
#' @param saveobj Logical. If TRUE, save SApopdat object to outfolder.
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
#' @param SAdata R List object. Output data list components from
#' FIESTA::SAdata().
#' @param pltdat R List object. Output data list components from
#' FIESTA::spGetPlots().
#' @param auxdat R List object. Output data list components from
#' FIESTA::spGetAuxiliary().
#' @param gui Logical. If gui, user is prompted for parameters.
#' @return A list with population data for Small-Area estimates.
#' 
#' \item{SAdomsdf}{ Data frame. Attribute table from SAdoms spatial layer.
#' Includes DOMAIN and AOI attributes. DOMAIN represents modeling domains.  AOI
#' identifies the small area of interest. } \item{condx}{ Data frame.
#' Condition-level data with condition proportions, domain and predictor
#' assignments, and adjusted condition proportions, if adj in('samp', 'plot').
#' } \item{pltcondx}{ Data frame. Plot/Condition data used for estimation. }
#' \item{cuniqueid}{ String. Unique identifier of plot in condx and pltcondx. }
#' \item{condid}{ String. Unique identifier of condition in condx and pltcondx.
#' } \item{treex}{ Data frame. If esttype='TREE', tree-level data, including
#' adjustment factors, if adj in('samp', 'plot'). } \item{tuniqueid}{ String.
#' If esttype='TREE', unique identifier of plot in treex. } \item{ACI.filter}{
#' String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . } \item{dunitarea}{
#' Data frame. Area by model domain unit. } \item{areavar}{ String. Name of
#' area variable in dunitarea. } \item{dunitvar}{ String. Name of variable
#' defining model domain units in dunitarea. } \item{dunitlut}{ Data frame.
#' Table of model domain units with zonal statistics of predictor values,
#' number of plots by domain unit. } \item{prednames}{ String vector. Name of
#' variables in dunitlut and condx defining potential predictors for small area
#' estimation. } \item{plotsampcnt}{ Data frame. Number of plots by
#' PLOT_STATUS_CD. } \item{condsampcnt}{ Data frame. Number of conditions by
#' COND_STATUS_CD. } \item{states}{ String. State names in dataset. }
#' \item{invyrs}{ String. Range of inventory years in dataset. }
#' \item{adjtree}{ Logical. If TRUE, treex includes adjustment factors. }
#' @note
#' 
#' ADJUSTMENT FACTOR:\cr The adjustment factor is necessary to account for
#' nonsampled conditions.  For model-based estimation, we calculate adjustment
#' factors by plot.
#' 
#' It is calculated by dividing 1 / summed condition proportions by plot. An
#' adjustment factor is determined for each tree based on the size of the plot
#' it was measured on. This is identified using TPA_UNADJ as follows:
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
#' @author Tracey S. Frescino, Paul L. Patterson, Elizabeth A. Freeman
#' @keywords data
#' @export modSApop
modSApop <- function(SAdoms=NULL, smallbnd=NULL, smallbnd.domain=NULL, 
	cond=NULL, plt=NULL, tree=NULL, seed=NULL, pltassgn=NULL, dsn=NULL, 
	puniqueid="CN", pltassgnid="PLT_CN", pjoinid="CN", tuniqueid="PLT_CN",  
	cuniqueid="PLT_CN", condid="CONDID", areawt="CONDPROP_UNADJ", 
	invyrs=NULL, intensity=NULL, measCur=FALSE, measEndyr=NULL,
	measEndyr.filter=NULL, ACI=FALSE, adj="plot", dunitvar="DOMAIN", 
	dunitvar2=NULL, dunitarea=NULL, areavar="ACRES", areaunits="acres", 
	minplotnum.unit=0, unit.action="keep", dunitzonal=NULL, 
	prednames=NULL, predfac=NULL, pvars2keep=NULL, cvars2keep=NULL, 
	saveobj=FALSE, objnm="SApopdat", savedata=FALSE, outfolder=NULL, 
	out_fmt="csv", out_dsn=NULL, outfn.pre=NULL, outfn.date=FALSE, 
	overwrite_dsn=FALSE, overwrite_layer=TRUE, append_layer=FALSE, 
	SAdata=NULL, pltdat=NULL, auxdat=NULL, gui=FALSE){

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data for small area estimation
  ## - check population data
  ## - check dunitarea data
  ## - calculate plot-level adjustment factors by dividing 1 by summed proportions in plot
  ## VALUE:
  ## - return all data needed for input to modSAest() function
  ##################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE

  ## If gui.. set variables to NULL
  if (gui) {
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar <- NULL
  }

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(modSApop)))) {
    miss <- input.params[!input.params %in% formals(modSApop)]
    stop("invalid parameter: ", toString(miss))
  }

  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=stratcombinelut <- NULL

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE)
  adjtree <- FALSE
  returnSApopdat <- FALSE
  nonsamp.pfilter=nonsamp.cfilter <- NULL 
  returnlst <- list()

# dunitvar2=NULL
# pvars2keep=NULL
# cvars2keep=NULL
# adj="plot"
# ACI=FALSE
# gui <- FALSE 


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
  if (!is.null(SAdata)) {
    list.items <- c("bnd", "cond", "plt",
		"pltassgn", "puniqueid", "pltassgnid", "pjoinid", "dunitarea",
		"dunitvar", "areavar", "dunitzonal")
    SAdata <- FIESTA::pcheck.object(SAdata, "SAdata", list.items=list.items)
    SAdoms <- SAdata$bnd
    #smallbnd <- SAdata$smallbnd
    plt <- SAdata$plt
    cond <- SAdata$cond
    tree <- SAdata$tree
    seed <- SAdata$seed
    pltassgn <- SAdata$pltassgn
    pltassgnid <- SAdata$pltassgnid
    dunitarea <- SAdata$dunitarea
    dunitvar <- SAdata$dunitvar
    areavar <- SAdata$areavar
    dunitzonal <- SAdata$dunitzonal
    puniqueid <- SAdata$puniqueid
    pjoinid <- SAdata$pjoinid
    predfac <- SAdata$predfac
    zonalnames <- SAdata$zonalnames

    if (is.null(prednames)) {
      prednames <- SAdata$prednames
    } else {
      if (!all(prednames %in% SAdata$prednames))
        stop("invalid prednames: ", 
		toString(prednames[!prednames %in% SAdata$prednames]))
      predfac <- predfac[predfac %in% prednames]
    }
  } else {
    if (!is.null(pltdat)) {
      list.items <- c("bndx", "tabs", "xypltx")

      ## Extract list objects
      puniqueid <- pltdat$puniqueid
      if ("tabs" %in% names(pltdat)) {
        pjoinid <- pltdat$pjoinid
        plt <- pltdat$tabs$pltx
        cond <- pltdat$tabs$condx
        tree <- pltdat$tabs$treex
        seed <- pltdat$tabs$seedx
      } else {
        pjoinid <- puniqueid
        plt <- pltdat$plt
        cond <- pltdat$cond
        tree <- pltdat$tree
        seed <- pltdat$seed
      }
    }
    if (!is.null(auxdat)) {
      list.items <- c("pltassgn", "dunitzonal", "dunitvar", "prednames", "dunitarea")
      auxdat <- FIESTA::pcheck.object(auxdat, "auxdat", list.items=list.items)
      pltassgn <- auxdat$pltassgn
      pltassgnid <- auxdat$pltassgnid
      dunitarea <- auxdat$dunitarea
      dunitvar <- auxdat$dunitvar
      areavar <- auxdat$areavar
      dunitzonal <- auxdat$dunitzonal
      zonalnames <- auxdat$zonalnames
      predfac <- auxdat$predfac

      if (is.null(prednames)) {
        prednames <- auxdat$prednames
      } else {
        if (!all(prednames %in% auxdat$prednames))
          stop("invalid prednames: ", 
		  toString(prednames[!prednames %in% auxdat$prednames]))
        predfac <- predfac[predfac %in% prednames]
      }
    } 
  }

  ## Check SAdoms
  if (!is.null(SAdoms) && !"sf" %in% class(SAdoms)) {
    stop("invalid SAdoms")
  }
 
  ###################################################################################
  ## CHECK PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots and conditions (if nonsamp.filter != "NONE")
  ## Applies plot and condition filters
  ###################################################################################
  popcheck <- check.popdata(gui=gui, module="SA", tree=tree, cond=cond, plt=plt, 
	seed=seed, pltassgn=pltassgn, dsn=dsn, tuniqueid=tuniqueid, cuniqueid=cuniqueid, 
	condid=condid, puniqueid=puniqueid, pltassgnid=pltassgnid, pjoinid=pjoinid,
	measCur=measCur, measEndyr=measEndyr, invyrs=invyrs, ACI=ACI, adj=adj, 
	nonsamp.pfilter=nonsamp.pfilter, nonsamp.cfilter=nonsamp.cfilter, 
	unitarea=dunitarea, areavar=areavar, areaunits=areaunits, unitvar=dunitvar, 
	unitvar2=dunitvar2, unit.action=unit.action, prednames=prednames, 
	predfac=predfac, pvars2keep=pvars2keep, cvars2keep=cvars2keep)
  condx <- popcheck$condx	
  pltcondx <- popcheck$pltcondx
  treef <- popcheck$treef
  seedf <- popcheck$seedf
  pltassgnx <- popcheck$pltassgnx
  cuniqueid <- popcheck$cuniqueid
  condid <- popcheck$condid
  tuniqueid <- popcheck$tuniqueid
  pltassgnid <- popcheck$pltassgnid
  ACI.filter <- popcheck$ACI.filter
  adj <- popcheck$adj
  dunitvar <- popcheck$unitvar
  dunitvar2 <- popcheck$unitvar2
  dunitarea <- popcheck$unitarea
  areavar <- popcheck$areavar
  areaunits <- popcheck$areaunits
  unit.action <- popcheck$unit.action
  prednames <- popcheck$prednames
  predfac <- popcheck$predfac
  plotsampcnt <- popcheck$plotsampcnt
  condsampcnt <- popcheck$condsampcnt
  states <- popcheck$states
  invyrs <- popcheck$invyrs
  cvars2keep <- popcheck$cvars2keep
  areawt <- popcheck$areawt
  tpropvars <- popcheck$tpropvars

  if (is.null(treef) && is.null(seedf)) {
    stop("must include tree data")
  }

  ###################################################################################
  ## CHECK STRATA
  ###################################################################################
  ## If strata=TRUE, check strata variables and number of plots by estimation unit
  ## - if < 2 plots, an error occurs, must collapse plots.
  ## - if 2-10 plots, a warning is displayed, with suggestion to collapse plots. 
  ## - if stratcombine=TRUE, combines strata classes to reach minplotnum.strat. 
  ## - if unit.action='combine', combines estimation units to reach minplotnum.unit.
  ###################################################################################
  auxdat <- check.auxiliary(pltx=pltassgnx, puniqueid=pltassgnid, module="SA",
		auxlut=dunitzonal, prednames=prednames, predfac=predfac, makedummy=TRUE,
		unitarea=dunitarea, unitvar=dunitvar, areavar=areavar, 
		minplotnum.unit=minplotnum.unit, unit.action=unit.action,
		auxtext="dunitlut", removetext="dunitarea", standardize=TRUE)  
  pltassgnx <- setDT(auxdat$pltx)
  dunitarea <- auxdat$unitarea
  dunitvar <- auxdat$unitvar
  dunitlut <- auxdat$auxlut
  prednames <- auxdat$prednames
  predfac <- auxdat$predfac
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)


  ###################################################################################
  ## CALCULATE ADJUSTMENT FACTORS FOR NONSAMPLED CONDITIONS
  ## If adj="samp", calculate adjustment factors by strata and estimation unit
  ## If adj="plot", calculate adjustment factors by plot
  ## adjfac = 1 / summed condition proportions (by plot-size), for area and trees
  ###################################################################################
  ## Returns:
  ##  Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ###################################################################################
  ## Merge plot strata info to condx
  if (is.null(key(condx))) setkeyv(condx, c(cuniqueid, condid))
  condx <- condx[pltassgnx[, c(pltassgnid, dunitvar, prednames), with=FALSE]]
  setkeyv(condx, c(cuniqueid, condid))

  if (adj == "plot") {
    adjtree <- TRUE
    bycond <- FALSE

    adjfacdata <- getadjfactorPLOT(condx=condx, treex=treef, seedx=seedf, 
				cuniqueid=cuniqueid, tuniqueid=tuniqueid, areawt=areawt,
				tpropvars=tpropvars)
    condx <- adjfacdata$condx
    treef <- adjfacdata$treex
    seedf <- adjfacdata$seedx
  }
 
  if (!is.null(SAdoms)) {
    returnlst$SAdomsdf <- sf::st_drop_geometry(SAdoms)
  }
  if (!is.null(smallbnd)) {
    if (is.null(smallbnd.domain)) {
      if ("DOMAIN" %in% names(smallbnd)) {
        smallbnd.domain <- "DOMAIN"
      } else if (length(names(sf::st_drop_geometry(smallbnd))) == 1) {
        smallbnd.domain <- names(sf::st_drop_geometry(smallbnd))
      } else {
        stop("must include smallbnd.domain for smallbnd")
      }
    } 
    returnlst$smallbnd <- smallbnd
    returnlst$smallbnd.domain <- smallbnd.domain
  }

  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  returnlst <- append(returnlst, list(condx=as.data.frame(condx), 
		pltcondx=as.data.frame(pltcondx), cuniqueid=cuniqueid, 
		condid=condid, ACI.filter=ACI.filter, 
		dunitarea=as.data.frame(dunitarea), areavar=areavar, areaunits=areaunits, 
		dunitvar=dunitvar, dunitlut=as.data.frame(dunitlut), prednames=prednames, 
		predfac=predfac, plotsampcnt=plotsampcnt, condsampcnt=condsampcnt, 
		states=states, invyrs=invyrs, estvar.area=estvar.area, adj=adj))

  if (!is.null(treef)) {
    returnlst$treex <- as.data.frame(treef)
    returnlst$tuniqueid <- tuniqueid
    returnlst$adjtree <- adjtree
  }
  if (!is.null(seedf)) {
    returnlst$seedx <- as.data.frame(seedf)
  }

  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
		overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=outfn.date)
    saveRDS(returnlst, file=objfn)
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

    datExportData(pltassgnx, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="pltassgn", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
    datExportData(dunitarea, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer)
    datExportData(dunitlut, outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer="dunitlut", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE, append_layer=append_layer)
  }

  return(returnlst)
}
