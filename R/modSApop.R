#' Small area module - Compile population data for SA module.
#' 
#' Compile population data for input to the modSA* modules.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' Necessary variables:\cr \tabular{llll}{ \tab \bold{Data} \tab
#' \bold{Variable} \tab \bold{Description}\cr 
#' \tab tree \tab tuniqueid \tab Unique identifier for each plot, to link to 
#' pltassgn (e.g. PLT_CN).\cr 
#' \tab \tab CONDID \tab Unique identifier of each condition on plot, to link to
#' cond.  Set CONDID=1, if only 1 condition per plot.\cr 
#' \tab \tab TPA_UNADJ \tab Number of trees per acre each sample tree 
#' represents (e.g. DESIGNCD=1: TPA_UNADJ=6.018046 for trees on subplot; 
#' 74.965282 for trees on microplot).\cr 
#' \tab cond \tab cuniqueid \tab Unique identifier for each plot, to link to 
#' pltassgn (e.g. PLT_CN).\cr 
#' \tab \tab CONDID \tab Unique identifier of each condition on plot. Set 
#' CONDID=1, if only 1 condition per plot.\cr 
#' \tab \tab CONDPROP_UNADJ \tab Unadjusted proportion of condition on
#' each plot.  Set CONDPROP_UNADJ=1, if only 1 condition per plot.\cr 
#' \tab \tab COND_STATUS_CD \tab Status of each forested condition on plot (i.e.
#' accessible forest, nonforest, water, etc.)\cr 
#' \tab \tab NF_COND_STATUS_CD \tab If ACI=TRUE. Status of each nonforest 
#' condition on plot (i.e. accessible nonforest, nonsampled nonforest)\cr 
#' \tab \tab SITECLCD \tab If landarea=TIMBERLAND. Measure of site productivity.\cr 
#' \tab \tab RESERVCD \tab If landarea=TIMBERLAND. Reserved status.\cr 
#' \tab \tab SUBPROP_UNADJ \tab Unadjusted proportion of subplot conditions 
#' on each plot. Set SUBPROP_UNADJ=1, if only 1 condition per subplot.\cr 
#' \tab \tab MICRPROP_UNADJ \tab If microplot trfee attributes. Unadjusted 
#' proportion of microplot conditions on each plot. Set MICRPROP_UNADJ=1, 
#' if only 1 condition per microplot.\cr 
#' \tab \tab MACRPROP_UNADJ \tab If macroplot tree attributes. Unadjusted 
#' proportion of macroplot conditions on each plot. Set MACRPROP_UNADJ=1, 
#' if only 1 condition per macroplot.\cr 
#' \tab pltassgn \tab puniqueid \tab Unique identifier for each plot, to 
#' link to cond (e.g. CN).\cr 
#' \tab \tab STATECD \tab Identifies state each plot is located in.\cr 
#' \tab \tab INVYR \tab Identifies inventory year of each plot.\cr 
#' \tab \tab PLOT_STATUS_CD \tab Status of each plot (i.e. sampled, nonsampled).  
#' If not included, all plots are assumed as sampled.\cr }
#' 
#' For available reference tables: sort(unique(FIESTAutils::ref_codes$VARIABLE)) \cr
#' 
#' @param popType String. Type of evaluation(s) to include in population data.
#' Note: currently only c('CURR', 'VOL', 'LULC') are available. See details
#' below for descriptions of each.
#' @param popTabs List of population tables the user would like returned.
#'  See help(popTables) for a list of options.
#' @param popTabIDs List of unique IDs corresponding to the population tables
#' that the user has requested. See help(popTableIDs) for a list of
#' options.
#' @param popFilter List of population filters. See help(popFilters) for a 
#' list of options. 
#' @param SAdoms sf object. SA domains with attributes for joining.
#' @param smallbnd sf object. small bound.
#' @param smallbnd.domain String. Name of attribute defining domain attribute.
#' @param largebnd.unique String. Name of the large boundary unique identifer
#' to define plots within a model extent. If NULL, all plots are used for model
#' extent.
#' @param pltassgn DF/DT, comma-separated values (CSV) file(*.csv), or layer in
#' dsn, Can also be a shapefile(*.shp) with one record per plot, a spatial
#' layer in dsn, or a sf R object. Plot-level assignment of estimation unit
#' and/or strata. Optional.
#' @param dsn String. Name of database where tree, cond, and pltassgn tables
#' reside.  The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html).
#' @param pltassgnid String. Unique identifier of plot in pltassgn.
#' @param datsource String. Name of data source ('obj', 'sqlite', 'postgres').
#' @param dsn String. Name of database where tree, cond, and plot-level tables
#' reside.  The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html).
#' @param dbconn Open database connection.
#' @param pjoinid String. Join variable in plot to match pltassgnid. Does not
#' need to be uniqueid. If using most current XY coordinates for plot
#' assignments, use identifier for plot (e.g., PLOT_ID).
#' @param areawt String. Name of variable for summarizing area weights (e.g.,
#' CONDPROP_UNADJ).
#' @param adj String. How to calculate adjustment factors for nonsampled
#' (nonresponse) conditions based on summed proportions for by plot ('samp',
#' 'none'). 'plot' - adjustments are calculated at plot-level. Adjustments are
#' only calculated for annual inventory plots (DESIGNCD=1).
#' @param defaultVars Logical. If TRUE, a set of default variables are selected.
#' @param dunitvar String. Name of the domain unit variable in cond, plt, or
#' pltassgn with domain unit assignment for each plot.
#' @param dunitarea Numeric or DF. Total area by domain unit.
#' @param areavar String. Name of area variable in unitarea. Default="ACRES".
#' @param dunitzonal DF/DT. Data frame with zonal auxiliary information by
#' domain unit. For continuous data, means by domain unit; for categorical
#' data, proportion of class by domain unit.
#' @param prednames String vector. Name(s) of predictor variables to use in
#' model. 
#' @param predfac String vector. Name(s) of factor predictor variables to use
#' in model. Names will change in output depending on number of categories.
#' @param addxy Logical. If TRUE, adds X/Y attributes to pltassgn.
#' @param returndata Logical. If TRUE, returns data objects.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder. 
#' @param saveobj Logical. If TRUE, saves returned list object to outfolder.
#' @param objnm String. Name of *.rds object.
#' @param unit_opts List. See help(unit_options()) for a list of options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options.
#' @param database_opts List. See help(database_options()) for a list
#' of options. Only used when datsource = 'postgres'.  
#' @param SAdata R List object. Output data list components from
#' FIESTA::SAdata().
#' @param pltdat R List object. Output data list components from
#' FIESTA::spGetPlots().
#' @param auxdat R List object. Output data list components from
#' FIESTA::spGetAuxiliary().
#' @param ... For extendibility. 
#' @return A list with population data for Small-Area estimates.
#' 
#' \item{SAdomsdf}{ Data frame. Attribute table from SAdoms spatial layer.
#' Includes DOMAIN and AOI attributes. DOMAIN represents modeling domains.  AOI
#' identifies the small area of interest. } 
#' \item{pltidsadj}{ Data frame. Condition-level data with condition proportions, 
#' domain and predictor assignments, and adjusted condition proportions, 
#' if adjplot = TRUE. } 
#' \item{pltcondx}{ Data frame. Plot/Condition data used for estimation. }
#' \item{cuniqueid}{ String. Unique identifier of plot in condx and pltcondx. }
#' \item{condid}{ String. Unique identifier of condition in condx and pltcondx. } 
#' \item{treex}{ Data frame. If esttype='TREE', tree-level data, including
#' adjustment factors, if adjplot = TRUE. } 
#' \item{tuniqueid}{ String. If esttype='TREE', unique identifier of plot in 
#' treex. } 
#' \item{ACI.filter}{ String. If ACI=FALSE, ACI.filter="COND_STATUS_CD == 1" . } 
#' \item{dunitarea}{ Data frame. Area by model domain unit. } 
#' \item{areavar}{ String. Name of area variable in dunitarea. } 
#' \item{dunitvar}{ String. Name of variable defining model domain units in 
#' dunitarea. } 
#' \item{dunitlut}{ Data frame. Table of model domain units with zonal statistics 
#' of predictor values, number of plots by domain unit. } 
#' \item{prednames}{ String vector. Name of variables in dunitlut and condx 
#' defining potential predictors for small area estimation. } 
#' \item{plotsampcnt}{ Data frame. Number of plots by PLOT_STATUS_CD. } 
#' \item{condsampcnt}{ Data frame. Number of conditions by COND_STATUS_CD. } 
#' \item{states}{ String. State names in dataset. }
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
#' \tabular{llr}{ \tab \bold{PLOT SIZE} \tab \bold{TPA_UNADJ} \cr 
#' \tab SUBPLOT \tab 6.018046 \cr 
#' \tab MICROPLOT \tab 74.965282 \cr 
#' \tab MACROPLOT \tab 0.999188 \cr }
#' 
#' If ACI=FALSE, only nonsampled forest conditions are accounted for in the
#' adjustment factor. \cr 
#' If ACI=TRUE, the nonsampled nonforest conditions are removed as well and 
#' accounted for in adjustment factor.  This is if you are interested in 
#' estimates for all lands or nonforest lands in the All-Condition-Inventory.
#' @author Tracey S. Frescino, Paul L. Patterson
#' @keywords data
#' @examples 
#' \donttest{
#' # NOTE: FIA data objects used in these examples are stored in `FIESTA`, but
#' # can be generated for populations of interest by the user with functions in
#' # `FIESTA` such as `spGetPlots()`, `spGetAuxiliary()`, etc. For more
#' # information, see `FIESTA`'s extensive vignettes.
#'  
#' # Population data for counties in Wyoming
#' modSApop(popTabs = list(tree = FIESTA::WYtree,
#'                         cond = FIESTA::WYcond),
#'          pltassgn = FIESTA::WYpltassgn,
#'          pltassgnid = "CN",
#'          dunitarea = FIESTA::WYunitarea,
#'          dunitvar = "ESTN_UNIT",
#'          dunitzonal = FIESTA::WYunitzonal,
#'          prednames = c("dem", "tcc", "tpi", "tnt"),
#'          predfac = "tnt")
#'          
#' # Adding seedling data as well
#' modSApop(popTabs = list(tree = FIESTA::WYtree,
#'                         cond = FIESTA::WYcond,
#'                         seed = FIESTA::WYseed),
#'          pltassgn = FIESTA::WYpltassgn,
#'          pltassgnid = "CN",
#'          dunitarea = FIESTA::WYunitarea,
#'          dunitvar = "ESTN_UNIT",
#'          dunitzonal = FIESTA::WYunitzonal,
#'          prednames = c("dem", "tcc", "tpi", "tnt"),
#'          predfac = "tnt")
#' }
#' @export modSApop
modSApop <- function(popType = "VOL",
                     popTabs = popTables(),
                     popTabIDs = popTableIDs(), 
                     popFilter = popFilters(),
                     pltassgn = NULL,
                     pltassgnid = "PLT_CN", 
                     datsource = "sqlite",
                     dsn = NULL, 
                     dbconn = NULL,
                     pjoinid = "CN", 
                     areawt = "CONDPROP_UNADJ", 
                     adj = "plot",
                     defaultVars = TRUE,
                     dunitvar = NULL, 
                     dunitarea = NULL, 
                     areavar = "ACRES",
                     dunitzonal = NULL, 
                     prednames = NULL, 
                     predfac = NULL, 
                     addxy = FALSE,
                     returndata = TRUE,
                     savedata = FALSE, 
                     saveobj = FALSE, 
                     objnm = "SApopdat", 
                     unit_opts = list(minplotnum.unit = 2,
                                      unit.action = "remove"), 
                     savedata_opts = NULL, 
                     database_opts = NULL,
                     SAdoms = NULL, 
                     smallbnd = NULL, 
                     smallbnd.domain = NULL, 
                     largebnd.unique = NULL,
                     SAdata = NULL, 
                     pltdat = NULL, 
                     auxdat = NULL,
                     ...) {

  ##################################################################################
  ## DESCRIPTION:
  ## Generates population data for small area estimation
  ## - check population data
  ## - check dunitarea data
  ## - calculate plot-level adjustment factors by dividing 1 by summed proportions in plot
  ## VALUE:
  ## - return all data needed for input to modSAest() function
  ##################################################################################

  gui <- FALSE

  ## If gui.. set variables to NULL
  if (gui) {
    areavar=strata=strvar=getwt=cuniqueid=ACI=tuniqueid=savedata=unitvar=projectid <- NULL
  }
  
  ## Set parameters
  adjtree <- FALSE
  returnSApopdat <- FALSE
  nonsamp.pfilter=nonsamp.cfilter=schema=vcondstrx=vcondsppx=outlst <- NULL 
  returnlst <- list(module = "SA")
  
  ## Set global variables
  ONEUNIT=n.total=n.strata=strwt=TOTAL=stratcombinelut <- NULL
  condid <- "CONDID"
  areawt2 <- NULL
  pvars2keep <- NULL
  pltidsadjindb=savepltids=dsnreadonly <- FALSE

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(modSApop)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params = input.params,
                unit_opts = unit_opts, 
                savedata_opts = savedata_opts, database_opts = database_opts)
  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
                         popFilter = popFilter,
                         popTabs = popTabs,
                         popTabIDs = popTabIDs,
                         unit_opts = unit_opts, 
                         savedata_opts = savedata_opts,
                         database_opts = database_opts))
  savedata_opts <- optslst$savedata_opts  
  unit_opts <- optslst$unit_opts  
  database_opts <- optslst$database_opts  
  popFilter <- optslst$popFilter
  popTabs <- optslst$popTabs
  popTabIDs <- optslst$popTabIDs
  
  for (i in 1:length(unit_opts)) {
    assign(names(unit_opts)[[i]], unit_opts[[i]])
  }
  for (i in 1:length(savedata_opts)) {
    assign(names(savedata_opts)[[i]], savedata_opts[[i]])
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check returndata 
  returndata <- FIESTAutils::pcheck.logical(returndata, varnm="returndata", 
        title="Return data as objectsd?", first="YES", gui=gui, stopifnull=TRUE)

    ## Check addxy 
  addxy <- pcheck.logical(addxy, varnm="addxy", 
    title="Add XY?", first="NO", gui=gui, stopifnull=TRUE)

  ## Check savedata 
  savedata <- FIESTAutils::pcheck.logical(savedata, varnm="savedata", 
                                          title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)
  if (!savedata) {
    message("savedata=FALSE with savedata parameters... no data are saved")
  }
  
  ## Check saveobj 
  saveobj <- FIESTAutils::pcheck.logical(saveobj, varnm="saveobj", 
                                         title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)
  
  ## Check output
  ########################################################
  if (savedata || saveobj) {
    outlst <- pcheck.output(savedata_opts = savedata_opts)
    if (savedata) {
      if (outlst$out_fmt == "sqlite" && is.null(outlst$out_dsn)) {
        outlst$out_dsn <- "SApopdat.db"
      }
      outlst$add_layer <- TRUE
    }
  }
  
  if (saveobj) {
    outobj_fmtlst <- c('rds', 'rda')
    outobj_fmt <- FIESTAutils::pcheck.varchar(var2check = outobj_fmt, varnm="outobj_fmt", 
                                              gui=gui, checklst = outobj_fmtlst, caption="outobj_fmt", 
                                              multiple = FALSE, stopifnull = TRUE)
    if (is.null(objnm)) {
      objnm <- "GBpopdat"
    }
    #if (append_layer) overwrite_layer <- FALSE
    if (append_layer) message("currently cannot append to object lists")
    objfn <- getoutfn(outfn = objnm, 
                      ext = outobj_fmt, 
                      outfolder = outlst$outfolder, 
                      overwrite = outlst$overwrite_layer, 
                      outfn.pre = outlst$outfn.pre, 
                      outfn.date = outlst$outfn.date)
  }
  

  ## Check popType
  ########################################################
  DWM_types <- c("CWD", "FWD_SM", "FWD_LG", "DUFF")
  evalTyplst <- c("ALL", "CURR", "VOL", "LULC", "P2VEG", "INV", "DWM",
                  "CHNG", "GRM", "GROW", "MORT", "REMV")
  popType <- pcheck.varchar(var2check=popType, varnm="popType", gui=gui,
                            checklst=evalTyplst, caption="popType", multiple=FALSE, 
                            stopifinvalid=FALSE)
  if (is.null(popType)) {
    message("popType is invalid... must be from following list:\n", toString(evalTyplst))
  }
  popevalid <- popFilter$evalid
  if (!is.null(popevalid)) {
    popevalid <- as.character(popevalid)
    substr(popevalid, nchar(popevalid)-1, nchar(popevalid)) <- 
      formatC(FIESTAutils::ref_popType[FIESTAutils::ref_popType$popType %in% popType, "EVAL_TYP_CD"], 
              width=2, flag="0")
  } 
  if (popType %in% c("GROW", "MORT", "REMV")) {
    popType <- "GRM"
  }

  ###################################################################################
  ## Load data
  ###################################################################################
  if (!is.null(SAdata)) {
    list.items <- c("tabs", "pltassgn", "pltassgnid", "pjoinid", "unitarea",
		                "unitvar", "areavar", "unitzonal")
    SAdata <- pcheck.object(SAdata, "SAdata", list.items=list.items)
    SAdoms <- SAdata$bnd
    #smallbnd <- SAdata$smallbnd
    popTabs <- SAdata$tabs
    popTabIDs <- SAdata$tabIDs
    pltassgn <- SAdata$pltassgn
    pltassgnid <- SAdata$pltassgnid
    dunitarea <- SAdata$unitarea
    areavar <- SAdata$areavar
    dunitvar <- SAdata$unitvar
    areavar <- SAdata$areavar
    dunitzonal <- SAdata$unitzonal
    puniqueid <- SAdata$puniqueid
    pjoinid <- SAdata$pjoinid
    predfac <- SAdata$predfac
    spxy <- SAdata$spxy
    xy.uniqueid <- SAdata$xy.uniqueid
    pvars2keep <- SAdata$vars2keep

    if (is.null(prednames)) {
      prednames <- SAdata$prednames
    } else {
      if (!all(prednames %in% SAdata$prednames))
        stop("invalid prednames: ", 
		            toString(prednames[!prednames %in% SAdata$prednames]))
    }
    if (is.null(predfac)) {
      predfac <- SAdata$predfac
    }
    predfac <- predfac[predfac %in% prednames]
    
  } else {
    if (!is.null(pltdat)) {
      datsource <- "obj"
      tabnames <- if (sum(names(pltdat$tabs) %in% names(popTables())) == 0) {
        stop("no tables exist in pltdat")
      }
      popTabs <- pltdat$tabs
      popTabIDs <- pltdat$tabIDs
      pjoinid <- pltdat$pjoinid
      spxy <- pltdat$spxy
      xy.uniqueid <- pltdat$xy.uniqueid
      SAdoms <- pltdat$bnd
    }
    if (!is.null(auxdat)) {
      list.items <- c("pltassgn", "unitzonal", "unitvar", "prednames", "unitarea")
      auxdat <- pcheck.object(auxdat, "auxdat", list.items=list.items)
      pltassgn <- data.table(auxdat$pltassgn)
      pltassgnid <- auxdat$pltassgnid
      dunitarea <- data.table(auxdat$unitarea)
      dunitvar <- auxdat$unitvar
      areavar <- auxdat$areavar
      dunitzonal <- data.table(auxdat$unitzonal)
      zonalnames <- auxdat$zonalnames
      predfac <- auxdat$predfac
      areavar <- auxdat$areavar
      SAdoms <- auxdat$unit_layer

      if (is.null(prednames)) {
        prednames <- auxdat$prednames
      } else {
        if (!all(prednames %in% auxdat$prednames))
          stop("invalid prednames: ", toString(prednames[!prednames %in% auxdat$prednames]))
      }
      if (is.null(predfac)) {
        predfac <- auxdat$predfac
      }
      predfac <- predfac[predfac %in% prednames]
    } 
  }
  ## check addxy
  if (addxy && is.null(spxy)) {
    message("no xy available...  use returnxy=TRUE when getting plot data")
  }
  
  ## Set user-supplied popTable values 
  popTables_defaults_list <- formals(popTables)[-length(formals(popTables))]
  if (length(popTabs) > 0) {
    for (i in 1:length(popTabs)) {
      if (names(popTabs)[[i]] %in% names(popTables_defaults_list)) {
        assign(names(popTabs)[[i]], popTabs[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(popTabs)[[i]]))
      }
    }
  } else {
    stop("need to include popTabs")
  }
  
  list.items <- {}
  if (popType == "LULC") {
    list.items <- c(list.items, "lulcx")
  }
  if (popType == "P2VEG") {
    list.items <- c(list.items, "vsubpspp", "vsubpstr", "subplot", "subp_cond")
  }
  popTabs <- pcheck.object(popTabs, "popTabs", list.items=list.items)
  
  
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
  popTabs <- pcheck.object(popTabs, "popTabs", list.items=list.items)
  
  
  ## Set user-supplied popTabIDs values
  ### Check for invalid parameters first
  popTableIDs_defaults_list <- formals(popTableIDs)[-length(formals(popTableIDs))]
  for (i in 1:length(popTabIDs)) {
    if (!(names(popTabIDs)[[i]] %in% names(popTableIDs_defaults_list))) {
      stop(paste("Invalid parameter: ", names(popTabIDs)[[i]]))
    }
  }
  ### Then actually set the values
  for (nm in names(popTabs)) {
    if (!any(names(popTabIDs) == nm)) {
      popTabIDs[[nm]] <- popTableIDs_defaults_list[[nm]]
    }
  }
  
  pvars2keep <- unique(c(largebnd.unique, "AOI", pvars2keep))
  ###################################################################################
  ## CHECK PLOT PARAMETERS AND DATA
  ## Generate table of sampled/nonsampled plots and conditions
  ## Remove nonsampled plots (if nonsamp.pfilter != "NONE")
  ## Applies plot filters
  ###################################################################################
  pltcheck <- 
    check.popdataPLT(dsn = dsn, dbconn = dbconn, schema = schema,
                     datsource = datsource, 
                     tabs = popTabs, tabIDs = popTabIDs, 
                     pltassgn = pltassgn,
                     pltassgnid = pltassgnid, pjoinid = pjoinid, 
                     module = "SA", popType = popType,
                     popevalid = popevalid, adj = adj, 
                     popFilter = popFilter,
                     nonsamp.pfilter = nonsamp.pfilter, 
                     unitarea = dunitarea, areavar = areavar,
                     unitvar = dunitvar, unitvar2 = unitvar2,
                     areaunits = areaunits, 
                     unit.action = unit.action, 
                     auxlut = dunitzonal, 
                     defaultVars = defaultVars,
                     prednames = prednames, predfac = predfac,
                     pvars2keep = pvars2keep, 
                     dsnreadonly = dsnreadonly)
  if (is.null(pltcheck)) return(0)
  pltassgnx <- pltcheck$pltassgnx
  pltassgnid <- pltcheck$pltassgnid
  pltassgn. <- pltcheck$pltassgn.
  plotlst <- pltcheck$plotlst
  pltidsWITHqry <- pltcheck$pltidsWITHqry
  pltidsid <- pltcheck$pltidsid
  pltidvars <- pltcheck$pltidvars
  projidvars <- pltcheck$projidvars
  pdoms2keep <- pltcheck$pdoms2keep
  ACI <- pltcheck$ACI
  dunitvar <- pltcheck$unitvar
  dunitvar2 <- pltcheck$unitvar2
  dunitarea <- pltcheck$unitarea
  areavar <- pltcheck$areavar
  areaunits <- pltcheck$areaunits
  dunit.action <- pltcheck$unit.action
  P2POINTCNT <- pltcheck$P2POINTCNT 
  plotsampcnt <- pltcheck$plotsampcnt
  states <- pltcheck$states
  invyrs <- pltcheck$invyrs
  dbconn <- pltcheck$dbconn
  SCHEMA. <- pltcheck$SCHEMA.
  pltaindb <- pltcheck$pltaindb
  datindb <- pltcheck$datindb
  POP_PLOT_STRATUM_ASSGN <- pltcheck$POP_PLOT_STRATUM_ASSGN
  getdataWITHqry <- pltcheck$getdataWITHqry
  getdataCNs <- pltcheck$getdataCNs
  plotunitcnt <- pltcheck$plotunitcnt
  prednames <- pltcheck$prednames
  predfac <- pltcheck$predfac
  auxlut <- dunitzonal
  getdataWITHqry <- pltcheck$getdataWITHqry
  getdataCNs <- pltcheck$getdataCNs
  if (ACI) {
    nfplotsampcnt <- pltcheck$nfplotsampcnt
  }
  
  # subset pvars2keep 
  if (!"AOI" %in% names(pltassgnx)) {
    pltassgnx$AOI <- 1
  }
  
  # if (!is.null(pvars2keep)) {
  #   pvars2keep <- pvars2keep[pvars2keep %in% names(pltx) & !pvars2keep %in% names(pltassgnx)]
  #   if (length(pvars2keep) > 0) {
  #     pltassgnx <- merge(pltassgnx, pltx[, c(puniqueid, pvars2keep), with=FALSE], 
  #                        by.x=pltassgnid, by.y=puniqueid)
  #     pltx <- pltx[, names(pltx)[!names(pltx) %in% pvars2keep], with=FALSE]
  #     setcolorder(pltassgnx, c(pltassgnid, pvars2keep, 
  #                              names(pltassgnx)[!names(pltassgnx) %in% c(pltassgnid, pvars2keep)]))
  #   }
  # }
  
  ###################################################################################
  ## Check auxiliary data
  ###################################################################################
  auxcheck <- 
    check.auxiliary(module = "SA",
                    pltx = pltassgnx,
                    puniqueid = pltassgnid, 
                    unitvar = dunitvar, 
                    unitarea = dunitarea, 
                    areavar = areavar, 
                    minplotnum.unit = minplotnum.unit, 
                    unit.action = dunit.action,
                    auxlut = dunitzonal, 
                    prednames = prednames, 
                    predfac = predfac, 
                    makedummy = TRUE,  
                    standardize = TRUE,                 
                    auxtext = "dunitlut",
                    removetext = "dunitarea",
                    AOI = popFilter$AOIonly)  
  pltassgnx <- setDT(auxcheck$pltx)
  dunitarea <- auxcheck$unitarea
  dunitvar <- auxcheck$unitvar
  dunitvars <- auxcheck$unitvars
  dunitlut <- auxcheck$auxlut
  prednames <- auxcheck$prednames
  predfac <- auxcheck$predfac
  npixels <- auxcheck$npixels
  unitNA <- auxcheck$unitNA
  unitwarnlut <- auxcheck$stratwarnlut
  if (is.null(key(pltassgnx))) setkeyv(pltassgnx, pltassgnid)
    
  # subset pvars2keep 
  if (!"AOI" %in% names(dunitlut)) {
    dunitlut$AOI <- 1
  }

  # ## Change names based on data.frame names
  # dunitlutcols <- which(names(dunitlut) %in% prednames)
  # pltassgnxcols <- which(names(pltassgnx) %in% prednames)
  # dfnames_unitlut <- colnames(data.frame(dunitlut[, dunitlutcols, with=FALSE]))
  # dfnames_pltassgn <- colnames(data.frame(pltassgnx[, pltassgnxcols, with = FALSE]))
  # names(dunitlut)[dunitlutcols] <- dfnames_unitlut
  # names(pltassgnx)[pltassgnxcols] <- dfnames_pltassgn
  # prednames <- unique(dfnames_unitlut, dfnames_pltassgn)

  
  if (popType %in% c("ALL", "CURR", "AREA", "VOL")) {
    
    ###################################################################################
    ## Check parameters and data for popType AREA/VOL
    ###################################################################################
    areawt <- "CONDPROP_UNADJ"
    popcheck <- 
      check.popdataVOL(tabs = popTabs, tabIDs = popTabIDs,
                       popType = popType,
                       datindb = datindb, pltaindb = pltaindb,
                       pltidsWITHqry = pltidsWITHqry, 
                       pltidsid = pltidsid,
                       pltidvars = pltidvars, projidvars = projidvars,
                       pdoms2keep = pdoms2keep,
                       defaultVars = defaultVars,
                       pltidsadjindb = pltidsadjindb, 
                       pltassgnid = pltassgnid, 
                       pltassgnx = pltassgnx,
                       POP_PLOT_STRATUM_ASSGN = POP_PLOT_STRATUM_ASSGN,
                       adj = adj, ACI = ACI, 
                       plotlst = plotlst,  
                       condid = condid, 
                       areawt = areawt, areawt2 = areawt2,
                       unitvars = dunitvars,
                       nonsamp.cfilter = nonsamp.cfilter,
                       dbconn = dbconn, SCHEMA. = SCHEMA.,
                       getdataWITHqry = getdataWITHqry,
                       getdataCNs = getdataCNs,
                       returndata = returndata,
                       savedata = savedata,
                       outlst = outlst,
                       cvars2keep = c("AOI", largebnd.unique))
    if (is.null(popcheck)) return(NULL)
    pltidsadj <- popcheck$pltidsadj
    pltcondx <- popcheck$pltcondx
    pltcondflds <- popcheck$pltcondflds
    cuniqueid <- popcheck$cuniqueid
    condid <- popcheck$condid
    adjfactors <- popcheck$adjfactors
    adjvarlst <- popcheck$adjvarlst
    condsampcnt <- popcheck$condsampcnt
    dbqueries <- popcheck$dbqueries
    dbqueriesWITH <- popcheck$dbqueriesWITH
    estfromqry <- popcheck$estfromqry
    adjcase <- popcheck$adjcase
    pjoinid <- popcheck$pjoinid
    
    if(popType == "VOL") {
      treex <- popcheck$treex
      seedx <- popcheck$seedx
      tuniqueid <- popcheck$tuniqueid
      if (is.null(treex) && is.null(seedx)) {
        stop("must include tree data")
      }
    }    
    
  } else {
    stop("invalid popType")
  }
  
  
  ## Change name of dunitvar to DOMAIN if not already
  # can we assume that in SA dunitarea will always be an object?
  # need to do this elsewhere in the case that everything is in the database
  if (dunitvar != "DOMAIN") {
    setnames(dunitlut, dunitvar, "DOMAIN")
    setnames(dunitarea, dunitvar, "DOMAIN")
    setnames(pltassgnx, dunitvar, "DOMAIN")
    dunitvars[dunitvars == dunitvar] <- "DOMAIN"
    dunitvar <- "DOMAIN"
  }

  ###################################################################################
  ## Return population data objects
  ###################################################################################
  estvar.area <- ifelse(adj == "none", "CONDPROP_UNADJ", "CONDPROP_ADJ")
  if (is.null(key(dunitarea))) {
    setkeyv(dunitarea, dunitvar)
  }

  if (!is.null(SAdoms)) {
    returnlst$SAdomsdf <- sf::st_drop_geometry(SAdoms)
  }
  if (!is.null(SAdoms) && is.null(smallbnd)) {
    if (!"AOI" %in% names(SAdoms)) {
      stop("missing AOI attribute in SAdoms")
    }
    smallbnd <- SAdoms[SAdoms$AOI == 1, ]
  } else {
    smallbnd <- pcheck.spatial(layer=smallbnd, caption="smallbnd")
    if (!"AOI" %in% names(smallbnd)) {
      smallbnd$AOI <- 1
    }
  }  
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


  ## Add xy attributes to pltassgnx
  ###############################################################################
  if (addxy) {
    if (is.null(smallbnd)) {
      stop("need smallbnd for addxy")
    }
    spxycompare <- crsCompare(spxy, smallbnd, nolonglat=TRUE)
    spxy <- spxycompare$x
    smallbnd <- spxycompare$y
    
    if (is.null(xy.uniqueid)) {
      if (pltassgnid %in% names(spxy)) {
        xy.uniqueid <- pltassgnid
      }
    }
    if (!is.null(xy.uniqueid) && xy.uniqueid %in% names(spxy)) {
      xy.coords <- data.frame(sf::st_drop_geometry(spxy[, xy.uniqueid]), sf::st_coordinates(spxy))
      pltassgnx <- merge(pltassgnx, xy.coords, by.x=pltassgnid, by.y=xy.uniqueid)
    } else {
      stop(pltassgnid, " not in spxy names... cannot merge")
    }
  }

  ###################################################################################
  ## Add new variables to pltcondx for estimation
  ###################################################################################
  if (returndata || savedata) {
    
    ## Get order of pltcondx columns
    pltcondxcols <- names(pltcondx)
    pltcondxkey <- key(pltcondx)
    newcols <- {}
    
    ## Add LANDSTATUSCD based on the following lookup table
    landstatuscdnm <- findnm("LANDSTATUSCD", pltcondxcols, returnNULL=TRUE)
    if (is.null(landstatuscdnm)) {
      condstatusnm <- findnm("COND_STATUS_CD", pltcondxcols, returnNULL=TRUE)
      reservcdnm <- findnm("RESERVCD", pltcondxcols, returnNULL=TRUE)
      siteclcdnm <- findnm("SITECLCD", pltcondxcols, returnNULL=TRUE)
      
      if (all(!sapply(c(condstatusnm, reservcdnm, siteclcdnm), is.null))) {
        lower <- ifelse (condstatusnm == "COND_STATUS_CD", FALSE, TRUE)
        landstatusnm <- ifelse(lower, "landstatus", "LANDSTATUS")
        
        LANDSTATUSlut <- data.frame(LANDSTATUS = c(101:108, 111:117),
                                    LANDSTATUSCD = c(rep(1, 6), rep(2, 2), rep(3, 6), 4),
                                    LANDSTATUSNM = c(rep("Timberland", 6), 
                                                     rep("Other forestland", 2), 
                                                     rep("Reserved productive forestland", 6),
                                                     "Reserved other forestland"))
        if (lower) names(LANDSTATUSlut) <- tolower(names(LANDSTATUSlut))
        
        pltcondx[[landstatusnm]] <- 
          with(pltcondx, get(condstatusnm) * 100 + get(reservcdnm) * 10 + get(siteclcdnm))
        pltcondx <- merge(pltcondx, LANDSTATUSlut, by=landstatusnm, all.x=TRUE)
        pltcondx[[landstatusnm]] <- NULL
        newcols <- c("LANDSTATUSCD", "LANDSTATUSNM")
        if (lower) newcols <- tolower(newcols)
        
        if (popType %in% c("CHNG", "GRM")) {
          prevnm <- ifelse(lower, "prev_", "PREV_")
          names(LANDSTATUSlut) <- paste0(prevnm, names(LANDSTATUSlut))
          
          pltcondx[[paste0(prevnm, landstatusnm)]] <- 
            with(pltcondx, get(paste0(prevnm, condstatusnm)) * 100 + 
                   get(paste0(prevnm, reservcdnm)) * 10 + get(paste0(prevnm, siteclcdnm)))
          pltcondx <- merge(pltcondx, LANDSTATUSlut, by=paste0(prevnm, landstatusnm), all.x=TRUE)
          pltcondx[[paste0(prevnm, landstatusnm)]] <- NULL
          newcols <- c(newcols, paste0(prevnm, newcols))
        }
      }
    }
    
    ## Add FORTYPGRPCD to pltcondx if not already in dataset
    fortypgrpnm <- findnm("FORTYPGRPCD", pltcondxcols, returnNULL=TRUE)
    
    if (is.null(fortypgrpnm)) {
      fortypnm <- findnm("FORTYPCD", pltcondxcols, returnNULL=TRUE)
      
      if (!is.null(fortypnm)) {
        lower <- ifelse (fortypnm == "FORTYPCD", FALSE, TRUE)
        
        ref_fortyp <- ref_codes[ref_codes$VARIABLE == "FORTYPCD", c("VALUE", "GROUPCD")]
        names(ref_fortyp) <- c("FORTYPCD", "FORTYPGRPCD")
        if (lower) names(ref_fortyp) <- tolower(names(ref_fortyp))
        
        pltcondx <- merge(pltcondx, ref_fortyp, by=fortypnm, all.x=TRUE)
        newcols <- c(newcols, ifelse(lower, "fortypgrpcd", "FORTYPGRPCD"))
        
        if (popType %in% c("CHNG", "GRM")) {
          prevnm <- ifelse(lower, "prev_", "PREV_")
          names(ref_fortyp) <- paste0(prevnm, names(ref_fortyp))
          
          pltcondx <- merge(pltcondx, ref_fortyp, by=paste0(prevnm, fortypnm), all.x=TRUE)
          newcols <- c(newcols, ifelse(lower, "prev_fortypgrpcd", "PREV_FORTYPGRPCD"))
        }  
      }
    }
    
    ## Add DSTRBGRP to pltcondx if not already in dataset
    dstrgrpnm <- findnm("DSTRBGRP", pltcondxcols, returnNULL=TRUE)
    
    if (is.null(dstrgrpnm)) {
      dstrbcd1nm <- findnm("DSTRBCD1", pltcondxcols, returnNULL=TRUE)
      
      ref_dstrbcd <- ref_codes[ref_codes$VARIABLE == "DSTRBCD", c("VALUE", "GROUPCD")]
      names(ref_dstrbcd) <- c("DSTRBCD1", "DSTRBGRP")
      if (lower) names(ref_dstrbcd) <- tolower(names(ref_dstrbcd))
      
      pltcondx <- merge(pltcondx, ref_dstrbcd, by=dstrbcd1nm, all.x=TRUE)
      newcols <- c(newcols, ifelse(lower, "dstrbgrp", "DSTRBGRP"))
      
      if (popType %in% c("CHNG", "GRM")) {
        prevnm <- ifelse(lower, "prev_", "PREV_")
        names(ref_dstrbcd) <- paste0(prevnm, names(ref_dstrbcd))
        
        pltcondx <- merge(pltcondx, ref_dstrbcd, by=paste0(prevnm, dstrbcd1nm), all.x=TRUE)
        newcols <- c(newcols, ifelse(lower, "prev_dstrbgrp", "PREV_DSTRBGRP"))
      }  
    }
    
    ## Move new columns to end of table
    setcolorder(pltcondx, c(pltcondxcols, newcols))
    pltcondflds <- c(pltcondflds, newcols)
    setkeyv(pltcondx, pltcondxkey)
  }
  
  if (savepltids) {
    message("saving pltids...")
    outlst$out_layer <- "pltids"
    if (!append_layer) index.unique.pltids <- c(projectid, puniqueid)
    datExportData(pltidsadj, savedata_opts = outlst)
  }
  
  ## Build list of data to return
  ###################################################################################
  returnlst$popType <- popType
  
  returnlst <- append(returnlst, list(
    pltidsadj = pltidsadj, pltcondx=pltcondx, 
    pltcondflds = pltcondflds, 
    pjoinid = pjoinid,
    cuniqueid = cuniqueid, pltassgnid = pltassgnid,
    condid = condid, ACI = ACI,
    areawt = areawt, areawt2 = areawt2, adjcase = adjcase,
    dbqueries = dbqueries, dbqueriesWITH = dbqueriesWITH,
    pltassgnx = pltassgnx, dunitlut = data.table(dunitlut),
    dunitarea = dunitarea, npixels = npixels,
    npixelvar = npixelvar, estvar.area = estvar.area,
    areavar = areavar, areaunits = areaunits, 
    dunitvar = dunitvar, dunitvars = dunitvars,
    plotsampcnt = plotsampcnt, condsampcnt = condsampcnt,
    states = states, invyrs = invyrs, adj = adj,
    P2POINTCNT = P2POINTCNT, plotunitcnt = plotunitcnt))

  if (popType == "VOL") {
    if (!is.null(treex)) {
      returnlst$treex <- treex
      returnlst$tuniqueid <- tuniqueid
      returnlst$adjtree <- adjtree
    }
    if (!is.null(seedx)) {
      returnlst$seedx <- seedx
    }
  }
  if (!is.null(unitwarnlut)) {
    returnlst$unitwarnlut <- unitwarnlut
  }
  
  if (!is.null(popevalid)) {
    returnlst$evalid <- popevalid
  }
  if (adj != "none") {
    returnlst$adjfactors <- adjfactors
    returnlst$adjvarlst <- adjvarlst
  }
  
  returnlst$prednames <- prednames
  returnlst$predfac <- predfac
  returnlst$largebnd.unique <- largebnd.unique


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

  ## Save data frames
  ##################################################################
  if (savedata) {
    datExportData(pltidsadj, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="condx",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    datExportData(pltcondx, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="pltcondx",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))

    if (!is.null(treex)) {
      datExportData(treex, 
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="treex",
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE))
    }
    if (!is.null(seedx)) {
      datExportData(seedx, 
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer="seedx",
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE))
    }
    
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
    datExportData(dunitarea, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="dunitarea",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
    datExportData(dunitlut, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="dunitlut",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE))
  }

  
  ## Save data frames
  ##################################################################
  if (returndata) {
    returnlst$popdatindb <- FALSE
  } else {
    returnlst$popdatindb <- TRUE
    
    if (savedata) {
      if (outlst$out_fmt == "sqlite") {
        returnlst$pop_fmt <- "sqlite"
        returnlst$pop_dsn <- file.path(outlst$outfolder, outlst$out_dsn)
        returnlst$pop_schema <- NULL
      }
      
      message("saving pltassgnx...")
      outlst$out_layer <- "pltassgn"
      datExportData(pltassgnx, 
                    savedata_opts = outlst)
      
      message("saving dunitarea...")
      outlst$out_layer <- "dunitarea"
      datExportData(dunitarea, 
                    savedata_opts = outlst)
      
      rm(pltassgnx)
      rm(dunitarea)
      
      
      # if (popType %in% c("TREE", "GRM")) {
      #   message("saving REF_SPECIES...")
      #   outlst$out_layer <- "REF_SPECIES"
      #   datExportData(REF_SPECIES,
      #                 savedata_opts = outlst)
      # }
      
      
      if (!is.null(vcondsppx)) {
        message("saving vcondsppx...")
        outlst$out_layer <- "vcondsppx"
        datExportData(vcondsppx, 
                      savedata_opts = outlst)
        rm(vcondsppx)
        # gc()
      }
      if (!is.null(vcondstrx)) {
        message("saving vcondstrx...")
        outlst$out_layer <- "vcondstrx"
        datExportData(vcondstrx, 
                      savedata_opts = outlst)
        rm(vcondstrx)
      }
      
    } else if (datindb) {
      
      returnlst$pop_fmt <- datsource
      returnlst$pop_dsn <- dsn
      returnlst$pop_schema <- schema
      returnlst$popconn <- dbconn
    }
  }
  
  return(returnlst)

}
