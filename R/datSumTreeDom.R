#' Data - Aggregates numeric tree data by tree domain (i.e. species) to plot or
#' condition-level.
#' 
#' Aggregates numeric tree domain data (e.g., SPCD) to plot or condition,
#' including options for filtering tree data or extrapolating to plot acre by
#' multiplying by TPA. Includes options for generating barplots, proportion
#' data, and cover data.
#' 
#' If variable = NULL, then it will prompt user for input.
#' 
#' If you want to get trees-per-acre information aggregated to plot or
#' condition level, you need to include a TPA variable in tree table. \cr For
#' tsumvars = GROWCFGS, GROWBFSL, GROWCFAL, FGROWCFGS, FGROWBFSL, or FGROWCFAL,
#' you must have TPAGROW_UNADJ \cr For tsumvars = MORTCFGS, MORTBFSL, MORTCFAL,
#' FMORTCFGS, FMORTBFSL, or FMORTCFAL, you must have TPAMORT_UNADJ \cr For
#' tsumvars = REMVCFGS, REMVBFSL, REMVCFAL, FREMVCFGS, FREMVBFSL, or FREMVCFAL,
#' you must have TPAREMV_UNADJ \cr
#' 
#' If you want to adjust plot-level information by condition proportions, you
#' need to include CONDID & CONDPROP_UNADJ in cond or tree table. \cr
#' 
#' If you want to adjust the aggregated tree data by the area of the strata
#' (estimation unit), you need to either have a variable in your tree data
#' named adjfact or you need to included the following variables in your
#' datasets: \cr Condition table: STATECD, CONDID, STRATA, ESTUNIT,
#' SUBPPROP_UNADJ, MICRPROP_UNADJ (if microplot trees) MACRPROP_UNADJ (if
#' macroplot trees). \cr Tree table: TPA_UNADJ
#' 
#' All trees where DIA=NA are removed from analysis. These are trees that were
#' remeasured but are no longer in inventory (ex. a tree that is dead and not
#' standing in the current inventory).
#' 
#' @param tree Data frame or comma-delimited file (*.csv). The tree-level table
#' with tree domain data.
#' @param seed Data frame or comma-delimited file (*.csv). The seedling table
#' with tree seedling counts. Only applicable for counts (tsumvar="PLT_CN").
#' @param cond Data frame or comma-delimited file (*.csv). Condition-level
#' table to join the aggregated tree data to, if bycond=TRUE. This table also
#' may be used for condition proportion or strata variables used if adjcond or
#' adjstrata = TRUE (See details below).  This table is optional. If included,
#' CONDID must be present in table.
#' @param plt Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Plot-level table to join the aggregated tree data to, if bycond=FALSE. This
#' table is optional.
#' @param subp_cond Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot condition-level table to use to sum condition proportions, 
#' if bysubp=TRUE. 
#' @param subplot Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot-level table to used to calculate adjustment factors, to remove 
#' nonsampled conditions (SUBP_STATUS_CD = 3). This table is optional.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param dbconn Open database connection.
#' @param dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param bycond Logical. If TRUE, data are aggregated to the condition level
#' (by: uniqueid, CONDID). If FALSE, data are aggregated to the plot level (by:
#' uniqueid).
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param tsumvar String. Name of the variable to aggregate (e.g., "BA"). For
#' summing number of trees, use tsumvar="TPA_UNADJ" with tfun=sum.
#' @param seedlings String. ('Y', 'N', 'only') If seedlings = 'Y', add
#' seedlings to summary ('TPA_UNADJ' %in% tsumvarlst). If seedlings = 'N',
#' do not add seedlings. If seedlings = 'only', only include seedlings.
#' @param woodland String. ('Y', 'N', 'only') If woodland = 'Y', include  
#' woodland tree species where measured. If woodland = 'N', only include 
#' timber species. See FIESTA::ref_species$WOODLAND ='Y/N'. If woodland = 'only', 
#' only include woodland species. If NULL, use whatever is in table.
#' @param tfilter String. A filter to subset the tree data before aggregating
#' (e.g., "STATUSCD == 1"). This must be in R syntax. If tfilter=NULL, user is
#' prompted.  Use tfilter="NONE" if no filters.
#' @param tdomvar String. The tree domain (tdom) variable used to aggregate by
#' (e.g., "SPCD", "SPGRPCD").
#' @param tdomvarlst String (vector). List of specific tree domains of tdomvar
#' to aggregate (e.g., c(108, 202)). If NULL, all domains of tdomvar are used.
#' @param tdomvar2 String. A second tree domain variable to use to aggregate by
#' (e.g. "DIACL").  The variables, tdomvar and tdomvar2 will be concatenated
#' before summed.
#' @param tdomvar2lst String (vector). List of specific tree domains of
#' tdomvar2 to aggregate.  If NULL, all domains of tdomvar2 are used.
#' @param tdomprefix String. The prefix used for naming the aggregated tree
#' data, before numeric codes (e.g., "SP" = SP102, SP746).
#' @param tdombarplot Logical. If TRUE and pivot=TRUE, calls datBarplot() and
#' outputs a barplot of tdom distributions. If savedata=TRUE, barplots are
#' written to outfolder.
#' @param tdomtot Logical. If TRUE and pivot=TRUE a total of all tree domains
#' in tdomvarlst is calculated and added to output data frame.
#' @param tdomtotnm String. If tdomtot=TRUE, the variable name for the total
#' column in output data frame. If NULL, the default will be tdomvar + 'TOT'.
#' @param bydomainlst String (vector). Categorical domain variables not in
#' tdomvar/tdomvar2. Variables must be in tree table or plt/cond table if tables 
#' are provided.
#' @param FIAname Logical. If TRUE, changes names of columns for SPCD and
#' SPGRPCD from code to FIA names.
#' @param spcd_name String. Output name type if tdomvar or tdomvar2 = "SPCD"
#' ('COMMON', 'SCIENTIFIC', 'SYMBOL').
#' @param pivot Logical. If TRUE, tdomvar data are transposed (pivoted) to
#' separate columns.
#' @param presence Logical. If TRUE, an additional table is output with tree
#' domain values as presence/absence (1/0).
#' @param proportion Logical. If TRUE and pivot=TRUE, an additional table will
#' be output with tree domain data as proportions of total tsumvar.
#' @param getadjplot Logical. If TRUE, adjustments are calculated for
#' nonsampled conditions on plot.
#' @param domclassify List. List for classifying domain variables in bydomainlst
#' (e.g., DIA = c(10,20,30)).
#' @param tderive List. List of derivative to add to output data (e.g., 
#' list(MEAN_DIA = 'AVG(DIA)', SDI = 'POWER(DIA / 10, 1.605)', 
#' QMD = 'SQRT(SUM(POWER(DIA,2) * 0.005454 * TPA_UNADJ) / (SUM(TPA_UNADJ)*0.005454))'))
#' @param pltidsWITHqry SQL query. A query identifying plots to sum (e.g., 
#' 'WITH pltids AS (SELECT cn AS PLT_CN FROM plot WHERE statecd=49 and INVYR=2018)')
#' @param pcwhereqry String. Plot/Condition filter if plot and/or cond table is 
#' included.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param tabIDs List of unique IDs corresponding to the tables. See
#' See help(tableIDs) for a list of options.
#' @param datSum_opts List. Options for summarizing tree data, such as TPA,
#' rounding, and adjusting TPA. See help(datSum_options()) for a list of 
#' options. 
#' @param database_opts List. Options for database, such as schema and 
#' password. See help(database_options()) for a list of options.  
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'tdomsum'. 
#' 
#' @return tdomdata - a list of the following objects:
#' 
#' \item{tdomdat}{ Data frame. Plot or condition-level table with aggregated
#' tree domain (tdom) attributes (filtered). } 
#' \item{tdomsum}{ Data frame. The tdom look-up table with data aggregated 
#' by species. } 
#' \item{tdomvar}{ String. Name of the tdom variable used to aggregate by. }
#' \item{tsumvar}{ String. Name of the aggregated output variable. } 
#' \item{tdomlst}{ Vector. List of the aggregated tree data in tdomdat. } 
#' \item{tdomdat.pres}{ Data frame. Plot or condition-level table with 
#' aggregated tree domain attributes represented as presence/absence (1/0). } 
#' \item{tdomdat.prop}{ Data frame. Plot or condition-level table with 
#' aggregated tree domain attributes represented as proportion of total by 
#' plot. } 
#' \item{tdomdat.cov}{ Data frame. Plot or condition-level table with 
#' aggregated tree domain attributes represented as percent cover, multipying 
#' cover attribute by tdom proportion by plot. }
#' 
#' If savedata=TRUE\cr - tdomdat will be saved to the outfolder
#' ('tdomprefix'_DAT.csv). \cr - a text file of input parameters is saved to
#' outfolder ('outfn'_parameters_'date'.txt). \cr - if presence=TRUE,
#' tdomdat.prop is saved to outfolder ('tdomprefix'_PRESDAT.csv) - if
#' proportion=TRUE, tdomdat.prop is saved to outfolder
#' ('tdomprefix'_PROPDAT.csv) - if cover=TRUE, tdomdat.prop is saved to
#' outfolder ('tdomprefix'_COVDAT.csv)
#' @note This function can be used to get tree domain data. This data can be
#' used for mapping tree domain distributions.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \donttest{
#'               
#' # Sum of Number of Live Trees by Species
#' datSumTreeDom(tree = FIESTA::WYtree,
#'               plt = FIESTA::WYplt, 
#'               bycond = FALSE, 
#'               tsumvar = "PLT_CN", 
#'               tdomtot = TRUE, 
#'               tdomprefix = "CNT", 
#'               tfilter = "STATUSCD==1",
#'               datSum_opts = list(tround = 0))
#'               
#' # Sum of Number of Live Trees by Species, Including Seedlings
#' datSumTreeDom(tree = WYtree,
#'               seed = WYseed, 
#'               bycond = FALSE, 
#'               tsumvar = "PLT_CN", 
#'               tdomtot = TRUE, 
#'               tdomprefix = "CNT", 
#'               datSum_opts = list(tround = 0))
#' }
#' @export datSumTreeDom
datSumTreeDom <- function(tree = NULL, 
                          seed = NULL, 
                          cond = NULL, 
                          plt = NULL,  
                          subp_cond = NULL,
                          subplot = NULL, 
                          datsource = "obj", 
                          dbconn = NULL,
                          dsn = NULL, 
                          bycond = FALSE, 
                          bysubp = FALSE, 
                          tsumvar = NULL, 
                          seedlings = "N", 
                          woodland = 'Y',
                          tfilter = NULL, 
                          tdomvar = "SPCD", 
                          tdomvarlst = NULL, 
                          tdomvar2 = NULL, 
                          tdomvar2lst = NULL, 
                          tdomprefix = NULL, 
                          tdombarplot = FALSE, 
                          tdomtot = FALSE, 
                          tdomtotnm = NULL, 
                          bydomainlst = NULL,
                          FIAname = FALSE, 
                          spcd_name = "COMMON",
                          pivot = TRUE, 
                          presence = FALSE, 
                          proportion = FALSE, 
                          getadjplot = FALSE, 
                          domclassify = NULL,
                          tderive = NULL,
                          pltidsWITHqry = NULL,
                          pcwhereqry = NULL,
                          savedata = FALSE,
                          tabIDs = tableIDs(),
                          datSum_opts = datSum_options(),
                          database_opts = NULL,
                          savedata_opts = NULL) {
  
  ####################################################################################
  ## DESCRIPTION: Aggregates tree domain data (ex. species) to condition or plot level  
  ##		for estimation, mapping, or exploratory data analyses. 
  ##
  ## 1. Set biomass and carbon variables for converting from pounds to tons (0.0005)
  ## 2. Checks input tables (tree, cond, plt)
  ## 3. Check unique identifiers (tuniqueid, cuniqueid, and puniqueid) and make sure
  ##		values and classes match
  ## Note: Condition table is needed if adjplot = TRUE, ACI = FALSE (COND_STATUS_CD)
  ########################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #gui <- ifelse(nargs() == 0, TRUE, FALSE)
  gui <- FALSE

  ## Set global variables  
  COND_STATUS_CD=COUNT=CONDPROP_UNADJ=V1=samenm=SUBP=NF_COND_STATUS_CD=
	seedx=tunits=TREECOUNT_CALC=cond.nonsamp.filter=ref_spcd=tdomvar2nm=concatvar <- NULL
  checkNApvars <- NULL
  checkNAcvars <- NULL
  checkNAtvars <- NULL
  seedclnm <- "<1"
  parameters <- FALSE
  ref_units <- FIESTAutils::ref_units
  ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry <- NULL
  cover <- FALSE
  pltsp = FALSE
  checkNA = FALSE
  returnDT = TRUE
  
  ## If gui.. set variables to NULL
  if (gui) bycond=tuniqueid=puniqueid=cuniqueid=ACI=TPA=tfun=tdomvar=tdomlst=
	tdombarplot=FIAname=addseed=proportion=presence=tdomtot=adjtree=tmp <- NULL


  ##################################################################
  ## SET VARIABLE LISTS
  ##################################################################
  biovars <- c("DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_BG", "DRYBIO_SAWLOG", 
               "DRYBIO_AG", "DRYBIO_STEM", "DRYBIO_STEM_BARK", "DRYBIO_STUMP_BARK",
               "DRYBIO_BOLE_BARK", "DRYBIO_BRANCH", "DRYBIO_FOLIAGE", "DRYBIO_SAWLOG_BARK",
			         "DRYBIOT", "DRYBIOM", "DRYBIOTB", "JBIOTOT")
  carbvars <- c("CARBON_BG", "CARBON_AG")

  ## SET VARIABLES TO CONVERT (from pounds to short tons.. * 0.0005)
  vars2convert <- c(biovars, carbvars, paste(biovars, "TPA", sep="_"), 
	paste(carbvars, "TPA", sep="_"))

  growvars <- c("TPAGROW_UNADJ", "GROWCFGS", "GROWBFSL", "GROWCFAL", "FGROWCFGS", 
	              "FGROWBFSL", "FGROWCFAL")
  mortvars <- c("TPAMORT_UNADJ", "MORTCFGS", "MORTBFSL", "MORTCFAL", "FMORTCFGS", 
	              "FMORTBFSL", "FMORTCFAL")
  remvars <- c("TPAREMV_UNADJ", "REMVCFGS", "REMVBFSL", "REMVCFAL", "FREMVCFGS", 
	             "FREMVBFSL", "FREMVCFAL")
  tpavars <- c("TPA_UNADJ", "TPAMORT_UNADJ", "TPAGROW_UNADJ", "TPAREMV_UNADJ")
  propvar <- "CONDPROP_UNADJ"

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumTreeDom)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts = savedata_opts, 
                datSum_opts = datSum_opts)
  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
                         savedata_opts = savedata_opts,
                         datSum_opts = datSum_opts))
  savedata_opts <- optslst$savedata_opts 
  datSum_opts <- optslst$datSum_opts

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  bydomainlst <- unique(c(tdomvar, tdomvar2, bydomainlst))

  ### Check tsumvar 
  ###########################################################  
  notdomdat <- ifelse(is.null(tsumvar) && presence, TRUE, FALSE)
  if (is.null(tsumvar)) {
    if (presence) tsumvar <- "TPA_UNADJ"
  } else if (tsumvar == "PLT_CN") {
    tsumvar <- "TPA_UNADJ"
  } else if (!is.null(tderive)) {
    
    ## Check tderive
    if (!is.null(tderive)) {
      if (!all(is.list(tderive), length(tderive) == 1, !is.null(names(tderive)))) {
        message("tderive must be a named list with one element")
        stop()
      }
    }
    if (!is.null(tsumvar) && !is.null(tderive)) {
      message("both tsumvar and tderive are populated... only 1 sum variable allowed")
      stop("")
    }
  }
    
  ## Check FIAname
  FIAname <- pcheck.logical(FIAname, varnm="FIAname", title="FIA name?", 
                              first="NO", gui=gui, stopifnull=TRUE)
  if (!any(c(tdomvar, tdomvar2) %in% c("SPCD", "SPGRPCD")) && FIAname) {
    message("FIAname is only available for tdomains in(SPCD, SPGRPCD)")
    FIAname <- FALSE
  }
    
  ## Check spcd_name
  if (FIAname && any(c(tdomvar, tdomvar2) == "SPCD")) {
    spcd_namelst <- c("COMMON", "SCIENTIFIC", "SYMBOL")
    spcd_name <- pcheck.varchar(var2check=spcd_name, varnm="spcd_name", 
                    checklst=spcd_namelst, gui=gui, caption="SPCD name type?") 
  }
  
  ## Check checkNA
  NAto0 <- pcheck.logical(datSum_opts$NAto0, varnm="NAto0", title="Convert NA to 0?", 
                          first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE
  
  ## Check pivot
  pivot <- pcheck.logical(pivot, varnm="pivot", title="Pivot columns?", 
                          first="NO", gui=gui)

  ## Check presence
  presence <- pcheck.logical(presence, varnm="presence", title="Presence only?", 
                               first="NO", gui=gui)
    
  ## Check proportion (proportion of all tree domains, values 0-1)
  proportion <- pcheck.logical(proportion, varnm="proportion", title="Proportions?", 
                                 first="NO", gui=gui)
    
  ## Check cover (proportion of LIVE_CANOPY_CVR_PCT per tree domain)
  ## Note: total of all tree domains will equal LIVE_CANOPY_CVR_PCT for plot/condition
  cover <- pcheck.logical(cover, varnm="cover", title="As Cover?", 
                            first="NO", gui=gui)
  
  if (cover) {
    bydomainlst <- c("LIVE_CANOPY_CVR_PCT", "CCLIVEPLT", bydomainlst)
    proportion <- TRUE
  }
  
  ## Check tdombarplot
  tdombarplot <- pcheck.logical(tdombarplot, varnm="tdombarplot", 
                                title="Barplot of tdomains?", first="NO", gui=gui)
  
  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(savedata_opts = savedata_opts, 
                            dbconnopen=TRUE, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "tdomsum"
    }
    outconn = outlst$out_conn
  }


  ##############################################################################
  ############################################################################## 
  ### DO WORK
  ##############################################################################
  ##############################################################################
  sumdat <- 
    datSumTree(tree = tree, seed = seed, 
               cond = cond, plt = plt, 
               subp_cond = subp_cond, subplot = subplot, 
               datsource = datsource, 
               dsn = dsn, 
               dbconn = dbconn, 
               bycond = bycond, 
               bysubp = bysubp, 
               bydomainlst = bydomainlst,
               tsumvarlst = tsumvar, 
               seedlings = seedlings,
               woodland = woodland,
               tfilter = tfilter, 
               getadjplot = getadjplot, 
               domclassify = domclassify, 
               tderive = tderive,
               pltidsWITHqry = pltidsWITHqry,
               pcwhereqry = pcwhereqry,
               datSum_opts = datSum_opts,
               tabIDs = tabIDs,
               database_opts = database_opts)
  tdomtree <- sumdat$treedat
  tsumvarnm <- sumdat$sumvars
  tsumuniqueid <- sumdat$tsumuniqueid
  treeqry <- sumdat$treeqry
  domainlst <- sumdat$domainlst     ## new pc and tree variables if classified
  tdomainlst <- sumdat$tdomainlst   ## original tree variables
  pcdomainlst <- sumdat$pcdomainlst ## original pc variables
  classifynmlst <- sumdat$classifynmlst
  tround <- sumdat$tround

  if (length(tsumvarnm) > 1) {
    tsumnm=tsumvarnm <- tsumvarnm[length(tsumvarnm)]
  } else {
    tsumnm=tsumvarnm <- tsumvarnm
  }
  
  if (!is.null(classifynmlst[[tdomvar]])) {
    tdomvar <- classifynmlst[[tdomvar]]
  }
  if (!is.null(tdomvar2) && !is.null(classifynmlst[[tdomvar2]])) {
    tdomvar2 <- classifynmlst[[tdomvar2]]
  }

  if (any(pcdomainlst %in% names(classifynmlst))) {
    pcdomain <- pcdomainlst[!pcdomainlst %in% names(classifynmlst)]
    bydomainlst <- c(pcdomain, unlist(classifynmlst[pcdomainlst]))
  } else {
    bydomainlst <- pcdomainlst
  }
  if (any(tdomainlst %in% names(classifynmlst))) {
    tdomain <- tdomainlst[!tdomainlst %in% names(classifynmlst)]
    bydomainlst <- c(bydomainlst, tdomain, unlist(classifynmlst[tdomainlst]))
  } else {
    bydomainlst <- c(bydomainlst)
  }

  ## Get unique values of tdomvar
  tdoms <- sort(unique(tdomtree[[tdomvar]]))

  ## Check tdomvarlst
  nbrtdoms <- length(tdoms)
  if (is.null(tdomvarlst)) {
    ## get tdomvarlst
    if (gui) {
      tdomvarlst <- select.list(as.character(tdoms), title="Tree domains?", multiple=TRUE)
      if (length(tdomvarlst) == 0) stop("")
      if (is.numeric(tdoms))  tdomvarlst <- as.numeric(tdomvarlst)
    } else {
      tdomvarlst <- tdoms
    }
  } else { 
    if (any(!tdomvarlst %in% tdoms)) {
      tdom.miss <- tdomvarlst[which(!tdomvarlst %in% tdoms)]
      if (length(tdom.miss) == 1 && addseed && tdom.miss == seedclnm) {
        tdom.miss <- NULL
      }
      if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
        message("tdomvarlst domain values not in tree table: ", toString(tdom.miss))
      }
      if (gui) {
        tdomvarlst <- select.list(as.character(tdoms), title="Tree domain(s)", multiple=TRUE)
      }      
      if (length(tdomvarlst) == 0) {
        stop("")
      }
      if (is.numeric(tdoms)) {
        tdomvarlst <- as.numeric(tdomvarlst) 
      }
    }  
    tdomtree <- tdomtree[tdomtree[[tdomvar]] %in% tdomvarlst,]
  }

  ## Check if want to include totals (tdomtot) and check for a totals name
  ## Check tdomtot
  tdomtot <- pcheck.logical(tdomtot, varnm="tdomtot", "Total for domains?", 
		                        first="NO", gui=gui)

  ## Check tdomtotnm
  if (tdomtot) {
    if (!is.null(tdomtotnm) & !is.character(tdomtotnm)) {
      warning("tdomtotnm is not valid... using default")
    }
  }

  ## GETS name for tdomvar
  #####################################################################
  ## If tdomvar2 exists, concatenate the columns to one column (if pivot=TRUE)
  ## treex is the tree table after filtered tree domains
  flag <- ifelse(datSum_opts$NAto0, "0", "")
  if (FIAname) {
    if (tdomvar == "SPCD") {
      tdomdata <- datLUTspp(x = tdomtree, spcdname = spcd_name)
      ref_spcd <- tdomdata$ref_spcd
    } else {    
      tdomdata <- datLUTnm(tdomtree, xvar=tdomvar, LUTvar="VALUE", FIAname=TRUE)
    }
    tdomtree <- tdomdata$xLUT
    tdomvarnm <- tdomdata$xLUTnm
    setkeyv(tdomtree, tsumuniqueid) 
    tdomvarlut <- unique(tdomtree[,c(tdomvar, tdomvarnm), with=FALSE]) 

    tdomvarlst2 <- tdomvarlut[match(tdomvarlst, tdomvarlut[[tdomvar]]), 
		              tdomvarnm, with=FALSE][[1]]

  } else if (is.numeric(tdomtree[[tdomvar]]) && !is.null(tdomprefix)) {

    tdomvarnm <- paste0(tdomvar, "NM") 
    maxchar <- max(sapply(tdomvarlst, function(x) {nchar(x)}))

    if (flag != "") {
      tdomtree[, (tdomvarnm):= paste0(tdomprefix, formatC(get(eval(tdomvar)), 
			          width=maxchar, flag=flag))]
      tdomvarlst2 <- paste0(tdomprefix, formatC(tdomvarlst, width=maxchar, flag=flag))
	  } else {
	    tdomtree[, (tdomvarnm) := paste0(tdomprefix, get(eval(tdomvar)))]
      tdomvarlst2 <- paste0(tdomprefix, tdomvarlst)
    }	  
  } else {
    tdomvarnm <- tdomvar
    #tdomvarlut <- data.frame(tdomvarlst, stringsAsFactors=FALSE)
    #names(tdomvarlut) <- tdomvarnm
    tdomvarlst2 <- as.character(tdomvarlst) 
  }
  sumbyvars <- unique(c(tsumuniqueid, pcdomainlst, tdomvarnm))


  ## GET tdomvarlst2 or CHECK IF ALL tree domains IN tdomvar2lst ARE INCLUDED IN tdomvar2.
  if (!is.null(tdomvar2)) {
    tdoms2 <- sort(unique(tdomtree[[tdomvar2]]))

    if (is.null(tdomvar2lst)) {
      ## GET tdomvar2lst
      if (gui) {
        tdomvar2lst <- select.list(as.character(tdoms2), title="Tree domains?", multiple=TRUE)
        if (length(tdomvar2lst) == 0) stop("")
        if (is.numeric(tdoms2))  tdomvar2lst <- as.numeric(tdomvar2lst)
      }else{
        tdomvar2lst <- tdoms2
      }
    } else { 
      if (any(!tdomvar2lst %in% unique(tdomtree[[tdomvar2]]))) {
        tdom.miss <- tdomvar2lst[!tdomvar2lst %in% unique(tdomtree[[tdomvar2]])]
        if (length(tdom.miss) == 1 && addseed && tdom.miss == seedclnm) {
          tdom.miss <- NULL
        }
        if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
          message("tdomvar2lst domain values not in tree table: ", toString(tdom.miss))
        }
        if (gui) {
          tdomvar2lst <- select.list(as.character(tdoms2), title="Tree domain(s)", multiple=TRUE)
        }
        if (length(tdomvar2lst) == 0) {
          stop("")
        }
        if (is.numeric(tdoms2))  {
          tdomvar2lst <- as.numeric(tdomvar2lst)
        }
      }
    }

    tdomtree <- tdomtree[tdomtree[[tdomvar2]] %in% tdomvar2lst,]
    tdomvar2nm <- tdomvar2
    if (FIAname) {
      if (tdomvar2 == "SPCD") {
        tdomdata <- datLUTspp(x=tdomtree, spcdname=spcd_name)
        ref_spcd <- tdomdata$ref_spcd
      } else {
        tdomdata <- datLUTnm(tdomtree, xvar=tdomvar2, LUTvar="VALUE", FIAname=TRUE)
      }
      tdomtree <- tdomdata$xLUT
      tdomvar2nm <- tdomdata$xLUTnm
    } 
    
    if (is.numeric(tdomtree[[tdomvar2]])) {
      
      maxchar2 <- max(sapply(tdomvar2lst, function(x) {nchar(x)}))
      if (!is.null(tdomprefix)) {
        tdomvar2nm <- paste0(tdomvar2, "NM") 
        if (flag != "") {
          tdomtree[, (tdomvar2nm) := paste0(tdomprefix, formatC(get(eval(tdomvar2)),
			             width=maxchar2, flag=flag))]
          tdomvarlst2 <- paste0(tdomprefix, formatC(tdomvarlst, width=maxchar2, flag=flag))
	      } else {
	        tdomtree[, (tdomvar2nm) := paste0(tdomprefix, get(eval(tdomvar2)))]
          tdomvarlst2 <- paste0(tdomprefix, tdomvar2lst)
        }
      } 
    }
    
    if (pivot) {
      concatvar <- paste0(tdomvar, "#", tdomvar2)
      tdomtree[, (concatvar) := paste0(tdomtree[[tdomvarnm]], "#", tdomtree[[tdomvar2]])] 
      sumbyvars <- unique(c(tsumuniqueid, pcdomainlst, concatvar))
    } else {
      sumbyvars <- unique(c(sumbyvars, tdomvar2nm))
    }
  }

  ## GET NAME FOR SUMMED TREE VARIABLE FOR FILTERED TREE DOMAINS 
  if (tdomtot && is.null(tdomtotnm) && pivot) {
    if (is.null(tdomprefix)) {
      tdomtotnm <- paste0(tsumvarnm, "TOT")
    } else {
      tdomtotnm <- paste0(tdomprefix, "TOT")
    }
  }

  ## GET NAME FOR SUMMED TREE VARIABLE FOR ALL TREE DOMAINS (IF PROPORTION = TRUE)
  if (proportion) denomvar <- paste0(tsumvarnm, "_ALL")

  ## Sum tree (and seed) by tdomvarnm
  #####################################################################
  tdomtreef <- tdomtree[, lapply(.SD, sum, na.rm=TRUE), by=sumbyvars, .SDcols=tsumvarnm]
  setkeyv(tdomtreef, tsumuniqueid)

  ## Generate tree domain look-up table (tdomvarlut)
  #####################################################################
  nvar <- ifelse(bycond, "NBRCONDS", "NBRPLOTS")

  if (!is.null(concatvar)) {
    tdomvarlut <- tdomtreef[, list(sum(.SD, na.rm=TRUE), .N), by=concatvar, .SDcols = tsumnm]
    names(tdomvarlut) <- c(concatvar, tsumnm, nvar)
    tdomvarlut <- tdomvarlut[, (c(tdomvarnm, tdomvar2nm)) := tstrsplit(get(concatvar), "#", fixed=TRUE)]
    tdomvarlut[[concatvar]] <- NULL
  } else {
    tdomvarlut <- tdomtreef[, list(sum(.SD, na.rm=TRUE), .N), by=tdomvarnm, .SDcols = tsumnm]
    names(tdomvarlut) <- c(tdomvarnm, tsumnm, nvar)
  
    if (!is.null(tdomvar2)) {
      tdomvar2lut <- tdomtreef[, list(sum(.SD, na.rm=TRUE), .N), by=tdomvar2nm, .SDcols = tsumnm]
      names(tdomvar2lut) <- c(tdomvar2nm, tsumnm, nvar)
    }
  }

  ## Add reference names to tdomvarlut if SPCD
  #####################################################################
  if (any(c(tdomvarnm, tdomvar2nm) == "SPCD")) {
    refcol <- ifelse(spcd_name == "COMMON", "COMMON_NAME", 
                   ifelse(spcd_name == "SYMBOL", "SPECIES_SYMBOL", 
                          ifelse(spcd_name == "SCIENTIFIC", "SCIENTIFIC_NAME")))
    if (tdomvarnm == "SPCD") {
      tdomvarlut <- merge(FIESTAutils::ref_species[, c("SPCD", refcol)], 
                        tdomvarlut, by="SPCD")
    } else {
      tdomvar2lut <- merge(FIESTAutils::ref_species[, c("SPCD", refcol)], 
                          tdomvar2lut, by="SPCD")
    }
  } else if (any(c(tdomvarnm, tdomvar2nm) == "SPGRPCD")) {
    ref_spgrpcd <- ref_codes[ref_codes$VARIABLE == "SPGRPCD", c("VALUE", "MEANING")]
    
    if (tdomvarnm == "SPGRPCD") {
      tdomvarlut <- merge(ref_spgrpcd, tdomvarlut, by.x="VALUE", by.y="SPGRPCD")
      names(tdomvarlut)[names(tdomvarlut) %in% c("VALUE", "MEANING")] <- c("SPGRPCD", "SPGRPNM")
    } else {
      tdomvar2lut <- merge(ref_spgrpcd, tdomvar2lut, by.x="VALUE", by.y="SPGRPCD")
      names(tdomvar2lut)[names(tdomvar2lut) %in% c("VALUE", "MEANING")] <- c("SPGRPCD", "SPGRPNM")
    }
  }        

  ######################################################################## 
  ## If pivot=FALSE
  ######################################################################## 
  if (!pivot) {
    tdoms <- tdomtreef
    tdomscolstot <- tsumvarnm
    tdomscols <- sort(unique(tdomtreef[[tdomvarnm]]))

  } else {

    ######################################################################## 
    ## If pivot=TRUE, aggregate tree domain data
    ######################################################################## 
    yvar <- ifelse (is.null(tdomvar2), tdomvarnm, concatvar)
    tdoms <- datPivot(tdomtreef, pvar = tsumnm, 
                      xvar = c(tsumuniqueid, pcdomainlst), yvar = yvar,
                      pvar.round = tround, returnDT = TRUE)
  	tdoms <- setDT(tdoms)

    ## check if tree domain in tdomlst.. if not, create column with all 0 values
    tdomscols <- colnames(tdoms)[!colnames(tdoms) %in% sumbyvars]
    if (is.null(concatvar)) {
      UNMATCH <- tdomvarlst2[is.na(match(tdomvarlst2, tdomscols))] 
      if (length(UNMATCH) > 0) {
        tdoms[, (UNMATCH) := 0]
        tdomvarlst2 <- c(tdomvarlst2, UNMATCH)
      }
    } else {
      tdomvarlst2 <- tdomscols
    }

    ## ADD TOTAL OF TREE DOMAINS IN tdomvarlst 
    if ((tdomtot || proportion || cover)) {
      ## Sum the total tree domains in tdomvarlst after any filtering by plot
      tdoms[, (tdomtotnm) := round(rowSums(.SD, na.rm=TRUE), tround), .SDcols=tdomvarlst2]
      tdomscolstot <- c(tdomvarlst2, tdomtotnm)
    } else {
      tdomscolstot <- tdomvarlst2
    }

    ## Create a table of proportions for each tdom by total by plot
    if (proportion) {
      tdoms.prop <- tdoms[, lapply(.SD, function(x, tdomtotnm) {
                                            round(x / get(eval(tdomtotnm)))
                                        }, tdomtotnm), 
                            by=key(tdoms), .SDcols=tdomscolstot]
      
      setcolorder(tdoms.prop, c(key(tdoms.prop), tdomscolstot))
    }

    ## Create a table of presence/absence (1/0) by plot
    if (presence) {
      tdoms.pres <- tdoms[, lapply(.SD, function(x) x / x), by=key(tdoms), .SDcols=tdomscolstot]
      tdoms.pres[is.na(tdoms.pres)] <- 0        
      setcolorder(tdoms.pres, c(key(tdoms.pres), tdomscolstot))
    }

    ## GENERATE TREE DOMAIN LOOK-UP TABLE (tdomvarlut)
    ## get total tsumvar and number of conditions by tdom and add to tdomvarlut
#    nvar <- ifelse(bycond, "NBRCONDS", "NBRPLOTS")
#    sumnm <- ifelse(is.null(tdomprefix) || tdomprefix=="", paste(tsumvar, "SUM", sep="_"),
#		paste(tdomprefix, "SUM", sep = "_")) 
#
#    sumtdomvar <- sapply(tdoms[, tdomvarlst2, with=FALSE], tfun)
#    tdomvarlut[[sumnm]] <- sumtdomvar[match(tdomvarlut[[tdomvarnm]], names(sumtdomvar))]
#    tdomvarlut[[nvar]] <- sapply(tdoms[, tdomvarlst2, with=FALSE], 
#		function(x) sum(x > 0))
  } 

  ## Generate barplot
  if (tdombarplot) {
    ## Frequency
    ylabel <- ifelse(bycond, "Number of Conditions", "Number of Plots")
    datBarplot(x = tdomvarlut, 
               xvar = tdomvarnm, 
               yvar = tsumvarnm, 
               savedata = savedata,
               outfolder=outfolder, 
               ylabel = tsumvarnm)

    ## Summed variable
    datBarplot(x = tdomvarlut, 
               xvar = tdomvarnm, 
               yvar = tsumvarnm, 
               savedata = savedata,
               outfolder = outfolder, 
               ylabel = tsumvarnm) 
  }

  ## Merge to cond or plot
  ###################################
#   if (bycond && !nocond) {
# 
#     ## Check for duplicate names
#     matchnames <- sapply(tdomscolstot, checknm, condnames) 
#     setnames(tdoms, tdomscolstot, matchnames)
# 
#     ## Check if class of cuniqueid matches class of cuniqueid
#     tabs <- check.matchclass(condx, tdoms, c(cuniqueid, condid))
#     condx <- tabs$tab1
#     tdoms <- tabs$tab2
#     
#     ## Merge summed data to cond table
#     sumtreef <- merge(condx, tdoms, all.x=TRUE, by=c(cuniqueid, condid))
#     if (NAto0) {
#       for (col in tdomscolstot) set(sumtreef, which(is.na(sumtreef[[col]])), col, 0)
#       #sumtreef[is.na(sumtreef)] <- 0
#     }
# 
#     ## Merge proportion table to cond table
#     if (proportion) {
#       sumtreef.prop <- merge(condx, tdoms.prop, all.x=TRUE)
#       if (NAto0) {
#         for (col in tdomscolstot) set(sumtreef.prop, which(is.na(sumtreef.prop[[col]])), col, 0)
#       }
#     }
#     ## Merge presence table to cond table
#     if (presence) {
#       sumtreef.pres <- merge(condx, tdoms.pres, all.x=TRUE)
#       if (NAto0) {
#         for (col in tdomscolstot) set(sumtreef.pres, which(is.na(sumtreef.pres[[col]])), col, 0)
#       }
#     }
#     ## Create a table of cover (absolute) based on proportion table and live canopy cover for cond
#     if (cover) {
#       sumtreef.cov <- copy(sumtreef.prop)
#       if (NAto0) {
#         for (col in tdomscolstot) {set(sumtreef.cov, i=NULL, j=col, 
# 				value=round(sumtreef.cov[[col]] * sumtreef.cov[[covervar]])) }
#       }
#     }
# 
#   } else if (!noplt) {  ## Plot-level
# 
#     ## Check for duplicate names
#     matchnames <- sapply(tdomscolstot, checknm, names(pltx)) 
#     setnames(tdoms, tdomscolstot, matchnames)
# 
#     ## Check if class of cuniqueid matches class of cuniqueid
#     tabs <- check.matchclass(pltx, tdoms, puniqueid, cuniqueid)
#     pltx <- tabs$tab1
#     tdoms <- tabs$tab2
# 
#     ## Merge summed data to plt table
#     setkeyv(tdoms, tuniqueid)
#     sumtreef <- merge(pltx, tdoms, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
#     if (NAto0) {
#       for (col in tdomscolstot) set(sumtreef, which(is.na(sumtreef[[col]])), col, 0)
#     }
#  
#     ## Merge proportion table to plt table
#     if (proportion) {
#       setkeyv(tdoms.prop, tuniqueid)
#       sumtreef.prop <- merge(pltx, tdoms.prop, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
#       if (NAto0) {
#         for (col in tdomscolstot) set(sumtreef.prop, which(is.na(sumtreef.prop[[col]])), col, 0)
#       }
#     }
# 
#     ## Merge presence table to plt table
#     if (presence) {
#       setkeyv(tdoms.pres, tuniqueid)
#       sumtreef.pres <- merge(pltx, tdoms.pres, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
#       if (NAto0) {
#         for (col in tdomscolstot) set(sumtreef.pres, which(is.na(sumtreef.pres[[col]])), col, 0)
#       }
#     }
# 
#     ## Create a table of cover (absolute) based on proportion table and live canopy cover for plot
#     if (cover) {
#       sumtreef.cov <- copy(sumtreef.prop)
#       if (NAto0) {
#         for (col in tdomscolstot) {set(sumtreef.cov, i=NULL, j=col, 
# 				value=sumtreef.cov[[col]] * sumtreef.cov[[covervar]]) }
#       }
#     }
#   } else {
    sumtreef <- tdoms

    if (proportion) sumtreef.prop <- tdoms.prop 
    if (presence) sumtreef.pres <- tdoms.pres
#  }

  if (savedata) {
    if (pltsp) {
      spExportSpatial(sumtreef, 
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=out_layer,
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer, 
                                add_layer=TRUE))
    } else {
      datExportData(sumtreef, dbconn = outconn, dbconnopen = TRUE,
          savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=out_layer,
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE)) 
    }
    
    if (proportion) {
      if (pltsp) {
        spExportSpatial(sumtreef.prop,
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=outsp_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=paste0(out_layer, "_prop"),
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer, 
                                add_layer=TRUE))
      } else {
        datExportData(sumtreef.prop, dbconn = outconn, dbconnopen = TRUE,
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=paste0(out_layer, "_prop"),
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE)) 
      }
    }
    if (presence) {
      if (pltsp) {
        spExportSpatial(sumtreef.pres, 
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=outsp_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=paste0(out_layer, "_pres"),
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer, 
                                add_layer=TRUE))
      } else {
        datExportData(sumtreef.pres, dbconn = outconn, dbconnopen = TRUE,
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=paste0(out_layer, "_pres"),
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE)) 
      }
    }
    if (cover) {
      if (pltsp) {
        spExportSpatial(sumtreef.cov,
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=outsp_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=paste0(out_layer, "_cov"),
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer, 
                                add_layer=TRUE))
      } else {
        datExportData(sumtreef.cov, dbconn = outconn, dbconnopen = TRUE,
            savedata_opts=list(outfolder=outfolder, 
                                out_fmt=out_fmt, 
                                out_dsn=out_dsn, 
                                out_layer=paste0(out_layer, "_cov"),
                                outfn.pre=outfn.pre, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite_layer,
                                append_layer=append_layer,
                                add_layer=TRUE)) 
      }
    }
    
    
    datExportData(tdomvarlut, dbconn = outconn, dbconnopen = TRUE,
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer=paste0(out_layer, "_lut"),
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            add_layer=TRUE))   
    
  
#     if (parameters) {
#       ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
#       ###########################################################
#       outfn.param <- paste(out_layer, "parameters", sep="_")
#       outparamfnbase <- paste(outfn.param, format(Sys.time(), "%Y%m%d"), sep="_")
#       outparamfn <- fileexistsnm(outfolder, outparamfnbase, "txt")
#   
#       tdomvarlstout <- addcommas(sapply(tdomvarlst, function(x) paste0("'", x, "'") ))
#       tdomvarlst2out <- addcommas(sapply(tdomvar2lst, function(x) paste0("'", x, "'") ))
#       strunitvars <- addcommas(sapply(strunitvars, function(x) paste0("'", x, "'") ))
# 
#       outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
#       cat(  "tree = ", as.character(bquote(tree)), "\n",
#       	"seed = ", as.character(bquote(seed)), "\n",
#       	"cond = ", as.character(bquote(cond)), "\n",
#       	"plt = ", as.character(bquote(plt)), "\n",
#       	"plt_dsn = \"", plt_dsn, "\"", "\n",
#       	"tuniqueid = \"", tuniqueid, "\"", "\n",
#       	"cuniqueid = \"", cuniqueid, "\"", "\n",
#       	"puniqueid = \"", puniqueid, "\"", "\n",
#       	"bycond = ", bycond, "\n",
#       	"condid = \"", condid, "\"", "\n",
#       	"bysubp = ", bysubp, "\n",
#       	"subpid = \"", subpid, "\"", "\n",
#       	"tsumvar = \"", tsumvar, "\"", "\n",
#       	"TPA = ", TPA, "\n",
#       	"tfun = ", noquote(tfunstr), "\n",
#       	"ACI = ", ACI, "\n",
#       	"tfilter = \"", tfilter, "\"", "\n",
#       	"lbs2tons = ", lbs2tons, "\n",
#       	"tdomvar = \"", tdomvar, "\"", "\n",
#       	"tdomvarlst = c(", tdomvarlstout, ")", "\n", 
#       	"tdomvar2 = \"", tdomvar2, "\"", "\n",
#       	"tdomvar2lst = c(", tdomvarlst2out, ")", "\n", 
#       	"tdomprefix = \"", tdomprefix, "\"", "\n",
#       	"tdombarplot = ", tdombarplot, "\n",
#       	"tdomtot = ", tdomtot, "\n",
#       	"tdomtotnm = \"", tdomtotnm, "\"", "\n",
#       	"FIAname = ", FIAname, "\n",
#       	"addseed = ", addseed, "\n",
#       	"presence = ", presence, "\n",
#       	"proportion = ", proportion, "\n",
#       	"cover = ", cover, "\n",
#       	"getadjplot = ", getadjplot, "\n",
#       	"adjtree = ", adjtree, "\n",
#       	"NAto0 = ", NAto0, "\n",
#       	"adjTPA = ", adjTPA, "\n",
#       	"savedata = ", savedata, "\n",
#       	"outfolder = \"", outfolder, "\"", "\n",
#       	"out_layer = ", out_layer, "\n",
#       	"outfn.date = ", outfn.date, "\n",
#       	"overwrite_dsn = ", overwrite_dsn, "\n",
#       	"tround = \"", tround, "\"", "\n", "\n",
#     	file = outfile, sep="")
# 
#     	cat(  "tdomdat <- datSumTreeDom(tree=tree, seed=seed, cond=cond, plt=plt, 
# 		plt_dsn=plt_dsn, tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid,
#  		bycond=bycond, condid=condid, bysubp=bysubp, subpid=subpid, tsumvar=tsumvar,
# 		TPA=TPA, tfun=tfun, ACI=ACI, tfilter=tfilter, lbs2tons=lbs2tons, tdomvar=tdomvar,
#  		tdomvarlst=tdomvarlst, tdomvar2=tdomvar2, tdomvar2lst=tdomvar2lst, 
# 		tdomprefix=tdomprefix, tdombarplot=tdombarplot, tdomtot=tdomtot, 
# 		tdomtotnm=tdomtotnm, FIAname=FIAname, addseed=addseed, presence=presence,
#  		proportion=proportion, cover=cover, getadjplot=getadjplot, adjtree=adjtree,
# 		NAto0=NAto0, adjTPA=adjTPA, savedata=savedata, outfolder=outfolder, 
# 		out_layer=out_layer, outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, tround=tround)",
#     	file = outfile, sep="")
#     	close(outfile)
#     }
  }

  tdomdata <- list()
  if (!notdomdat) {
    if (!returnDT) {
      sumtreef <- setDF(sumtreef)
    }
    tdomdata$tdomdat <- sumtreef
  }
  tdomdata$tsumuniqueid <- tsumuniqueid
  if (length(tunits) > 0) {
    tdomdata$tunits <- tunits
  }
  tdomdata$tsumvarnm <- tsumvarnm
  tdomdata$tdomvarnm <- tdomvarnm
  if (!is.null(tdomvar2)) {
    tdomdata$tdomvar2nm <- tdomvar2nm
  }
  if (proportion) {
    if (returnDT) {
      sumtreef.prop <- setDF(sumtreef.prop)
    }
    tdomdata$tdomdat.prop <- sumtreef.prop
  }
  if (presence) {
    if (returnDT) {
      sumtreef.pres <- setDF(sumtreef.pres)
    }
    tdomdata$tdomdat.pres <- setDF(sumtreef.pres)
  }
  if (cover) {
    if (returnDT) {
      sumtreef.cov <- setDF(sumtreef.cov)
    }
    tdomdata$tdomdat.cov <- setDF(sumtreef.cov)
  }
  if (!notdomdat) tdomdata$tdomvarlut <- tdomvarlut

  tdomdata$tdomlst <- tdomscols
  if (tdomtot && !is.null(tdomtotnm)) {
    tdomdata$tdomtotnm <- tdomtotnm
  }
  tdomdata$domainlst <- domainlst
  tdomdata$tdomainlst <- tdomainlst
  tdomdata$pcdomainlst <- pcdomainlst

  if (any(c(tdomvar, tdomvar2) == "SPCD")) {
    tdomdata$ref_spcd <- ref_spcd
  }
  if (!is.null(classifynmlst)) {
    tdomdata$classifynmlst <- classifynmlst
  }
  tdomdata$treeqry <- treeqry
  
  if (!is.null(dbconn) && database_opts$dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
 
  return(tdomdata)
} 
