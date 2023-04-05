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
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param tuniqueid String. Unique identifier of the tree table.  If including
#' seedling table, this should be the same for seed.
#' @param cuniqueid String. Unique identifier of the cond table if cond is NOT
#' NULL.
#' @param puniqueid String. Unique identifier of the plt table if plt is NOT
#' NULL.
#' @param bycond Logical. If TRUE, data are aggregated to the condition level
#' (by: uniqueid, CONDID). If FALSE, data are aggregated to the plot level (by:
#' uniqueid).
#' @param condid String. Unique identifier for conditions.
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param subpid String. Unique identifier of each subplot.
#' @param presence Logical. If TRUE, an additional table is output with tree
#' domain values as presence/absence (1/0).
#' @param tsumvar String. Name of the variable to aggregate (e.g., "BA"). For
#' summing number of trees, use tsumvar="PLT_CN" with tfun=sum.
#' @param TPA Logical. If TRUE, tsumvarlst variable(s) are multiplied by the
#' respective trees-per-acre variable (see details) to get per-acre
#' measurements.
#' @param tfun Function. Name of the function to use to aggregate the data
#' (e.g., sum, mean, max).
#' @param ACI Logical. If TRUE, if ACI (All Condition Inventory) plots exist,
#' any trees on these plots will be included in summary. If FALSE, you must
#' include condition table.
#' @param tfilter String. A filter to subset the tree data before aggregating
#' (e.g., "STATUSCD == 1"). This must be in R syntax. If tfilter=NULL, user is
#' prompted.  Use tfilter="NONE" if no filters.
#' @param lbs2tons Logical. If TRUE, converts biomass or carbon variables from
#' pounds to tons. If metric=TRUE, converts to metric tons, else short tons.
#' @param metric Logical. If TRUE, converts response to metric units based on
#' FIESTA::ref_conversion, if tsumvar is in FIESTAutils::ref_estvar. Note: if TPA,
#' TPA is converted to trees per hectare (TPH: 1 / (1/ tpavar * 0.4046860)).
#' @param tdomvar String. The tree domain (tdom) variable used to aggregate by
#' (e.g., "SPCD", "SPGRPCD").
#' @param tdomvarlst String (vector). List of specific tree domains of tdomvar
#' to aggregate (ex. c(108, 202)). If NULL, all domains of tdomvar are used.
#' @param tdomvar2 String. A second tree domain variable to use to aggregate by
#' (ex. "DIACL").  The variables, tdomvar and tdomvar2 will be concatenated
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
#' @param FIAname Logical. If TRUE, changes names of columns for SPCD and
#' SPGRPCD from code to FIA names.
#' @param addseed Logical. If TRUE, add seedling counts to tree counts. Note:
#' tdomvar must be 'SPCD' or 'SPGRPCD'.
#' @param pivot Logical. If TRUE, tdomvar data are transposed (pivoted) to
#' separate columns.
#' @param proportion Logical. If TRUE and pivot=TRUE, an additional table will
#' be output with tree domain data as proportions of total tsumvar.
#' @param cover Logical. If TRUE and pivot=TRUE, , an additional table will be
#' output with tree domain data as percent cover, based on proportions of
#' tsumvar (see proportion) and tree canopy cover variable in cond
#' (LIVE_CANOPY_CVR_PCT) or in plt (CCLIVEPLT).  Does not include seedlings.
#' @param getadjplot Logical. If TRUE, adjustments are calculated for
#' nonsampled conditions on plot.
#' @param adjtree Logical. If TRUE, trees are individually adjusted by
#' adjustment factors.  Adjustment factors must be included in tree table (see
#' adjvar).
#' @param adjvar String. If adjtree=TRUE, the name of the variable to use for
#' multiplying by adjustment (e.g., tadjfac).
#' @param adjTPA Numeric. A tree-per-acre adjustment. Use for DESIGNCD=1
#' (annual inventory), if using less than 4 subplots. If using only 1 sublot
#' for estimate, adjTPA=4. The default is 1.
#' @param NAto0 Logical. If TRUE, convert NA values to 0.
#' @param tround Number. The number of digits to round to. If NULL, default=6.
#' @param checkNA Logical. If TRUE, checks if NA values exist in necessary
#' variables.
#' @param returnDT Logical. If TRUE, returns data.table object(s). If FALSE,
#' returns data.frame object(s).
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'tdomsum'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return tdomdata - a list of the following objects:
#' 
#' \item{tdomdat}{ Data frame. Plot or condition-level table with aggregated
#' tree domain (tdom) attributes (filtered). } \item{tdomsum}{ Data frame. The
#' tdom look-up table with data aggregated by species. } \item{tdomvar}{
#' String. Name of the tdom variable used to aggregate by. } \item{tsumvar}{
#' String. Name of the aggregated output variable. } \item{tdomlst}{ Vector.
#' List of the aggregated tree data in tdomdat. } \item{tdomdat.pres}{ Data
#' frame. Plot or condition-level table with aggregated tree domain attributes
#' represented as presence/absence (1/0). } \item{tdomdat.prop}{ Data frame.
#' Plot or condition-level table with aggregated tree domain attributes
#' represented as proportion of total by plot. } \item{tdomdat.cov}{ Data
#' frame. Plot or condition-level table with aggregated tree domain attributes
#' represented as percent cover, multipying cover attribute by tdom proportion
#' by plot. }
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
#' # Sum of Live Basal Area Per Acre by Species
#' datSumTreeDom(tree = FIESTA::WYtree, 
#'               cond = FIESTA::WYcond, 
#'               plt = FIESTA::WYplt, 
#'               puniqueid = "CN", 
#'               bycond = FALSE, 
#'               tsumvar = "BA", 
#'               TPA = TRUE, 
#'               tdomtot = TRUE, 
#'               tdomtotnm = "BA_LIVE", 
#'               tdomprefix = "BA_LIVE", 
#'               tround = 2, 
#'               tfilter = "STATUSCD==1")
#'               
#' # Sum of Number of Live Trees by Species
#' datSumTreeDom(tree = FIESTA::WYtree, 
#'               cond = FIESTA::WYcond, 
#'               plt = FIESTA::WYplt, 
#'               puniqueid = "CN", 
#'               bycond = FALSE, 
#'               tsumvar = "PLT_CN", 
#'               TPA = TRUE, 
#'               tdomtot = TRUE, 
#'               tdomprefix = "CNT", 
#'               tround = 0, 
#'               tfilter = "STATUSCD==1")
#'               
#' # Sum of Number of Live Trees by Species, Including Seedlings
#' datSumTreeDom(cond = WYcond, 
#'               plt = WYplt, 
#'               seed = WYseed, 
#'               puniqueid = "CN", 
#'               bycond = FALSE, 
#'               tsumvar = "PLT_CN", 
#'               TPA = TRUE, 
#'               tdomtot = TRUE, 
#'               tdomprefix = "CNT", 
#'               tround = 0)
#' }
#' @export datSumTreeDom
datSumTreeDom <- function(tree = NULL, 
                          seed = NULL, 
                          cond = NULL, 
                          plt = NULL,  
                          subp_cond = NULL,
                          subplot = NULL, 
                          datsource = "obj", 
                          data_dsn = NULL, 
                          tuniqueid = "PLT_CN", 
                          cuniqueid = "PLT_CN", 
                          puniqueid = "CN", 
                          bycond = FALSE, 
                          condid = "CONDID", 
                          bysubp = FALSE, 
                          subpid = "SUBP", 
                          tsumvar = NULL, 
                          TPA = TRUE, 
                          tfun = sum, 
                          ACI = FALSE, 
                          tfilter = NULL, 
                          lbs2tons = TRUE, 
                          metric = FALSE, 
                          tdomvar = "SPCD", 
                          tdomvarlst = NULL, 
                          tdomvar2 = NULL, 
                          tdomvar2lst = NULL, 
                          tdomprefix = NULL, 
                          tdombarplot = FALSE, 
                          tdomtot = FALSE, 
                          tdomtotnm  =NULL, 
                          FIAname = FALSE, 
                          addseed = FALSE, 
                          pivot = TRUE, 
                          presence = FALSE, 
                          proportion = FALSE, 
                          cover = FALSE, 
                          getadjplot = FALSE, 
                          adjtree = FALSE, 
                          adjvar = "tadjfac", 
                          NAto0 = FALSE, 
                          adjTPA = 1,
                          tround = 5, 
                          checkNA = FALSE, 
                          returnDT = TRUE,
                          savedata = FALSE,
                          savedata_opts = NULL,
                          gui = FALSE){
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
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  COND_STATUS_CD=COUNT=CONDPROP_UNADJ=V1=samenm=SUBP=NF_COND_STATUS_CD=
	seedf=estunits=TREECOUNT_CALC=cond.nonsamp.filter <- NULL
  checkNApvars <- {}
  checkNAcvars <- {}
  checkNAtvars <- {}
  seedclnm <- "<1"
  seedonly=parameters <- FALSE
  ref_estvar <- FIESTAutils::ref_estvar

  ## If gui.. set variables to NULL
  if (gui) bycond=tuniqueid=puniqueid=cuniqueid=ACI=TPA=tfun=tdomvar=tdomlst=
	tdombarplot=FIAname=addseed=proportion=presence=tdomtot=adjtree=tmp <- NULL


  ##################################################################
  ## SET VARIABLE LISTS
  ##################################################################
  biovars <- c("DRYBIO_AG", "DRYBIO_BG", "DRYBIO_WDLD_SPP", "DRYBIO_SAPLING",
 	"DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_BOLE", "DRYBIOT", "DRYBIOM", "DRYBIOTB",
 	"JBIOTOT")
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
  tsumvar.not <- c(condid)

  
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
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
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
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  FIAtnamelst <- c("SPCD", "SPGRPCD")

  ### CHECK tables
  ###########################################################  
  noplt=nocond <- TRUE
  pltsp <- FALSE

  ## Set datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "sqlite", "gdb")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (is.null(datsource)) {
    if (!is.null(data_dsn) && file.exists(data_dsn)) {
      dsn.ext <- getext(data_dsn)
      if (!is.na(dsn.ext) && dsn.ext != "") {
        datsource <- ifelse(dsn.ext == "gdb", "gdb", 
		ifelse(dsn.ext %in% c("db", "db3", "sqlite", "sqlite3"), "sqlite", 
             ifelse(dsn.ext == "csv", "csv",
			ifelse(dsn.ext == "shp", "shp", "datamart"))))
      } 
    } else {
      stop("datsource is invalid")
    }
  }

  ## Check tree  
  treex <- pcheck.table(tree, tab_dsn=data_dsn, gui=gui, tabnm="tree", 
			caption="Tree table?")

  ## Check seed 
  seedx <- pcheck.table(seed, tab_dsn=data_dsn, gui=gui, tabnm="seed", 
			caption="Seed table?")

  ## Check cond 
  condx <- pcheck.table(cond, tab_dsn=data_dsn, gui=gui, tabnm="cond", 
			caption="Condition table?")

  ## Check addseed
  addseed <- pcheck.logical(addseed, varnm="addseed", title="Add seeds?", 
		first="NO", gui=gui)

  ## Check seedonly
  seedonly <- pcheck.logical(seedonly, varnm="seedonly", title="Seed only?", 
		first="NO", gui=gui)

  if (is.null(treex) && is.null(seedx)) {
    stop("must include tree and/or seed table")
  }
  if (addseed && is.null(seedx)) {
    stop("if addseed=TRUE, must include seed table")
  }
  if (addseed && is.null(treex)) {
    addseed <- FALSE
  }
  if (is.null(treex) && !is.null(seedx)) {
    addseed <- FALSE
    seedonly <- TRUE
    treex <- seedx
  }
 
  ## Check bycond
  ###################################################################################
  bycond <- pcheck.logical(bycond, varnm="bycond", title="By condition?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check bysubp
  ###################################################################################
  bysubp <- pcheck.logical(bysubp, varnm="bysubp", title="By subplot?", 
		first="YES", gui=gui, stopifnull=TRUE)

  if (bysubp) {
    subpuniqueid <- "PLT_CN"
    subpids <- c(subpuniqueid, subpid)

    ## Check subplot
    subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot", gui=gui, 
			caption="Subplot table?")
  
    ## Check subpid
    if (!is.null(subplotx)) {
      if (!all(subpids %in% names(subplotx))) {
        stop("uniqueids not in subplot: ", toString(subpids))
      }
      setkeyv(subplotx, subpids)
    }

    ## Check subplot
    subpcondx <- pcheck.table(subp_cond, tab_dsn=data_dsn, tabnm="subp_cond", gui=gui, 
			caption="Subpcond table?", stopifnull=TRUE)
    if (!all(subpids %in% names(subpcondx))) {
      stop("uniqueids not in subp_cond: ", toString(subpids))
    }
    setkeyv(subpcondx, subpids)
  }

  ## Check lbs2tons
  ###################################################################################
  lbs2tons <- pcheck.logical(lbs2tons, varnm="lbs2tons", title="Pounds to tons?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check metric
  ###################################################################################
  metric <- pcheck.logical(metric, varnm="metric", title="Metric?", 
		first="NO", gui=gui, stopifnull=TRUE)

  ## Check checkNA
  ###################################################################################
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
		first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE

  ## Check checkNA
  ###################################################################################
  checkNA <- pcheck.logical(checkNA, varnm="checkNA", title="Check NA values?", 
		first="YES", gui=gui)
  if (is.null(checkNA)) checkNA <- FALSE

  ## Check unique identifiers, set keys, and matching values/classes
  ###################################################################################
   
  ## Check tuniqueid
  tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", 	
		checklst=names(treex), caption="UniqueID variable - tree", 
		warn=paste(tuniqueid, "not in tree table"))
  setkeyv(treex, tuniqueid)
  checkNAtvars <- c(checkNAtvars, tuniqueid)
  setkeyv(treex, tuniqueid)

  if (addseed) {
    if (!tuniqueid %in% names(seedx)) {
      stop("must included tree uniqueid in seed table: ", tuniqueid)
    }
    setkeyv(seedx, tuniqueid)
  }

  ## If bycond, check condid in tree table and setkey to tuniqueid, condid
  if (bycond) {
    pltsp <- FALSE
    noplt <- TRUE

    ## Check condid in tree table
    condid <- pcheck.varchar(var2check=condid, varnm="condid", 
		checklst=names(treex), caption="Cond ID - tree", 
		warn=paste(condid, "not in tree table"))
    if (is.null(condid)) {
      warning("assuming all 1 condition plots")
      treex$CONDID <- 1
      condid <- "CONDID"

      if (addseed) {
        if (!condid %in% names(seedx)) {
          seedx$CONDID <- 1
        } else {
          stop(condid, " in seed but not in cond")
        }
      }  
    } else {
      if (addseed && !condid %in% names(seedx)) {
        stop(condid, " not in seed") 
      }
    }
    tsumuniqueid <- c(tuniqueid, condid)
    setkeyv(treex, tsumuniqueid)
    
    if (addseed) {
      setkeyv(seedx, tsumuniqueid)
    }

    if (!is.null(condx)) {
      nocond <- FALSE

      ## Check cuniqueid and condid in cond table
      condnmlst <- names(condx)
      cuniqueid <- pcheck.varchar(var2check=cuniqueid, varnm="cuniqueid", 
		checklst=condnmlst, caption="UniqueID variable - cond", 
		warn=paste(cuniqueid, "not in cond table"))
      if (is.null(cuniqueid)) {
        if (tuniqueid %in% condnmlst) {
          cuniqueid <- tuniqueid
        } else {
          stop("cuniqueid is invalid")
        }
      }

      ## Check if class of tuniqueid matches class of cuniqueid
      tabs <- check.matchclass(treex, condx, tuniqueid, cuniqueid)
      treex <- tabs$tab1
      condx <- tabs$tab2

      if (!condid %in% names(condx)) {
        stop("condid must be in condx")
      }
      csumuniqueid <- c(cuniqueid, condid)
      setkeyv(condx, csumuniqueid)
      checkNAcvars <- c(checkNAcvars, csumuniqueid)

      ## Check that values of tuniqueid in treex are all in puniqueid in pltx
      treex <- check.matchval(treex, condx, tsumuniqueid, csumuniqueid,
		tab1txt="tree", tab2txt="cond")

      if (addseed) {
        setkeyv(seedx, tsumuniqueid)

        ## Check if class of tuniqueid matches class of cuniqueid
        tabs <- check.matchclass(seedx, condx, tuniqueid, cuniqueid)
        seedx <- tabs$tab1
        condx <- tabs$tab2

        ## Check that values of tuniqueid in treex are all in puniqueid in pltx
        seedx <- check.matchval(seedx, condx, tsumuniqueid, csumuniqueid)
      }
    } 
  } else {   ## byplt
    tsumuniqueid <- tuniqueid
  }

  if (bysubp) {
    pltshp <- FALSE
    noplt <- TRUE
    nocond <- TRUE

    ## Check condid in tree table and setkey to tuniqueid, condid
    subpid <- pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(treex), caption="subplot ID - tree", 
		warn=paste(subpid, "not in tree table"))
    if (is.null(subpid)) {
      warning("assuming only 1 subplot")
      treex[[subpid]] <- 1

      if (addseed) {
        if (!subpid %in% names(seedx)) {
          seedx[[subpid]] <- 1
        } 
      }  
    } else {
      if (addseed) {
        if (!subpid %in% names(seedx)) {
          stop(subpid, "not in seed")
        } 
      }
    }

    tsumuniqueid <- c(tsumuniqueid, subpid)
    if (bycond) {
      tsumuniqueid <- c(tuniqueid, subpid, condid)
    }
    setkeyv(treex, tsumuniqueid)

    checkNAtvars <- c(checkNAtvars, subpid)
    if (addseed) {
      setkeyv(seedx, tsumuniqueid)
    }

    ## Set pltx to NULL   
    pltx <- NULL
  } else {

    pltx <- pcheck.table(plt, tab_dsn=data_dsn, gui=gui, tabnm="plt", 
			caption="Plot table?")
  }

  if (!is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (3 %in% unique(pltx[["PLOT_STATUS_CD"]])) {
        warning(paste("There are", sum(pltx[["PLOT_STATUS_CD"]] == 3), "nonsampled plots"))
        pltx <- pltx[pltx[["PLOT_STATUS_CD"]] != 3,]
      }
    }

    if ("sf" %in% class(pltx))
      pltsp <- TRUE

    ## Check puniqueid
    pltnmlst <- names(pltx)
    puniqueid <- pcheck.varchar(var2check=puniqueid, varnm="puniqueid", 
		checklst=pltnmlst, caption="UniqueID variable - plt", 
		warn=paste(puniqueid, "not in plot table"), stopifnull=TRUE)

    ## Check for unique plot records
    if (nrow(pltx) > length(unique(pltx[[puniqueid]]))) {
      message("plt table has > 1 record per uniqueid... will not be merged to plt.")
      noplt <- TRUE
    }

    ## Check if class of tuniqueid matches class of puniqueid
    tabs <- check.matchclass(treex, pltx, tuniqueid, puniqueid)
    treex <- tabs$tab1
    pltx <- tabs$tab2

    ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
    treex <- check.matchval(treex, pltx, tuniqueid, puniqueid)

    if (addseed) {
      ## Check if class of tuniqueid matches class of puniqueid
      tabs <- check.matchclass(seedx, pltx, tuniqueid, puniqueid)
      seedx <- tabs$tab1
      pltx <- tabs$tab2

      ## Check that the values of tuniqueid in seedx are all in puniqueid in pltx
      check.matchval(seedx, pltx, tuniqueid, puniqueid)
    }

    ## Check that the values of cuniqueid in condx are all in puniqueid in pltx
    if (!is.null(condx)) 
      ## Check that the values of tuniqueid in treex are all in puniqueid in pltx
      check.matchval(condx, pltx, cuniqueid, puniqueid)

    ## Change uniqueid in plt table to match tree uniqueid
#    if (puniqueid == "CN" && tuniqueid == "PLT_CN") {
#      names(pltx)[names(pltx) == puniqueid] <- tuniqueid
#      puniqueid <- tuniqueid
#    }

    if (is.data.table(pltx)) {
      setkeyv(pltx, puniqueid)
    }
    checkNApvars <- c(checkNApvars, puniqueid)
  } 


  ## Check ACI. If TRUE, include all trees, If FALSE, filter for forested plots only 
  ## (COND_STATUS_CD = 1)
  ######################################################################################
  ACI <- pcheck.logical(ACI, varnm="ACI", title="Include ACI tree data?", 
		first="NO", gui=gui)
  if (!ACI) {
    if (is.null(condx) || (!"COND_STATUS_CD" %in% names(condx))) {
      warning("COND_STATUS_CD not in table, assuming forested plots with no ACI plots")
    } else {
      cond.ids <- na.omit(condx[COND_STATUS_CD == 1, 
		do.call(paste, .SD), .SDcols=c(cuniqueid, condid)])
      treex <- treex[paste(get(eval(tuniqueid)), get(eval(condid))) %in% cond.ids]
    }
  }

  ## Check for NA values in necessary variables in all tables
  if (checkNA) {
    treex.na <- sapply(checkNAtvars, 
		function(x, treex){ sum(is.na(treex[,x, with=FALSE])) }, treex)
    if (any(treex.na) > 0) {
      stop(treex.na[treex.na > 0], " NA values in tree variable: ", 
		paste(names(treex.na[treex.na > 0]), collapse=", "))
    }
    condx.na <- sapply(checkNAcvars, 
		function(x, condx){ sum(is.na(condx[,x, with=FALSE])) }, condx)
    if (any(condx.na) > 0) {
      stop(condx.na[condx.na > 0], " NA values in cond variable: ", 
		paste(names(condx.na[condx.na > 0]), collapse=", "))
    }
    pltx.na <- sapply(checkNApvars, 
		function(x, pltx){ sum(is.na(pltx[,x, with=FALSE])) }, pltx)
    if (any(pltx.na) > 0) {
      stop(pltx.na[pltx.na > 0], " NA values in plt variable: ", 
		paste(names(pltx.na[condx.na > 0]), collapse=", "))
    }
  }

  ## Check getadjplot
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot", 
		title="Get plot adjustment?", first="NO", gui=gui)

  ## Check adjtree
  adjtree <- pcheck.logical(adjtree, varnm="adjtree", title="Adjust trees", 
		first="NO", gui=gui)
  if (is.null(adjtree)) adjtree <- FALSE
  if (getadjplot && !adjtree) {
    message("getadjplot=TRUE, and adjtree=FALSE... setting adjtree=TRUE")
    adjtree <- TRUE
  }
  if (adjtree && !getadjplot && !adjvar %in% names(treex)) {
    message(adjvar, " variable not in tree table... setting getadjplot=TRUE")
    getadjplot <- TRUE
  }
  if (getadjplot) {
    if (bysubp) {
      if (sum(is.null(subpcondx), is.null(condx)) < 3) {
        missing <- c("subp_cond", "cond")[which(is.null(subpcondx), is.null(condx))]
        if (length(missing) > 0) {
          stop("must include following tables to calculate adjustments: ", toString(missing))
        }
      } 
    } else {
      if (is.null(condx)) {
        stop("must include condx to adjust to plot")
      }
    }
  }


  ###########################################################  
  ### Check tsumvar 
  ###########################################################  
  notdomdat <- ifelse(is.null(tsumvar) && presence, TRUE, FALSE)
  if (is.null(tsumvar)) {
    if (presence) {
      #tsumvar <- "BA"
      tsumvar <- tuniqueid
    } else {
      tsumvar <- select.list(names(treex), title="Aggregate variable", multiple=FALSE)
      if (tsumvar == "") stop("")
    }
  } else if (tsumvar %in% tsumvar.not) {
    stop("tsumvar is invalid")
  } else if (!tsumvar %in% names(treex)) {
    warning("check tsumvar: tree aggregation variable not in tree table")
    tsumvar <- select.list(names(treex), title="Tree aggregate variable", multiple=FALSE)
    if (tsumvar == "") stop("")
  }
  ## Check seedonly
  if (seedonly && !tsumvar %in% c("TPA_UNADJ", "PLT_CN")) {
    stop("tsumvar must be TPA_UNADJ for seedonly")
  }

  ## If tsumvar is a TPA* variable, add a variable, COUNT=1 to tree table
  if (tsumvar %in% c(tuniqueid, tpavars)) {
    treex[, COUNT := 1]
    tpavar <- tsumvar
    tsumvar <- "COUNT"
    #TPA <- TRUE
    if (!is.null(seedx)) {
      seedx[, COUNT := 1]
    }
  }

  ## Convert variables from pound to tons if lbs2tons=TRUE
  if (lbs2tons && tsumvar %in% vars2convert) {
    convfac <- ifelse(metric, 0.00045359237, 0.0005)
    message("converting from pounds to tons")
    treex[[tsumvar]] <- treex[[tsumvar]] * convfac
  }

  ## Convert to metric
  if (tsumvar != "COUNT") {
    if (tsumvar %in% ref_estvar$ESTVAR) { 
      estunits <- unique(ref_estvar$ESTUNITS[ref_estvar$ESTVAR == tsumvar])
    } else {
      if (metric) {
        message(tsumvar, " not in ref_estvar... no metric conversion")
        metric <- FALSE
      } else {
        message(tsumvar, " not in ref_estvar... no units found")
      }
    }
    if (metric) {
      metricunits <- unique(ref_estvar$METRICUNITS[ref_estvar$ESTVAR == tsumvar])
      if (estunits != metricunits) {
        cfactor <- FIESTA::ref_conversion$CONVERSION[FIESTA::ref_conversion$METRIC == 
			metricunits]
        tsumvarm <- paste0(tsumvar, "_m")
        treex[, (tsumvarm) := get(eval(tsumvar)) * cfactor]
        estunits <- metricunits
        tsumvar <- tsumvarm
      }
    }
  }   

  ## CHECK TPA and tsumvars
  TPA <- pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO", gui=gui)
  if (TPA) {
    if (tsumvar %in% mortvars) {
      if (!"TPAMORT_UNADJ" %in% names(treex)) {
        stop("must have TPAMORT_UNADJ variable in tree table to calculate trees per acre")
      }
      tpavar <- "TPAMORT_UNADJ"
    } else if (any(tsumvar %in% growvars)) {
      if (!"TPAGROW_UNADJ" %in% names(treex)) {
        stop("must have TPAGROW_UNADJ variable in tree table to calculate trees per acre")
      }
      tpavar <- "TPAGROW_UNADJ"
    } else if (tsumvar %in% remvars) {
      if (!"TPAREMV_UNADJ" %in% names(treex)) {
        stop("must have TPAREMV_UNADJ variable in tree table to calculate trees per acre")
      }
      tpavar <- "TPAREMV_UNADJ"
    } else {  
      if (!"TPA_UNADJ" %in% names(treex)) {
        stop("must have TPA_UNADJ variable in tree table to calculate trees per acre")
      }
      tpavar <- "TPA_UNADJ"
    }

    ## Check adjTPA and adjust TPA (default = 1)
    ## (e.g., if adjTPA=4 (only 1 subplot measured), multiply TPA* by 4)
    if (is.null(adjTPA)) {
      message("adjTPA is invalid, assuming no adjustments")
      adjTPA <- 1
    } else if (!is.numeric(adjTPA)) {
      stop("adjTPA must be a numeric number from 1 to 4")
    } else if (!adjTPA %in% 1:4) {
      stop("adjTPA must be between 1 and 4")
    } else if (adjTPA > 1) {
      if ("SUBP" %in% names(treex)) {
        if (adjTPA == 2 && any(treex[, unique(SUBP), by=tuniqueid][[2]] > 3)) {
          stop("more than 3 SUBP in dataset")
        } else if (adjTPA == 3 && any(treex[, unique(SUBP), by=tuniqueid][[2]] > 2)) {
          stop("more than 2 SUBP in dataset")
        } else if (adjTPA == 4 && any(treex[, unique(SUBP), by=tuniqueid][[2]] > 1)) {
          stop("more than 1 SUBP in dataset")
        }
      } else {
        message("assuming less than 3 SUBP in dataset")
      }
    }
    ## If metric, convert tpavar to trees per hectare
    if (metric) {
      tpa.m <- paste0(tpavar, "_m")
      treex[, (tpa.m) := 1 / ((1/ get(eval(tpavar)) * 0.4046860))]
      tpavar <- tpa.m
    }
  }

  ### GET tfun USED FOR AGGREGATION
  tfunlst <- c("sum", "mean", "max", "min", "length", "median")
  if (tsumvar == tuniqueid) tfun <- length

  if (is.null(tfun)) {
    if (gui) {
      tfunstr <- select.list(tfunlst, title="Aggregate function", multiple=FALSE)
      if (tfunstr == "") stop("")
      tfun <- get(tfunstr)
    } else {
      tfun <- sum
    }
  }

  if (!is.function(tfun)) {
    stop("tfun is not a function")
  } else {
    if(tuniqueid %in% tsumvar & !identical(tfun, sum))
      stop("use sum with PLT_CN for getting number of trees.")

    if (length(grep("mean", deparse(tfun))) > 0) {
      tfunstr <- "mean"
    } else {
      tfunnm <- noquote(strsplit(deparse(tfun), ".Primitive")[[1]][2])
      if (is.na(tfunnm))
        tfunnm <- noquote(strsplit(deparse(tfun), "UseMethod"))[[2]][2]
      if (is.na(tfunnm)) {
        warning("unknown function")
        tfunstr <- "fun"
      } else {  
        tfunstr <- substr(tfunnm, 3, nchar(tfunnm)-2)
      }
    }
  }
 
  ### Filter tree data 
  ###########################################################  

  ## Filter trees that are no longer in survey (DIA = NA)  
#  if ("DIA" %in% names(treef)) {
#    treef <- suppressWarnings(
#	datFilter(x=treef, xfilter="DIA >= 1.0", title.filter="DIA < 1.0in"))$xf 
#  }

  ## Check tfilter and filter tree data
  tdat <- datFilter(x=treex, xfilter=tfilter, title.filter="tfilter", 
			stopifnull=TRUE, gui=gui)
  treef <- tdat$xf
  tree.filter <- tdat$xfilter

  if (addseed) {
    xfilter <- tryCatch( check.logic(seedx, tfilter),
		error=function(e) return(NULL))
    if (!is.null(xfilter)) {
      ## Seed filter
      sdat <- datFilter(x=seedx, xfilter=tfilter, title.filter="tfilter", xnm="seed")
      seedf <- sdat$xf
    } else {
      seedf <- seedx
    }
  }
 
  ## Check tdomvar 
  tdomvar <- pcheck.varchar(var2check=tdomvar, varnm="tdomvar", gui=gui, 
		checklst=sort(names(treef)), caption="Tree domain name?", 
		warn=paste(tdomvar, "not in tree table"), stopifnull=TRUE)

  ## GET tdomvarlst or CHECK IF ALL tree domains IN tdomvarlst ARE INCLUDED IN tdomvar.
  if (is.factor(treef[[tdomvar]])) {
    tdoms <- levels(treef[[tdomvar]])[levels(treef[[tdomvar]]) %in% unique(treef[[tdomvar]])]
  } else {
    tdoms <- sort(unique(treef[[tdomvar]]))
  }
 
  ## check seed table
  if (addseed) {
    if (tdomvar == "DIACL") {
      if ("DIACL" %in% names(seedf)) {
        seedf$DIACL <- seedclnm
      }
    } else if (!tdomvar %in% names(seedf)) {
      message("tdomvar not in seedf: ", tdomvar, "... no seeds included")
      addseed <- FALSE
    }
  }

  nbrtdoms <- length(tdoms)
  if (is.null(tdomvarlst)) {
    ## GET tdomvarlst
    if (gui) {
      tdomvarlst <- select.list(as.character(tdoms), title="Tree domains?", multiple=TRUE)
      if (length(tdomvarlst) == 0) stop("")
      if (is.numeric(tdoms))  tdomvarlst <- as.numeric(tdomvarlst)
    } else {
      tdomvarlst <- tdoms
    }
  } else { 
    if (any(!tdomvarlst %in% unique(treef[[tdomvar]]))) {
      tdom.miss <- tdomvarlst[which(!tdomvarlst %in% unique(treef[,get(tdomvar)]))]
        tdom.miss <- tdomvarlst[!tdomvarlst %in% unique(treef[[tdomvar]])]
        if (length(tdom.miss) == 1 && addseed && tdom.miss == seedclnm) {
          tdom.miss <- NULL
        }
        if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
          message("tdomvarlst domain values not in tree table: ", toString(tdom.miss))
        }
        if (gui) {
          tdomvarlst <- select.list(as.character(tdoms), title="Tree domain(s)", 
			multiple=TRUE)
        }      
      if (length(tdomvarlst) == 0) {
        stop("")
      }
      if (is.numeric(tdoms)) {
        tdomvarlst <- as.numeric(tdomvarlst) 
      }
    }  
    treef <- treef[treef[[tdomvar]] %in% tdomvarlst,]
    if (addseed) {
      if (tdomvar == "DIACL") {
        seedf <- seedf[seedf[[tdomvar]] %in% seedclnm,]
      } else {
        seedf <- seedf[seedf[[tdomvar]] %in% tdomvarlst,]
      }
    }
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
  ## treef is the tree table after filtered tree domains

  ## GETS FIAname for tdomvar
  if (gui & tdomvar %in% FIAtnamelst & is.null(FIAname)) {
    FIAnameresp <- select.list(c("NO", "YES"), title=paste0("FIA name: ", tdomvar, "?"), 
		multiple=FALSE)
    if (FIAnameresp=="") stop("") 
    FIAname <- ifelse(FIAnameresp == "YES", TRUE, FALSE)
  }
  if (FIAname) {
    tdomdata <- datLUTnm(treef, xvar=tdomvar, LUTvar="VALUE", FIAname=TRUE)
    treef <- tdomdata$xLUT
    tdomvarnm <- tdomdata$xLUTnm
    setkeyv(treef, tsumuniqueid) 
    tdomvarlut <- unique(treef[,c(tdomvar, tdomvarnm), with=FALSE]) 

    if (addseed || seedonly) {
      sdomdata <- datLUTnm(seedf, xvar=tdomvar, LUTvar="VALUE", FIAname=TRUE)
      seedf <- sdomdata$xLUT
    }
    tdomvarlst2 <- tdomvarlut[match(tdomvarlst, tdomvarlut[[tdomvar]]), 
		tdomvarnm, with=FALSE][[1]]

  } else if (is.numeric(treef[[tdomvar]]) && !is.null(tdomprefix)) {

    tdomvarnm <- paste0(tdomvar, "NM") 
    maxchar <- max(sapply(tdomvarlst, function(x) {nchar(x)}))

    treef[, (tdomvarnm):= paste0(tdomprefix, formatC(get(eval(tdomvar)), 
			width=maxchar, flag="0"))]
    tdomvarlst2 <- paste0(tdomprefix, formatC(tdomvarlst, width=maxchar, flag="0"))
    #treef[, (tdomvarnm) := paste0(tdomprefix, get(eval(tdomvar)))]
    #tdomvarlst2 <- paste0(tdomprefix, tdomvarlst)

    if (addseed) {
      seedf[, (tdomvarnm):= paste0(tdomprefix, formatC(get(eval(tdomvar)), 
			width=maxchar, flag="0"))]
    }
    #tdomvarlut <- data.frame(tdomvarlst, tdomvarlst2, stringsAsFactors=FALSE)
    #names(tdomvarlut) <- c(tdomvar, tdomvarnm) 

  } else {
    tdomvarnm <- tdomvar
    #tdomvarlut <- data.frame(tdomvarlst, stringsAsFactors=FALSE)
    #names(tdomvarlut) <- tdomvarnm
    tdomvarlst2 <- as.character(tdomvarlst) 
  }

  ## GET tdomvarlst2 or CHECK IF ALL tree domains IN tdomvar2lst ARE INCLUDED IN tdomvar2.
  if (!is.null(tdomvar2)) {
    tdoms2 <- sort(unique(treef[[tdomvar2]]))

    ## check seed table
    if (addseed) {
      if (tdomvar2 == "DIACL") {
        if ("DIACL" %in% names(seedf)) {
          seedf$DIACL <- seedclnm
        }
        levels(tdoms2) <- c(seedclnm, levels(tdoms2))
      } else if (!tdomvar2 %in% names(seedf)) {
        message("tdomvar2 not in seed: ", tdomvar2, "... no seeds included")
        addseed <- FALSE
      }
    }
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
      if (any(!tdomvar2lst %in% unique(treef[[tdomvar2]]))) {
        tdom.miss <- tdomvar2lst[!tdomvar2lst %in% unique(treef[[tdomvar2]])]
        if (length(tdom.miss) == 1 && addseed && tdom.miss == seedclnm) {
          tdom.miss <- NULL
        }
        if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
          message("tdomvar2lst domain values not in tree table: ", toString(tdom.miss))
        }
        if (gui) {
          tdomvar2lst <- select.list(as.character(tdoms2), title="Tree domain(s)", 
			multiple=TRUE)
        }
        if (length(tdomvar2lst) == 0) {
          stop("")
        }
        if (is.numeric(tdoms2))  {
          tdomvar2lst <- as.numeric(tdomvar2lst)
        }
      }
    }
    if (!is.null(tdomvar2)) {
      treef <- treef[treef[[tdomvar2]] %in% tdomvar2lst,]

      ## GETS FIAname for tdomvar
      if (gui && tdomvar2 %in% FIAtnamelst && is.null(FIAname)) {
        FIAnameresp <- select.list(c("NO", "YES"), title=paste0("FIA name: ", tdomvar2, "?"),
 		multiple=FALSE)
        if (FIAnameresp == "") stop("")
        FIAname <- ifelse(FIAnameresp == "YES", TRUE, FALSE)
      }
      if (FIAname) {
        tdomdata <- datLUTnm(treef, xvar=tdomvar2, LUTvar="VALUE", FIAname=TRUE)
        treef <- tdomdata$xLUT
        tdomvar2nm <- tdomdata$xLUTnm
      
        if (addseed) {
          sdomdata <- datLUTnm(seedf, xvar=tdomvar2, LUTvar="VALUE", FIAname=TRUE)
          seedf <- sdomdata$xLUT
        }
      }

      if (is.numeric(treef[[tdomvar2]])) {
        maxchar2 <- max(sapply(tdomvar2lst, function(x) {nchar(x)}))
        treef[, (tdomvarnm) := paste0(treef[[tdomvarnm]], "#", 
            formatC(treef[[tdomvar2]], width=maxchar2, flag="0"))]

        if (addseed) {
          seedf[, (tdomvarnm) := paste0(seedf[[tdomvarnm]], "#", 
            formatC(seedf[[tdomvar2]], width=maxchar2, flag="0"))]
        }
      } else {
        treef[, (tdomvarnm) := paste0(treef[[tdomvarnm]], "#", treef[[tdomvar2]])]
        if (addseed) {
          seedf[, (tdomvarnm) := paste0(seedf[[tdomvarnm]], "#", seedf[[tdomvar2]])]
        }
      }
      tdomvarlst2 <- sort(unique(treef[[tdomvarnm]]))
    }
    #tdomvarlut <- data.frame(unique(treef[, c(tdomvar, tdomvar2, tdomvarnm), with=FALSE]) )
    #tdomvarlut <- tdomvarlut[order(tdomvarlut[[tdomvar]], tdomvarlut[[tdomvar2]]), ] 
  }

  ## Check pivot
  pivot <- pcheck.logical(pivot, varnm="pivot", title="Pivot columns?", 
		first="NO", gui=gui)
  if (!pivot) {
    presence=proportion=cover <- FALSE

  } else {
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
  }

  if (cover) {
    covervar <- ifelse(bycond, "LIVE_CANOPY_CVR_PCT", "CCLIVEPLT")
    if (bycond) {
      if (is.null(condx) || !covervar %in% names(condx)) {
        warning (paste(covervar, "must be in cond.. not included in output"))
        cover <- FALSE
      }
    } else {
      if (is.null(pltx) || !covervar %in% names(pltx)) {
        warning (paste(covervar, "must be in plt.. not included in output"))
        cover <- FALSE
      }
    }
    proportion <- TRUE
  }

  ## Check tdombarplot
  tdombarplot <- pcheck.logical(tdombarplot, varnm="tdombarplot", 
	title="Barplot of tdomains?", first="NO", gui=gui)

  ## Check tround
  if (is.null(tround) | !is.numeric(tround)) {
    warning("tround is invalid.. rounding to 6 digits")
    tround <- 4
  }

  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
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
    if (is.null(out_layer)) {
      out_layer <- "tdomsum"
    }
  }
  
  ################################################################################  
  ################################################################################  
  ### DO WORK
  ################################################################################ 
  ################################################################################  

  if (getadjplot) {

    if (bysubp) {
      ## Remove nonsampled conditions by subplot and summarize to condition-level
      subpcx <- subpsamp(cond = condx, 
                         subp_cond = subpcondx, 
                         subplot = subplotx, 
                         subpuniqueid = subpuniqueid, 
                         subpid = subpid)

      ## Check if class of tuniqueid matches class of cuniqueid
      tabs <- check.matchclass(treex, subpcx, c(tuniqueid, subpid), c(subpuniqueid, subpid))
      treex <- tabs$tab1
      subpcx <- tabs$tab2

      adjfacdata <- getadjfactorPLOT(treex=treef, seedx=seedf, condx=subpcx, 
		tuniqueid=c(tuniqueid, subpid), cuniqueid=c(subpuniqueid, subpid),
		areawt="CONDPROP_UNADJ")
      condx <- adjfacdata$condx
      cuniqueid <- c(subpuniqueid, subpid)
 
      varadjlst <- c("ADJ_FACTOR_COND", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR")
      if (any(varadjlst %in% names(condx))) {
        varadjlst <- varadjlst[varadjlst %in% names(condx)]
        condx[, (varadjlst) := NULL]
      }
    
    } else {

      ## Remove nonsampled plots 
      if ("COND_STATUS_CD" %in% names(condx)) {
        cond.nonsamp.filter <- "COND_STATUS_CD != 5"
        nonsampn <- sum(condx$COND_STATUS_CD == 5, na.rm=TRUE)
        if (length(nonsampn) > 0) {
          message("removing ", nonsampn, " nonsampled forest conditions")
        } else {
          message("assuming all sampled conditions in cond")
        }
      } else {
        message("assuming all sampled conditions in cond")
      }
      if (ACI && "NF_COND_STATUS_CD" %in% names(condx)) {
        cond.nonsamp.filter.ACI <- "(is.na(NF_COND_STATUS_CD) | NF_COND_STATUS_CD != 5)"
        message("removing ", sum(is.na(NF_COND_STATUS_CD) & NF_COND_STATUS_CD == 5, na.rm=TRUE), 
		" nonsampled nonforest conditions")
        if (!is.null(cond.nonsamp.filter)) 
          cond.nonsamp.filter <- paste(cond.nonsamp.filter, "&", cond.nonsamp.filter.ACI)
      }
      condx <- datFilter(x=condx, xfilter=cond.nonsamp.filter, 
		title.filter="cond.nonsamp.filter")$xf

      adjfacdata <- getadjfactorVOL(treex=treef, seedx=seedf, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, adj=TRUE)
      condx <- adjfacdata$condx
      varadjlst <- c("ADJ_FACTOR_COND", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR")
      if (any(varadjlst %in% names(condx))) {
        varadjlst <- varadjlst[varadjlst %in% names(condx)]
        condx[, (varadjlst) := NULL]
      }
    }
    
    treef <- adjfacdata$treex
    if (addseed) {
      seedf <- adjfacdata$seedx
    }  
    adjtree <- TRUE
  }
 
  if (adjtree && !adjvar %in% names(treef)) {
    message(adjvar, " variable not in tree table... no adjustment was added")
    adjtree <- FALSE
  }
  if (nrow(treef) == 0) {
    stop("no tree exists for your dataset")
    return(NULL)
  }

  ## AGGREGATE THE TREE SUM VARIABLE TO PLOT LEVEL USING THE SPECIFIED FUNCTION
  if (adjtree) {
    newname <- ifelse(TPA, paste0(tsumvar, "_TPA_ADJ"), paste0(tsumvar, "_ADJ"))
    if (TPA) {
      treef[, (newname) := get(eval(tsumvar)) * get(eval(tpavar)) * get(eval(adjvar))]
    } else {
      treef[, (newname) := get(eval(tsumvar)) * get(eval(adjvar))]
    }

    if (!is.null(seedf) && tsumvar == "COUNT") {
      seed_newname <- paste0("SEED_", newname)
      if (TPA) {
        seedf[, (seed_newname) := get(eval(tsumvar)) * get(eval(tpavar)) * get(eval(adjvar))]
      } else {
        seedf[, (seed_newname) := get(eval(tsumvar)) * get(eval(adjvar))]
      }
    }
  } else {
    newname <- ifelse(TPA, paste0(tsumvar, "_TPA"), tsumvar)
    if (TPA) {
      treef[, (newname) := get(eval(tsumvar)) * get(eval(tpavar))]
    } else {
      treef[, (newname) := get(eval(tsumvar))]
    }
    if (!is.null(seedf) && tsumvar == "COUNT") {
      seed_newname <- paste0("SEED_", newname)
      if (TPA) {
        seedf[, (seed_newname) := get(eval(tsumvar)) * get(eval(tpavar))]
      } else {
        seedf[, (seed_newname) := get(eval(tsumvar)) * TREECOUNT_CALC]
      }
    }
  }
  treef <- treef[, unique(c(key(treef), tdomvar, tdomvarnm, newname)), with=FALSE]

  ## GET NAME FOR SUMMED TREE VARIABLE FOR FILTERED TREE DOMAINS 
  if (is.null(tdomtotnm) && pivot) {
    if (is.null(tdomprefix)) {
      tdomtotnm <- paste0(newname, "TOT")
    } else {
      tdomtotnm <- paste0(tdomprefix, "TOT")
    }
  }

  ## GET NAME FOR SUMMED TREE VARIABLE FOR ALL TREE DOMAINS (IF PROPORTION = TRUE)
  if (proportion) denomvar <- paste0(newname, "_ALL")

  ## Sum tree (and seed) by tdomvarnm
  #####################################################################
  byvars <- unique(c(tsumuniqueid, tdomvar, tdomvarnm))

  if (seedonly) {
    tdomtreef <- treef[, tfun(.SD, na.rm=TRUE), by=byvars, .SDcols=newname]
    setnames(tdomtreef, "V1", newname)
    setkeyv(tdomtreef, byvars)
  } else {
    tdomtreef <- treef[, tfun(.SD, na.rm=TRUE), by=byvars, .SDcols=newname]
    setnames(tdomtreef, "V1", newname)
    setkeyv(tdomtreef, byvars)

    if (addseed) { 
      seedname <- ifelse(TPA, seed_newname, "TREECOUNT_CALC")
      tdomseedf <- seedf[, tfun(.SD, na.rm=TRUE), by=byvars, .SDcols=seed_newname]

      setnames(tdomseedf, "V1", seedname)
      setkeyv(tdomseedf, byvars)
      tdomtreef <- merge(tdomtreef, tdomseedf, by=byvars, all.x=TRUE, all.y=TRUE)
      tdomtreef[, (paste0("TREE_", newname)) := get(newname)]
      tdomtreef[, (newname) := rowSums(.SD, na.rm=TRUE), .SDcols=c(newname, seedname)]
 
      #tdomtreef[, c(newname, seedname) := NULL]
      #setnames(tdomtreef, "tmp", newname)

      ## Set 0 to NA values
      tdomtreef[is.na(tdomtreef)] <- 0
    }
  }
  setkeyv(tdomtreef, tsumuniqueid)

  ######################################################################## 
  ## If pivot=FALSE
  ######################################################################## 
  if (!pivot) {
    tdoms <- tdomtreef
    tdomscolstot <- newname
    tdomscols <- sort(unique(tdomtreef[[tdomvarnm]]))
    tdomtotnm <- newname

    if (!is.null(tdomvar2)) {
      tdoms <- tdoms[, c(tdomvar, tdomvar2) := tstrsplit(get(tdomvarnm), "#")]
      tdomvarnm <- c(tdomvar, tdomvar2)
    }
  } else {

    ######################################################################## 
    ## If pivot=TRUE, aggregate tree domain data
    ######################################################################## 
    tdoms <- datPivot(tdomtreef, pvar=newname, xvar=tsumuniqueid,
			yvar=tdomvarnm, pvar.round=tround)

    ## check if tree domain in tdomlst.. if not, create column with all 0 values
    tdomscols <- colnames(tdoms)[!colnames(tdoms) %in% tsumuniqueid]
    UNMATCH <- tdomvarlst2[is.na(match(tdomvarlst2, tdomscols))] 
    if (length(UNMATCH) > 0) {
      tdoms[, (UNMATCH) := 0]
      tdomvarlst2 <- c(tdomvarlst2, UNMATCH)
    }

    ## ADD TOTAL OF TREE DOMAINS IN tdomvarlst 
    if ((tdomtot || proportion || cover)) {
      tdomtotnm <- newname

      ## Sum the total tree domains in tdomvarlst after any filtering by plot
      tdoms[, (tdomtotnm) := round(rowSums(.SD, na.rm=TRUE), tround), .SDcols=tdomvarlst2]
      tdomscolstot <- c(tdomvarlst2, tdomtotnm)
    } else {
      tdomscolstot <- tdomvarlst2
    }
 
    ## Create a table of proportions for each tdom by total by plot
    if (proportion) {
      tdoms.prop <- tdoms[, lapply(.SD, function(x, tdomtotnm) round(x / get(eval(tdomtotnm))), 
			tdomtotnm), by=key(tdoms), .SDcols=tdomscolstot]
      setcolorder(tdoms.prop, c(key(tdoms.prop), tdomscolstot))
    }

    ## Create a table of presence/absence (1/0) by plot
    if (presence) {
      tdoms.pres <- tdoms[, lapply(.SD, function(x) x / x), by=key(tdoms), 
		.SDcols=tdomscolstot]
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

  ## Generate tree domain look-up table (tdomvarlut)
  nvar <- ifelse(bycond, "NBRCONDS", "NBRPLOTS")
  byvars <- unique(c(tdomvar, tdomvarnm))
  tdomvarlut <- tdomtreef[, list(sum(get(newname), na.rm=TRUE), .N), by=byvars]
  names(tdomvarlut) <- c(byvars, newname, nvar)

  if (tdomvar == "SPCD") {
    ref_spcd <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "SPCD", c("VALUE", "MEANING")]
    tdomvarlut <- merge(ref_spcd, tdomvarlut, by.x="VALUE", by.y="SPCD")
    names(tdomvarlut)[names(tdomvarlut) %in% c("VALUE", "MEANING")] <- c("SPCD", "SPNM")
  } else if (tdomvar == "SPGRPCD") {
    ref_spgrpcd <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "SPGRPCD", c("VALUE", "MEANING")]
    tdomvarlut <- merge(ref_spgrpcd, tdomvarlut, by.x="VALUE", by.y="SPGRPCD")
    names(tdomvarlut)[names(tdomvarlut) %in% c("VALUE", "MEANING")] <- c("SPGRPCD", "SPGRPNM")
  }        
 
  ## Generate barplot
  if (tdombarplot) {
    ## Frequency
    ylabel <- ifelse(bycond, "Number of Conditions", "Number of Plots")
    datBarplot(x=setDF(tdomvarlut), xvar=tdomvarnm, yvar=newname, savedata=savedata, 
		outfolder=outfolder, ylabel=newname)

    ## Summed variable
    datBarplot(x=setDF(tdomvarlut), xvar=tdomvarnm, yvar=newname, savedata=savedata, 
		outfolder=outfolder, ylabel=newname) 
  }

  ## Merge to cond or plot
  ###################################
  if (bycond && !nocond) {

    ## Check for duplicate names
    matchnames <- sapply(tdomscolstot, checknm, names(condx)) 
    setnames(tdoms, tdomscolstot, matchnames)

    ## Check if class of cuniqueid matches class of cuniqueid
    tabs <- check.matchclass(condx, tdoms, c(cuniqueid, condid))
    condx <- tabs$tab1
    tdoms <- tabs$tab2
     
    ## Merge summed data to cond table
    sumtreef <- merge(condx, tdoms, all.x=TRUE, by=c(cuniqueid, condid))

    for (col in tdomscolstot) set(sumtreef, which(is.na(sumtreef[[col]])), col, 0)
    sumtreef[is.na(sumtreef)] <- 0

    ## Merge proportion table to cond table
    if (proportion) {
      sumtreef.prop <- merge(condx, tdoms.prop, all.x=TRUE)
      for (col in tdomscolstot) set(sumtreef.prop, which(is.na(sumtreef.prop[[col]])), col, 0)
    }
    ## Merge presence table to cond table
    if (presence) {
      sumtreef.pres <- merge(condx, tdoms.pres, all.x=TRUE)
      for (col in tdomscolstot) set(sumtreef.pres, which(is.na(sumtreef.pres[[col]])), col, 0)
    }
    ## Create a table of cover (absolute) based on proportion table and live canopy cover for cond
    if (cover) {
      sumtreef.cov <- copy(sumtreef.prop)
      for (col in tdomscolstot) {set(sumtreef.cov, i=NULL, j=col, 
				value=round(sumtreef.cov[[col]] * sumtreef.cov[[covervar]])) }
    }

  } else if (!noplt) {  ## Plot-level

    ## Check for duplicate names
    matchnames <- sapply(tdomscolstot, checknm, names(pltx)) 
    setnames(tdoms, tdomscolstot, matchnames)

    ## Check if class of cuniqueid matches class of cuniqueid
    tabs <- check.matchclass(pltx, tdoms, puniqueid, cuniqueid)
    pltx <- tabs$tab1
    tdoms <- tabs$tab2

    ## Merge summed data to plt table
    setkeyv(tdoms, tuniqueid)
    sumtreef <- merge(pltx, tdoms, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
    for (col in tdomscolstot) set(sumtreef, which(is.na(sumtreef[[col]])), col, 0)
 
    ## Merge proportion table to plt table
    if (proportion) {
      setkeyv(tdoms.prop, tuniqueid)
      sumtreef.prop <- merge(pltx, tdoms.prop, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
      for (col in tdomscolstot) set(sumtreef.prop, which(is.na(sumtreef.prop[[col]])), col, 0)
    }

    ## Merge presence table to plt table
    if (presence) {
      setkeyv(tdoms.pres, tuniqueid)
      sumtreef.pres <- merge(pltx, tdoms.pres, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
      for (col in tdomscolstot) set(sumtreef.pres, which(is.na(sumtreef.pres[[col]])), col, 0)
    }

    ## Create a table of cover (absolute) based on proportion table and live canopy cover for plot
    if (cover) {
      sumtreef.cov <- copy(sumtreef.prop)
      for (col in tdomscolstot) {set(sumtreef.cov, i=NULL, j=col, 
				value=sumtreef.cov[[col]] * sumtreef.cov[[covervar]]) }
    }
  } else {
    sumtreef <- tdoms

    if (proportion) sumtreef.prop <- tdoms.prop 
    if (presence) sumtreef.pres <- tdoms.pres
  }
  
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
      datExportData(sumtreef, 
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
        datExportData(sumtreef.prop, 
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
        datExportData(sumtreef.pres, 
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
        datExportData(sumtreef.cov, 
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
    
    
    datExportData(tdomvarlut, 
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
    if (returnDT) {
      sumtreef <- setDF(sumtreef)
    }
    tdomdata$tdomdat <- sumtreef
  }
  tdomdata$estunits <- estunits
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
  if (!is.null(tdomtotnm)) {
    tdomdata$tdomtotnm <- tdomtotnm
  }
    
  return(tdomdata)
} 
