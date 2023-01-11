#' Data - Aggregates numeric tree data to the plot or condition-level.
#' 
#' Aggregates numeric tree-level data (e.g., VOLCFNET) to plot or condition,
#' including options for filtering tree data or extrapolating to plot acre by
#' multiplying by TPA.
#' 
#' If variable = NULL, then it will prompt user for input.
#' 
#' Dependent external functions: datFilter Dependent internal functions:
#' addcommas, fileexistsnm, getadjfactor
#' 
#' For adjcond (bycond=FALSE): \cr If you want to get trees-per-acre
#' information aggregated to plot or condition level, you need to include a TPA
#' variable in tree table. \cr For tsumvars = GROWCFGS, GROWBFSL, GROWCFAL,
#' FGROWCFGS, FGROWBFSL, or FGROWCFAL, you must have TPAGROW_UNADJ \cr For
#' tsumvars = MORTCFGS, MORTBFSL, MORTCFAL, FMORTCFGS, FMORTBFSL, or FMORTCFAL,
#' you must have TPAMORT_UNADJ \cr For tsumvars = REMVCFGS, REMVBFSL, REMVCFAL,
#' FREMVCFGS, FREMVBFSL, or FREMVCFAL, you must have TPAREMV_UNADJ \cr
#' 
#' If you want to adjust plot-level information by condition proportions
#' (adjplot), you need to include CONDID & CONDPROP_UNADJ in cond or tree table
#' and COND_STATUS_CD and FORTYPCD if adjplot="FVS". For adjplot="WEIGHTED" -
#' each tree is weighted (or multiplied) by the condition proportions
#' (CONDPROP_UNADJ). For adjplot="FVS", each tree is weighted (or multiplied)
#' by the sum of the condition proportions (CONDPROP_UNADJ) that are forested
#' and not nonstocked (COND_STATUS_CD = 1 and FORTYPCD != 999).  \cr
#' 
#' @param tree Dataframe or comma-delimited file (*.csv). The tree-level table.
#' @param seed Dataframe or comma-delimited file (*.csv). The seedling table.
#' @param cond Dataframe or comma-delimited file (*.csv). Condition-level table
#' to join the aggregated tree data to, if bycond=TRUE. This table also may be
#' used for condition proportion or strata variables used if adjcond or
#' adjstrata = TRUE (See details below).  This table is optional.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param plt Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Plot-level table to join the aggregated tree data to, if bycond=FALSE. This
#' table is optional.
#' @param plt_dsn String. The data source name (dsn; i.e., folder or database
#' name) of plt. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional.
#' @param tuniqueid String. Unique identifier of plt in tree table.
#' @param cuniqueid String. Unique identifier of plt in cond table if cond is
#' NOT NULL.
#' @param puniqueid String. Unique identifier of plt table if plt is NOT NULL.
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid).
#' @param condid String. Unique identifier for conditions.
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param subpid String. Unique identifier of each subplot.
#' @param tsumvarlst String (vector). Tree-level variable(s) to aggregate
#' (e.g., "TPA_UNADJ", "BA"). Use tsumvar="PLT_CN" (tfun=sum) for summed tree
#' count.
#' @param tsumvarnmlst String (vector). Name of the tree-level variable(s) to
#' aggregate (e.g., "TPALIVE", "BALIVE"). This list must have the same number
#' of variables as tsumvarlst and be in respective order. If NULL, the default
#' names will be tsumvar'_tfun' (e.g., "TPA_UNADJ_SUM", "BA_SUM").
#' @param TPA Logical. If TRUE, tsumvarlst variable(s) are multiplied by the
#' respective trees-per-acre variable (see details) to get per-acre
#' measurements.
#' @param tfun Function. The name of the function to use to aggregate the data
#' (e.g., sum, mean, max).
#' @param ACI Logical. If TRUE, if ACI (All Condition Inventory) plots exist,
#' any trees on these plots will be included in summary. If FALSE, you must
#' include condition table.
#' @param tfilter String. Filter to subset the tree data before aggregating
#' (e.g., "STATUSCD == 1"). This must be in R syntax. If tfilter=NULL, user is
#' prompted.  Use tfilter="NONE" if no filters.
#' @param addseed Logical. If TRUE, add seedling data to tree counts (if TPA
#' variable in tsumvarlst).
#' @param lbs2tons Logical. If TRUE, converts biomass or carbon variables from
#' pounds to tons. If metric=TRUE, converts to metric tons, else short tons.
#' @param metric Logical. If TRUE, converts response to metric units based on
#' FIESTA::ref_conversion, if any variable in tsumvarlst is in
#' FIESTAutils::ref_estvar.  Note: if TPA, TPA is converted to trees per hectare
#' (TPH: 1 / (1/ tpavar * 0.4046860)).
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
#' default = 'treesum'. 
#' @param gui Logical. If gui, user is prompted for parameters.
#' 
#' @return A list of the following items: \item{treedat}{ Data frame. Plot or
#' condition-level table with aggregated tree attributes. } \item{sumvars}{
#' String vector. Name(s) of the output aggregated tree attributes. }
#' 
#' If savedata=TRUE\cr - treedat will be saved to the outfolder. \cr - a text
#' file of input parameters is saved to outfolder
#' ('outfn'_parameters_'date'.txt).
#' @note If a dat table is provided, the aggregated tree data will be merged to
#' table and NULL values will be output as 0.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Aggregate LIVE_CANOPY_CVR_PCT to plot, weighted by CONDPROP_UNADJ
#' treesum <- datSumTree(tree = FIESTA::WYtree,
#'                       tsumvarlst = "TPA_UNADJ")$treedat
#' 
#' # Check results
#' treesum[treesum$PLT_CN == 40404737010690,]
#' FIESTA::WYtree[FIESTA::WYtree$PLT_CN == 40404737010690,]
#' @export datSumTree
datSumTree <- function(tree = NULL, 
                       seed = NULL, 
                       cond = NULL, 
                       datsource = "obj", 
                       data_dsn = NULL, 
                       plt = NULL, 
                       plt_dsn = NULL, 
                       tuniqueid = "PLT_CN", 
                       cuniqueid = "PLT_CN", 
                       puniqueid = "CN", 
                       bycond = FALSE, 
                       condid = "CONDID", 
                       bysubp = FALSE, 
                       subpid = "SUBP", 
                       tsumvarlst = NULL, 
                       tsumvarnmlst = NULL, 
                       TPA = TRUE, 
                       tfun = sum, 
                       ACI = FALSE, 
                       tfilter = NULL, 
                       addseed = FALSE, 
                       lbs2tons = TRUE, 
                       metric = FALSE, 
                       getadjplot = FALSE, 
                       adjtree = FALSE, 
                       adjvar = "tadjfac", 
                       adjTPA = 1, 
                       NAto0 = FALSE, 
                       tround = 5, 
                       checkNA = FALSE, 
                       returnDT = TRUE,
                       savedata = FALSE, 
                       savedata_opts = NULL,
                       gui = FALSE) {
  ####################################################################################
  ## DESCRIPTION: Aggregates tree variable(s) to plot(/cond)-level, 
  ##        using specified tree filters (ex. live trees only)
  ####################################################################################
  
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  COND_STATUS_CD=PLOT_STATUS_CD=COUNT=plts=SUBP=NF_COND_STATUS_CD=
	seedf=TREECOUNT_CALC=estunits=fname=NF_SUBP_STATUS_CD=
	CONDPROP_UNADJ=MACRPROP_UNADJ=SUBPPROP_UNADJ <- NULL
  subpuniqueid <- "PLT_CN"


  ## If gui.. set variables to NULL
  if (gui) ACI=bycond=tuniqueid=puniqueid=cuniqueid=TPA=tfun=adjtree=adjsamp=
	savedata=outfolder <- NULL
  checkNApvars <- {}
  checkNAcvars <- {}
  checkNAtvars <- {}
  seedonly=parameters <- FALSE
  ref_estvar <- FIESTAutils::ref_estvar
  subplot=subpcond <- NULL

  ## For documentation
  # subplot Dataframe or comma-delimited file (*.csv). If getadjplot=TRUE, 
  # The subplot-level table with SUBP_STATUS_CD variable for calculating
  # adjustment factors by subplot.


  ## SET VARIABLE LISTS
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


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumTree)) 
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
  condx <- pcheck.table(cond, tab_dsn=data_dsn, tabnm="cond", gui=gui, 
			caption="Condition table?")

  ## Check addseed
  addseed <- pcheck.logical(addseed, varnm="addseed", title="Add seeds?", 
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
    ## Check subplot
    subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot", gui=gui, 
			caption="Subplot table?")

    ## Check subplot
    subpcondx <- pcheck.table(subpcond, tab_dsn=data_dsn, tabnm="subp_cond", gui=gui, 
			caption="Subpcond table?")
  }

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

  ## Check unique ids and set keys
  if (bycond) {
    pltsp <- FALSE
    noplt <- TRUE

    ## Check condid in tree table and setkey to tuniqueid, condid
    condid <- pcheck.varchar(var2check=condid, varnm="condid", 
		checklst=names(treex), caption="cond ID - tree", 
		warn=paste(condid, "not in tree table"))
 
    if (is.null(condid)) {
      message("assuming all 1 condition plots")
      treex$CONDID <- 1
      condid <- "CONDID"

      if (addseed) {
        if (!condid %in% names(seedx)) {
          seedx$CONDID <- 1
        } else {
          stop(condid, "in seed but not in cond")
        }
      }  
    } else {
      if (addseed && !condid %in% names(seedx)) {
        stop(condid, "not in seed") 
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
        ## Check if class of tuniqueid matches class of cuniqueid
        tabs <- check.matchclass(seedx, condx, tuniqueid, cuniqueid)
        seedx <- tabs$tab1
        condx <- tabs$tab2

        ## Check that values of tuniqueid in treex are all in puniqueid in pltx
        seedx <- check.matchval(seedx, condx, tsumuniqueid, csumuniqueid)
      }
    } 
  } else {
    tsumuniqueid <- tuniqueid
  }

  if (bysubp) {
    pltsp <- FALSE
    noplt <- TRUE
    nocond <- TRUE

    ## Check subpdid in tree table and setkey to tuniqueid, condid
    subpid <- pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(treex), caption="subplot ID - tree", 
		warn=paste(subpid, "not in tree table"))
    if (is.null(subpid)) {
      warning("assuming only 1 subplot")
      treex$SUBPID <- 1
      subpid <- "SUBPID"

      if (addseed) {
        if (!subpid %in% names(seedx)) {
          seedx$SUBPID <- 1
        } else {
          stop(subpid, "in seed but not in cond")
        }
      }  
    } else {
      if (!subpid %in% names(seedx)) {
        stop(subpid, "not in seed") 
      }
    }

    tsumuniqueid <- c(tsumuniqueid, subpid)
    setkeyv(treex, tsumuniqueid)
    checkNAtvars <- c(checkNAtvars, subpid)
    if (addseed) {
      setkeyv(seedx, tsumuniqueid)
    }
  } 

  pltx <- pcheck.table(plt, tab_dsn=plt_dsn, gui=gui, tabnm="plt", 
			caption="Plot table?")
 
  if (!is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("PLOT_STATUS_CD" %in% names(pltx)) {
      if (3 %in% unique(pltx[["PLOT_STATUS_CD"]])) {
        message(paste("there are", sum(pltx[["PLOT_STATUS_CD"]] == 3), "nonsampled plots"))
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

    setkeyv(pltx, puniqueid)
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
      stop("not available yet")
      if (is.null(subplotx) || is.null(subpcondx)) {
        stop("must include subplotx and subpcondx to adjust to subplot")
      }
    } else {
      if (is.null(condx)) {
        stop("must include condx to adjust to plot")
      }
    }
  }

      
  ###########################################################  
  ### Check tsumvarlst
  ###########################################################  
  tsumvarlst <- pcheck.varchar(var2check=tsumvarlst, 
	varnm="tsumvarlst", checklst=names(treex), caption="Aggregate variable(s)", 
	multiple=TRUE, stopifnull=TRUE, gui=gui)
  if (any(tsumvarlst == tuniqueid)) {
    tsumvarlst[tsumvarlst == tuniqueid] <- "TPA_UNADJ"
  }
  
  ### Convert variables from pound to tons if lbs2tons=TRUE
  if (lbs2tons && any(tsumvarlst %in% vars2convert)) {
    convfac <- ifelse(metric, 0.00045359237, 0.0005)
    vars2convert <- tsumvarlst[which(tsumvarlst %in% vars2convert)]
    message("converting from pounds to tons: ", paste(vars2convert, collapse=", "))
    for (j in vars2convert) set(treex, i=NULL, j=j, value=treex[[j]] * convfac)
  }

  ## Check metric and convert
  metric <- pcheck.logical(metric, varnm="metric", title="Metric converstion?", 
	first="NO", stopifnull=TRUE, gui=gui)

  ## Check TPA and if the TPA variable is in treex
  TPA <- pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO", 
		stopifnull=TRUE, gui=gui)
 
  if (TPA) {
    if (any(tsumvarlst %in% mortvars)) {
      if (!"TPAMORT_UNADJ" %in% names(treex)) {
        stop("you must have TPAMORT_UNADJ in tree table to calculate trees per acre")
      }
    } else if (any(tsumvarlst %in% growvars)) {
      if (!"TPAGROW_UNADJ" %in% names(treex)) {
        stop("you must have TPAGROW_UNADJ in tree table to calculate trees per acre")
      }
    } else if (any(tsumvarlst %in% remvars)){
      if (!"TPAREMV_UNADJ" %in% names(treex)) {
        stop("you must have TPAREMV_UNADJ in tree table to calculate trees per acre")
      }
    } else {  
      if (!"TPA_UNADJ" %in% names(treex)) {
        stop("you must have TPA_UNADJ in tree table to calculate trees per acre")
      }
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
  }
 
  ### GET tfun USED FOR AGGREGATION
  tfunlst <- c("sum", "mean", "max", "min", "length", "median")

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
    if(tuniqueid %in% tsumvarlst & !identical(tfun, sum))
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

  ## Get name for summed tree variable(s)
  getnm <- FALSE
  if (is.null(tsumvarnmlst)) {
    getnm <- TRUE
  } else {
    if (length(tsumvarnmlst) != length(tsumvarlst)) {
      message(paste("number of names in tsumvarnmlst does not match number of tsumvars.",
 		"using default names."))
      getnm <- TRUE
    }
  } 

  ### Filter tree data 
  ###########################################################  

  ## FILTER TREES FOR DIA >= 1
#  if ("DIA" %in% names(treex)) 
#    treef <- datFilter(x=treex, xfilter="DIA >= 1.0", title.filter="DIA < 1.0in")$xf 
  
  ## Tree filter
  tdat <- datFilter(x=treex, xfilter=tfilter, title.filter="tfilter", 
		stopifnull=TRUE, gui=gui)
  treef <- tdat$xf
  tfilter <- tdat$xfilter

  if (addseed || seedonly) {
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

  ## CHECK tround
  if (is.null(tround) | !is.numeric(tround)) {
    warning("tround is invalid.. rounding to 6 digits")
    tround <- 6
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
      out_layer <- "treesum"
    }
  }
  
  ################################################################################  
  ################################################################################  
  ### DO WORK
  ################################################################################ 
  ################################################################################  

  if (getadjplot) {

    ## Remove nonsampled conditions by subplot
    if (bysubp) {
      stop("not figured out yet")

      if ("SUBP_STATUS_CD" %in% names(subplotx)) {
        subp.nonsamp.filter <- "SUBP_STATUS_CD != 3"
        nonsampn <- sum(subplotx[["SUBP_STATUS_CD"]] == 3, na.rm=TRUE)
        if (length(nonsampn) > 0) {
          message("removing ", nonsampn, " nonsampled forest conditions")
        } else {
          message("assuming all sampled conditions in subplot")
        }
      } else {
        message("assuming all sampled conditions in subplot")
      }
      
      if (ACI && "NF_COND_STATUS_CD" %in% names(subplotx)) {
        subp.nonsamp.filter.ACI <- "(is.na(NF_SUBP_STATUS_CD) | NF_SUBP_STATUS_CD != 3)"
        message("removing ", sum(is.na(NF_SUBP_STATUS_CD) & NF_SUBP_STATUS_CD == 3, na.rm=TRUE), 
		" nonsampled nonforest conditions")
        if (!is.null(subp.nonsamp.filter)) {
          subp.nonsamp.filter <- paste(subp.nonsamp.filter, "&", subp.nonsamp.filter.ACI)
        }
      }
      subplotx <- datFilter(x=subplotx, xfilter=subp.nonsamp.filter, 
		title.filter="subp.nonsamp.filter")$xf


      ## Check subpuniqueid and subpid in subplot table
      subpuniqueid <- pcheck.varchar(var2check=subpuniqueid, varnm="subpuniqueid", 
		checklst=names(subplotx), caption="Plot ID - subplot", 
		warn=paste(subpid, "not in tree table"))
      subpid <- pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(subplotx), caption="subplot ID - subplot", 
		warn=paste(subpid, "not in tree table"))
      setkeyv(subplotx, c(subpuniqueid, subpid))

      ## Check subpuniqueid and subpid in subpcond table
      if (!all(c(subpuniqueid, subpid) %in% names(subpcondx))) {
        stop("need ", subpuniqueid, " and ", subpid, " variables in subpcond")
      }
      setkeyv(subpcondx, c(subpuniqueid, subpid))


      if (!is.null(condx)) {
        if ("SUBP_STATUS_CD" %in% names(subpcondx) && "COND_STATUS_CD" %in% names(condx)) {
          sumcprop.qry <- paste0("SELECT subp.", subpuniqueid, ", subp.", subpid,  
                    ", SUM(COALESCE(subc.SUBPCOND_PROP,0)) / 4 AS SUBPPROP_UNADJ,
                    SUM(COALESCE(subc.MICRCOND_PROP,0)) / 4 AS MICRPROP_UNADJ,
                    SUM(COALESCE(subc.MACRCOND_PROP,0)) / 4 AS MACRPROP_UNADJ
                FROM condx c
                JOIN subplotx subp ON(subp.", subpuniqueid, " = c.", cuniqueid, ")
                JOIN subpcondx subc ON(subc.", subpuniqueid, " = c.", cuniqueid, " and subc.CONDID=c.CONDID)
                                  and subc.", subpid, " = subp.", subpid, ")
        		WHERE subp.SUBP_STATUS_CD < 3 AND c.COND_STATUS_CD <> 5
        		GROUP BY subp.PLT_CN")
        } else {
          message("need to include SUBP_STATUS_CD and COND_STATUS_CD")
        }
      } else {
        if ("SUBP_STATUS_CD" %in% names(subpcondx)) {
          sumcprop.qry <- paste0("SELECT subp.", subpuniqueid, ", subp.", subpid,  
                    ", SUM(COALESCE(subc.SUBPCOND_PROP,0)) / 4 AS SUBPPROP_UNADJ,
                    SUM(COALESCE(subc.MICRCOND_PROP,0)) / 4 AS MICRPROP_UNADJ,
                    SUM(COALESCE(subc.MACRCOND_PROP,0)) / 4 AS MACRPROP_UNADJ
                FROM subplotx subp 
                JOIN subpcondx subc ON(subc.", subpuniqueid, " = subp.", subpuniqueid, ")
                                   and subc.", subpid, " = subp.", subpid, ")
        		WHERE subp.SUBP_STATUS_CD < 3 AND c.COND_STATUS_CD <> 5
        		GROUP BY subp.PLT_CN")
        } else {
          message("need to include SUBP_STATUS_CD")
        }
      }
      subpcx <- data.table(sqldf::sqldf(sumcprop.qry))
      subpcx[, CONDPROP_UNADJ := ifelse(MACRPROP_UNADJ > 0, MACRPROP_UNADJ, SUBPPROP_UNADJ)]

    
      ## Check if class of tuniqueid matches class of cuniqueid
      tabs <- check.matchclass(treex, subpcx, c(tuniqueid, subpid), c(subpuniqueid, subpid))
      treex <- tabs$tab1
      subpcondx <- tabs$tab2

      adjfacdata <- getadjfactorVOL(treex=treef, seedx=seedf, condx=subpcx, 
		tuniqueid=c(tuniqueid, subpid), cuniqueid=c(subpuniqueid, subpid), adj="plot",
		areawt="CONDPROP_UNADJ")
    

    } else {
      ## Remove nonsampled conditions  
      if ("COND_STATUS_CD" %in% names(condx)) {
        cond.nonsamp.filter <- "COND_STATUS_CD != 5"
        nonsampn <- sum(condx[["COND_STATUS_CD"]] == 5, na.rm=TRUE)
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
        if (!is.null(cond.nonsamp.filter)) {
          cond.nonsamp.filter <- paste(cond.nonsamp.filter, "&", cond.nonsamp.filter.ACI)
        }
      }
      condx <- datFilter(x=condx, xfilter=cond.nonsamp.filter, 
		title.filter="cond.nonsamp.filter")$xf
    
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

      adjfacdata <- getadjfactorVOL(treex=treef, seedx=seedf, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid, adj="plot")
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
  tsumvarlst2 <- {}
  tsumvarnmlst2 <- {} 
  seedcountvar <- NULL

  ## If any variable in tsumvarlst is a TPA variable, add a count variable to treex
  if (any(tsumvarlst %in% tpavars)) {
    treef[, COUNT := 1]
    if (addseed || seedonly) {
      seedf[, COUNT := 1]
    }
  }   
 
  ## ADDS '_TPA' TO VARIABLE NAME, MULTIPLIES BY TPA_UNADJ, AND DIVIDES BY adjfac
  for (tvar in tsumvarlst) {
    if (!is.null(tfilter)) {
      ref <- ref_estvar[ref_estvar$ESTVAR %in% tvar, ] 
      ref <- ref[grep(gsub(" ", "", tfilter), gsub(" ", "", ref$ESTFILTER)), ]
      fname <- ref[, "FILTERNM"][1]
      if (!is.na(fname)) {
        if (fname == "standing-dead") fname <- "dead"
      }
    }

    if (tvar %in% c(tuniqueid, tpavars)) {
      tvar <- "COUNT"
    }
    if (tvar != "COUNT") {
      if (tvar %in% ref_estvar$ESTVAR) { 
        estunits <- unique(ref_estvar$ESTUNITS[ref_estvar$ESTVAR == tvar])
      } else {
        if (metric) {
          message(tvar, " not in ref_estvar... no metric conversion")
          metric <- FALSE
        } else {
          message(tvar, " not in ref_estvar... no units found")
        }
      }
    
      if (metric) {
        metricunits <- unique(ref_estvar$METRICUNITS[ref_estvar$ESTVAR == tvar])
        if (estunits != metricunits) {
          cfactor <- FIESTA::ref_conversion$CONVERSION[FIESTA::ref_conversion$METRIC == 
			metricunits]
          tvarm <- paste0(tvar, "_m")
          treef[, (tvarm) := get(eval(tvar)) * cfactor]
          estunits <- metricunits
          tvar <- tvarm
        }
      }
    } 
 
    ## MULTIPLY tvar BY TPA VARIABLE IF DESIRED
    if (TPA) {
      if (tvar %in% mortvars) {
        tpavar <- "TPAMORT_UNADJ"
      } else if (tvar %in% growvars) {
        tpavar <- "TPAGROW_UNADJ"
      } else if (tvar %in% remvars) {
        tpavar <- "TPAREMV_UNADJ"
      } else{
        #tpavar <- "TPAGROW_UNADJ"
        tpavar <- "TPA_UNADJ"
      } 

      ## Add filter name (e.g., live/dead) to newname
      if (!is.null(fname) && !is.na(fname)) {
        newname <- paste0(tvar, "_TPA", "_", fname)
      } else {
        newname <- paste0(tvar, "_TPA")
      }
      ## Adjust by adjTPA variable (Default is 1)
      if (adjTPA > 1) {
        treef[, (tpavar) := get(eval(tpavar)) * adjTPA]
      }
      ## If metric, convert tpavar to trees per hectare
      if (metric) {
        tpa.m <- paste0(tpavar, "_m")
        treef[, (tpa.m) := 1 / ((1/ get(eval(tpavar)) * 0.4046860))]
        tpavar <- tpa.m
      }
      treef[, (newname) := get(eval(tvar)) * get(eval(tpavar))]

      if ((addseed || seedonly) && tvar=="COUNT" && tpavar %in% names(seedf)) {
        #seedf[, COUNT := TREECOUNT_CALC]
        if (adjTPA > 1) {
          seedf[, (tpavar) := get(eval(tpavar)) * adjTPA]
        }
        seedf[, (newname) := get(eval(tvar)) * get(eval(tpavar))]
        seedcountvar=treecountvar <- newname
      }
    } else {
      if (!is.null(fname) && !is.na(fname)) {
        newname <- paste0(tvar, "_", fname)
        setnames(treef, tvar, newname)
      } else {
        newname <- tvar
      }
    }

    ## ADJUSTMENT FACTORS
    if (adjtree) {
      ## Create new name for adjusted variable
      if (length(grep("UNADJ", tvar)) == 1) {
        newname2 <- sub("UNADJ", "ADJ", tvar)
      } else {
        newname2 <- paste0(newname, "_ADJ")
      }

      ## Apply adjustments
      treef[, (newname2) := get(eval(newname)) * get(eval(adjvar))]
   
      if ((addseed || seedonly) && tvar=="COUNT" && adjvar %in% names(seedf)) {
        seedf[, (newname2) := get(eval(newname)) * get(eval(adjvar))]
        seedcountvar=treecountvar <- newname2
      }       
    } else {
      newname2 <- newname
    }

    tsumvarlst2 <- c(tsumvarlst2, newname2)
    if (getnm) {
      if (toupper(tfunstr) != "SUM") {
        tsumvarnmlst <- c(tsumvarnmlst, paste0(newname2, "_", toupper(tfunstr)))  
        tsumvarnmlst2 <- sapply(tsumvarnmlst, checknm, names(treef))
      } else {
        tsumvarnmlst2 <- c(tsumvarnmlst2, newname2)  
      }
    } else {
      tsumvarnmlst2 <- tsumvarnmlst
    } 
  }
 
  ######################################################################## 
  ## Aggregate tree variables
  ######################################################################## 

  if (seedonly) {
    datvars <- seedf[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=tsumuniqueid, .SDcols=tsumvarlst2]
    setnames(datvars, c(tsumuniqueid, tsumvarnmlst2))
  } else {
    datvars <- treef[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=tsumuniqueid, .SDcols=tsumvarlst2]
    setnames(datvars, unique(c(tsumuniqueid, tsumvarnmlst2)))

    if (addseed && !is.null(seedcountvar)) {
      sdatvars <- seedf[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=key(seedf), .SDcols=seedcountvar]
      setnames(sdatvars, c(tsumuniqueid, paste0("SEED_", seedcountvar)))

      ## Merge using all.x and all.y in case there are plots with seedlings, no trees
      datvars <- merge(datvars, sdatvars, all.x=TRUE, all.y=TRUE)
      datvars[, (paste0("TREE_", treecountvar)) := get(treecountvar)]
      datvars[, (treecountvar) := sum(.SD, na.rm=TRUE), by=key(datvars), 
			.SDcols=c(paste0("TREE_", treecountvar), paste0("SEED_", treecountvar))]
      tsumvarnmlst2 <- treecountvar	
    } 
  }   
 
  ######################################################################## 
  ######################################################################## 
 
  ## Merge to cond or plot
  ###################################
  if (bycond && !nocond) {
    ## Merge to cond
    sumdat <- merge(condx, datvars, by.x=c(cuniqueid, condid),
		by.y=c(tuniqueid, condid), all.x=TRUE)
  } else if (!noplt) {
    if (is.data.table(pltx)) {
      setkeyv(datvars, tuniqueid)
      setkeyv(pltx, puniqueid)
    }
    sumdat <- merge(pltx, datvars, by.x=puniqueid, by.y=tuniqueid, all.x=TRUE)
  } else {
    sumdat <- datvars
  }

  if (NAto0) 
    ## Change NA values TO 0
    for (col in tsumvarnmlst2) set(sumdat, which(is.na(sumdat[[col]])), col, 0) 

#   if (savedata && parameters) {
#     ## OUTPUTS A TEXTFILE OF INPUT PARAMETERS TO OUTFOLDER
#     ###########################################################
#     outfn.param <- paste(out_layer, "parameters", sep="_")
#     outparamfnbase <- paste(outfn.param, format(Sys.time(), "%Y%m%d"), sep="_")
#     outparamfn <- fileexistsnm(outfolder, outparamfnbase, "txt")
# 
#     tsumvarlstout <- addcommas(sapply(tsumvarlst, function(x) {paste0("'", x, "'")}))
#     tsumvarnmlstout <- addcommas(sapply(tsumvarnmlst, function(x) {paste0("'", x, "'")}))
#     strunitvars <- addcommas(sapply(strunitvars, function(x) {paste0("'", x, "'")}))
# 
#     outfile <- file(paste0(outfolder, "/", outparamfn, ".txt"), "w")
#     cat(  "tree = ", as.character(bquote(tree)), "\n",
#       "cond = ", as.character(bquote(cond)), "\n",
#       "plt = ", as.character(bquote(plt)), "\n",
#       "plt_dsn = \"", plt_dsn, "\"", "\n",
#       "tuniqueid = \"", tuniqueid, "\"", "\n",
#       "cuniqueid = \"", cuniqueid, "\"", "\n",
#       "puniqueid = \"", puniqueid, "\"", "\n",
#       "bycond = ", bycond, "\n",
#       "condid = \"", condid, "\"", "\n",
#       "bysubp = ", bysubp, "\n",
#       "subpid = \"", subpid, "\"", "\n",
#       "tsumvarlst = c(", tsumvarlstout, ")", "\n",
#       "tsumvarnmlst = c(", tsumvarnmlstout, ")", "\n",  
#       "TPA = ", TPA, "\n",
#       "tfun = ", noquote(tfunstr), "\n",
#       "ACI = ", ACI, "\n",
#       "tfilter = \"", tfilter, "\"", "\n",
#       "lbs2tons = ", lbs2tons, "\n",
#       "getadjplot = ", getadjplot, "\n",
#       "adjtree = ", adjtree, "\n",
#       "adjTPA = ", adjTPA, "\n",
#       "NAto0 = ", NAto0, "\n",
#       "savedata = ", savedata, "\n",
#       "outfolder = \"", outfolder, "\"", "\n",
#       "out_layer = ", out_layer, "\n",
#       "outfn.date = ", outfn.date, "\n",
#       "overwrite_dsn = ", overwrite_dsn, "\n",
#       "tround = \"", tround, "\"", "\n", "\n",
#     file = outfile, sep="")
# 
#     cat(  "sumdat <- datSumTree(tree=tree, cond=cond, plt=plt, plt_dsn=plt_dsn,
# 	tuniqueid=tuniqueid, cuniqueid=cuniqueid, puniqueid=puniqueid, bycond=bycond, 
# 	condid=condid, bysubp=bysubp, subpid=subpid, tsumvarlst=tsumvarlst, 
# 	tsumvarnmlst=tsumvarnmlst, TPA=TPA, tfun=tfun, ACI=ACI, tfilter=tfilter, 
# 	lbs2tons=lbs2tons, getadjplot=getadjplot, adjtree=adjtree, adjTPA=adjTPA, 
# 	NAto0=NAto0, savedata=savedata, outfolder=outfolder, out_layer=out_layer, 
# 	outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, tround=tround)",
#     file = outfile, sep="")
#     close(outfile)
#   }

  ## Get metadata
  #############################################################  
  if (bycond) {
    meta = FIESTA::ref_cond[FIESTA::ref_cond$VARIABLE %in% names(sumdat), ]
    missnames <- names(sumdat)[!names(sumdat) %in% meta$VARIABLE]
    meta2 = FIESTA::ref_plt[FIESTA::ref_plt$VARIABLE %in% missnames, ]
    if (nrow(meta2) > 0) {
      meta <- rbind(meta, meta2)
    } 
  } else {
    meta = FIESTA::ref_plt[names(sumdat) %in% FIESTA::ref_plt$VARIABLE, ]
  }

  metanames <- names(sumdat)[which(names(sumdat) %in% meta$VARIABLE)]
  meta <- meta[meta$VARIABLE %in% metanames, ]
  meta <- meta[match(metanames, meta$VARIABLE),]

  tree_ref <- FIESTA::ref_tree[match(tsumvarlst, FIESTA::ref_tree$VARIABLE),]
  tree_ref$VARIABLE[tree_ref$VARIABLE == "TPA_UNADJ"] <- "COUNT"

  if (nrow(tree_ref) > 0) {
    tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_TPA")
    if (!is.null(fname)) {
      tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_", fname)
      tree_ref$DESCRIPTION <- paste0(tree_ref$DESCRIPTION, " (", tfilter, ")")
    }
    if (adjtree) {
      tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_ADJ")
      tree_ref$DESCRIPTION <- paste(tree_ref$DESCRIPTION, "- adjusted for partial nonresponse at plot-level")
    }
    meta <- rbind(meta, tree_ref)
  }

  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if (pltsp) {
      spExportSpatial(sumdat, 
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
      datExportData(sumdat, 
            savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=out_layer,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE)) 

      datExportData(meta, 
            savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer="meta",
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE)) 

    }
  } 

  if (!returnDT) {     
    sumdat <- setDF(sumdat)
  }
  sumtreelst <- list(treedat=sumdat, sumvars=tsumvarnmlst2)
  sumtreelst$estunits <- estunits
  if (!is.null(tfilter)) {
    sumtreelst$tfilter <- tfilter
  }
  sumtreelst$meta <- meta
  return(sumtreelst)
} 
