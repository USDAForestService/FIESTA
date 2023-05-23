#' Data - Aggregates numeric tree data to the plot or condition-level.
#' 
#' Aggregates numeric tree-level data (e.g., VOLCFNET) to plot or condition,
#' including options for filtering tree data or extrapolating to plot aseedonlycre by
#' multiplying by TPA.
#' 
#' If variable = NULL, then it will prompt user for input.
#' 
#' Dependent external functions: datFilter Dependent internal functions:
#' addcommas, fileexistsnm, getadjfactor
#' 
#' For adjcond (bycond=FALSE): \cr If you want to summarize trees-per-acre
#' information aggregated to plot or condition level, you need to include a TPA
#' variable in tree table. \cr For tsumvars = GROWCFGS, GROWBFSL, GROWCFAL,
#' FGROWCFGS, FGROWBFSL, or FGROWCFAL, you must have TPAGROW_UNADJ \cr For
#' tsumvars = MORTCFGS, MORTBFSL, MORTCFAL, FMORTCFGS, FMORTBFSL, or FMORTCFAL,
#' you must have TPAMORT_UNADJ \cr For tsumvars = REMVCFGS, REMVBFSL, REMVCFAL,
#' FREMVCFGS, FREMVBFSL, or FREMVCFAL, you must have TPAREMV_UNADJ \cr
#' 
#' If you want to adjust plot-level or subplot-level information by condition 
#' proportions (adjplot), you need to include CONDID & CONDPROP_UNADJ in cond 
#' or tree table and COND_STATUS_CD. \cr
#' 
#' @param tree Dataframe or comma-delimited file (*.csv). The tree-level table.
#' @param seed Dataframe or comma-delimited file (*.csv). The seedling table.
#' @param cond Dataframe or comma-delimited file (*.csv). Condition-level table
#' to join the aggregated tree data to, if bycond=TRUE. This table also may be
#' used for condition proportion or strata variables used if adjcond or
#' adjstrata = TRUE (See details below).  This table is optional.
#' @param plt Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Plot-level table to join the aggregated tree data to, if bycond=FALSE. This
#' table is optional.
#' @param subp_cond Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot condition-level table to use to sum condition proportions, 
#' if bysubp=TRUE. 
#' @param subplot Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot-level table to used to calculate adjustment factors, to remove 
#' nonsampled conditions (SUBP_STATUS_CD = 3). This table is optional. If 
#' included the aggregated tree data are joined to subplot before returning.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param tuniqueid String. Unique identifier of plt in tree table.
#' @param cuniqueid String. Unique identifier of plt in cond table if cond is
#' NOT NULL.
#' @param puniqueid String. Unique identifier of plt table if plt is NOT NULL.
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). If bysubp = TRUE and bycond = TRUE, data are 
#' aggregated by subpuniqueid, subpid, condid.
#' @param condid String. Unique identifier for conditions.
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param subpuniqueid String. Unique identifier of plot in subplot and 
#' subp_cond table.
#' @param subpid String. Unique identifier of each subplot.
#' @param tsumvarlst String (vector). Tree-level variable(s) to aggregate
#' (e.g., "TPA_UNADJ", "BA"). Use "TPA_UNADJ" (tfun=sum) for summed tree
#' count.
#' @param tsumvarnmlst String (vector). Name of the tree-level variable(s) to
#' aggregate (e.g., "TPALIVE", "BALIVE"). This list must have the same number
#' of variables as tsumvarlst and be in respective order. If NULL, the default
#' names will be tsumvar'_tfun' (e.g., "TPA_UNADJ_SUM", "BA_SUM").
#' @param addseed Logical. If TRUE, add seedling counts to tree counts. Note:
#' tdomvar must be 'SPCD' or 'SPGRPCD'.
#' @param seedonly Logical. If TRUE, seedling counts only. Note: tdomvar
#' must be 'SPCD' or 'SPGRPCD'.
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
                       subpuniqueid = "PLT_CN",
                       subpid = "SUBP", 
                       tsumvarlst = NULL, 
                       tsumvarnmlst = NULL, 
                       addseed = FALSE, 
                       seedonly = FALSE,
                       TPA = TRUE, 
                       tfun = sum, 
                       ACI = FALSE, 
                       tfilter = NULL, 
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
      seedx=TREECOUNT_CALC=estunits=fname=NF_SUBP_STATUS_CD=
      CONDPROP_UNADJ=MACRPROP_UNADJ=SUBPPROP_UNADJ=sumbcvars=treex=
      cond.nonsamp.filter  <- NULL


  ## If gui.. set variables to NULL
  if (gui) ACI=bycond=tuniqueid=puniqueid=cuniqueid=TPA=tfun=adjtree=adjsamp=
	savedata=outfolder <- NULL
  checkNApvars <- {}
  checkNAcvars <- {}
  checkNAtvars <- {}
  parameters <- FALSE
  ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry <- NULL


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

  ## Check bycond
  ###########################################################################
  bycond <- pcheck.logical(bycond, varnm="bycond", title="By condition?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check bysubp
  ###########################################################################
  bysubp <- pcheck.logical(bysubp, varnm="bysubp", title="By subplot?", 
		first="YES", gui=gui, stopifnull=TRUE)


  ## Check tree, seed tables
  ###########################################################################
  treenm=seednm=dbname <- NULL
  if (datsource %in% c("obj", "csv")) {
    treex <- pcheck.table(tree, gui=gui, tabnm="tree", caption="Tree table?")
    if (!is.null(treex)) {
      treenames <- names(treex)
      treenm <- "treex"
    }
    seedx <- pcheck.table(seed, gui=gui, tabnm="seed", caption="Seed table?")
    if (!is.null(seedx)) {
      seednames <- names(seedx)
      seednm <- "seedx"
    }

  } else {
    dbname <- data_dsn
    dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE)
    dbtablst <- DBI::dbListTables(dbconn)

    treex <- chkdbtab(dbtablst, tree)
    if (!is.null(treex)) {
      treenames <- DBI::dbListFields(dbconn, treex)
      treenm <- treex
    }
    seedx <- chkdbtab(dbtablst, seed)
    if (!is.null(seedx)) {
      seednames <- DBI::dbListFields(dbconn, seedx)
      seednm <- seedx
    }
    DBI::dbDisconnect(dbconn)
  }

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
  if (is.null(treex) && !is.null(seedx)) {
    addseed <- FALSE
    seedonly <- TRUE
    treex <- seedx
    treenm <- seednm
    treenames <- seednames
  }
  if (!addseed && !seedonly && !is.null(seedx)) {
    seedx <- NULL
  }
 
  ## Check unique identifiers and set unique keys for R objects
  ###########################################################################

  ## Check tuniqueid
  if (!is.null(treex)) {
    tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", 	
		checklst=treenames, caption="UniqueID variable - tree", 
		warn=paste(tuniqueid, "not in tree table"), stopifnull=TRUE)
    tsumuniqueid <- tuniqueid

    if (addseed) {
      if (!tuniqueid %in% seednames) {
        stop(tuniqueid, " not in seedx")
      }
    }
    if (bysubp) {
      if (!subpid %in% treenames) {
        stop(subpid, " not in tree")
      }
      if (addseed) {
        if (!subpid %in% seednames) {
          stop("bysubp=TRUE but ", subpid, " is not in seed table") 
        }
      }
      tsumuniqueid <- c(tsumuniqueid, subpid)
    }

    if (bycond) {
      if (!condid %in% treenames) {
        message(condid, " not in tree... assuming only 1 condition")
        treex[[condid]] <- 1
      }
      if (addseed) {
        if (!condid %in% seednames) {
          message(condid, " not in seed table")
          seedx[[condid]] <- 1
        }
      }
      tsumuniqueid <- c(tsumuniqueid, condid)
    }
  }

  if (seedonly) {
    tuniqueid <- pcheck.varchar(var2check=tuniqueid, varnm="tuniqueid", 	
		checklst=seednames, caption="UniqueID variable - seed", 
		warn=paste(tuniqueid, "not in seed table", stopifnull=TRUE))
    tsumuniqueid <- tuniqueid

    if (bysubp) {
      if (!subpid %in% seednames) {
        stop("bycond=TRUE but ", subpid, " is not in seed table") 
      }
      tsumuniqueid <- c(tsumuniqueid, subpid)
    }

    if (bycond) {
      if (!condid %in% seednames) {
        message(condid, " not in seed... assuming only 1 condition")
        seedx[[condid]] <- 1
      }
      tsumuniqueid <- c(tsumuniqueid, condid)
    }
  }


  ## Build query parts for tree table
  ##################################################
  tfromqry <- paste("FROM", treenm)
  if (addseed || seedonly) {
    sfromqry <- paste("FROM", seednm)
  }

  selectvars <- tsumuniqueid
  if (!is.null(tfilter)) {
    twhereqry <- paste("WHERE", RtoSQL(tfilter, x=treenames))

    if (addseed || seedonly) {
      sfilter <- check.logic(seednames, statement=tfilter, stopifinvalid=FALSE)
      if (!is.null(sfilter)) {
        swhereqry <- paste("WHERE", RtoSQL(tfilter))
      }
    }
  }
      
  ### Check tsumvarlst
  ###########################################################  
  tsumvarlst <- pcheck.varchar(var2check=tsumvarlst, 
	varnm="tsumvarlst", checklst=treenames, caption="Aggregate variable(s)", 
	multiple=TRUE, stopifnull=TRUE, gui=gui)
  if (any(tsumvarlst == tuniqueid)) {
    tsumvarlst[tsumvarlst == tuniqueid] <- "TPA_UNADJ"
  }

  tselectvars <- unique(c(selectvars, tsumvarlst))

  ## check seed table
  if (seedonly || addseed) {
    if (!any(tsumvarlst %in% c("TPA_UNADJ", "PLT_CN"))) {
      stop("tsumvarlst must be TPA_UNADJ for seedonly")
    } else {
      tsumvarlst[tsumvarlst == "PLT_CN"] <- "TPA_UNADJ"
    }

    sselectvars <- unique(c(selectvars, "TPA_UNADJ"))
  }


  ## CHECK TPA and tsumvars
  ###########################################################  
  TPA <- pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO", 
		stopifnull=TRUE, gui=gui)
 
  if (TPA) {
    if (any(tsumvarlst %in% mortvars)) {
      if (!"TPAMORT_UNADJ" %in% treenames) {
        stop("you must have TPAMORT_UNADJ in tree table to calculate trees per acre")
      }
      tpavar <- "TPAMORT_UNADJ"
    } else if (any(tsumvarlst %in% growvars)) {
      if (!"TPAGROW_UNADJ" %in% treenames) {
        stop("you must have TPAGROW_UNADJ in tree table to calculate trees per acre")
      }
      tpavar <- "TPAGROW_UNADJ"
    } else if (any(tsumvarlst %in% remvars)){
      if (!"TPAREMV_UNADJ" %in% treenames) {
        stop("you must have TPAREMV_UNADJ in tree table to calculate trees per acre")
      }
      tpavar <- "TPAREMV_UNADJ"
    } else {  
      if (!"TPA_UNADJ" %in% treenames) {
        stop("you must have TPA_UNADJ in tree table to calculate trees per acre")
      }
      tpavar <- "TPA_UNADJ"
    }
    tselectvars <- unique(c(tselectvars, tpavar))

    if (addseed || seedonly) {
      sselectvars <- unique(c(sselectvars, "TPA_UNADJ"))
    }
  }


  ## CHECK getadjplot and adjtree
  ###########################################################  
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

  if (adjtree && !getadjplot) {
    if (!adjvar %in% treenames) {
      message(adjvar, " variable not in tree table... setting getadjplot=TRUE")
      getadjplot <- TRUE
    } else {
      tselectvars <- unique(c(tselectvars, adjvar))
    }
    if (addseed || seedonly) {
      if (!adjvar %in% seednames) {
        message(adjvar, " variable not in seed table... setting getadjplot=TRUE")
        if (seedonly) getadjplot <- TRUE
      } else {
        sselectvars <- unique(c(sselectvars, adjvar))
      }
    }
  }


  #####################################################################
  ## Get tree data
  #####################################################################
  tree.qry <- paste("SELECT", toString(tselectvars), 
                   tfromqry)
  if (!is.null(twhereqry)) {
    tree.qry <- paste(tree.qry, twhereqry)
  }
  #message(tree.qry)
  treex <- setDT(sqldf::sqldf(tree.qry, dbname=dbname))
  setkeyv(treex, tsumuniqueid)

  if (addseed) {
    seed.qry <- paste("SELECT", toString(sselectvars), 
                   sfromqry)
    if (!is.null(swhereqry)) {
      seed.qry <- paste(seed.qry, swhereqry)
    }
    #message(seed.qry)
    seedx <- setDT(sqldf::sqldf(seed.qry, dbname=dbname))
    setkeyv(seedx, tsumuniqueid)
  }


  ## Check cond and plot tables
  ########################################################################
  condx <- pcheck.table(cond, gui=gui, tab_dsn=data_dsn, tabnm="cond", 
                  caption="Condition table?")
  if (!is.null(condx)) {
    condnames <- names(condx)
    nocond <- FALSE
  } 
  pltx <- pcheck.table(plt, gui=gui, tab_dsn=data_dsn, tabnm="plt", 
                  caption="Plot table?")
  if (!is.null(pltx)) {
    pltnames <- names(pltx)
  }

  ## Check subplot tables
  if (bysubp) {
    subpcondx <- pcheck.table(subp_cond, tab_dsn=data_dsn, tabnm="subp_cond", 
                  gui=gui, caption="Subpcond table?")
    if (!is.null(subpcondx)) {
      subpcnames <- names(subpcondx)
    }
    subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot", 
                  gui=gui, caption="Subplot table?")
    if (!is.null(subplotx)) {
      subpnames <- names(subplotx)
    }
  }


  ## Check if have correct data for adjusting plots
  ##########################################################################
  if (getadjplot) {
    if (bysubp) {
      if (sum(is.null(subpcondx), is.null(condx)) < 3) {
        if (sum(is.null(subpcondx), is.null(condx)) == 2) {
          stop("must include subp_cond and cond to adjust to plot")
        } else if (is.null(condx)) {
          stop("must include cond to adjust to plot")
        } else if (is.null(subpcondx)) {
          stop("must include subp_cond to adjust to plot")
        }
      } 
    } else {
      if (is.null(condx)) {
        stop("must include cond to adjust to plot")
      }
    }
  }

  ## Check uniqueids
  ##########################################################################
  if (!is.null(condx)) {
    if (!cuniqueid %in% condnames) {
      stop(cuniqueid, " not in cond")
    }
    if (bycond) {
      if (!condid %in% condnames) {
        if (nocond) {
          condx[[condid]] <- 1
        } else {
          stop("bycond=TRUE but ", condid, " is not in cond")
        }
      }
      tjoinid <- c(tuniqueid, condid)
      cjoinid <- c(cuniqueid, condid)
    } else {
      tjoinid <- tuniqueid
      cjoinid <- cuniqueid  
    }


    ## Check if class of tuniqueid matches class of cuniqueid
    tabs <- check.matchclass(treex, condx, tjoinid, cjoinid)
    treex <- tabs$tab1
    condx <- tabs$tab2

    ## Check that values of uniqueids in treex are all in uniqueids in condx
    treex <- check.matchval(treex, condx, tjoinid, cjoinid,
		tab1txt="tree", tab2txt="cond")

    if (addseed) {
      ## Check if class of tuniqueid matches class of cuniqueid
      tabs <- check.matchclass(seedx, condx, tjoinid, cjoinid)
      seedx <- tabs$tab1
      condx <- tabs$tab2

      ## Check that values of tuniqueid in seedx are all in puniqueid in condx
      seedx <- check.matchval(seedx, condx, tjoinid, cjoinid)
    }
  }

  if (bysubp) {
    subpuniqueid <- cuniqueid
    subpids <- c(subpuniqueid, subpid)
  
    ## Check subpids
    if (!is.null(subpcondx)) {
      if (!all(subpids %in% subpcnames)) {
        stop("uniqueids not in subp_cond: ", toString(subpids))
      }
      setkeyv(subpcondx, subpids)
    }
    if (!is.null(subplotx)) {
      if (!all(subpids %in% subpnames)) {
        stop("uniqueids not in subplot: ", toString(subpids))
      }
      setkeyv(subplotx, subpids)
    }

    ## Set pltx to NULL   
    pltx <- NULL
  }

  if (!bycond && !is.null(pltx)) {
    noplt <- FALSE

    ## Remove totally nonsampled plots
    if ("sf" %in% class(pltx)) {
      pltsp <- TRUE
    }

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
  } 

  ## Check lbs2tons
  ##########################################################################
  lbs2tons <- pcheck.logical(lbs2tons, varnm="lbs2tons", title="Pounds to tons?", 
		first="YES", gui=gui, stopifnull=TRUE)

  ## Check metric
  ##########################################################################
  metric <- pcheck.logical(metric, varnm="metric", title="Metric?", 
		first="NO", gui=gui, stopifnull=TRUE)


  ## Check checkNA
  ##########################################################################
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
		first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE

  ## Check checkNA
  ##########################################################################
  checkNA <- pcheck.logical(checkNA, varnm="checkNA", title="Check NA values?", 
		first="YES", gui=gui)
  if (is.null(checkNA)) checkNA <- FALSE


  ## Check ACI. If TRUE, include all trees, If FALSE, filter for forested plots only 
  ## (COND_STATUS_CD = 1)
  ######################################################################################
  ACI <- pcheck.logical(ACI, varnm="ACI", title="Include ACI tree data?", 
		first="NO", gui=gui)
  if (!ACI) {
    if (is.null(condx) || (!"COND_STATUS_CD" %in% condnames)) {
      #message("COND_STATUS_CD not in table, assuming forested plots with no ACI plots")
    } else {
      cond.ids <- na.omit(condx[COND_STATUS_CD == 1, 
		do.call(paste, .SD), .SDcols = cjoinid])

      if (bycond) {
        treex <- treex[paste(get(eval(tuniqueid)), get(eval(condid))) %in% cond.ids]
      } else {
        treex <- treex[get(eval(tuniqueid)) %in% cond.ids]
      }
    }
  }


  ## Check for NA values in necessary variables in all tables
  ###########################################################################
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


  ### Convert variables from pound to tons if lbs2tons=TRUE
  ###########################################################################
  if (lbs2tons && any(tsumvarlst %in% vars2convert)) {
    convfac <- ifelse(metric, 0.00045359237, 0.0005)
    vars2convert <- tsumvarlst[which(tsumvarlst %in% vars2convert)]
    message("converting from pounds to tons: ", paste(vars2convert, collapse=", "))
    for (j in vars2convert) set(treex, i=NULL, j=j, value=treex[[j]] * convfac)
  }


  ## CHECK adjTPA
  ###########################################################################
  if (TPA) {
     
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
 
  ### Get tfun used for aggregation
  ###########################################################################
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

    if (bysubp) {
      ## Remove nonsampled conditions by subplot and summarize to condition-level
      subpcx <- subpsamp(cond = condx, 
                         subp_cond = subpcondx, 
                         subplot = subplotx, 
                         subpuniqueid = subpuniqueid, 
                         subpid = subpid, ACI=ACI)

      ## Check if class of tuniqueid matches class of cuniqueid
      tabs <- check.matchclass(treex, subpcx, c(tuniqueid, subpid), c(subpuniqueid, subpid))
      treex <- tabs$tab1
      subpcx <- tabs$tab2

      adjfacdata <- getadjfactorPLOT(treex = treex, seedx = seedx, 
                                     condx = subpcx, 
		                          tuniqueid = c(tuniqueid, subpid), 
                                     cuniqueid = c(subpuniqueid, subpid),
		                          areawt = "CONDPROP_UNADJ")
      condx <- adjfacdata$condx
      cuniqueid <- c(subpuniqueid, subpid)
      tuniqueid <- c(tuniqueid, subpid)
 
      varadjlst <- c("ADJ_FACTOR_COND", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR")
      if (any(varadjlst %in% names(condx))) {
        varadjlst <- varadjlst[varadjlst %in% names(condx)]
        condx[, (varadjlst) := NULL]
      }
    
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

      adjfacdata <- getadjfactorPLOT(treex=treex, seedx=seedx, condx=condx, 
		tuniqueid=tuniqueid, cuniqueid=cuniqueid)
      condx <- adjfacdata$condx
      varadjlst <- c("ADJ_FACTOR_COND", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR")
      if (any(varadjlst %in% names(condx))) {
        varadjlst <- varadjlst[varadjlst %in% names(condx)]
        condx[, (varadjlst) := NULL]
      }
    }
       
    treex <- adjfacdata$treex
    if (addseed) {
      seedx <- adjfacdata$seedx
    }   
    adjtree <- TRUE
  }

  if (adjtree && !adjvar %in% names(treex)) {
    message(adjvar, " variable not in tree table... no adjustment was added")
    adjtree <- FALSE
  }
  tsumvarlst2 <- {}
  tsumvarnmlst2 <- {} 
  seedcountvar <- NULL

  ## If any variable in tsumvarlst is a TPA variable, add a count variable to treex
  if (any(tsumvarlst %in% tpavars)) {
    treex[, COUNT := 1]
    if (addseed) {
      seedx[, COUNT := 1]
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
          treex[, (tvarm) := get(eval(tvar)) * cfactor]
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
        treex[, (tpavar) := get(eval(tpavar)) * adjTPA]
      }
      ## If metric, convert tpavar to trees per hectare
      if (metric) {
        tpa.m <- paste0(tpavar, "_m")
        treex[, (tpa.m) := 1 / ((1/ get(eval(tpavar)) * 0.4046860))]
        tpavar <- tpa.m
      }
      treex[, (newname) := get(eval(tvar)) * get(eval(tpavar))]

      if (addseed && tvar=="COUNT" && tpavar %in% names(seedx)) {
        #seedx[, COUNT := TREECOUNT_CALC]
        if (adjTPA > 1) {
          seedx[, (tpavar) := get(eval(tpavar)) * adjTPA]
        }
        seedx[, (newname) := get(eval(tvar)) * get(eval(tpavar))]
        seedcountvar=treecountvar <- newname
      }
    } else {
      if (!is.null(fname) && !is.na(fname)) {
        newname <- paste0(tvar, "_", fname)
        setnames(treex, tvar, newname)
      } else {
        newname <- tvar
      }
      if (addseed && tvar=="COUNT") {
        #seedx[, COUNT := TREECOUNT_CALC]
        seedx[, (newname) := get(eval(tvar)) * TREECOUNT_CALC]
        seedcountvar=treecountvar <- newname
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
      treex[, (newname2) := get(eval(newname)) * get(eval(adjvar))]
   
      if (addseed && tvar=="COUNT" && adjvar %in% names(seedx)) {
        seedx[, (newname2) := get(eval(newname)) * get(eval(adjvar))]
        seedcountvar=treecountvar <- newname2
      }       
    } else {
      newname2 <- newname
    }

    if (tvar %in% c(biovars, carbvars)) {
      unittxt <- ifelse (lbs2tons, "TONS", "LBS")

      ## Apply new name
      setnames(treex, newname2, paste0(newname2, "_", unittxt))
      newname2 <- paste0(newname2, "_", unittxt)
    }

    tsumvarlst2 <- c(tsumvarlst2, newname2)
    if (getnm) {
      if (toupper(tfunstr) != "SUM") {
        tsumvarnmlst <- c(tsumvarnmlst, paste0(newname2, "_", toupper(tfunstr)))  
        tsumvarnmlst2 <- sapply(tsumvarnmlst, checknm, names(treex))
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
    datvars <- treex[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=tsumuniqueid, .SDcols=tsumvarlst2]
    setnames(datvars, c(tsumuniqueid, tsumvarnmlst2))
  } else {
    datvars <- treex[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=tsumuniqueid, .SDcols=tsumvarlst2]
    setnames(datvars, unique(c(tsumuniqueid, tsumvarnmlst2)))

    if (addseed && !is.null(seedcountvar)) {
      sdatvars <- seedx[, lapply(.SD, function(x) round(tfun(x, na.rm=TRUE), tround) ), 
		by=key(seedx), .SDcols=seedcountvar]
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
  } else if (bysubp && !is.null(subplotx)) {
    sumdat <- merge(subplotx, datvars, by=tsumuniqueid, all.x=TRUE)
  } else {
    sumdat <- datvars
  }

  ## Change NA values TO 0
  if (NAto0) {
    for (col in tsumvarnmlst2) set(sumdat, which(is.na(sumdat[[col]])), col, 0) 
  }

  ## Get metadata
  #############################################################  
  sumdatcols <- names(sumdat)
 
  if (bycond) {  
    meta = FIESTAutils::ref_cond[FIESTAutils::ref_cond$VARIABLE %in% sumdatcols, ]
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

  tree_ref <- FIESTAutils::ref_tree[match(tsumvarlst, FIESTAutils::ref_tree$VARIABLE),]
  tree_ref$VARIABLE[tree_ref$VARIABLE == "TPA_UNADJ"] <- "COUNT"

  if (nrow(tree_ref) > 0) {
    if (TPA) {
      tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_TPA")
    } 
    if (!is.null(fname)) {
      tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_", fname)
      tree_ref$DESCRIPTION <- paste0(tree_ref$DESCRIPTION, " (", tfilter, ")")
    }
    if (adjtree) {
      tree_ref$VARIABLE <- paste0(tree_ref$VARIABLE, "_ADJ")
      tree_ref$DESCRIPTION <- paste(tree_ref$DESCRIPTION, "- adjusted for partial nonresponse at plot-level")
    }

    ## Check for biomass and/or carbon variables to add units
    if (any(tsumvarlst %in% c(biovars, carbvars))) {
      bcvars <- tsumvarlst[tsumvarlst %in% c(biovars, carbvars)]
      refbcvars <- unlist(lapply(bcvars, function(x) tree_ref$VARIABLE[grepl(x, tree_ref$VARIABLE)]))

      unittxt <- ifelse(lbs2tons, "tons", "lbs")
      tree_ref[tree_ref$VARIABLE %in% refbcvars, "DESCRIPTION"] <- 
			paste0(tree_ref[tree_ref$VARIABLE %in% refbcvars, "DESCRIPTION"], " (in ", unittxt, ")")    
      tree_ref[tree_ref$VARIABLE %in% refbcvars, "VARIABLE"] <- 
			paste0(tree_ref[tree_ref$VARIABLE %in% refbcvars, "VARIABLE"], "_", toupper(unittxt))
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
