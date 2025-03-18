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
#' @param dbconn Open database connection.
#' @param dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). If bysubp = TRUE and bycond = TRUE, data are
#' aggregated by subplotid, subpid, condid.
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param bydomainlst String (vector). Categorical domain variables for
#' summing tree data by (e.g., SPCD). Variables must be in tree table or
#' plt/cond table if tables are provided.
#' @param tsumvarlst String (vector). Tree-level variable(s) to aggregate
#' (e.g., "TPA_UNADJ", "BA"). Use "TPA_UNADJ" for summed tree
#' count.
#' @param tsumvarnmlst String (vector). Name of the tree-level variable(s) to
#' aggregate (e.g., "TPALIVE", "BALIVE"). This list must have the same number
#' of variables as tsumvarlst and be in respective order. If NULL, the default
#' names will be tsumvar_SUM (e.g., "TPA_UNADJ_SUM", "BA_SUM").
#' @param seedlings String. ('Y', 'N', 'only') If seedlings = 'Y', add
#' seedlings to summary ('TPA_UNADJ' %in% tsumvarlst). If seedlings = 'N',
#' do not add seedlings. If seedlings = 'only', only include seedlings.
#' @param woodland String. ('Y', 'N', 'only') If woodland = 'Y', include
#' woodland tree species where measured. If woodland = 'N', only include
#' timber species. See FIESTA::ref_species$WOODLAND ='Y/N'. If woodland = 'only',
#' only include woodland species. If NULL, use whatever is in table.
#' @param tfilter String. Filter to subset the tree data before aggregating
#' (e.g., "STATUSCD == 1"). This must be in R syntax. If tfilter=NULL, user is
#' prompted.  Use tfilter="NONE" if no filters.
#' @param getadjplot Logical. If TRUE, and adj='plot', adjfactors are
#' calculated for nonsampled conditions at plot-level.
#' @param domclassify List. List for classifying domain variables in bydomainlst
#' (e.g., DIA = c(10,20,30)).
#' @param tderive List. List of derivative from tree table to add to output data
#' (e.g., list(MEAN_DIA = 'AVG(DIA)', SDI = 'POWER(DIA / 10, 1.605)',
#' QMD = 'SQRT(SUM(POWER(DIA,2) * 0.005454 * TPA_UNADJ) / (SUM(TPA_UNADJ)*0.005454))'))
#' @param pltidsWITHqry SQL query. A query identifying plots to sum (e.g.,
#' 'WITH pltids AS (SELECT cn AS PLT_CN FROM plot WHERE statecd=49 and INVYR=2018)')
#' @param pltidsid Sting. Name of unique identifier in pltidsWITHqry.
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
#' \donttest{
#' # Aggregate LIVE_CANOPY_CVR_PCT to plot
#' treesum <- datSumTree(tree = FIESTA::WYtree,
#'                       tsumvarlst = "TPA_UNADJ")$treedat
#'
#' # Check results
#' treesum[treesum$PLT_CN == 40404737010690,]
#' FIESTA::WYtree[FIESTA::WYtree$PLT_CN == 40404737010690,]
#' }
#' @export datSumTree
datSumTree <- function(tree = NULL,
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
                       bydomainlst = NULL,
                       tsumvarlst = NULL,
                       tsumvarnmlst = NULL,
                       seedlings = "N",
                       woodland = "Y",
                       tfilter = NULL,
                       domclassify = NULL,
                       tderive = NULL,
                       getadjplot = FALSE,
                       pltidsWITHqry = NULL,
                       pltidsid = NULL,
                       pcwhereqry = NULL,
                       savedata = FALSE,
                       tabIDs = tableIDs(),
                       datSum_opts = datSum_options(),
                       database_opts = NULL,
                       savedata_opts = NULL) {

  ####################################################################################
  ## DESCRIPTION: Aggregates tree variable(s) to plot(/cond)-level,
  ##        using specified tree filters (ex. live trees only)
  ####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #gui <- ifelse(nargs() == 0, TRUE, FALSE)
  gui <- FALSE
  
  ## If gui.. set variables to NULL
  #if (gui) ACI=bycond=tuniqueid=puniqueid=cuniqueid=TPA=adjtree=adjsamp=
  #    savedata=outfolder <- NULL
  
  ## Set global variables
  pltx=treex=seedx=cond.nonsamp.filter=meta=tvars2convert=ssumvarlst=
    cntvar=fname=tderivevars=pltidsnm=domainlst=classifyvars=propvars=tpcwhereqry=
    condx=pltx=sppltx <- NULL
  
  #ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry=pcfromqry=pcselectvars=tpavarnm=pcdomainlst <- NULL
  
  datindb <- FALSE
  pltassgnid <- "PLT_CN"
  SCHEMA. <- ""
  checkNA = FALSE
  returnDT = TRUE
  seedonly=addseed=keepall <- FALSE

  ## Query alias.
  talias. <- "t."
  salias. <- "s."
  
  
  ## For documentation
  # subplot Dataframe or comma-delimited file (*.csv). If getadjplot=TRUE,
  # The subplot-level table with SUBP_STATUS_CD variable for calculating
  # adjustment factors by subplot.
  adjvarlst <- unlist(list(COND="ADJ_FACTOR_COND", SUBP="ADJ_FACTOR_SUBP",
                           MICR="ADJ_FACTOR_MICR", MACR="ADJ_FACTOR_MACR"))
  
  ## Define variable lists
  biovars <- c("DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_BG", "DRYBIO_SAWLOG",
               "DRYBIO_AG", "DRYBIO_STEM", "DRYBIO_STEM_BARK", "DRYBIO_STUMP_BARK",
               "DRYBIO_BOLE_BARK", "DRYBIO_BRANCH", "DRYBIO_FOLIAGE", "DRYBIO_SAWLOG_BARK",
               "DRYBIOT", "DRYBIOM", "DRYBIOTB", "JBIOTOT")
  carbvars <- c("CARBON_BG", "CARBON_AG")
  
  ## Define variables to convert (from pounds to short tons.. * 0.0005)
  vars2convert <- c(biovars, carbvars, paste(biovars, "TPA", sep="_"), paste(carbvars, "TPA", sep="_"))
  
  timberonly <- FIESTAutils::ref_units[FIESTAutils::ref_units$WOODLAND == "N", "VARIABLE"]
  woodlandvars <- FIESTAutils::ref_units[FIESTAutils::ref_units$WOODLAND == "Y", "VARIABLE"]
  
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
  pcheck.params(input.params, savedata_opts = savedata_opts,
                datSum_opts = datSum_opts,
                database_opts = database_opts)
  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
    savedata_opts = savedata_opts,
    database_opts = database_opts,
    datSum_opts = datSum_opts,
    tabIDs = tabIDs))
  savedata_opts <- optslst$savedata_opts
  database_opts <- optslst$database_opts
  datSum_opts <- optslst$datSum_opts
  tabIDs <- optslst$tabIDs
  
  for (i in 1:length(datSum_opts)) {
    assign(names(datSum_opts)[[i]], datSum_opts[[i]])
  }
  
  ## Check savedata
  savedata <- FIESTAutils::pcheck.logical(savedata, varnm="savedata",
                                          title="Save data tables?", first="YES", gui=gui, stopifnull=TRUE)
  if (savedata) {
    outlst <- FIESTAutils::pcheck.output(savedata_opts)
  }
  
  ###############################################################################
  ## 1. Check datsource or database connection
  ###############################################################################
  pltsp=pg <- FALSE

  ## Check dbconn
  ########################################################
  if (!is.null(dbconn)) {
    if (!DBI::dbIsValid(dbconn)) {
      stop("invalid database dbconnection")
    }

    # indicator for whether dbconn is for a postgresql database
    pg <- ifelse(!is.null(dbconn),
                 ifelse(attr(class(dbconn), "package") == "RPostgres", TRUE, FALSE),
                 FALSE)

    datindb <- TRUE
  } else {

    ## Check datsource
    datsourcelst <- c("obj", "csv", "sqlite", "gdb", "postgres")
    datsource <- pcheck.varchar(var2check=datsource, varnm="datsource",
                                checklst=datsourcelst, gui=gui, caption="Data source?")
    if (is.null(datsource)) {
      if (!is.null(dsn) && file.exists(dsn)) {
        dsn.ext <- getext(dsn)
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
    ## Check dsn
    if (datsource == "sqlite") {
      if (!is.null(dsn)) {
        dbconn <- DBtestSQLite(dsn, dbconnopen = TRUE,
                               createnew = FALSE, returnpath = FALSE)
        if (is.null(dbconn)) {
          stop("invalid database")
        } else {
          datindb <- TRUE
        }
      } else {
        stop("datsource = 'sqlite' and dsn is NULL")
      }
    }
    if (datsource == "postgres") {
      dbconn <- DBtestPostgreSQL(dbname = database_opts$dbname,
                                 host = database_opts$host,
                                 port = database_opts$port,
                                 user = database_opts$user,
                                 password = database_opts$password,
                                 dbconnopen = TRUE)
    }
    if (!is.null(database_opts$schema)) {
      SCHEMA. <- paste0(database_opts$schema, ".")
    }
  }
  if (datindb) {
    dbtablst <- DBI::dbListTables(dbconn)
  }


  ###############################################################################
  ## 2. Check parameters
  ###############################################################################

  ## Check bycond
  bycond <- pcheck.logical(bycond, varnm="bycond", title="By condition?",
                           first="YES", gui=gui, stopifnull=TRUE)

  ## Check bysubp
  bysubp <- pcheck.logical(bysubp, varnm="bysubp", title="By subplot?",
                           first="YES", gui=gui, stopifnull=TRUE)

  ## Check getadjplot
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot",
                               title="Get plot adjustment?", first="NO", gui=gui)

  ## Check adjtree
  adjtree <- pcheck.logical(adjtree, varnm="adjtree", title="Adjust trees",
                            first="NO", gui=gui)
  if (getadjplot) adjtree <- TRUE


  ## Check seedlings
  seedlingslst <- c("Y", "N", "only")
  seedlings <- pcheck.varchar(var2check=seedlings, varnm="seedlings",
                             checklst=seedlingslst, gui=gui, caption="add seedlings?")
  if (seedlings == "Y") {
    addseed <- TRUE
  } else if (seedlings == "only") {
    seedonly <- TRUE
  }

  ## Check PROP names and build query for calculating adjustment factors
  if (getadjplot) {
    if (seedonly) {
      propvars <- "MICRPROP_UNADJ"
    } else {
      propvars <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
    }
  }

  ## Check woodland
  woodlandlst <- c("Y", "N", "only")
  woodland <- pcheck.varchar(var2check=woodland, varnm="woodland",
                             checklst=woodlandlst, gui=gui, caption="Woodland?")

  ## Check checkNA
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?",
                          first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE


  ###############################################################################
  ## 3. Check tree, seedling tables
  ## If woodland in('N','only'), check for ref_species table to determine
  ## which species are woodland.
  ###############################################################################
  treenm=seednm=ref_sppnm=woodlandnm=wtwhereqry=wtfromqry=wswhereqry=wsfromqry <- NULL

  if (!is.null(dbconn)) {
    treex <- chkdbtab(dbtablst, tree)
    if (!is.null(treex)) {
      treeflds <- DBI::dbListFields(dbconn, treex)
      treenm <- treex
    }
    seedx <- chkdbtab(dbtablst, seed)
    if (!is.null(seedx)) {
      if (is.null(treex)) {
        seedonly <- TRUE
      }
      seedflds <- DBI::dbListFields(dbconn, seedx)
      seednm <- seedx
    }
    if (!is.null(woodland) && woodland %in% c('Y', "N", "only")) {
      if (!seedonly) {
        twoodlandref <- FALSE
        twoodlandnm <- findnm("WOODLAND", treeflds, returnNULL=TRUE)
        if (is.null(twoodlandnm)) {
          twoodlandref <- TRUE
          ref_sppnm <- chkdbtab(dbtablst, "REF_SPECIES")
          if (!is.null(ref_sppnm)) {
            refflds <- DBI::dbListFields(dbconn, ref_sppnm)
            twoodlandnm <- findnm("WOODLAND", refflds, returnNULL=TRUE)
            refspcdnm <- findnm("SPCD", refflds)
            tspcdnm <- findnm("SPCD", treeflds)
            if (is.null(twoodlandnm)) {
              stop("WOODLAND attribute not in ref_species table... returning NULL")
            }
          } else {
            stop("ref_species table not in database... returning NULL")
          }
        }
      }
      if (seedonly || addseed) {
        swoodlandref <- FALSE
        swoodlandnm <- findnm("WOODLAND", seedflds, returnNULL=TRUE)
        if (is.null(swoodlandnm)) {
          swoodlandref <- TRUE
          ref_sppnm <- chkdbtab(dbtablst, "REF_SPECIES")
          if (!is.null(ref_sppnm)) {
            refflds <- DBI::dbListFields(dbconn, ref_sppnm)
            swoodlandnm <- findnm("WOODLAND", refflds, returnNULL=TRUE)
            refspcdnm <- findnm("SPCD", refflds)
            sspcdnm <- findnm("SPCD", seedflds)
            if (seedonly && is.null(swoodlandnm)) {
              stop("WOODLAND attribute not in ref_species table... returning NULL")
            }
          } else if (seedonly) {
            stop("ref_species table not in database... returning NULL")
          }
        }
      }
    }
  } else if (datsource %in% c("obj", "csv")) {
    treex <- pcheck.table(tree, gui=gui, tabnm="tree", caption="Tree table?")
    if (!is.null(treex)) {
      treex <- setDT(int64tochar(treex))
      treeflds <- names(treex)
      treenm <- "treex"
    } else {
      if (is.character(tree)) {
        if (!is.null(dsn)) {
          stop("set datsource if data in a database")
        }
      }
    }
    seedx <- pcheck.table(seed, gui=gui, tabnm="seed", caption="Seed table?")
    if (!is.null(seedx)) {
      if (is.null(treex)) {
        seedonly <- TRUE
      }
      seedx <- setDT(int64tochar(seedx))
      seedflds <- names(seedx)
      seednm <- "seedx"
    }

    ## If excluding woodland ('N') or including only woodland ('only')),
    ## include ref_species table
    if (!is.null(woodland) && woodland %in% c("N", "only")) {
      if (!seedonly) {
        twoodlandref <- FALSE
        twoodlandnm <- findnm("WOODLAND", treeflds, returnNULL=TRUE)
        if (is.null(twoodlandnm)) {
          twoodlandref <- TRUE
          ref_sppnm <- "ref_species"
          twoodlandnm <- "WOODLAND"
          refspcdnm <- "SPCD"
          tspcdnm <- findnm("SPCD", treeflds)
        }
      }
      if (seedonly || addseed) {
        swoodlandref <- FALSE
        swoodlandnm <- findnm("WOODLAND", seedflds, returnNULL=TRUE)
        if (is.null(swoodlandnm)) {
          swoodlandref <- TRUE
          ref_sppnm <- "ref_species"
          swoodlandnm <- "WOODLAND"
          refspcdnm <- "SPCD"
          sspcdnm <- findnm("SPCD", seedflds)
        }
      }
    }
  }

  ## If excluding woodland ('N') or including only woodland ('only')),
  ## define FROM statement and WHERE statement
  ###############################################################################
  if (!is.null(woodland) && woodland %in% c("N", "only")) {
    if (!seedonly) {
      if (twoodlandref) {
        wtfromqry <- paste0("\n JOIN ", SCHEMA., ref_sppnm,
                            " ref ON (ref.", refspcdnm, " = ", talias., tspcdnm, ")")
      }
      if (woodland == "only") {
        wtwhereqry <- paste(twoodlandnm, "= 'Y'")
      } else if (woodland == "N") {
        wtwhereqry <- paste(twoodlandnm, "= 'N'")
      }
    }
    if (seedonly || addseed) {
      if (swoodlandref) {
        wsfromqry <- paste0("\n JOIN ", SCHEMA., ref_sppnm,
                            " ref ON (ref.", refspcdnm, " = ", salias., sspcdnm, ")")
      } else {
        wsfromqry <- NULL
      }
      if (woodland == "only") {
        wswhereqry <- paste(swoodlandnm, "= 'Y'")
      } else if (woodland == "N") {
        wswhereqry <- paste(swoodlandnm, "= 'N'")
      }
    }
  }


  ## Check seedling
  ###############################################################################
  if ((seedonly || addseed) && is.null(seednm)) {
    message("must include seed table")
    return(NULL)
  }
  if (is.null(treenm)) {
    if (!is.null(seednm)) {
      seedonly <- TRUE
      treex <- seedx
      treeflds <- seedflds
      treenm <- "seedx"
    } else {
      message("must include tree table")
      stop()
    }
  }

  # # indicator for whether dbconn is for a postgresql database
  # pg <- ifelse(!is.null(dbconn),
  #              ifelse(attr(class(dbconn), "package") == "RPostgres", TRUE, FALSE),
  #              FALSE)
  #
  # if (pg) {
  #   all_dbtabs <- list(tree, seed, cond, plt, subp_cond, subplot)
  #   for (nm in seq_along(all_dbtabs)) {
  #     if (!is.null(all_dbtabs[[nm]])) {
  #       all_dbtabs[[nm]] <- paste0("\"", all_dbtabs[[nm]], "\"")
  #     }
  #   }
  #   treenm <- all_dbtabs[[1]]
  #   seednm <- all_dbtabs[[2]]
  #   condnm <- all_dbtabs[[3]]
  #   pltnm <- all_dbtabs[[4]]
  #   subp_condnm <- all_dbtabs[[5]]
  #   subplotnm <- all_dbtabs[[6]]
  # }


  ###############################################################################
  ## 4. Check pltidsWITHqry and pltidsid
  ###############################################################################
  
  ## Check for condition table in WITH queries
  condinWITHqry <- FALSE
  if (!is.null(pltidsWITHqry)) {
    
    if (!all(grepl("WITH", pltidsWITHqry))) {
      pltidsWITHqry <- paste0("WITH pltids AS",
                              "\n(", pltidsWITHqry, ")")
    } else {
      
      chk <- check.logic.vars("pltids", pltidsWITHqry)
      #if (!check.logic.vars("pltids", pltidsWITHqry, returnVars=TRUE))
      if (!chk) {
        message("must include pltids in pltidsWITHqry...")
        message("e.g. \nWITH",
                "\npltids AS",
                "\n(SELECT CN FROM plt",
                "\nWHERE countycd = 1)")
        stop()
      }
    }
    
    ## Check pltidsid... make sure the variable is in the pltidsWITHqry
    chk <- check.logic.vars(pltidsid, pltidsWITHqry)
    if (!chk) {
      stop("invalid pltidsid... make sure it is in pltidsWITHqry")
    }
    
    ## Set name of pltids and alias path
    pltidsnm <- "pltids"
    pltidsa. <- "pltids."
    
    ## check if cond in pltidsWITHqry
    if (!is.null(cond) && is.character(cond) &&
        check.logic.vars(cond, pltidsWITHqry, ignore.case=TRUE)) {
      condinWITHqry <- TRUE
    }
  }
  
  ###############################################################################
  ## 5. Check unique identifiers and set unique keys if R objects
  ###############################################################################
  
  ## Check tuniqueid
  if (!seedonly) {
    tuniqueid <- tabIDs[["tree"]]
    tuniqueid <- pcheck.varchar(var2check = tuniqueid, varnm = "tuniqueid",
                                checklst = treeflds, caption = "UniqueID variable - tree",
                                warn = paste(tuniqueid, "not in tree table"), stopifnull = TRUE)
    tsumuniqueid <- tuniqueid
    
    if (addseed) {
      if (!tuniqueid %in% seedflds) {
        stop(tuniqueid, " not in seedx")
      }
    }
    if (bysubp) {
      subplotid <- tabIDs[["subplot"]]
      subpid <- tabIDs[["subpid"]]
      if (!subpid %in% treeflds) {
        stop(subpid, " not in tree")
      }
      if (addseed) {
        if (!subpid %in% seedflds) {
          stop("bysubp=TRUE but ", subpid, " is not in seed table")
        }
      }
      tsumuniqueid <- c(tsumuniqueid, subpid)
    }
    if (bycond) {
      condid <- tabIDs[["condid"]]
      condidchk <- findnm(condid, treeflds, returnNULL = TRUE)
      if (is.null(condidchk)) {
        message(condid, " not in tree... assuming only 1 condition")
        if (is.data.frame(treex)) {
          treex[[condid]] <- 1
        } else {
          stop()
        }
      } else {
        condid <- condidchk
      }
      if (addseed) {
        if (!condid %in% seedflds) {
          message(condid, " not in seed table")
          if (is.data.frame(seedx)) {
            seedx[[condid]] <- 1
          } else {
            stop()
          }
        }
      }
      tsumuniqueid <- c(tsumuniqueid, condid)
    }
  }
  
  if (seedonly) {
    tuniqueid <- tabIDs[["seedling"]]
    tuniqueid <- pcheck.varchar(var2check = tuniqueid, varnm = "tuniqueid",
                                checklst = seedflds, caption="UniqueID variable - seed",
                                warn = paste(tuniqueid, "not in seed table", stopifnull = TRUE))
    tsumuniqueid <- tuniqueid
    
    if (bysubp) {
      subplotid <- tabIDs[["subplot"]]
      subpid <- tabIDs[["subpid"]]
      if (!subpid %in% seedflds) {
        stop("bycond=TRUE but ", subpid, " is not in seed table")
      }
      tsumuniqueid <- c(tsumuniqueid, subpid)
    }
    if (bycond) {
      condid <- tabIDs[["condid"]]
      condidchk <- findnm(condid, seedflds, returnNULL=TRUE)
      if (is.null(condidchk)) {
        message(condid, " not in seed... assuming only 1 condition")
        if (is.data.frame(seedx)) {
          seedx[[condid]] <- 1
        } else {
          stop()
        }
      } else {
        condid <- condidchk
      }
      tsumuniqueid <- c(tsumuniqueid, condid)
    }
  }
  
  ###############################################################################
  ### 6. Check tsumvarlst
  ###############################################################################
  if (!seedonly) {
    tsumvarchklst <- pcheck.varchar(var2check = tsumvarlst, varnm = "tsumvarlst",
                                    checklst = treeflds, caption = "Aggregate variable(s)",
                                    multiple = TRUE, stopifnull = FALSE, stopifinvalid = FALSE)
    if (is.null(tsumvarchklst) || length(tsumvarchklst) < length(tsumvarlst)) {
      BAchk <- findnm("BA", tsumvarlst, returnNULL = TRUE)
      if (!is.null(BAchk) && is.null(findnm("BA", tsumvarchklst, returnNULL = TRUE))) {
        BAderivechk <- findnm("BA", names(tderive), returnNULL = TRUE)
        if (is.null(BAderivechk)) {
          baderive <- "power(dia, 2) * 0.005454"
          if (TPA) baderive <- paste0(baderive, " * tpa_unadj")
          if (adjtree) baderive <- paste0(baderive, " * tadjfac")

          if (!is.null(tderive)) {
            tderive[["BA"]] <- paste0("SUM(", baderive, ")")
          } else {
            tderive <- list(BA = paste0("SUM(", baderive, ")"))
          }
          tsumvarlst <- tsumvarlst[tsumvarlst != BAderivechk]
          if (length(tsumvarlst) == 0) tsumvarlst <- NULL
        }
      }
    } else {
      tsumvarlst <- tsumvarchklst
    }
    
    if (is.null(tsumvarchklst) && is.null(tderive)) {
      stop("must include tsumvarlst or tderive variables")
    }
    if (!is.null(tsumvarlst) && any(tsumvarlst == tuniqueid)) {
      tsumvarlst[tsumvarlst == tuniqueid] <- "TPA_UNADJ"
    }
  }
  
  ## Check seed table
  ## Note: to get counts measured, use TPA_UNADJ and TPA = FALSE
  if (addseed) {
    if (!is.null(tsumvarlst) && is.null(findnm("TPA_UNADJ", tsumvarlst, returnNULL = TRUE))) {
      stop("tsumvarlst must include TPA_UNADJ for seedonly or addseed")
    }
  }
  
  ###############################################################################
  ## 7. Get names for summed tree variable(s)
  ###############################################################################
  getnm <- FALSE
  if (is.null(tsumvarnmlst)) {
    getnm <- TRUE
  } else {
    if (seedonly) {
      if (length(tsumvarnmlst) != 1) {
        message("tsumvarnmlst must be a vector of length 1 with name for number of seedlings")
        getnm <- TRUE
      } else {
        ssumvarnmlst <- tsumvarnmlst
      }
    } else if (addseed) {
      tsumvarnmlst[tsumvarlst == "TPA_UNADJ"]
    }
    if (!is.null(tsumvarlst) && length(tsumvarnmlst) != length(tsumvarlst)) {
      message(paste("number of names in tsumvarnmlst does not match number of tsumvars.",
                    "using default names."))
      getnm <- TRUE
    }
  }
  
  ###############################################################################
  ## 8. Check tderive
  ## Check if variables in tderive are in treeflds and identify them for query.
  ###############################################################################
  if (!is.null(tderive)) {
    if (!is.list(tderive) || is.null(names(tderive))) {
      stop(paste0("tderive must be a named list object...\n",
                  "e.g., tderive = list(SDI = '(POWER(DIA / 10, 1.605)) * TPA_UNADJ')"))
    }
    tderivevars <- lapply(tderive, function(x,varlst)
    {check.logic.vars(statement=x, varlst=varlst, ignore.case=TRUE, returnVars=TRUE)}, varlst=treeflds)
    tderive_invalid <- names(tderivevars)[(lapply(tderivevars, length) == 0)]
    if (length(tderive_invalid) > 0) {
      stop(paste0("the following variables listed in tderive do not exist in tree data: \n", tderive_invalid))
    } else {
      tderivevars <- unique(unlist(tderivevars, use.names = FALSE))
    }
  }
  
  ###############################################################################
  ## 9. Check domclassify
  ###############################################################################
  if (!is.null(domclassify)) {
    classifynmlst <- vector(mode = "list", length = length(domclassify))
    names(classifynmlst) <- names(domclassify)
    if (!is.list(domclassify) || is.null(names(domclassify))) {
      stop(paste0("domclassify must be a named list object...\n",
                  "e.g., domclassify = list(DIA = c(0, 20, 60))"))
    }
    if (!all(sapply(domclassify, function(x) is.vector(x) || is.data.frame(x)))) {
      message("invalid domclassify... all elements of the domclassify list must be a vector of class breaks or a data.frame")
      stop()
    }
    ## Check if variables in domclassify are in bydomainlst... if not, add them
    classifyvars <- names(domclassify)
    if (any(!classifyvars %in% bydomainlst)) {
      classifymiss <- classifyvars[!classifyvars %in% bydomainlst]
      message("names in domclassify must be in bydomainlst... adding ", toString(classifymiss))
      bydomainlst <- c(bydomainlst, classifymiss)
    }
  }
  
  ###############################################################################
  ## 10. Check bydomainlst
  ###############################################################################
  tdomainlst <- NULL
  domainlst <- bydomainlst
  if (!is.null(domainlst)) {
    if (seedonly) {
      if (any(bydomainlst %in% seedflds)) {
        tdomainlst <- bydomainlst[bydomainlst %in% seedflds]
        pcdomainlst <- bydomainlst[!bydomainlst %in% tdomainlst]
      } else {
        pcdomainlst <- bydomainlst
      }
    } else {
      if (any(bydomainlst %in% treeflds)) {
        tdomainlst <- bydomainlst[bydomainlst %in% treeflds]
        pcdomainlst <- bydomainlst[!bydomainlst %in% tdomainlst]
      } else {
        pcdomainlst <- bydomainlst
      }
    }
  }
  
  ###############################################################################
  ## 11. Get variables to classify from domainlst or domclassify list
  ###############################################################################
  sclassifyvars <- NULL
  tclassifyvars <- classifyvars[classifyvars %in% tdomainlst]
  pcclassifyvars <- classifyvars[classifyvars %in% pcdomainlst]
  if (addseed || seedonly) {
    sclassifyvars <- classifyvars[classifyvars %in% seedflds]
  }
  
  ###############################################################################
  ## 12. Check TPA and tree summary variables (tsumvarlst)
  ###############################################################################
  TPA <- pcheck.logical(TPA, varnm="TPA", title="Calculate TPA?", first="NO",
                        stopifnull=TRUE, gui=gui)
  
  if (TPA && is.null(tsumvarlst)) {
    if (!any(grepl("TPA", tderive, ignore.case = TRUE)))
      message("TPA must be included in derivation if want to multiply by...")
  }
  
  if (TPA) {
    if (!seedonly && !is.null(tsumvarlst)) {
      if (any(tsumvarlst %in% mortvars)) {
        tpachk <- findnm("TPAMORT_UNADJ", treeflds, returnNULL = TRUE)
        if (is.null(tpachk)) {
          stop("you must have TPAMORT_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- tpachk
      } else if (any(tsumvarlst %in% growvars)) {
        tpachk <- findnm("TPAGROW_UNADJ", treeflds, returnNULL = TRUE)
        if (is.null(tpachk)) {
          stop("you must have TPAGROW_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- tpachk
      } else if (any(tsumvarlst %in% remvars)){
        tpachk <- findnm("TPAREMV_UNADJ", treeflds, returnNULL = TRUE)
        if (is.null(tpachk)) {
          stop("you must have TPAREMV_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- tpachk
      } else {
        tpachk <- findnm("TPA_UNADJ", treeflds, returnNULL = TRUE)
        if (is.null(tpachk)) {
          stop("you must have TPA_UNADJ in tree table to calculate trees per acre")
        }
        tpavar <- tpachk
      }
    }
    if (seedonly || addseed) {
      tpachk <- findnm("TPA_UNADJ", seedflds, returnNULL = TRUE)
      if (is.null(tpachk)) {
        stop("you must have TPA_UNADJ in seedling table to calculate trees per acre")
      }
      tpavar <- tpachk
      ssumvarlst <- tpachk
    }
  } else {
    if (seedonly || addseed) {
      tpachk <- findnm("TREECOUNT_CALC", seedflds, returnNULL = TRUE)
      if (is.null(tpachk)) {
        stop("you must have TREECOUNT_CALC in seedling table to calculate number of trees")
      }
      ssumvarlst <- tpachk
    }
  }
  
  ###############################################################################
  ## 13. Check plot, subplot, and condition tables
  ###############################################################################
  condnm=plotnm=condflds=pltflds <- NULL
  
  if (bysubp) {
    subp_condnm=subplotnm <- NULL
    
    ## check subplot/subp_cond table
    if (!is.null(subp_cond) && is.data.frame(subp_cond)) {
      subpcondx <- pcheck.table(subp_cond, gui=gui, tabnm="subp_cond",
                                caption="Subpcond table?")
      if (!is.null(subpcondx)) {
        subpcondx <- setDT(int64tochar(subpcondx))
        subpcflds <- names(subpcondx)
        subp_condnm <- "subpcondx"
      }
      subplotx <- pcheck.table(subplot, gui=gui, tabnm="subplot",
                               caption="Subplot table?")
      if (!is.null(subplotx)) {
        subplotx <- setDT(int64tochar(subplotx))
        subplotflds <- names(subplotx)
        subplotnm <- "subplotx"
      }
    } else {
      subpcondx <- chkdbtab(dbtablst, subp_cond)
      if (!is.null(subpcondx)) {
        subpcflds <- DBI::dbListFields(dbconn, subpcondx)
        subp_condnm <- subpcondx
      }
      subplotx <- chkdbtab(dbtablst, subplot)
      if (!is.null(subplotx)) {
        subplotflds <- DBI::dbListFields(dbconn, subplotx)
        subplotnm <- subplotx
      }
    }
    
    ## Check unique identifiers
    subplotid <- tabIDs[[subplot]]
    subpid <- tabIDs[["subpid"]]
    subpids <- c(subplotid, subpid)
    
    ## Check subpids
    if (!is.null(subp_condnm)) {
      if (!all(subpids %in% subpcflds)) {
        stop("uniqueids not in subp_cond: ", toString(subpids))
      }
    }
    if (!is.null(subplotnm)) {
      if (!all(subpids %in% subplotflds)) {
        stop("uniqueids not in subplot: ", toString(subpids))
      }
    }
    
    ## check pltidsWITHqry
    if (is.null(pltidsWITHqry)) {
      if (bycond && !is.null(subp_condnm)) {
        pltidsWITHqry <- paste0("WITH",
                                "\npltids AS",
                                "\n(SELECT PLT_CN, SUBP, CONDID",
                                "\n FROM ", SCHEMA., subp_condnm, ")")
      } else {
        pltidsWITHqry <- paste0("WITH",
                                "\npltids AS",
                                "\n(SELECT PLT_CN, SUBP",
                                "\n FROM ", SCHEMA., subplotnm, ")")
      }
      pltidsnm <- "pltids"
      pltidsa. <- "pltids."
      pltidsid <- "PLT_CN"
      
    } else {
      
      subpjoinqry <- getjoinqry(subplotid, pltidsid, "subp.", pltidsa.)
      if (bycond && !is.null(subp_condnm)) {
        subpfromqry <- paste0("\n FROM ", SCHEMA., subp_condnm, " subp",
                              "\n JOIN ", pltidsnm, " pltids ", subpjoinqry)
        
        pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                                "\npltidsSUBP AS",
                                "\n(SELECT PLT_CN, SUBP, CONDID",
                                subpfromqry, ")")
      } else {
        subpfromqry <- paste0("\n FROM ", SCHEMA., subplotnm, " subp",
                              "\n JOIN ", pltidsnm, " pltids ", subpjoinqry)
        
        pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                                "\npltidsSUBP AS",
                                "\n(SELECT PLT_CN, SUBP",
                                subpfromqry, ")")
      }
      ## Set name of pltids and alias path
      #subplotnm <- "pltidsSUBP"
    }
    if (getadjplot && is.null(subp_condnm) || is.null(subplotnm)) {
      stop("must include subplot and subp_cond tables to calculate adjustment factors")
    }
    
    ## Set alias path for group by unique identifier
    grpby. <- "subp."
    
  } else if (!is.null(plt)) {
    pltindb <- FALSE
    
    ## Check plt table
    if (!is.null(plt) && is.data.frame(plt)) {
      
      pltx <- pcheck.table(plt, gui=gui, tabnm="plot", caption="Plot table?")
      if (!is.null(pltx)) {
        if ("sf" %in% class(pltx)) {
          pltsp <- TRUE
          sppltx <- pltx
          pltx <- sf::st_drop_geometry(pltx)
          if ("geometry" %in% names(pltx)){
            pltx$geometry <- NULL
          }
        }
        
        pltx <- setDF(int64tochar(pltx))
        pltflds <- names(pltx)
        plotnm <- "pltx"
      }
    } else {
      pltx <- chkdbtab(dbtablst, plt)
      if (!is.null(pltx)) {
        pltindb <- TRUE
        pltflds <- DBI::dbListFields(dbconn, pltx)
        plotnm <- pltx
      }
    }
    
    ## get puniqueid
    puniqueid <- findnm("CN", pltflds, returnNULL = TRUE)
    if (is.null(puniqueid)) {
      puniqueid <- findnm("PLT_CN", pltflds, returnNULL = TRUE)
    }
    if (is.null(puniqueid)) {
      puniqueid <- findnm(tabIDs$plt, pltflds, returnNULL = TRUE)
    }
    if (is.null(puniqueid)) {
      stop("invalid unqueid in plt table")
    }
    
        
    ## get pltx (if keepall = TRUE)
    if (pltindb && keepall && !bycond && !bysubp) {
      plt.qry <- paste0("SELECT ", toString(pltflds),
                        "\nFROM ", plotnm) 
      if (!is.null(pltidsWITHqry)) {
        pjoinqry <- getjoinqry(puniqueid, pltidsid, plotnm, pltidsa.)
        plt.qry <- paste0(pltidsWITHqry,
                            plt.qry)
      }
        
      ## get plt data from database
      pltx <- tryCatch(
            DBI::dbGetQuery(dbconn, plt.qry),
            error = function(e) {
              return(NULL)
            })
    } 
    
    # ## set key 
    # if (!is.null(pltx) && is.data.frame(pltx)) {
    #   pltx <- setDT(pltx)
    #   setkeyv(pltx, puniqueid)
    # }
    
    ## build pltidsWITHqry
    if (is.null(pltidsWITHqry)) {
      pltidsFROM.qry <- paste0("\n FROM ", SCHEMA., plotnm)
      pltidsSELECT.qry <- paste0("SELECT ", puniqueid)
      
      ## build pltids WITH qry
      pltidsWITHqry <- paste0("WITH",
                              "\npltids AS",
                              "\n(", pltidsSELECT.qry,
                              pltidsFROM.qry, ")")
    
      pltidsid <- puniqueid
      pltidsa. <- "pltids."
    } else {
      pltidsa. <- "pltids."
    }
    
    ## Set alias path for group by unique identifier
    pltidsnm <- "pltids"
    grpby. <- pltidsa.
  }
  
  ## Check cond table
  if (!is.null(cond)) {
    condindb <- FALSE
    
    if (is.data.frame(cond)) {
      condx <- pcheck.table(cond, gui=gui, tabnm="cond", caption="Condition table?")
      if (!is.null(condx)) {
        condx <- setDT(int64tochar(condx))
        condflds <- names(condx)
        condnm <- "condx"
      }
    } else {
      if (!is.null(pltidsWITHqry) && check.logic.vars(cond, pltidsWITHqry)) {
        condnm <- cond
        grpby. <- "pc."
      
        condflds.qry <- paste0(
          pltidsWITHqry,
          "\nSELECT * FROM ", SCHEMA., condnm, " LIMIT 0"
        )
        condfldsdf <- tryCatch(
            DBI::dbGetQuery(dbconn, condflds.qry),
            error = function(cond) {
              return(NULL)
            })
        if (is.null(condfldsdf)) {
          message("pltidsWITHqry is invalid...")
          message(pltidsWITHqry)
          stop()
        } else {
          condflds <- names(condfldsdf)
        }
        
        ## get condx (if keepall = TRUE)
        if (keepall) {
          cond.qry <- paste0(
            pltidsWITHqry,
            "\nSELECT * FROM ", SCHEMA., condnm)
          
          condx <- tryCatch(
            DBI::dbGetQuery(dbconn, cond.qry),
            error = function(cond) {
              return(NULL)
            })
        }
        
      } else {
        condx <- chkdbtab(dbtablst, cond)
        if (!is.null(condx)) {
          condflds <- DBI::dbListFields(dbconn, condx)
          condnm <- condx
          condindb <- TRUE
        }
      }
    }
    
    ## Check uniqueids
    if (!is.null(condflds)) {
      cuniqueid <- tabIDs[["cond"]]
      condid <- tabIDs[["condid"]]
      cvars <- c(cuniqueid, condid, propvars)
      conda. <- "c."
      cuniqueidchk <- findnm(cuniqueid, condflds, returnNULL = TRUE)
      if (is.null(cuniqueidchk)) {
        stop(cuniqueid, " not in cond")
      } else {
        cuniqueid <- cuniqueidchk
      }
      if (bycond) {
        condidchk <- findnm(condid, condflds, returnNULL = TRUE)
        if (is.null(condidchk)) {
          stop("bycond=TRUE but ", condid, " is not in cond")
        }
        tjoinid <- c(tuniqueid, condid)
        cjoinid <- c(cuniqueid, condid)
      } else {
        tjoinid <- tuniqueid
        cjoinid <- cuniqueid
      }
    }

    ## list of plot and cond fields
    pcflds <- c(pltflds, condflds)
    
    ## get pltx (if keepall = TRUE)
    if (condindb && keepall) {
      cond.qry <- paste0("SELECT ", toString(condflds),
                         "\nFROM ", condnm) 
      if (!is.null(pltidsWITHqry)) {
        cjoinqry <- getjoinqry(cuniqueid, pltidsid, condnm, pltidsa.)
        cond.qry <- paste0(pltidsWITHqry,
                          cond.qry)
      }
      
      ## get plt data from database
      condx <- tryCatch(
        DBI::dbGetQuery(dbconn, cond.qry),
        error = function(e) {
          return(NULL)
        })
    } 
    
    ## set key 
    if (!is.null(condx) && is.data.frame(condx)) {
      condx <- setDT(condx)
      setkeyv(condx, c(cuniqueid, condid))
    }
    
    
    ## Check pcdomainlst
    if (!is.null(pcdomainlst) && length(pcdomainlst) > 0) {
      if (!is.null(pcflds)) {
        missdom <- pcdomainlst[!pcdomainlst %in% pcflds]
        if (length(missdom) > 0) {
          message("variables in bydomainlst are not in dataset: ", toString(missdom))
        }
        pcdomainlst <- pcdomainlst[pcdomainlst %in% condflds]
        
        cvars <- unique(c(cvars, pcdomainlst))
        
      } else {
        message("must include cond and/or plot table for: ", toString(pcdomainlst))
      }
    }
    
    
    ## Check pcwhereqry
    if (!is.null(pcwhereqry)) {
      if (is.null(pcflds)) {
        stop("must include plot and/or cond if including pcwhereqry")
      } else {
        pcwhereqry <- check.logic(pcflds, pcwhereqry)
        pcwhereqry <- RtoSQL(pcwhereqry)
        chkvars <- check.logic.vars(pcflds, pcwhereqry, returnVars = TRUE)
        cvars <- c(cvars, chkvars)
        
        if (!(startsWith(gsub(" ", "", pcwhereqry), "\nWHERE"))) {
          if (startsWith(gsub(" ", "", pcwhereqry), "WHERE")) {
            pcwhereqry <- paste0("\n ", pcwhereqry)
          } else {
            pcwhereqry <- paste0("\n WHERE ", pcwhereqry)
          }
        }
      }
      #if (grepl("pc.", pcwhereqry)) {
      #  pcwhereqry <- gsub("pc.", "c.", pcwhereqry)
      #}
    }
    
    ## If ACI, include COND_STATUS_CD = 1 to exclude trees measured on ACI plots
    if (!ACI) {
      if (is.null(condflds)) {
        message("must include cond to exclude ACI plots... assuming data has no ACI plots")
      } else {
        cond_status_cdnm <- findnm("COND_STATUS_CD", condflds, returnNULL = TRUE)
        if (is.null(cond_status_cdnm)) {
          message("must include COND_STATUS_CD in cond to exclude ACI plots... assuming data has no ACI plots")
        }
        if (!is.null(pcwhereqry)) {
          if (!(grepl("COND_STATUS_CD", pcwhereqry, ignore.case = TRUE) &&
                (grepl("COND_STATUS_CD=1", gsub(" ", "", pcwhereqry), ignore.case = TRUE) ||
                 grepl("COND_STATUS_CDin(1)", gsub(" ", "", pcwhereqry), ignore.case = TRUE)))) {
            pcwhereqry <- paste0(pcwhereqry, " AND pc.COND_STATUS_CD = 1")
          }
        } else {
          pcwhereqry <- "\n WHERE pc.COND_STATUS_CD = 1"
        }
        cvars <- unique(c(cvars, cond_status_cdnm))
      }
    }
    
    ## check and append to pltidsWITHqry
    if (is.null(pltidsWITHqry)) {
      pltidsWITHqry <- paste0("WITH",
                              "\npltids AS",
                              "\n(SELECT DISTINCT ", cuniqueid,
                              "\n FROM ", SCHEMA., condnm, ")")
      pltidsnm <- "pltids"
      pltidsa. <- "pltids."
      pltidsid <- cuniqueid
    }
    
    if (!condinWITHqry) {
      pltidsnm <- "pltids"
      pltidsa. <- "pltids."
      
      pcfromqry <- paste0("\n FROM ", pltidsnm, " pltids")
      cjoinqry <- getjoinqry(cuniqueid, pltidsid, conda., pltidsa.)
      
      if (!is.null(pltx)) {
        puniqueid <- tabIDs[["plt"]]
        condfldsqry <- paste0("c.", cvars)
        pltflds <- pltflds[!pltflds %in% c(cvars, puniqueid)]
        
        if (length(pltflds) > 0) {
          pltfldsqry <- paste0("p.", pltflds)
          pltcondfldsqry <- toString(c(pltfldsqry, condfldsqry))
        } else {
          pltcondfldsqry <- toString(condfldsqry)
        }
        pjoinqry <- getjoinqry(puniqueid, cuniqueid, "p.", conda.)
        pcfromqry <- paste0(pcfromqry,
                            "\n JOIN ", SCHEMA., condnm, " c ", cjoinqry,
                            "\n JOIN ", SCHEMA., plotnm, " p ", pjoinqry)
      } else {
        #pltcondfldsqry <- toString(paste0("c.", condflds))
        pltcondfldsqry <- toString(paste0("c.", cvars))
        pcfromqry <- paste0(pcfromqry,
                            "\n JOIN ", SCHEMA., condnm, " c ", cjoinqry)
      }
      
      pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                              "\n----- pltcondx",
                              "\npltcondx AS",
                              "\n(SELECT ", pltcondfldsqry,
                              pcfromqry, ")")
      condnm <- "pltcondx"
      
      if (!bysubp) {
        grpby. <- "pc."
      }
    }
  } else {
    
    if (getadjplot) {
      stop("must include cond to calculate adjustments")
    }
    
    if (!is.null(pltidsWITHqry)) {
      grpby. <- "pltids."
    }
  }
  
  ## Check lbs2tons
  ##########################################################################
  if (!seedonly) {
    lbs2tons <- pcheck.logical(lbs2tons, varnm="lbs2tons", title="Pounds to tons?",
                               first="YES", gui=gui, stopifnull=TRUE)
  }
  
  ## Check metric
  ##########################################################################
  metric <- pcheck.logical(metric, varnm="metric", title="Metric?",
                           first="NO", gui=gui, stopifnull=TRUE)
  
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
  
  
  ## if adjtree = TRUE, first check if 'tadjfac' is in treeflds
  ## If 'tadjfac' is not in treeflds, check if getadjplot = TRUE or pltidsWITHqry is not NULL
  if (adjtree) {
    adjvar <- datSum_opts$adjvar
    if (!seedonly) {
      if (!adjvar %in% treeflds) {
        if (is.null(condnm) && is.null(pltidsWITHqry)) {
          if (bysubp) {
            msg <- paste0("must include cond, subplot, subp_cond tables or pltidsWITHqry or ",
                          adjvar, " in tree table when adj != 'none'")
          } else {
            msg <- paste0("must include cond or pltidsWITHqry or ",
                          adjvar, " in tree table when adj != 'none'")
          }
          stop(msg)
        } else {
          if (!is.null(pltidsWITHqry) && check.logic.vars("pltidsadj", pltidsWITHqry)) {
            getadjplot <- FALSE
          } else {
            getadjplot <- TRUE
          }
        }
        if (addseed) {
          if (!adjvar %in% seedflds) {
            if (is.null(condnm)) {
              stop(paste0("must include ", adjvar, " in seed table when adj != 'none'"))
            }
          }
        }
      }
    } else if (seedonly) {
      if (!adjvar %in% seedflds) {
        if (is.null(condnm) && is.null(pltidsWITHqry)) {
          if (bysubp) {
            msg <- paste0("must include cond, subplot, subp_cond tables or pltidsWITHqry or ", adjvar, " in seed table when adj != 'none'")
          } else {
            msg <- paste0("must include cond or pltidsWITHqry or ", adjvar, " in seed table when adj != 'none'")
          }
        } else {
          if (is.null(condnm)) {
            getadjplot <- TRUE
          }
        }
      }
    }
  }
  
  ## Check tround
  if (is.null(tround) || !is.numeric(tround) || (tround %% 1 != 0)) {
    warning("tround is invalid.. rounding to 5 digits")
    tround <- 5
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
    outlst$out_layer <- "treesum"
  }
  
  #########################################################################################
  #########################################################################################
  ## Build queries
  #########################################################################################
  #########################################################################################

  ## Build fromqry for twithqry/swithqry
  #############################################################################
  if (!seedonly) {
    tfromqry <- paste0("\n FROM ", SCHEMA., treenm, " t")
  }
  if (seedonly || addseed) {
    sfromqry <- paste0("\n FROM ", SCHEMA., seednm, " s")
  }

  #############################################################################
  ## Check and build where queries for filtering tree data
  #############################################################################

  ### Check tfilter and build WHERE qry for twithqry/swithqry
  ###############################################################
  if (!is.null(tfilter)) {
    if (!seedonly) {
      twhereqry <- paste0("\n WHERE ", RtoSQL(tfilter, x=treeflds))
    }

    if (addseed || seedonly) {
      sfilter <- suppressMessages(check.logic(seedflds,
                                              statement=tfilter, stopifinvalid=FALSE))
      if (!is.null(sfilter)) {
        swhereqry <- paste0("\n WHERE ", RtoSQL(tfilter))
      }
    }
  }

  
  ## Build queries for adjfactors
  ##########################################################################
  if (adjtree) {

    if (getadjplot) {
      adjjoinid <- cuniqueid

      ## Build WHERE query to filter nonsampled plots
      pcADJwhereqry <- getADJwherePLOT(condflds)


      if (bysubp) {
        adjjoinid <- subplotid

        ## Build WHERE query for removing nonsampled subplots
        subpwhereqry <- getADJwhereSUBP(subplotflds, adjwhereqry = pcADJwhereqry)

        ## Build FROM query including subplot and subp_cond
        subpa. <- "subp."
        subpca. <- "subpc."
        subpjoinqry <- getjoinqry(subplotid, pltidsid, subpa., pltidsa.)
        subpfromqry <- paste0(
          pcfromqry,
          "\n JOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry,
          "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subplotid, " = ", conda., cuniqueid,
          " AND ", subpca., condid, " = ", conda., condid,
          " AND ", subpca., subpid, " = ", subpa., subpid, ")")


        ## First, get query for summarizing subplot sampled proportions
        sumpropqry <- sumpropSUBPqry(fromqry = subpfromqry,
                                     whereqry = subpwhereqry,
                                     ACI = ACI,
                                     selectvars = NULL,
                                     SCHEMA. = SCHEMA.)

        ADJqrySUBP <-
          getADJqry(popType = "VOL",
                    adj = "plot",
                    propvars = propvars,
                    adjfromqry = "\n FROM subpcprop c",
                    pwhereqry = NULL,
                    pltidsid = subplotid,
                    pltassgnid = c(pltassgnid, subpid),
                    pltidsa. = "c.")
        #message(ADJqrySUBP)


        ## Build final query for adjustment factors, including pltids WITH query
        pltidsWITHqry <- paste0(
          pltidsWITHqry, ", ",
          "\n----- sum sampled subplot proportions",
          "\nsubpcprop AS ",
          "\n(", sumpropqry, "),",
          "\n----- adjustment factors",
          "\npltidsadj AS ",
          "\n(", ADJqrySUBP, ")")
        #message(pltidsWITHqry)


      } else {   bysubp = FALSE

        ## Build FROM query
        conda. <- "c."
        pcfromqry <- paste0("\n FROM ", SCHEMA., condnm, " c")

        ADJqry <-
          getADJqry(popType = "VOL",
                    adj = "plot",
                    propvars = propvars,
                    adjfromqry = pcfromqry,
                    pwhereqry = pcADJwhereqry,
                    pltidsid = cuniqueid,
                    pltassgnid = pltassgnid,
                    pltidsa. = "c.")
        #message(ADJqry)

        ## Build final query for adjustment factors, including pltids WITH query
        pltidsWITHqry <- paste0(
            pltidsWITHqry, ", ",
            "\n----- adjustment factors",
            "\npltidsadj AS ",
            "\n(", ADJqry, ")")
           #message(pltidsWITHqry)
      }

      if (addseed || seedonly) {
        sadjcase <- paste0("adj.", adjvarlst[["MICR"]])
      }
    } else { ## END getadjplot
      #adjjoinid <- cuniqueid
      adjjoinid <- pltidsid
    }

    ## Build query for select CASE statement to add adjfactors
    ######################################################################################
    adjalias. <- NULL
    if (!seedonly) {
      if (adjvar %in% treeflds) {
        tadjcase <- paste0("t.", adjvar)
      } else if (!is.null(findnm("TPROP_BASIS", treeflds, returnNULL=TRUE))) {
        tadjcase <- paste0(
          "\n      CASE WHEN t.TPROP_BASIS = 'MICR' THEN ", adjalias., adjvarlst[["MICR"]],
          "\n           WHEN t.TPROP_BASIS = 'MACR' THEN ", adjalias., adjvarlst[["MACR"]],
          "\n           ELSE ", adjalias., adjvarlst[["SUBP"]], " END AS tadjfac")
      } else if (!is.null(tpavar)) {
        tadjcase <- paste0(
          "\n      CASE WHEN t.DIA IS NULL THEN ", adjalias., adjvarlst[["SUBP"]],
          "\n           WHEN ", tpavar, " > 50 THEN ", adjalias., adjvarlst[["MICR"]],
          "\n           WHEN ", tpavar, " > 5 AND ", tpavar, " < 10 THEN ", adjalias., adjvarlst[["SUBP"]],
          "\n           ELSE ", adjalias., adjvarlst[["MACR"]], " END AS tadjfac")
      } else if (!is.null(findnm("DIA", treeflds, returnNULL=TRUE))) {
        tadjcase <- paste0(
          "\n      CASE WHEN t.DIA IS NULL THEN ", adjalias., adjvarlst[["SUBP"]],
          "\n           WHEN MIN(t.DIA, 5 - 0.001) THEN ", adjalias., adjvarlst[["MICR"]],
          "\n           WHEN MIN(t.DIA, 9999 - 0.001) THEN ", adjalias., adjvarlst[["SUBP"]],
          "\n           ELSE ", adjalias., adjvarlst[["MACR"]], " END AS tadjfac")
      } else {
        stop("for adjfactors, need TPROP_BASIS or DIA in tree data")
      }
    }
    if (addseed || seedonly) {
      if ("tadjfac" %in% seedflds) {
        sadjcase <- paste0("s.", adjvar)
      } else {
        sadjcase <- paste0("adj.", adjvarlst[["MICR"]], " AS tadjfac")
      }
    }
#  }	else {



  #
  #   if (bysubp) {
  #
  #     ## Build FROM query including subplot and subp_cond
  #     subpa. <- "subp."
  #     subpca. <- "subpc."
  #     subpjoinqry <- getjoinqry(subpid, pltidsid, subpa., pltidsa.)
  #     subpfromqry <- paste0(
  #       pcfromqry,
  #       "\n JOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry,
  #       "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subpid, " = ", conda., cuniqueid,
  #       " AND ", subpca., condid, " = ", conda., condid,
  #       " AND ", subpca., subpid, " = ", subpa., subpid, ")")
  #
  #     ## First, get query for summarizing subplot sampled proportions
  #     sumpropqry <- sumpropSUBPqry(fromqry = subpfromqry,
  #                                  whereqry = NULL,
  #                                  ACI = ACI,
  #                                  selectvars = NULL,
  #                                  SCHEMA. = SCHEMA.)
  #
  #     ## Build final query for adjustment factors, including pltids WITH query
  #     pltidsWITHqry <- paste0(
  #       pltidsWITHqry, ", ",
  #       "\n----- sum sampled subplot proportions",
  #       "\nsubpcprop AS ",
  #       "\n(", sumpropqry, ")")
  #     #message(pltidsWITHqry)
  #
  #     pltidsnm <- "subpcprop"
  #   }
  }

  #############################################################################
  ## Build tsumvardf for tree SELECT statement
  #############################################################################
  tsumvardf <-
    data.frame(TSUMVAR = character(), # tsumvar
               TABLE = character(),   # which table tsumvar is from ('TREE', 'SEED')
               NEW = character(),     # new tsumvar, after manipulations
               NAME = character(),    # name of output variable (tsumvarnm)
               TUNITS = character(),  # units (from ref_units)
               DERIVE = logical())    # if from tderive

  if (!is.null(tsumvarlst)) {

    ## Add tsumvarlst to tsumvardf
    if (TPA) {
      tpavarnm <- tpavar
    } else {
      tpavarnm <- NULL
    }
    tsumvarlst <- tsumvarlst
    tpavars <- tpavars
    if (any(tsumvarlst %in% tpavars)) {
      cntvar <- tsumvarlst[tsumvarlst %in% tpavars]
      cntvarid <- which(tsumvarlst %in% tpavars)
      tsumvarlst <- tsumvarlst[-cntvarid]
      if (!is.null(tsumvarnmlst)) {
        cntnm <- tsumvarnmlst[cntvarid]
        tsumvarnmlst <- tsumvarnmlst[-cntvarid]
      } else {
        if (length(cntvar) > 1) {
          cntnm <- sapply(cntvar, function(x) strsplit(x, "_"))
          cntnm <- paste0("COUNT_", sapply(cntnm, '[', 2))
          #cntnm[cntnm == "COUNT_ADJ"] <- "COUNT"
        } else {
          cntnm <- "COUNT"
        }
        tsumvarnmlst <- tsumvarlst
      }
    } else {
      if (is.null(tsumvarnmlst)) {
        tsumvarnmlst <- tsumvarlst
      }
    }

    if (seedonly) {
      NEWt <- ifelse(TPA, cntvar, paste0(cntvar, " > 0"))
      tsumvardf <- rbind(tsumvardf,
                         data.frame(TSUMVAR = "TPA_UNADJ",
                                    TABLE = "SEED",
                                    NEW = NEWt,
                                    NAME = "COUNT",
                                    TUNITS = "trees",
                                    DERIVE = FALSE))
    } else {
      if (addseed) {
        NEWt <- ifelse(TPA, cntvar, paste0(cntvar, " > 0"))
        #NAMEs <- ifelse(getadjplot, paste0(NAMEs, "_ADJ"), NAMEs)
        NAMEs <- ifelse(TPA, paste0(cntnm, "_SEED"), cntnm)

        tsumvardf <- rbind(tsumvardf,
                           data.frame(TSUMVAR = cntvar,
                                      TABLE = "TREE",
                                      NEW = NEWt,
                                      NAME = paste0(cntnm, "_TREE"),
                                      TUNITS = "trees",
                                      DERIVE = FALSE))
        tsumvardf <- rbind(tsumvardf,
                           data.frame(TSUMVAR = cntvar,
                                      TABLE = "SEED",
                                      NEW = NEWt,
                                      NAME = NAMEs,
                                      TUNITS = "trees",
                                      DERIVE = FALSE))
        tsumvardf <- rbind(tsumvardf,
                           data.frame(TSUMVAR = cntvar,
                                      TABLE = "TREESEED",
                                      NEW = NEWt,
                                      NAME = cntnm,
                                      TUNITS = "trees",
                                      DERIVE = FALSE))
      } else {
        if (!is.null(cntvar)) {
          for (cvar in cntvar) {
            NEW <- ifelse(TPA, cvar, paste0(cvar, " > 0"))
            tsumvardf <- rbind(tsumvardf,
                               data.frame(TSUMVAR = cvar,
                                          TABLE = "TREE",
                                          NEW = NEW,
                                          NAME = cntnm,
                                          TUNITS = "trees",
                                          DERIVE = FALSE))
          }
        }
      }
      if (length(tsumvarlst) > 0) {
        tsumvardf <- rbind(tsumvardf,
                           data.frame(TSUMVAR = tsumvarlst,
                                      TABLE = "TREE",
                                      NEW = tsumvarlst,
                                      NAME = tsumvarnmlst,
                                      TUNITS = "trees",
                                      DERIVE = FALSE))
      }
    }

    ### Convert variables from pound to tons if lbs2tons=TRUE
    ###########################################################################
    if (any(tsumvarlst %in% vars2convert)) {
      tvars2convert <- tsumvarlst[which(tsumvarlst %in% vars2convert)]

      tvarnew <- tvars2convert
      tunits <- "pounds"
      if (lbs2tons) {
        tvarnew <- paste0(tvarnew, "_TON")
        message("converting pounds to tons: ", toString(tvars2convert))
        convfac <- 0.0005
        tunits <- "tons"
        if (metric) {
          message("converting tons to metric tons: ", toString(tvars2convert))
          tvarnew <- paste0(tvarnew, "m")
          convfac <- 0.0005 * 0.90718474
          tunits <- "metric tons"
        }
      } else {
        if (metric) {
          message("converting pounds to kilograms: ", toString(tvars2convert))
          tvarnew <- paste0(tvarnew, "_KG")
          convfac <- 0.45359237
          tunits <- "kilograms"
        } else {
          convfac <- 1
          tunits <- "pounds"
        }
      }
      if (convfac != 1) {
        tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NEW"] <-
          sapply(tvars2convert, function(x) paste0(x, " * ", convfac))
      }
      tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NAME"] <- tvarnew
      tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"TUNITS"] <- tunits

      if (TPA) {
        tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NEW"] <-
          paste0(tsumvardf[tsumvardf$TSUMVAR %in% tvars2convert,"NEW"],
                 " * ", tpavar)
      }
    }

    ## Add conversion to metric to tsumvardf
    if (any(!tsumvarlst %in% vars2convert)) {
      tvar2convert <- tsumvarlst[!tsumvarlst %in% vars2convert]

      for (tvar in tvar2convert) {
        tvarnew <- tvar
        tvarchk <- findnm(tvar, ref_units$VARIABLE, returnNULL=TRUE)
        if (is.null(tvarchk)) {
          message(tvar, " not in ref_estvar... no units found")
          metric <- FALSE
        } else {
          tunits <- unique(ref_units$UNITS[ref_units$VARIABLE == tvarchk])

          if (metric) {
            munits <- ref_units$METRICUNITS[ref_units$VARIABLE == tvarchk]
            convfac <- ref_conversion$CONVERSION[ref_conversion$METRIC == munits]
            tvarnew <- paste0(tvarnew, "_m")
            message("converting ", tunits, " to ", munits, ": ", tvar)
            tunits <- munits

            tsumvardf[tsumvardf$TSUMVAR == tvar, "NEW"] <-
              paste0(tvar, " * ", convfac)

            tsumvardf[tsumvardf$TSUMVAR == tvar, "NAME"] <- tvarnew
            tsumvardf[tsumvardf$TSUMVAR == tvar, "TUNITS"] <- tunits
          }
        }
      }
      if (TPA) {
        tsumvardf[tsumvardf$TSUMVAR %in% tvar2convert,"NEW"] <-
          paste0(tsumvardf[tsumvardf$TSUMVAR %in% tvar2convert,"NEW"], " * ", tpavar)
      }
    }

    ### Add adjTPA to tsumvardf
    ###########################################################################
    if (TPA) {

      ## Check adjTPA (default = 1)
      ## (e.g., if adjTPA=4 (only 1 subplot measured), multiply TPA* by 4)
      if (adjTPA) {
        if (is.null(adjTPA)) {
          warning("adjTPA is invalid, assuming no adjustments")
          adjTPA <- 1
        } else if (!is.numeric(adjTPA)) {
          stop("adjTPA must be a numeric number from 1 to 4")
        } else if (!adjTPA %in% 1:4) {
          stop("adjTPA must be between 1 and 4")
        } else if (adjTPA > 1) {
          if (seedonly) {
            subpnm <- findnm("SUBP", seedflds, returnNULL=TRUE)
          } else {
            subpnm <- findnm("SUBP", treeflds, returnNULL=TRUE)
          }
          if (is.null(subpnm)) {
            stop("SUPB variable not in table")
          }
          if (datindb) {
            message("multiplying ", tpavar, " by ", adjTPA)
          } else {
            if (adjTPA == 2 && any(treex[, unique(get(subpnm)), by=tuniqueid][[2]] > 3)) {
              stop("more than 3 SUBP in dataset")
            } else if (adjTPA == 3 && any(treex[, unique(get(subpnm)), by=tuniqueid][[2]] > 2)) {
              stop("more than 2 SUBP in dataset")
            } else if (adjTPA == 4 && any(treex[, unique(get(subpnm)), by=tuniqueid][[2]] > 1)) {
              stop("more than 1 SUBP in dataset")
            }
          }
        }
      }

      ## If metric=TRUE, convert trees per acre to trees per hectare
      if (metric) {
        ac2ha <- 0.40468564
        message("converting ", tpavar, " from acres to hectares")
        tsumvardf$NEW <- paste0(tsumvardf$NEW, " * 1 / ", ac2ha,")")

        if (getnm) {
          tsumvardf$NAME <- paste0(tsumvardf$NAME, "_TPH")
        }
      } else {
        if (getnm) {
          tsumvardf$NAME <- paste0(tsumvardf$NAME, "_TPA")
        }
      }
      if (adjTPA > 1) {
        tsumvardf$NEW <- paste0(tsumvardf$NEW, " * ", adjTPA)

        if (getnm) {
          tsumvardf$NAME <- paste0(tsumvardf$NAME, adjTPA)
        }
      }
    }

    ## Define name - add _ADJ to name if adjusting
    if (adjtree) {
      if (getnm) {
        tsumvardf$NAME <- paste0(tsumvardf$NAME, "_ADJ")
      }
      tsumvardf$NEW <- paste0(tsumvardf$NEW, " * ", adjvar)
    }

    ## Add a new column to SUM variables
    ###########################################################################
    tsumvardf$SELECT <- paste0("COALESCE(SUM(", tsumvardf$NEW, "),0)")

  }

  ## Add derived variables to tsumvardf
  ###########################################################################
  if (!is.null(tderive)) {
    for (i in seq_along(tderive)) {
      tderivedf <- data.frame(TSUMVAR = tderive[[i]],
                              TABLE = "TREE",
                              NEW = tderive[[i]],
                              NAME = names(tderive)[i],
                              TUNITS = "trees",
                              DERIVE = TRUE,
                              SELECT = tderive[[i]])
      tsumvardf <- rbind(tsumvardf, tderivedf)
      #tsumvarlst <- unique(c(tsumvarlst, tderive[[i]]))
    }
  }

  ### Define name - adding tfilter
  ###########################################################################
  if (!is.null(tfilter) && getnm) {
    ref <- ref_estvar[ref_estvar$ESTVAR %in% tsumvarlst, ]
    ref <- ref[grep(gsub(" ", "", tfilter), gsub(" ", "", ref$ESTFILTER)), ]
    fname <- ref[, "FILTERNM"][1]
    if (!is.na(fname)) {
      if (fname == "standing-dead") fname <- "dead"
      tsumvardf$NAME <- paste0(tsumvardf$NAME, "_", fname)
    }
  }

  ## SELECT variables
  ###########################################################################
  tsumvardf$SELECT <- paste0("\n  ", tsumvardf$SELECT, " AS ", tsumvardf$NAME)

  if (addseed) {
    tsumvardf$SELECT[tsumvardf$TABLE == "TREE"] <-
        paste0("\n  COALESCE(SUM(CASE WHEN src = 'TREE' THEN ",
               tsumvardf$NEW[tsumvardf$TABLE == "TREE"], " ELSE 0 END),0)",
               " AS ", tsumvardf$NAME[tsumvardf$TABLE == "TREE"])

    tsumvardf$SELECT[tsumvardf$TABLE == "SEED"] <-
        paste0("\n  COALESCE(SUM(CASE WHEN src = 'SEED' THEN ",
               tsumvardf$NEW[tsumvardf$TABLE == "SEED"], " ELSE 0 END),0)",
               " AS ", tsumvardf$NAME[tsumvardf$TABLE == "SEED"])
  }

  #################################################################################
  ## Build WITH query to get tree data (tdat)
  #################################################################################
  adjalias. <- "adj."
  twithalias <- "tdat"

  if (!seedonly) {
    ## Build twithqry
    twithqry <- "SELECT 'TREE' src,"
    twithvars <- c("CONDID", "SUBP", "TREE")
    twithvarschk <- sapply(twithvars, findnm, treeflds, returnNULL = TRUE)
    if (all(is.null(twithvarschk))) {
      twithvars <- NULL
    } else {
      twithvars <- unlist(twithvarschk)
    }
    twithSelect <- paste0(talias., unique(c(tsumuniqueid, twithvars)))
    tvarlst <- unique(c(tdomainlst, tsumvarlst, tpavarnm))
    alltvarlst <- unique(c(tsumuniqueid, twithvars, tvarlst))

    if (addseed) {
      if (!is.null(tvarlst)) {
        spcdnm <- findnm("SPCD", tvarlst, returnNULL = TRUE)
        if (is.null(spcdnm)) {
          spcdnm <- findnm("SPCD", treeflds, returnNULL = TRUE)
        } else {
          tvarlst <- tvarlst[tvarlst != spcdnm]
        }
        if (!is.null(spcdnm)) {
          twithSelect <- c(twithSelect, paste0(talias., spcdnm))
        }
        tpanm <- findnm("TPA_UNADJ", tvarlst, returnNULL = TRUE)
        if (!is.null(tpanm)) {
          twithSelect <- c(twithSelect, paste0(talias., tpanm))
          tvarlst <- tvarlst[tvarlst != tpanm]
        }
      }
      if (length(tvarlst) != 0) {
        twithSelect <- unique(c(twithSelect, paste0(talias., tvarlst)))
      }
    } else {
      if (!is.null(tvarlst)) {
        twithSelect <- unique(c(twithSelect, paste0(talias., tvarlst)))
      }
    }


    ## Build final select statement for tdat WITH query
    #twithqry <- paste(twithqry, toString(paste0(talias., twithSelect)))

    if (!is.null(tderive)) {
      tderivevars <- tderivevars[!tderivevars %in% alltvarlst]

      if (length(tderivevars) > 0) {
        #twithSelect <- c(paste0(talias., twithSelect), tderivevars)
        twithSelect <- c(twithSelect, paste0(talias., tderivevars))
      }
      alltvarlst <- unique(c(alltvarlst, tderivevars))
    }

    ## Build final select statement for tdat WITH query
    #twithqry <- paste(twithqry, toString(paste0(talias., twithSelect)))
    twithqry <- paste(twithqry, toString(twithSelect))
    twithfromqry <- tfromqry
    if (adjtree) {
      if (!adjvar %in% treeflds) {
        tadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., talias.)
        twithfromqry <- paste0(tfromqry,
                               "\n JOIN pltidsadj adj ", tadjjoinqry)
      }
      twithqry <- paste0(twithqry, ", ", tadjcase)
    } else {

      if (!is.null(pltidsWITHqry)) {
        tjoinqry <- getjoinqry(tuniqueid, pltidsid, talias., "pltids.")
        twithfromqry <- paste0(twithfromqry,
                             "\n JOIN pltids ", tjoinqry)
      }
    }

    ## WHERE statement - Woodland
    twithwhereqry <- twhereqry
    if (!is.null(woodland) && woodland %in% c("N", "only")) {
      twithfromqry <- paste0(twithfromqry, wtfromqry)
      if (is.null(twithwhereqry)) {
        twithwhereqry <- paste0("\n WHERE ", wtwhereqry)
      } else {
        twithwhereqry <- paste0(twithwhereqry, " AND ", wtwhereqry)
      }
    }

    ## Build final tree WITH query
    twithqry <- paste0(twithqry,
                       twithfromqry,
                       twithwhereqry)

    ## Build WITH query - seedling data (sdat)
    ################################################################
    if (addseed) {
      swithalias <- "sdat"

      ## Build swithqry
      nbrvar <- length(tsumvardf$TSUMVAR[tsumvardf$TABLE == "TREE"][
        !tsumvardf$TSUMVAR[tsumvardf$TABLE == "TREE" & !tsumvardf$DERIVE] %in% tpavarnm])
      swithqry <- paste0("\n SELECT 'SEED' src, ")
      swithSelect <- unique(c(paste0("s.", tsumuniqueid), "s.CONDID", "s.SUBP", 0))
      allsvarlst <- c(tsumuniqueid, "CONDID", "SUBP", 0)

      spcdanm <- findnm("t.SPCD", twithSelect, returnNULL = TRUE)
      if (!is.null(spcdanm)) {
        swithSelect <- c(swithSelect, paste0(salias., spcdnm))
        allsvarlst <- c(allsvarlst, spcdnm)
      }
      tpaanm <- findnm("t.TPA_UNADJ", twithSelect, returnNULL = TRUE)
      if (!is.null(tpaanm)) {
        swithSelect <- c(swithSelect, paste0("s.", tpanm))
        allsvarlst <- c(allsvarlst, tpanm)
      }
      swithqry <- paste0(swithqry, toString(swithSelect))

      sdomainlst <- NULL
      if (!is.null(tdomainlst)) {
        for (tdomain in tdomainlst) {
          if (tdomain == "DIACL") {
            swithqry <- paste0(swithqry, ", '<1' AS DIACL")
            sdomainlst <- c(sdomainlst, tdomain)
          } else if (tdomain %in% seedflds && tdomain != "SPCD") {
            swithqry <- paste0(swithqry, ", s.", tdomain)
            sdomainlst <- c(sdomainlst, tdomain)
          }
        }
      }
      allsvarlst <- c(allsvarlst, sdomainlst)

      #nbrvar <- nbrvar + (length(tvarlst) - length(sdomainlst))
      nbrvar <- length(alltvarlst) - length(allsvarlst)
      if (nbrvar > 0) {
        swithqry <- paste0(swithqry, ", ", toString(rep("'null'", nbrvar)))
      }

      swithfromqry <- sfromqry
      if (adjtree) {
        sadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., salias.)
        if (!adjvar %in% seedflds) {
          sadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., salias.)
          swithfromqry <- paste0(sfromqry,
                                 "\n JOIN pltidsadj adj ", sadjjoinqry)
        }
        swithqry <- paste0(swithqry, ", ", sadjcase)

      }
      if (!is.null(pltidsWITHqry)) {
        sjoinqry <- getjoinqry(tuniqueid, pltidsid, salias., "pltids.")
        swithfromqry <- paste0(swithfromqry,
                               "\n JOIN pltids ", sjoinqry)
      }

      ## WHERE statement - Woodland
      swithwhereqry <- swhereqry
      if (!is.null(woodland) && woodland %in% c("N", "only")) {
        swithfromqry <- paste(swithfromqry, wsfromqry)
        if (is.null(swithwhereqry)) {
          swithwhereqry <- paste0("\n WHERE ", wswhereqry)
        } else {
          swithwhereqry <- paste0(swithwhereqry, " AND ", wswhereqry)
        }
      }

      ## Build final seedling WITH query
      swithqry <- paste0(swithqry,
                         swithfromqry,
                         swithwhereqry)

      ## Build UNION WITH query, including tree and seedling data
      twithqry <- paste0(twithqry,
                         "\n UNION",
                         swithqry)
    }

  } else {  ## if seedonly

    swithalias <- "tdat"

    ## Build swithqry
    swithqry <- "SELECT 'SEED' src,"
    spcdnm <- findnm("SPCD", tdomainlst, returnNULL = TRUE)
    if (is.null(spcdnm) && !is.null(findnm("SPCD", seedflds, returnNULL = TRUE))) {
      tdomainlst <- c(spcdnm, tdomainlst)
    }
    swithSelect <- unique(c(tsumuniqueid, "CONDID", "SUBP", tdomainlst,
                            unique(c(tpavarnm, tsumvardf$TSUMVAR[tsumvardf$TABLE == "SEED" & !tsumvardf$DERIVE]))))
    if (!is.null(tderive)) {
      tderivevars <- tderivevars[!tderivevars %in% swithSelect]
      if (length(tderivevars) > 0) {
        swithSelect <- c(swithSelect, tderivevars)
      }
    }
    ## Build final select statement for tdat WITH query
    swithqry <- paste(swithqry, toString(paste0(salias., swithSelect)))

    swithfromqry <- sfromqry
    if (adjtree) {
      adjjoinid <- pltidsid
      if (!adjvar %in% seedflds) {
        sadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., salias.)
        swithfromqry <- paste0(sfromqry,
                           "\n JOIN pltidsadj adj ", sadjjoinqry)
      }
      swithqry <- paste0(swithqry, ", ", sadjcase)
    }

    if (!is.null(pltidsWITHqry)) {
      sjoinqry <- getjoinqry(tuniqueid, pltidsid, salias., "pltids.")
      swithfromqry <- paste0(swithfromqry,
                         "\n JOIN pltids ", sjoinqry)
    }

    ## WHERE statement - Woodland
    swithwhereqry <- swhereqry
    if (!is.null(woodland) && woodland %in% c("N", "only")) {
      swithfromqry <- paste(swithfromqry, wsfromqry)
      if (is.null(swithwhereqry)) {
        swithwhereqry <- paste0("\n WHERE ", wswhereqry)
      } else {
        swithwhereqry <- paste0(swithwhereqry, " AND ", wswhereqry)
      }
    }

    ## Build final seedling WITH query
    twithqry <- paste0(swithqry,
                       swithfromqry,
                       swithwhereqry)
  }

  ## Append to pltidsWITHqry
  if (!is.null(pltidsWITHqry)) {
    if (!is.null(condnm)) {
      uniqueid <- cuniqueid
    } else {
      uniqueid <- pltidsid
    }
    if (bysubp) {
      uniqueid <- unique(c(uniqueid, subpids))
    }
    if (bycond) {
      uniqueid <- c(uniqueid, condid)
    }
    pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                            "\n----- get tree data",
                            "\ntdat AS",
                            "\n(", twithqry, ")")

  } else {
    uniqueid <- tuniqueid
    if (bycond) {
      uniqueid <- c(uniqueid, condid)
    }
    
    grpby. <- "tdat."
    pltidsWITHqry <- paste0("WITH tdat AS",
                            "\n(", twithqry, ")")
  }

  #################################################################################
  ## Build query for summarizing tree data
  #################################################################################

  ## Build FROM statement
  ################################################################
  if (bysubp) {
    subpa. <- "subp."
    tfromqry <- paste0("\nFROM pltidsSUBP subp")
    if (!is.null(condnm)) {
      conda. <- "pc."
      if (bycond) {
        cjoinid <- getjoinqry(c(cuniqueid, condid), c(subplotid, condid), "pc.", subpa.)
      } else {
        cjoinid <- getjoinqry(cuniqueid, subplotid, "pc.", subpa.)
      }
      tfromqry <- paste0(tfromqry,
                         "\nJOIN ", SCHEMA., condnm, " pc ", cjoinid)
      tjoinid <- getjoinqry(c(tuniqueid, condid), uniqueid, "tdat.", subpa.)

      ## use LEFT JOIN for tdat to get all records, no data filled with 0
#      tfromqry <- paste0(tfromqry,
#                         "\nJOIN tdat ", tjoinid)
      tfromqry <- paste0(tfromqry,
                         "\nLEFT JOIN tdat ", tjoinid)
    }
  } else if (!is.null(condnm)) {
    conda. <- "pc."
    tfromqry <- paste0("\nFROM ", condnm, " pc")
    tjoinid <- getjoinqry(c(tuniqueid, condid), c(cuniqueid, condid), "tdat.", conda.)
    
    ## use LEFT JOIN for tdat to get all records, no data filled with 0
#    tfromqry <- paste0(tfromqry,
#                       "\nJOIN tdat ", tjoinid)
    tfromqry <- paste0(tfromqry,
                       "\nLEFT JOIN tdat ", tjoinid)

  } else if (!is.null(pltidsnm)) {
    tfromqry <- paste0("\nFROM ", pltidsnm, " pltids")
    tjoinid <- getjoinqry(tuniqueid, pltidsid, "tdat.", pltidsa.)
    
    ## use LEFT JOIN for tdat to get all records, no data filled with 0
#    tfromqry <- paste0(tfromqry,
#                         "\nJOIN tdat ", tjoinid)
    tfromqry <- paste0(tfromqry,
                           "\nLEFT JOIN tdat ", tjoinid)
    
  } else if (!is.null(plotnm)) {
    tfromqry <- paste0("\nFROM ", plotnm)
    tjoinid <- getjoinqry(tuniqueid, pltidsid, "tdat.", pltidsa.)

    ## use LEFT JOIN for tdat to get all records, no data filled with 0
#    tfromqry <- paste0(tfromqry,
#                         "\nJOIN tdat ", tjoinid)
    tfromqry <- paste0(tfromqry,
                         "\nLEFT JOIN tdat ", tjoinid)
    
  } else {
    tfromqry <- paste0("\nFROM ", twithalias)
  }

  ## Build SELECT statement
  ####################################################################

  #define grpby variables
  tgrpbyvars <- paste0(grpby., uniqueid)
  #tgrpbyvars <- paste0("tdat.", tsumuniqueid)

  ## add grpby variable to select qry query
  tselectqry <- paste0("\nSELECT ", toString(tgrpbyvars))

  ## Add classifications to select query
  domclassifyqry <- NULL
  if (!is.null(domclassify)) {
    for (i in 1:length(domclassify)) {
      classifyvar <- names(domclassify)[i]
      classifylut <- domclassify[[i]]
      if (is.vector(classifylut)) {
        classifynm <- paste0(classifyvar, "CL")
        if (classifyvar %in% pcclassifyvars) {
          classvar. <- "pc."
          #pcdomainlst[pcdomainlst == classifyvar] <- classifynm
        } else {
          classvar. <- "tdat."
          #tcdomainlst[tdomainlst == classifyvar] <- classifynm
        }
        domainlst <- domainlst[domainlst != classifyvar]

        ## Get classification query
        cutbreaks <- domclassify[[classifyvar]]
        fill <- NULL
        if ((addseed || seedonly) && min(cutbreaks) == 0) {
          fill <- "<1"
        }
        domclassqry <- classifyqry(classcol = classifyvar,
                                   cutbreaks = cutbreaks,
                                   classnm = classifynm,
                                   class. = classvar.,
                                   fill = fill)
      } else if (is.data.frame(classifylut)) {
        if (ncol(classifylut) != 2) {
          message("invalid number of columns for ", classifyvar,
                    "... must be a vector of class breaks or a data.frame with 2 columns")
          stop()
        }
        classifynm <- names(classifylut)[!names(classifylut) %in% classifyvar]
        if (length(classifynm) != 1) {
          message("invalid column names for ", classifyvar,
                    "... the data.frame must include name of variable to domclassify: ", classifyvar)
          stop()
        }
        fromval <- classifylut[[classifyvar]]
        toval <- classifylut[[classifynm]]
        if (classifyvar %in% pcclassifyvars) {
          classvar. <- "pc."
          #pcdomainlst[pcdomainlst == classifyvar] <- classifynm
        } else {
          classvar. <- "tdat."
          #tdomainlst[tdomainlst == classifyvar] <- classifynm
        }
        domainlst <- domainlst[domainlst != classifyvar]

        ## Get classification query
        domclassqry <- classqry(classifyvar, fromval, toval,
                                  classnm = classifynm,
                                  class. = classvar.,
                                  fill = NULL)
      }
      tgrpbyvars <- c(tgrpbyvars, domainlst)
      classifynmlst[[classifyvar]] <- classifynm

      domclassifyqry <- paste0(domclassifyqry, "\n", domclassqry)
      if (length(domclassify) > 1 && i < length(domclassify)) {
        domclassifyqry <- paste0(domclassifyqry, ",")
      }
    }
    if (length(domainlst) > 0) {
      tselectqry <- paste0(tselectqry, ", ", toString(domainlst), ", ", domclassifyqry)
    } else {
      tselectqry <- paste0(tselectqry, ", ", domclassifyqry)
    }
  } else if (!is.null(domainlst) && length(domainlst) > 0) {
    tselectqry <- paste0(tselectqry, ", ", toString(domainlst))
    tgrpbyvars <- unique(c(tgrpbyvars, domainlst))
  }

  if (!seedonly) {

    ## Build select tree query
    tselectqry <- paste0(tselectqry,
                         ",   ", paste(tsumvardf$SELECT[tsumvardf$TABLE == "TREE"],
                                       collapse=",  "))
    if (addseed) {
      ## Append to select tree query
      tselectqry <- paste0(tselectqry, ", ",
                           paste(tsumvardf$SELECT[tsumvardf$TABLE == "SEED"]))
      tselectqry <- paste0(tselectqry, ", ",
                           paste(tsumvardf$SELECT[tsumvardf$TABLE == "TREESEED"]))
    }
  } else {

    ## Build select
    tselectqry <- paste0(tselectqry,
                         ",   ", paste(tsumvardf$SELECT[tsumvardf$TABLE == "SEED"],
                                       collapse=",\n  "))

  }

  ## Build query to summarize tree data
  ################################################################
  tqry <- paste0(tselectqry,
                 tfromqry,
                 pcwhereqry,
                 "\nGROUP BY ", toString(tgrpbyvars))

  ## Build final query to summarize tree data including WITH queries
  ################################################################
  if (!is.null(pltidsWITHqry)) {
    tree.qry <- paste0(pltidsWITHqry,
                       "\n-------------------------------------------",
                       tqry)
  }

  # replace instances of twithSelect vars with their double quoted versions in tree.qry
  # only replace instances that are not already single quoted or double quoted
  #  for (s in twithSelect) {
  #    pat <- paste0("(?<!['\"])", "\\b", s, "\\b", "(?!['\"])")
  #    tree.qry <- gsub(pat, paste0("\"", s, "\""), tree.qry, perl = TRUE)
  #  }

  #cat(tree.qry, "\n")

  if (datindb) {
    sumdat <- tryCatch(
      {
        DBI::dbGetQuery(dbconn, tree.qry)
      },
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  } else {
    sumdat <- tryCatch(
      {
        sqldf::sqldf(tree.qry)
      },
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  }
  if (is.null(sumdat)) {
    message("tree query is invalid...")
    message(tree.qry)
    stop()
  }

  # uniqueidchk <- unlist(sapply(uniqueid, findnm, names(sumdat), returnNULL = TRUE))
  # if (length(uniqueidchk) < length(uniqueid)) {
  #   message("uniqueid (", toString(uniqueid), ") is not in resulting table: ",
  #           toString(names(sumdat))
  #   stop()
  # }
  # setkeyv(setDT(sumdat), uniqueidchk)
  setkeyv(setDT(sumdat), uniqueid)

  ## Round digits
  if (!is.null(tround)) {
    tcols <- tsumvardf$NAME
    sumdat[, (tcols) := round(.SD, tround), .SDcols=tcols]
  }

  ## Join tables
  #############################################################
  if (keepall) {
    if (!bycond && !is.null(pltx)) {
      if (pltsp) {
        sumdat <- merge(sppltx, sumdat, by.x=puniqueid, by.y=uniqueid, all.x=TRUE)
        tsumuniqueid <- puniqueid
        returnDT <- FALSE
        if (NAto0) {
          for (col in tcols) sumdat[is.na(sumdat[[col]]), col] <- 0
        }

      } else {
        ## set key
        pltx <- setDT(pltx)
        setkeyv(pltx, puniqueid)
        
        sumdat <- sumdat[pltx]
        #sumdat <- merge(pltx, sumdat, all.x=TRUE)
      }
      
      #if (NAto0) {
      #  sumdat <- DT_NAto0(sumdat, cols=tcols)
      #}
    }
    if (bycond && !is.null(condx)) {
      sumdat <- sumdat[condx]
      if (NAto0) {
        sumdat <- DT_NAto0(sumdat, cols=tcols)
      }
    }
    setcolorder(sumdat, c(names(sumdat)[!names(sumdat) %in% tcols], tcols))
  }

  ## Get metadata
  #############################################################
  sumdatcols <- names(sumdat)

  if (bycond) {
    meta <- ref_cond[ref_cond$VARIABLE %in% sumdatcols, ]
    missnames <- names(sumdat)[!names(sumdat) %in% meta$VARIABLE]
    meta2 <- FIESTA::ref_plt[FIESTA::ref_plt$VARIABLE %in% missnames, ]
    if (nrow(meta2) > 0) {
      meta <- rbind(meta, meta2)
    }
  } else {
    meta <- ref_plt[names(sumdat) %in% ref_plt$VARIABLE, ]
  }

  metanames <- names(sumdat)[which(names(sumdat) %in% meta$VARIABLE)]
  meta <- meta[meta$VARIABLE %in% metanames, ]
  meta <- meta[match(metanames, meta$VARIABLE),]

  if (!is.null(tsumvarlst) && nrow(meta) > 0) {
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
  }

  #### WRITE TO FILE
  #############################################################
  if (savedata) {
    message("saving ", out_layer, "...")

    if (pltsp) {
      spExportSpatial(sumdat,
                      savedata_opts = outlst)
    } else {
      datExportData(sumdat,
                    savedata_opts = outlst)

      outlst$out_layer <- "meta"
      message("saving meta...")
      datExportData(meta,
                    savedata_opts = outlst)

    }
  }

  if (returnDT) {
    if ("sf" %in% class(sumdat)) {
      sumdat <- setDT(sf::st_drop_geometry(sumdat))
    } else {
      sumdat <- setDT(sumdat)
    }
  }
  sumtreelst <- list(treedat = sumdat,
                     sumvars = tsumvardf$NAME,
                     tsumuniqueid = uniqueid,
                     treeqry = tree.qry,
                     pltsp = pltsp)
  #sumtreelst$estunits <- estunits
  if (!is.null(tfilter)) {
    sumtreelst$tfilter <- tfilter
  }
  #if (!is.null(domainlst)) {
  #  sumtreelst$domainlst <- domainlst
  #}
  if (!is.null(tdomainlst)) {
    sumtreelst$tdomainlst <- tdomainlst
  }
  if (!is.null(pcdomainlst)) {
    sumtreelst$pcdomainlst <- pcdomainlst
  }
  if (!is.null(domclassify)) {
    sumtreelst$classifynmlst <- classifynmlst
  }
  if (!is.null(tround)) {
    sumtreelst$tround <- tround
  }
  if (!is.null(meta) && nrow(meta) > 0) {
    sumtreelst$meta <- meta
  }

  return(sumtreelst)

}
