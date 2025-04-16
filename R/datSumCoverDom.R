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
datSumCoverDom <- function(tree = NULL,
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
                       covtype = "P2VEG",
                       domclassify = NULL,
                       tfilter = NULL, 
                       bytdom = FALSE,
                       tdomvar = "SPCD", 
                       tdomvarlst = NULL, 
                       tdomvar2 = NULL, 
                       tdomvar2lst = NULL, 
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
  ## DESCRIPTION: Aggregates tab variable(s) to plot(/cond)-level,
  ##        using specified tab filters 
  ####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE
  
  ## Set global variables
  pltx=tabx=pltidsnm=domainlst=classifyvars=propvars=tpcwhereqry=
    condx=pltx=sppltx=pcflds <- NULL
  
  #ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry=pcfromqry=pcselectvars=tpavarnm=pcdomainlst <- NULL
  
  datindb <- FALSE
  pltassgnid <- "PLT_CN"
  SCHEMA. <- ""
  checkNA = keepall = FALSE
  returnDT = TRUE

  ## Query alias.
  talias. <- "t."

  
  ## For documentation
  # subplot Dataframe or comma-delimited file (*.csv). If getadjplot=TRUE,
  # The subplot-level table with SUBP_STATUS_CD variable for calculating
  # adjustment factors by subplot.
  adjvarlst <- unlist(list(COND="ADJ_FACTOR_COND", SUBP="ADJ_FACTOR_SUBP",
                           MICR="ADJ_FACTOR_MICR", MACR="ADJ_FACTOR_MACR",
                           P2VEG="ADJ_FACTOR_P2VEG_SUBP"))
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumCoverDom))
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


  ## Check PROP names and build query for calculating adjustment factors
  if (getadjplot) {
    propvars <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
  }

  ## Check checkNA
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?",
                          first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE


  ###############################################################################
  ## 3. Check tab, seedling tables
  ## If woodland in('N','only'), check for ref_species table to determine
  ## which species are woodland.
  ###############################################################################
  tabnm=seednm=ref_sppnm=woodlandnm=wtwhereqry=wtfromqry=wswhereqry=wsfromqry <- NULL

  if (!is.null(dbconn)) {
    tabx <- chkdbtab(dbtablst, tab)
    if (!is.null(tabx)) {
      tabflds <- DBI::dbListFields(dbconn, tabx)
      tabnm <- tabx
    }
  } else if (datsource %in% c("obj", "csv")) {
    tabx <- pcheck.table(tab, gui=gui, tabnm="tab", caption="Tree table?")
    if (!is.null(tabx)) {
      tabx <- setDT(int64tochar(tabx))
      tabflds <- names(tabx)
      tabnm <- "tabx"
    } else {
      if (is.character(tab)) {
        if (!is.null(dsn)) {
          stop("set datsource if data in a database")
        }
      }
    }
  }

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
  tuniqueid <- tabIDs[["tree"]]
  tuniqueid <- pcheck.varchar(var2check = tuniqueid, varnm = "tuniqueid",
                              checklst = treeflds, caption = "UniqueID variable - tree",
                              warn = paste(tuniqueid, "not in tree table"), stopifnull = TRUE)
  tsumuniqueid <- tuniqueid
  
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
    tsumuniqueid <- c(tsumuniqueid, condid)
  }

  
  ###############################################################################
  ### 6. Check tsumvarlst
  ###############################################################################
  tsumvar <- NULL
  if (is.null(tsumvar)) {
    if (covtype == "P2VEG") {
      tsumvar <- "COVER_PCT"
      if (bysubp) {
        if (adjtree) {
          tsumvarnew <- COALESCE("COVER_PCT * ", adjvar, ", 0)")
        } else {
          tsumvarnew <- "COVER_PCT"
        }
      } else {
        if (adjtree) {
          tsumvarnew <- paste0("COALESCE(SUM(COVER_PCT * 1.0) / 4 / 100 * ", adjvar, ", 0)")
        } else {
          tsumvarnew <- "COALESCE(SUM(COVER_PCT * 1.0) / 4 / 100, 0)"
        }
      }
    }    
  # } else {
  #   if (!tsumvar %in% treeflds) {
  #     stop("tsumvar is invalid... must be a variable in tree")
  #   }
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
    if (any(bydomainlst %in% treeflds)) {
      tdomainlst <- bydomainlst[bydomainlst %in% treeflds]
      pcdomainlst <- bydomainlst[!bydomainlst %in% tdomainlst]
    } else {
      pcdomainlst <- bydomainlst
    }
  }
  
  ###############################################################################
  ## 11. Get variables to classify from domainlst or domclassify list
  ###############################################################################
  tclassifyvars <- classifyvars[classifyvars %in% tdomainlst]
  pcclassifyvars <- classifyvars[classifyvars %in% pcdomainlst]
  
  

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

  ## Build fromqry for twithqry
  #############################################################################
  tfromqry <- paste0("\n FROM ", SCHEMA., treenm, " t")

  
  #############################################################################
  ## Check and build where queries for filtering tree data
  #############################################################################

  ### Check tfilter and build WHERE qry for twithqry/swithqry
  ###############################################################
  if (!is.null(tfilter)) {
    twhereqry <- paste0("\n WHERE ", RtoSQL(tfilter, x=treeflds))
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

    } else { ## END getadjplot
      #adjjoinid <- cuniqueid
      adjjoinid <- pltidsid
    }

    ## Build query for select CASE statement to add adjfactors
    ######################################################################################
    adjalias. <- NULL
    
    
    if (adjvar %in% treeflds) {
      tadjcase <- paste0("t.", adjvar)
    } 
    
    ## Define name - add _ADJ to name if adjusting
    tsumvarnm <- paste0(tsumvar, "_ADJ")
  }


  ## SELECT variables
  ###########################################################################
  twithSelect <- paste0(tsumvar, " AS ", tsumvarnm)

  
  #################################################################################
  ## Build WITH query to get tree data (tdat)
  #################################################################################
  adjalias. <- "adj."
  twithalias <- "tdat"
  
  ## Build twithqry
  twithqry <- paste0("SELECT")
  
  ## Build twithSelect
  twithSelect <- toString(c(tsumuniqueid, tdomainlst, tsumvar))
  
  

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

  ## Build final tree WITH query
  twithqry <- paste0(twithqry,
                     twithfromqry,
                     twithwhereqry)
  
   

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


  ## Build select tree query
  tselectqry <- paste0(tselectqry,
                       ",\n   ", tsumvarnew, " AS ", tsumvarnm)

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
    sumdat[, (tsumvarnm) := round(.SD, tround), .SDcols=tsumvarnm]
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
                     sumvar = tsumvarnm,
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

  return(sumtreelst)

}
