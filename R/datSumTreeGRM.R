#' Data - Aggregates numeric tree data to the plot or condition-level.
#'
#' Aggregates numeric tree-level data (e.g., VOLCFNET) to plot or condition,
#' including options for filtering tree data or extrapolating to acre by
#' multiplying by TPA.
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
#' @param grmtables List of tables used for summarizing grm data.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param dbconn Open database connection.
#' @param dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param landarea String. The sample area filter for estimates ("ALL",
#' "FOREST", "TIMBERLAND").  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param grmtype String. Type of GRM to summarize ('GROW', 'MORT', 'REMV')
#' @param tstatus String. The status type of GRM to summarize ('Sawtimber',
#' 'Growing-stock', 'All live').
#' @param tsumvar String. Tree-level variable to summarize (e.g., "VOLCFNET"). 
#' @param tsumvarnm String. Name of summarized output column. The default
#' is 'ESTIMATED_VALUE'.
#' @param tdomvar String. The tree domain (tdom) variable used to aggregate by
#' (e.g., "SPCD", "SPGRPCD").
#' @param tdomvarlst String (vector). List of specific tree domains of tdomvar
#' to aggregate (e.g., c(108, 202)). If NULL, all domains of tdomvar are used.
#' @param tdomvar2 String. A second tree domain variable to use to aggregate by
#' (e.g. "DIACL").  The variables, tdomvar and tdomvar2 will be concatenated
#' before summed.
#' @param tdomvar2lst String (vector). List of specific tree domains of
#' tdomvar2 to aggregate.  If NULL, all domains of tdomvar2 are used.
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). If bysubp = TRUE and bycond = TRUE, data are
#' aggregated by subplotid, subpid, condid.
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param bydomainlst String (vector). Categorical domain variables for
#' summing tree data by (e.g., SPCD). Variables must be in tree table or
#' plt/cond table if tables are provided.
#' @param grm_typ_cd Integer Type of annual GRM code (1:Current annual; 2:A2A)
#' @param pivot Logical. If TRUE, tdomvar data are transposed (pivoted) to
#' separate columns.
#' @param domclassify List. List for classifying domain variables in bydomainlst
#' (e.g., DIA = c(10,20,30)).
#' @param pltidsWITHqry SQL query. A query identifying plots to sum (e.g.,
#' 'WITH pltids AS (SELECT cn FROM plot WHERE statecd=49 and invyr=2018)')
#' @param pltidsid Sting. Name of unique identifier in pltidsWITHqry.
#' @param pltcondx Data table with plot / condition variables.
#' @param pltidsadj Data table with plot adjustment factors.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param dbconnopen Logical. If TRUE, keep database connection open.
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
#' @export datSumTreeGRM
datSumTreeGRM <- function(grmtables = list(plt = "plot",
                                           cond = "cond",
                                           tree = "tree",
                                           tree_grm_component = "tree_grm_component",
                                           tree_grm_begin = "tree_grm_begin",
                                           tree_grm_midpt = "tree_grm_midpt",
                                           beginend = "beginend"),
                          datsource = "obj",
                          dbconn = NULL,
                          dsn = NULL,
                          landarea,
                          tstatus,
                          grmtype,
                          tsumvar,
                          tsumvarnm = "ESTIMATED_VALUE",
                          tdomvar = NULL, 
                          tdomvarlst = NULL, 
                          tdomvar2 = NULL, 
                          tdomvar2lst = NULL, 
                          bycond = FALSE,
                          bysubp = FALSE,
                          bydomainlst = NULL,
                          grm_typ_cd = 2,
                          pivot = TRUE,
                          domclassify = NULL,
                          pltidsWITHqry = NULL,
                          pltidsid = NULL,
                          pltcondx = NULL,
                          pltidsadj = NULL,
                          savedata = FALSE,
                          dbconnopen = TRUE,
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
    condx=pltx=sppltx=pcflds=tdomscols=tdomtotnm=classifynmlst <- NULL
  
  #ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry=pcfromqry=pcselectvars=tpavarnm=pcdomainlst=
    tdomvarlst=tdomvarlst2=tsumvarnm=tdomtotnm=concatvar=subpid=pvars <- NULL
  
  datindb <- FALSE
  pltassgnid <- "PLT_CN"
  SCHEMA. <- ""
  checkNA = keepall = FALSE
  returnDT = TRUE
  FIAname <- FALSE
  tdomtot <- TRUE
  getadjplot = FALSE
  condid <- "CONDID"
  subpid <- "SUBP"
  
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
  formallst <- names(formals(datSumTreeGRM)) 
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
  pltsp <- FALSE
  
  ## Check database connection
  ######################################################
  dbinfo <- pcheck.datsource(dbconn = dbconn, 
                             datsource = datsource, 
                             dsn = dsn, 
                             database_opts = database_opts)
  if (is.null(dbinfo)) {
    stop()
  } else {
    datindb <- dbinfo$indb
    datsource <- dbinfo$datsource
    dbtablst <- dbinfo$dbtablst
    schema <- dbinfo$schema
    SCHEMA. <- dbinfo$SCHEMA.
    dbconn <- dbinfo$dbconn
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
  
  
  ## Check pivot
  pivot <- pcheck.logical(pivot, varnm="pivot", title="Pivot columns?", 
                          first="NO", gui=gui)
  
  
  ## Check grmtype
  grmtypelst <- c('GROW', 'MORT', 'REMV')
  grmtype <- pcheck.varchar(var2check = grmtype, varnm = "grmtype", gui = gui,
                            checklst = grmtypelst, caption = "grmtype", 
                            multiple = FALSE, stopifnull = TRUE)
  
  ## Check eststatus
  tstatuslst <- c('Sawtimber', 'Growing-stock', 'All live')
  tstatus <- pcheck.varchar(var2check = tstatus, varnm = "tstatus", gui = gui,
                            checklst = tstatuslst, caption = "tstatus", 
                            multiple = FALSE, stopifnull = TRUE)
  
  ## Check grm_typ_cd
  grm_typ_cdlst <- c(1,2)
  if (length(grm_typ_cd) != 1 &&  grm_typ_cd %in% grm_typ_cdlst) {
    stop("grm_typ_cd must be 1 or 2")
  }
     
  ## get estn_type
  estn_type <- ifelse (tstatus == "Sawtimber", "SL", 
                       ifelse (tstatus == "Growing-stock", "GS",
                               ifelse (tstatus == "All live", "AL", NA)))
  
  ## get land_basis
  land_basis <- ifelse (landarea == 'FOREST', 'FOREST', 
                        ifelse (landarea == "TIMBERLAND", 'TIMBER', 'ALL'))
  
  
  ## Check ACI. If TRUE, include all trees, If FALSE, filter for forested plots only
  ## (COND_STATUS_CD = 1)
  ACI <- pcheck.logical(ACI, varnm="ACI", title="Include ACI tree data?",
                        first="NO", gui=gui)
  
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
    outlst$out_layer <- "tabsum"
  }
 
  
  ###############################################################################
  ## 3. Check tables
  ###############################################################################
  beginend <- grmtables$beginend
  grm <- grmtables$tree_grm_component
  begin <- grmtables$tree_grm_begin
  midpt <- grmtables$tree_grm_midpt
  plt <- grmtables$plt
  cond <- grmtables$cond
  tree <- grmtables$tree
  
  ## 3.1. Check grm tables
  ###############################################################################
  grmlst <- datTabchk(tab = grm, tabtext = "grm",
                      tabid = "TRE_CN",
                      dbconn = dbconn, schema = schema, 
                      dbtablst = dbtablst,
                      bycond = FALSE)
  grmflds <- grmlst$tabflds
  grmuniqueid <- grmlst$uniqueid
  grmx <- grmlst$tabx
  indb <- grmlst$indb
  if (is.data.frame(grmx)) {
    grmnm <- "grmx"
  } else {
    grmnm <- grmlst$tabnm
  }
  
  ## BEGINEND
  if (!indb && is.null(beginend)) {
    beginend <- data.frame(ONEORTWO = c(1,2))
  }
  beginendlst <- datTabchk(tab = beginend, tabtext = "beginend",
                        tabid = "ONEORTWO",
                        dbconn = dbconn, schema = schema, 
                        dbtablst = dbtablst,
                        bycond = FALSE)
  beginendx <- beginendlst$tabx
  if (is.data.frame(beginendx)) {
    beginendnm <- "beginendx"
  } else {
    beginendnm <- beginendlst$tabnm
  }
  
  ## TREE_GRM_BEGIN
  beginlst <- datTabchk(tab = begin, tabtext = "begin",
                        tabid = "TRE_CN",
                        dbconn = dbconn, schema = schema, 
                        dbtablst = dbtablst,
                        bycond = FALSE)
  beginflds <- beginlst$tabflds
  beginuniqueid <- beginlst$uniqueid
  beginx <- beginlst$tabx
  if (is.data.frame(beginx)) {
    beginnm <- "beginx"
  } else {
    beginnm <- beginlst$tabnm
  }
 
  ## TREE_GRM_MIDPT
  midptlst <- datTabchk(tab = midpt, tabtext = "midpt",
                        tabid = "TRE_CN",
                        dbconn = dbconn, schema = schema, 
                        dbtablst = dbtablst,
                        bycond = FALSE)
  midptflds <- midptlst$tabflds
  midptuniqueid <- midptlst$uniqueid
  midptx <- midptlst$tabx
  if (is.data.frame(midptx)) {
    midptnm <- "midptx"
  } else {
    midptnm <- midptlst$tabnm
  }
  
  ## TREE
  treelst <- datTabchk(tab = tree, tabtext = "tree",
                       dbconn = dbconn, schema = schema, 
                       dbtablst = dbtablst,
                       bycond = FALSE)
  treeflds <- treelst$tabflds
  tuniqueid <- treelst$uniqueid
  treex <- treelst$tabx
  treeindb <- treelst$indb
  if (is.data.frame(treex)) {
    treenm <- "treex"
  } else {
    treenm <- treelst$tabnm
  }
  
  
  ## 3.2 Check plot, cond
  ###############################################################################
  plotnm <- condnm <- NULL
  pltindb <- condindb <- FALSE
  
  
  ## Check for pltcondx table 
  ## (including both, plot and cond variables and with "PREV_* names for T1 variables)
  pltcondxlst <- datTabchk(tab = pltcondx, tabtext = "pltcondx",
                       tabid = "PLT_CN",
                       dbconn = dbconn, schema = schema, 
                       dbtablst = dbtablst,
                       bycond = TRUE)
  condflds <- pltcondxlst$tabflds
  pcxuniqueid <- pltcondxlst$uniqueid
  pltcondx <- pltcondxlst$tabx
  pcindb <- pltcondxlst$indb
  if (is.data.frame(pltcondx)) {
    condnm <- "pltcondx"
    cuniqueid <- pcxuniqueid
    pltidsid <- cuniqueid
  } else {
    condnm <- pltcondx$tabnm
  }  
  
  if (is.null(pltcondx)) {
    ## Check plt table
    if (!is.null(plt)) {
      plotlst <- datTabchk(tab = plt, tabtext = "plot",
                           dbconn = dbconn, schema = schema, 
                           dbtablst = dbtablst) 
      pltflds <- plotlst$tabflds
      puniqueid <- plotlst$uniqueid
      pltx <- plotlst$tabx
      sppltx <- plotlst$sptabx
      pltindb <- plotlst$indb
      pltkey <- plotlst$tabkey
      if (is.data.frame(pltx)) {
        plotnm <- "pltx"
      } else {
        plotnm <- plotlst$tabnm
      }
      
      if (!is.null(sppltx)) {
        pltsp <- TRUE
      }
    }
    
    ## Check cond table
    if (!is.null(cond)) {
      condlst <- datTabchk(tab = cond, tabtext = "cond", 
                           dbconn = dbconn, schema = schema, 
                           dbtablst = dbtablst,
                           bycond = bycond) 
      condflds <- condlst$tabflds
      cuniqueid <- condlst$uniqueid
      condx <- condlst$tabx
      condindb <- condlst$indb
      condkey <- condlst$tabkey
      cvars <- c(cuniqueid, condid, propvars)
      if (is.data.frame(condx)) {
        condnm <- "condx"
      } else {
        condnm <- condlst$tabnm
      }
    }
  }
  
  
  if (bysubp) {
    keepall <- FALSE
    subplotnm = subp_condnm <- NULL
    
    ## Check subplot table
    subplotlst <- datTabchk(tab = subplot, tabtext = "subplot", 
                            dbconn = dbconn, schema = schema, 
                            dbtablst = dbtablst) 
    subplotflds <- subplotlst$tabflds
    subplotid <- subplotlst$uniqueid
    subplotx <- subplotlst$tabx
    if (is.data.frame(subplotx)) {
      subplotnm <- "subplotx"
    } else {
      subplotnm <- subplotlst$tabnm
    }
    
    ## Check subp_cond table
    subpcondlst <- datTabchk(tab = subp_cond, tabtext = "subp_cond", 
                             dbconn = dbconn, schema = schema, 
                             dbtablst = dbtablst) 
    subpcondflds <- subpcondlst$tabflds
    subpcondid <- subpcondlst$uniqueid
    subpcondx <- subpcondlst$tabx
    if (is.data.frame(subpcondx)) {
      subp_condnm <- "subpcondx"
    } else {
      subp_condnm <- subpcondlst$tabnm
    }
  }  
  
  ## check pltidsadj
  if (adjtree) {
    
    ## Check for pltcondx table 
    ## (including both, plot and cond variables and with "PREV_* names for T1 variables)
    pltadjlst <- datTabchk(tab = pltidsadj, tabtext = "pltidsadj",
                           tabid = "CN",
                           bycond = FALSE)
    pltadjflds <- pltadjlst$tabflds
    pltadjid <- pltadjlst$uniqueid
    pltidsadj <- pltadjlst$tabx
    pcindb <- pltadjlst$indb
    if (is.data.frame(pltidsadj)) {
      pltidsid <- pltadjid
    } else {
      pltidsadjnm <- pltadjlst$tabnm
    }  
  }  
  
  
  ###############################################################################
  ## 4. Check if condition table is in WITH queries (e.g., pltcondx)
  ###############################################################################
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
    if (is.null(pltidsid)) {
      stop("use pltidsid to define the unique plot identifier variable in pltidsWITHqry (e.g., pltidsid = 'PLT_CN')")
    }
    chk <- check.logic.vars(pltidsid, pltidsWITHqry)
    if (!chk) {
      chk <- check.logic.vars(tolower(pltidsid), tolower(pltidsWITHqry))
      if (!chk) {
        chk <- check.logic.vars(tolower(pltidsid), tolower(pltidsWITHqry))
        stop("invalid pltidsid... make sure it is in pltidsWITHqry")
      }
    }
    
    ## Set name of pltids and alias path
    pltidsnm <- "pltids"
    pltidsa. <- "pltids."
    pltidsa <- "pltids"
    
    ## check if condnm in pltidsWITHqry
    if (!is.null(condnm) && !condindb &&
        check.logic.vars(condnm, pltidsWITHqry, ignore.case=TRUE)) {
      condinWITHqry <- TRUE
    }
    
    if (condinWITHqry) {
      
      condflds.qry <- paste0(
        pltidsWITHqry,
        "\nSELECT * FROM ", condnm, " LIMIT 0"
      )
      condfldsdf <- tryCatch(
        DBI::dbGetQuery(dbconn, condflds.qry),
        error = function(cond) {
          return(NULL)
        })
      if (is.null(condfldsdf)) {
        message("pltidsWITHqry is invalid...")
        message(condflds.qry)
        stop()
      } else {
        condflds <- toupper(names(condfldsdf))
      }
      
      ## get condx (if keepall = TRUE)
      if (keepall) {
        cond.qry <- paste0(
          pltidsWITHqry,
          "\nSELECT * FROM ", condnm)
        
        condx <- tryCatch(
          DBI::dbGetQuery(dbconn, cond.qry),
          error = function(cond) {
            return(NULL)
          })
        if (!is.null(condx)) {
          names(condx) <- toupper(names(condx))
          setkeyv(condx, condkey)
        }
      }
      
      ## Check unique identifier of plot (cuniqueid)
      if (is.null(cuniqueid)) {
        cuniqueid <- findnm(tabIDs[["cond"]], condflds, returnNULL = TRUE)
        if (is.null(cuniqueid)) {
          stop("missing uniqueid in ", condnm, "... define in tabIDs")
        }
      }
      
      ## Check unique identifier of plot (cuniqueid)
      condid <- findnm(tabIDs[["condid"]], condflds, returnNULL = TRUE)
      if (is.null(condid)) {
        stop("missing uniqueid of a condition in ", condid, "... define in tabIDs")
      }
    }
  }
  
  ## list of plot and cond fields
  pcflds <- toupper(c(pltflds, condflds))
  
  
  ###############################################################################
  ## 5. Check unique identifiers and set unique keys if R objects
  ###############################################################################
  tsumuniqueid <- tuniqueid
  
  ## check if subplot uniqueid is in tab
  if (bysubp) {
    subpidchk <- findnm(subpid, treeflds, returnNULL = TRUE)
    if (is.null(subpidchk)) {
      stop(subpid, " not in tab")
    }
    tsumuniqueid <- c(tsumuniqueid, subpid)
  }
 
  ## check if cond uniqueid is in tab... if it is not, append CONDID = 1
  if (bycond) {
    condidchk <- findnm(condid, treeflds, returnNULL = TRUE)
    if (is.null(condidchk)) {
      message(condid, " not in tab... assuming only 1 condition")
      if (is.data.frame(tabx)) {
        tabx[[condid]] <- 1
      } else {
        stop()
      }
    } else {
      condid <- condidchk
    }
    tsumuniqueid <- c(tsumuniqueid, condid)
  }
  
  ## make uniqueids lowercase
  tsumuniqueid <- tolower(tsumuniqueid)
  cuniqueid <- tolower(cuniqueid)
  pltidsid <- tolower(pltidsid)
  tuniqueid <- tolower(tuniqueid)
  

  ###############################################################################
  ### 6. Check tsumvar
  ###############################################################################
  addtdat <- FALSE
  tsumvarnew <- NULL

  if (is.null(tsumvar)) {
    stop("tsumvar is null")
  }
  
  tsumvar <- tolower(tsumvar)
  treeflds <- tolower(treeflds)
  tsumvars <- unique(treeflds[FIESTAutils::check.logic.vars(treeflds, tsumvar)])
  if (length(tsumvars) == 0) {
    stop("invald tsumvar: ", tsumvar)
  }
 
  ## create variables for SELECT CASE statement
  tsumvar_t <- tsumvar_tmidpt <- tsumvar_tbegin <- tsumvar_ptree <- tsumvar
  for (i in 1:length(tsumvars)) {
    tsumvar_t <- gsub(tsumvars[i], paste0(talias., tsumvars[i]), tsumvar_t)
    tsumvar_tmidpt <- gsub(tsumvars[i], paste0("tre_midpt.", tsumvars[i]), tsumvar_tmidpt)
    tsumvar_tbegin <- gsub(tsumvars[i], paste0("tre_begin.", tsumvars[i]), tsumvar_tbegin)
    tsumvar_ptree <- gsub(tsumvars[i], paste0(talias., "prev_", tsumvars[i]), tsumvar_ptree)
  }

  ## build variables for subquery
  land_basis <- tolower(land_basis)
  estn_type <- tolower(estn_type)
  grmtype <- tolower(grmtype)
  
  ## build tpavar based on grmtype
  tpavar <- tolower(paste0("tpa", grmtype, "_unadj"))
  
  ## build variables for grm subquery based on estn_type and land_basis
  subp_component <- paste0("subp_component_", estn_type, "_", land_basis)
  subp_subptyp <- paste0("subp_subptyp_grm_", estn_type, "_", land_basis)
  subp_tpa <- paste0("subp_tpa", grmtype, "_unadj_", estn_type, "_", land_basis)

  ## check if prev_'tsumvar' exists in the tree fields
  if (!tsumvar_ptree %in% treeflds) {
    addtdat <- TRUE
  }
  
  ###############################################################################
  ### 7. Check tsumvarnm
  ###############################################################################
  if (is.null(tsumvarnm)) {
    tsumvarnm <- "estimated_value"
  } else {
    if (!is.character(tsumvarnm) || length(tsumvarnm) > 0) {
      stop("invalid tsumvarnm")
    }
    ## replace spaces in tsumvarnm with '_'
    tsumvarnm <- sub(" ", "_", tsumvarnm)
  }
  tsumvarnm <- toupper(tsumvarnm)
  
  
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
  tdomainlst <- pdomainlst <- cdomainlst <- NULL
  domainlst <- bydomainlst
  
  
  ## check if all variables in bydomainlst are in the input tables
  if (!all(bydomainlst %in% c(treeflds, pcflds))) {
    missdomain <- bydomainlst[!bydomainlst %in% c(treeflds, pcflds)]
    stop("invalid variable in bydomainlst: ", toString(missdomain))
  }
  
  ## Check if domain variables are in tree or seedling table
  ## Check if domain variables are in tree or seedling table
  if (!is.null(domainlst)) {
    if (any(bydomainlst %in% pcflds)) {
      pcdomainlst <- bydomainlst[bydomainlst %in% pcflds]
      tdomainlst <- bydomainlst[!bydomainlst %in% pcdomainlst]
      if (length(tdomainlst) == 0) tdomainlst <- NULL
    }
    
    # if (seedonly) {
    #   if (any(bydomainlst %in% seedflds)) {
    #     tdomainlst <- bydomainlst[bydomainlst %in% seedflds]
    #     pcdomainlst <- bydomainlst[!bydomainlst %in% tdomainlst]
    #   } else {
    #     pcdomainlst <- bydomainlst
    #   }
    # } else {
    #   if (any(bydomainlst %in% treeflds)) {
    #     tdomainlst <- bydomainlst[bydomainlst %in% treeflds]
    #     pcdomainlst <- bydomainlst[!bydomainlst %in% tdomainlst]
    #   } else {
    #     pcdomainlst <- bydomainlst
    #   }
    # }
  }
  if (length(pcdomainlst) == 0) pcdomainlst <- NULL
  
  if (!is.null(pcdomainlst) && !bycond) {
    warning("variables in bydomainlst may result in condition-level data summaries")
  }
  
  ## Define variables to classify from domainlst or domclassify list
  tclassifyvars <- classifyvars[classifyvars %in% tdomainlst]
  pcclassifyvars <- classifyvars[classifyvars %in% pcdomainlst]

    
  
  ###############################################################################
  ## 12. Check pwhereqry and ACI
  ###############################################################################
  pwhereqry = cwhereqry <- NULL
  
  # if (!is.null(pcwhereqry)) {
  #   if (is.null(pcflds)) {
  #     stop("must include plot and/or cond if including pcwhereqry")
  #   } else {
  #     pcwhereqry <- check.logic(pcflds, pcwhereqry)
  #     pcwhereqry <- RtoSQL(pcwhereqry)
  #     
  #     if (!is.null(plotnm)) {
  #       pwhereqry <- check.logic(pltflds, pcwhereqry, stopifinvalid = FALSE)
  #       
  #       ## Add alias to plot filters
  #       if (!is.null(pwhereqry)) {
  #         pchkvars <- check.logic.vars(pltflds, pcwhereqry, returnVars = TRUE)
  #         for (pchkvar in pchkvars) {
  #           if (!grepl(paste0("pc.", pchkvar), pcwhereqry)) {
  #             pcwhereqry <- sub(pchkvar, paste0("pc.", pchkvar), pcwhereqry)
  #           }
  #         }
  #         pvars <- c(pvars, pchkvars)
  #       }
  #     }
  #     if (!is.null(condnm)) {
  #       cwhereqry <- check.logic(condflds, pcwhereqry, stopifinvalid = FALSE)
  #       
  #       ## Add alias to cond filters
  #       if (!is.null(cwhereqry)) {
  #         cchkvars <- check.logic.vars(condflds, pcwhereqry, returnVars = TRUE)
  #         for (cchkvar in cchkvars) {
  #           if (!grepl(paste0("pc.", cchkvar), pcwhereqry)) {
  #             pcwhereqry <- sub(cchkvar, paste0("pc.", cchkvar), pcwhereqry)
  #           }
  #         }
  #         cvars <- c(cvars, cchkvars)
  #       }
  #     }
  #     
  #     if (!(startsWith(gsub(" ", "", pcwhereqry), "\nWHERE"))) {
  #       if (startsWith(gsub(" ", "", pcwhereqry), "WHERE")) {
  #         pcwhereqry <- paste0("\n ", pcwhereqry)
  #       } else {
  #         pcwhereqry <- paste0("\nWHERE ", pcwhereqry)
  #       }
  #     }
  #   }
  # }
  
  # ## If ACI, include COND_STATUS_CD = 1 to exclude conditions measured on ACI plots
  # if (!ACI) {
  #   if (is.null(condflds)) {
  #     message("must include cond to exclude ACI plots... assuming data has no ACI plots")
  #   } else {
  #     cond_status_cdnm <- findnm("COND_STATUS_CD", condflds, returnNULL = TRUE)
  #     if (is.null(cond_status_cdnm)) {
  #       message("must include COND_STATUS_CD in cond to exclude ACI plots... assuming data has no ACI plots")
  #     }
  #     if (!is.null(cwhereqry)) {
  #       if (!(grepl("COND_STATUS_CD", cwhereqry, ignore.case = TRUE) &&
  #             (grepl("COND_STATUS_CD=1", gsub(" ", "", cwhereqry), ignore.case = TRUE) ||
  #              grepl("COND_STATUS_CDin(1)", gsub(" ", "", cwhereqry), ignore.case = TRUE)))) {
  #         pcwhereqry <- paste0(cwhereqry, " AND ", cond_status_cdnm, " = 1")
  #       }
  #     } else if (!is.null(pltidsWITHqry)) {
  #       if (!(grepl("COND_STATUS_CD", pltidsWITHqry, ignore.case = TRUE) &&
  #             (grepl("COND_STATUS_CD=1", gsub(" ", "", pltidsWITHqry), ignore.case = TRUE) ||
  #              grepl("COND_STATUS_CDin(1)", gsub(" ", "", pltidsWITHqry), ignore.case = TRUE)))) {
  #         cwhereqry <- pcwhereqry <- paste0("\n WHERE ", cond_status_cdnm, " = 1")
  #       }
  #     } else {
  #       cwhereqry <- pcwhereqry <- paste0("\n WHERE ", cond_status_cdnm, " = 1")
  #     }
  #     cvars <- unique(c(cvars, cond_status_cdnm))
  #     
  #     if (!is.null(pcwhereqry) && !grepl(paste0("pc.", cond_status_cdnm), pcwhereqry)) {
  #       pcwhereqry <- sub(cond_status_cdnm, paste0("pc.", cond_status_cdnm), pcwhereqry)
  #     }
  #   }
  # }
  
  
  ###############################################################################
  ## 13. Check if adjustment variable is in tab
  ###############################################################################
  ## For GRM, there will be no tadjfac in the tables, because the tpa variable
  ## depends on what the user grmtype asked for (i.e., grow, mort, remv)
  ## but, we will check if getadjplot = TRUE or pltidsWITHqry is not NULL
  if (adjtree) {
    if (!is.null(pltidsWITHqry) && check.logic.vars("pltidsadj", pltidsWITHqry)) {
      getadjplot <- FALSE
    } else if (datsource == "obj" && exists("pltidsadj") && is.data.frame(pltidsadj)) {
      getadjplot <- FALSE
    } else {
      getadjplot <- TRUE
    }
  }
  

  ###############################################################################
  ## 14. Get plot and cond if keepall = TRUE
  ###############################################################################
  if (!is.null(plotnm) && pltindb && keepall && !bycond && !bysubp) {
    
    plt.qry <- paste0("SELECT ", toString(pltflds),
                      "\nFROM ", SCHEMA., plotnm)
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
    if (!is.null(pltx)) {
      names(pltx) <- toupper(names(pltx))
      setkeyv(pltx, pltkey)
    }
  }
  
  ## get condx (if keepall = TRUE)
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
    names(condx) <- toupper(names(condx))
    condx <- setDT(condx)
    setkeyv(condx, c(cuniqueid, condid))
  }
  
  
  #########################################################################################
  #########################################################################################
  ## Build queries
  #########################################################################################
  #########################################################################################
  
  
  #################################################################################
  ## 15. Build WITH query to get cond data (pltcondx)
  #################################################################################
  message("building query for plot/cond data...")
  
  pjoin <- ifelse((!is.null(pdomainlst) || !is.null(pwhereqry)), TRUE, FALSE)
  cjoin <- ifelse((!is.null(cdomainlst) || !is.null(cwhereqry) || getadjplot), TRUE, FALSE)
  
  if (!condinWITHqry && (!is.null(condnm) && condnm != "pltcondx")) {
    
    ## FROM statement for pltcondx WITH query
    ##################################################################
    if (pjoin) {
      if (!is.null(pltidsWITHqry)) {
        pjoinqry <- getjoinqry(puniqueid, pltidsid, "p.", pltidsa.)
        pltcondxFROMqry <- paste0("\nFROM ", pltidsnm, " ", pltidsa,
                                  "\nJOIN ", SCHEMA., plotnm, " p ", pjoinqry)
      } else {
        pltcondxFROMqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
      }
    }
    if (cjoin) {
      if (pjoin) {
        if (!is.null(pltidsWITHqry)) {
          pjoinqry <- getjoinqry(puniqueid, pltidsid, "p.", pltidsa.)
          pltcondxFROMqry <- paste0("\nFROM ", pltidsnm, " ", pltidsa,
                                    "\nJOIN ", SCHEMA., plotnm, " p ", pjoinqry)
        } else {
          pltcondxFROMqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
        }
        
        joinqry <- getjoinqry(cuniqueid, puniqueid, "c.", "p.")
        pltcondxFROMqry <- paste0(pltcondxFROMqry, 
                                  "\nJOIN ", SCHEMA., condnm, " c ", joinqry)
      } else {
        if (!is.null(pltidsWITHqry)) {
          cjoinqry <- getjoinqry(cuniqueid, pltidsid, "c.", pltidsa.)
          pltcondxFROMqry <- paste0("\nFROM ", pltidsnm, " ", pltidsa,
                                    "\nJOIN ", SCHEMA., condnm, " c ", cjoinqry)
        } else {
          pltcondxFROMqry <- paste0("\nFROM ", SCHEMA., condnm, " c")
        }
      }
    }
    
    ## SELECT statement for pltcondx WITH query
    ##################################################################
    if (pjoin || cjoin) {
      pltcondxSELECTqry <- paste0("\n(SELECT")
      
      if (pjoin) {
        pltcondxSELECTqry <- paste0(pltcondxSELECTqry, " ", toString(paste0("p.", c(pvars, pdomainlst))))
        
        if (cjoin) {
          pltcondxSELECTqry <- paste0(pltcondxSELECTqry, ", ", toString(paste0("c.", c(cvars, cdomainlst))))
        }
      } else {
        if (cjoin) {
          pltcondxSELECTqry <- paste0(pltcondxSELECTqry, " ", toString(paste0("c.", c(cvars, cdomainlst))))
        }
      }
      
      ## Build final WITH query for pltcondx
      if (!is.null(pltidsWITHqry)) {
        pltidsWITHqry <- paste0(
          pltidsWITHqry, ", ",
          "\n----- get pltcondx data",
          "\npltcondx AS", 
          pltcondxSELECTqry,
          pltcondxFROMqry, ")")
      } else {
        pltidsWITHqry <- paste0(
          "WITH ",
          "\npltcondx AS", 
          pltcondxSELECTqry,
          pltcondxFROMqry, ")")
      }
      
      
      pltidsnm <- "pltcondx"
      pltidsid <- cuniqueid
      pltidsa <- "pc"
      pltidsa. <- "pc."
    }
  }
  
  
  #################################################################################
  ## 16. Build query for adjustment factors (if getadjplot = TRUE)
  #################################################################################
  pca. <- "pc."
  
  if (adjtree) {
    if (getadjplot) {
      stop("not available yet")
    #   
    #   message("building query for plot-level adjustments...")
    #   adjjoinid <- cuniqueid
    #   
    #   ## Build FROM query including subplot and subp_cond
    #   pcfromqry <- "\n FROM pltcondx pc"
    #   
    #   ## Build WHERE query to filter nonsampled plots
    #   pcADJwhereqry <- getADJwherePLOT(condflds, conda. = pca.)
    #   
    #   
    #   if (bysubp) {
    #     adjjoinid <- subplotid
    #     
    #     ## Build WHERE query for removing nonsampled subplots
    #     subpwhereqry <- getADJwhereSUBP(subplotflds, 
    #                                     adjwhereqry = pcADJwhereqry)
    #     
    #     subpa. <- "subp."
    #     subpca. <- "subpc."
    #     subpjoinqry <- getjoinqry(subplotid, cuniqueid, subpa., pca.)
    #     subpfromqry <- paste0(
    #       pcfromqry,
    #       "\n JOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry,
    #       "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subplotid, " = ", pca., cuniqueid,
    #       " AND ", subpca., condid, " = ", pca., condid,
    #       " AND ", subpca., subpid, " = ", subpa., subpid, ")")
    #     
    #     
    #     ## First, get query for summarizing subplot sampled proportions
    #     sumpropqry <- sumpropSUBPqry(fromqry = subpfromqry,
    #                                  whereqry = subpwhereqry,
    #                                  ACI = ACI,
    #                                  selectvars = NULL,
    #                                  SCHEMA. = SCHEMA.)
    #     ADJqrySUBP <-
    #       getADJqry(popType = "VOL",
    #                 adj = "plot",
    #                 propvars = propvars,
    #                 adjfromqry = "\n FROM subpcprop",
    #                 pwhereqry = NULL,
    #                 pltidsid = subplotid,
    #                 pltassgnid = c(pltassgnid, subpid),
    #                 pltidsa. = NULL)
    #     #message(ADJqrySUBP)
    #     
    #     
    #     ## Build final query for adjustment factors, including pltids WITH query
    #     if (!is.null(pltidsWITHqry)) {
    #       
    #       pltidsWITHqry <- paste0(
    #         pltidsWITHqry, ", ",
    #         "\n----- sum sampled subplot proportions",
    #         "\nsubpcprop AS ",
    #         "\n(", sumpropqry, "),",
    #         "\n----- adjustment factors",
    #         "\npltidsadj AS ",
    #         "\n(", ADJqrySUBP, ")")
    #       #message(pltidsWITHqry)
    #     } else {
    #       
    #       pltidsWITHqry <- paste0(
    #         "\n----- sum sampled subplot proportions",
    #         "\nsubpcprop AS ",
    #         "\n(", sumpropqry, "),",
    #         "\n----- adjustment factors",
    #         "\npltidsadj AS ",
    #         "\n(", ADJqrySUBP, ")")
    #     }
    #     
    #   } else {   ## bysubp = FALSE
    #     
    #     ADJqry <-
    #       getADJqry(popType = "VOL",
    #                 adj = "plot",
    #                 propvars = propvars,
    #                 adjfromqry = pcfromqry,
    #                 pwhereqry = pcADJwhereqry,
    #                 pltidsid = cuniqueid,
    #                 pltassgnid = pltassgnid,
    #                 pltidsa. = "pc.")
    #     #message(ADJqry)
    #     
    #     ## Build final query for adjustment factors, including pltids WITH query
    #     if (!is.null(pltidsWITHqry)) {
    #       
    #       pltidsWITHqry <- paste0(
    #         pltidsWITHqry, ", ",
    #         "\n----- adjustment factors",
    #         "\npltidsadj AS ",
    #         "\n(", ADJqry, ")")
    #       #message(pltidsWITHqry)
    #     }
    #   }
    } else { ## END getadjplot
      adjjoinid <- pltidsid
    }
    
    ## Build query for select CASE statement to add adjfactors
    ######################################################################################
    adjalias. <- NULL
  
    tadjcase <- paste0(
      "\n  (CASE WHEN COALESCE(grm.subptyp_grm, 0) = 0 THEN (0)",
      "\n        WHEN grm.subptyp_grm = 1 THEN padj.adj_factor_subp",
      "\n        WHEN grm.subptyp_grm = 2 THEN padj.adj_factor_micr",
      "\n        WHEN grm.subptyp_grm = 3 THEN padj.adj_factor_macr",
      "\n   ELSE (0) END)")
  
  }
  

  #################################################################################
  #################################################################################
  ## 18. Build WITH query to get tree data (tdat)
  #################################################################################
  #################################################################################
  ## Note: only do this if you do not have prev_* columns in your tree data
  if (addtdat) {
    message("building query for tree data...")
    
    adjalias. <- "adj."
    twithalias <- "tdat."
    adjjoinid <- "cn"
    
    
    ## 18.1. FROM statement for tdat WITH query
    ##########################################################################
    if (treeindb) {
      twithfromqry <- paste0("\n FROM ", SCHEMA., treenm, " t")
    } else {
      twithfromqry <- paste0("\n FROM ", treenm, " t")
    }
    
    ## FROM statement - adjustment factors / pltidsWITHqry
    if (!is.null(pltidsWITHqry)) {
      tjoinqry <- getjoinqry(tuniqueid, pltidsid, talias., pltidsa.)
      twithfromqry <- paste0(twithfromqry,
                             "\n JOIN ", pltidsnm, " ", pltidsa, " ", tjoinqry)
    }

    ## Add previous tree data
    prev_tre_cnnm <- findnm("PREV_TRE_CN", treeflds, returnNULL = TRUE)
    prevtjoinqry <- getjoinqry("CN", prev_tre_cnnm, "ptree.", talias.)
    twithfromqry <- paste0(twithfromqry,
                           "\n LEFT OUTER JOIN ", SCHEMA., treenm, " ptree ", prevtjoinqry)
    
    
    ## 18.2. WHERE statement for tdat WITH query - tfilter
    ##########################################################################
    ## no tree filters are used for grm estimates
    
    
    ## 18.3. SELECT statement for tdat WITH query
    #######################################################################
    ## Compile initial select variables
    twithvars <- c("CN", "CONDID", "PREVCOND", "SUBP", "TREE")
    twithvarschk <- sapply(twithvars, findnm, treeflds, returnNULL = TRUE)
    if (all(is.null(twithvarschk))) {
      twithvars <- NULL
    } else {
      twithvars <- unlist(twithvarschk)
    }
    twithSelect <- paste0(talias., unique(c(tsumuniqueid, twithvars)))
    
    
    ## Build SELECT statement 
    twithqry <- paste0("SELECT ", toString(twithSelect))
    
    ## add sumvars with alias to SELECT statement
    twithqry <- paste0(twithqry, ", ",
                       "\n       ", toString(paste0(talias., tsumvars)))
    
    ## add sumvars and previous tsumvars with alias to SELECT statement 
    prev_tsumvars <- paste0(tsumvars, " AS prev_", tsumvars)
    twithqry <- paste0(twithqry,  ", ",
                       "\n       ", toString(paste0("ptree.", prev_tsumvars)))
    
    
    ## Build final tree WITH query
    twithqry <- paste0(twithqry,
                       twithfromqry)
    
    
    ## Append tdat WITH query to pltidsWITHqry
    ##########################################################################
    if (!is.null(pltidsWITHqry)) {
      pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                              "\n----- get tree data",
                              "\ntdat AS",
                              "\n(", twithqry, ")")
    } else {
      pltidsWITHqry <- paste0("WITH tdat AS",
                              "\n(", twithqry, ")")
    }
    
    treenm <- "tdat"
  } 
  
  
  #################################################################################
  #################################################################################
  ## 19. Build query for summarizing tree data
  #################################################################################
  #################################################################################
  message("building query for summarizing tree data...\n")
  
  ## 19.1. FROM statement 
  ####################################################################
  uniqueid <- tuniqueid
  adjalias. <- "padj."
  calias. <- "c."
  palias. <- "p."
  
  ## Start FROM statement with beginend and tree tables
  fromqry <- paste0(
    "\nFROM ", SCHEMA., "beginend be,",
    "\n", treenm, " t")
  
  
  ## Now, build the rest of the FROM statement
  if (adjtree) {
    if (getadjplot) {
      ## add query for calculating adjustment factors
    } else {
      tadjjoinqry <- getjoinqry(adjjoinid, tuniqueid, adjalias., talias.)
      fromqry <- paste0(fromqry,
                        "\nJOIN pltidsadj padj ", tadjjoinqry)
    }
  }

  
  ## Add table(s) for plot / cond (i.e., pltcondx) for current and previous data
  if (condnm == "pltcondx") {
    calias. <- "pc."
    palias. <- "pc."
    
    prevcondnm <- findnm("prevcond", treeflds, returnNULL = TRUE)
    if (is.null(prevcondnm)) stop("need prevcond variable in tree table")
    prevpccondnm <- findnm("prev_condid", pcflds, returnNULL = TRUE)
    if (is.null(prevpccondnm)) stop("need prev_condid variable in pltcond table")
    
    tjoinqry <- getjoinqry(c(cuniqueid, condid, prevpccondnm), c(tuniqueid, condid, prevcondnm), calias., talias.)
    fromqry <- paste0(fromqry,
                       "\nJOIN pltcondx pc ", tjoinqry)
  } else {
    palias. <- "p."
    
    tjoinqry <- getjoinqry(puniqueid, tuniqueid, palias., talias.)
    fromqry <- paste0(fromqry,
                      "\nJOIN ", SCHEMA., plotnm, " p ", tjoinqry,
                      "\nJOIN ", SCHEMA., plotnm, " pplot ON (pplot.cn = p.prev_plt_cn)",
                      "\nJOIN ", SCHEMA., condnm, " c ON (c.plt_cn = p.cn)",
                      "\nJOIN ", SCHEMA., condnm, " pcond ON (pcond.plt_cn = p.prev_plt_cn)")
  }
  

  ## Next, build subquery to query grm table based on tpavar
  grm_subqry_select.qry <- paste0(
    "SELECT tre_cn, dia_begin, dia_midpt, dia_end,", 
    "\n                        ", subp_component, " AS component,", 
    "\n                        ", subp_subptyp, " AS subptyp_grm,", 
    "\n                        ", subp_tpa, " AS ", tpavar)
  
  grm_subqry_from.qry <- paste0(
    "\n                 FROM ", SCHEMA., grmnm)
  
  grm_subqry.qry <- paste0(grm_subqry_select.qry,
                           grm_subqry_from.qry)
  
  
  ## And finally, add tree table for previous data, grm tables (begin, midpt), and the grm subquery
  tfromqry <- paste0(fromqry,
                     "\nLEFT OUTER JOIN ", SCHEMA., beginnm, " tre_begin ON (tre_begin.tre_cn = ", talias., "cn)",
                     "\nLEFT OUTER JOIN ", SCHEMA., midptnm, " tre_midpt ON (tre_midpt.tre_cn = ", talias., "cn)",
                     "\nLEFT OUTER JOIN (", grm_subqry.qry, ") grm ON (grm.tre_cn = ", talias., "cn)")

  
  ## 19.2 Build SELECT statement 
  ####################################################################
  
  ## First, get grm_typ_cd based on grmtype
  ## The grm_typ_cd determines the type case query to use for SELECT statement
  #grm_typ_cd <- get(tolower(paste0(grmtype, "_typ_cd")))
  
  
  #define grpby variables
  tgrpbyvars <- paste0(talias., tsumuniqueid)
  
  ## add grpby variable to SELECT statement
  tselectqry <- paste0("\nSELECT ", toString(tgrpbyvars))
  
  ## Add classifications to SELECT statement
  domclassifyqry <- NULL
  
  nbrclassify <- length(domclassify)
  if (!is.null(domclassify)) {
    classnames <- NULL
    
    ## keep track of classes in a list
    classifylist <- list()
    
    ## loop through domclassify
    for (j in 1:nbrclassify) {
      
      ## create a nested list for class info
      classifylist[[j]] <- list()
      
      classifyvar <- names(domclassify)[j]
      classifylut <- domclassify[[j]]
      if (is.vector(classifylut)) {
        classifynm <- paste0(classifyvar, "CL")
        if (classifyvar %in% pcclassifyvars) {
          classvar. <- "pc."
          pcdomainlst[pcdomainlst == classifyvar] <- classifynm
        } else {
          classvar. <- "tdat."
          tdomainlst[tdomainlst == classifyvar] <- classifynm
        }
        domainlst <- domainlst[domainlst != classifyvar]
        
        ## Get classification query
        cutbreaks <- classifylut
        fill <- NULL

        ## create labels for cutbreaks
        cutlabels <- {}
        for (i in 1:length(cutbreaks)) {
          brk <- cutbreaks[i]
          if (i == length(cutbreaks)) {
            cutlabels <- c(cutlabels, paste0(brk, "+"))
          } else {
            cutlabels <- c(cutlabels, paste0(brk, "-", cutbreaks[i+1]))
          }
        }
        
        ## create class name
        classifynm <- paste0(classifyvar, "CL")
        classifynm <- checknm(classifynm, classnames)
        classnames <- unique(c(classnames, classifynm))
        classifylist[[j]]$classifynm <- classifynm
        tgrpbyvars <- c(tgrpbyvars, classifynm)
        
        ## define factor levels for new class
        classifylist[[j]]$factorlevels <- cutlabels
        
        domclassqry <- classifyqry(classcol = classifyvar,
                                   cutbreaks = cutbreaks,
                                   cutlabels = cutlabels,
                                   classnm = classifynm,
                                   class. = classvar.,
                                   fill = fill)
        
      } else if (is.data.frame(classifylut)) {
        if (ncol(classifylut) != 2) {
          message("invalid number of columns for ", classifyvar,
                  "... must be a vector of class breaks or a data.frame with 2 columns")
          stop()
        }
        
        ## get class name
        classifynm <- names(classifylut)[!names(classifylut) %in% classifyvar]
        classifynm <- checknm(classifynm, classnames)
        classnames <- unique(c(classnames, classifynm))
        classifylist[[j]]$classifynm <- classifynm
        tgrpbyvars <- c(tgrpbyvars, classifynm)
        
        
        ## define factor levels for new class
        classifylist[[j]]$factorlevels <- unique(classifylut[[classifynm]])
        
        if (length(classifynm) != 1) {
          message("invalid column names for ", classifyvar,
                  "... the data.frame must include name of variable to domclassify: ", classifyvar)
          stop()
        }
        fromval <- classifylut[[classifyvar]]
        toval <- classifylut[[classifynm]]
        if (classifyvar %in% pcclassifyvars) {
          classvar. <- "pc."
          pcdomainlst[pcdomainlst == classifyvar] <- classifynm
        } else {
          classvar. <- "tdat."
          tdomainlst[tdomainlst == classifyvar] <- classifynm
        }
        domainlst <- domainlst[domainlst != classifyvar]
        
        ## Get classification query
        domclassqry <- classqry(classifyvar, fromval, toval,
                                classnm = classifynm,
                                class. = classvar.,
                                fill = NULL)
      }
      classifynmlst[[classifyvar]] <- classifynm
      
      domclassifyqry <- paste0(domclassifyqry, "\n", domclassqry)
      if (nbrclassify > 1 && j < nbrclassify) {
        domclassifyqry <- paste0(domclassifyqry, ",")
      }
    }
    tgrpbyvars <- tolower(c(domainlst, tgrpbyvars))
    
    if (length(domainlst) > 0) {
      tselectqry <- paste0(tselectqry, ", ", toString(domainlst), ", ", domclassifyqry)
    } else {
      tselectqry <- paste0(tselectqry, ", ", domclassifyqry)
    }
  } else if (!is.null(domainlst) && length(domainlst) > 0) {
    
    if (length(tdomainlst) > 0) {
      tdomainlst <- tolower(tdomainlst)
      tselectqry <- paste0(tselectqry, ", ", toString(paste0("tdat.", tdomainlst)))
      tgrpbyvars <- unique(c(tgrpbyvars, paste0("tdat.", tdomainlst)))
    } 
    if (length(pcdomainlst) > 0) {
      pcdomainlst <- tolower(pcdomainlst)
      tselectqry <- paste0(tselectqry, ", ", toString(paste0(pca., pcdomainlst)))
      tgrpbyvars <- unique(c(tgrpbyvars, paste0(pca., pcdomainlst)))
    }
  }
  
  ## Add tpavar and adjustment factors to SELECT statement
  tselectqry <- paste0(tselectqry, ", ",
                       "\nSUM(grm.", tpavar)
  if (adjtree) {
    tselectqry <- paste0(tselectqry, " * ",
                         tadjcase)
  }
  
  
  ## build case query for SELECT statement
  if (grmtype == 'mort') {
    
    grm_case.qry <- paste0(
      "\n  (CASE WHEN grm.component LIKE 'MORTALITY%'",
      "\n      THEN ", tsumvar_tmidpt, " ELSE (0) END)") 
    
  } else if (grmtype == 'remv') {
    grm_case.qry <- paste0(
      "\n  (CASE WHEN (grm.component LIKE 'CUT%' OR grm.component LIKE 'DIVERSION%')", 
      "\n      THEN ", tsumvar_tmidpt, " ELSE (0) END)")
    
  } else if (grmtype == 'grow') {
    
    
    if (all(grm_typ_cd == 2)) {
      grm_case.qry <- paste0(
        "\n  (CASE WHEN be.oneortwo = 2 THEN",
        "\n     (CASE WHEN (grm.component = 'SURVIVOR' OR grm.component = 'INGROWTH' OR grm.component LIKE 'REVERSION%')",
        "\n              THEN ((", tsumvar_t, ") / ", palias., "remper)", 
        "\n           WHEN (grm.component LIKE 'CUT%' OR grm.component LIKE 'DIVERSION%')",
        "\n              THEN ((", tsumvar_tmidpt, ") / ", palias., "remper)",
        "\n      ELSE (0) END)", 
        "\n   ELSE",
        "\n     (CASE WHEN (grm.component = 'SURVIVOR' OR grm.component = 'CUT1' 
                            OR grm.component = 'DIVERSION1' OR grm.component = 'MORTALITY1')", 
        "\n              THEN CASE WHEN tre_begin.tre_cn IS NOT NULL THEN - ((", tsumvar_tbegin, ") / ", palias., "remper)",
        "\n                        ELSE - ((", tsumvar_ptree, ") / ", palias., "remper)",
        "\n                   END",
        "\n      ELSE (0) END)",
        "\n   END)) AS ", tsumvarnm) 
      
    } else {
      grm_case.qry <- "grm.ann_net_growth"
    }
  }

  
  ## Build final SELECT statement
  tselectqry <- paste0(tselectqry, " * ",
                       grm_case.qry)
  
 
  ## Build query to summarize tree data, including pcwhereqry
  ################################################################
  grm.qry <- paste0(tselectqry,
                    tfromqry,
                    "\nGROUP BY ", toString(tgrpbyvars))
  
  ## Build final query to summarize tree data including WITH queries
  ################################################################
  if (!is.null(pltidsWITHqry)) {
    grm.qry <- paste0(pltidsWITHqry,
                       "\n-------------------------------------------",
                      grm.qry)
  }

  
  message("running query...")
  if (datindb) {
    sumdat <- tryCatch(
      DBI::dbGetQuery(dbconn, grm.qry),
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  } else {
    sumdat <- tryCatch(
      sqldf::sqldf(grm.qry),
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  }
  if (is.null(sumdat)) {
    message("tree query is invalid...")
    message(grm.qry)
    stop()
  } else {
    names(sumdat) <- toupper(names(sumdat))
  }
  setkeyv(setDT(sumdat), toupper(tsumuniqueid))
  
  
  ## set factors to classify variables, if any
  if (!is.null(domclassify)) {
    for (i in 1:length(classifylist)) {
      sumdat[[classifylist[[i]]$classifynm]] <-
        factor(sumdat[[classifylist[[i]]$classifynm]], levels=classifylist[[i]]$factorlevels)
    }
  }
  
  ## Round digits
  tcols <- toupper(sumdat$NAME)
  if (!is.null(tround)) {
    sumdat[, (tcols) := round(.SD, tround), .SDcols=tcols]
  }
  
  ## Join tables
  #############################################################
  if (keepall) {
    if (!bycond && !is.null(pltx)) {
      if (pltsp) {
        sumdat <- merge(sppltx, sumdat, by.x=puniqueid, by.y=tsumuniqueid, all.x=TRUE)
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
  
  
  
  ## Round digits
  if (!is.null(tround)) {
    sumdat[, (tsumvarnm) := round(.SD, tround), .SDcols=tsumvarnm]
  }
  
  
  ######################################################################## 
  ## Check tdomvar and tdomvar2
  ######################################################################## 
  if (!is.null(tdomvar)) {
    tdomtree <- sumdat
    
    if (!is.null(classifynmlst[[tdomvar]])) {
      tdomvar <- classifynmlst[[tdomvar]]
    }
    if (!is.null(tdomvar2) && !is.null(classifynmlst[[tdomvar2]])) {
      tdomvar2 <- classifynmlst[[tdomvar2]]
    }
    
    ## Get unique values of tdomvar
    tdoms <- sort(unique(tdomtree[[tdomvar]]))
    
    ## Check tdomvarlst
    nbrtdoms <- length(tdoms)
    if (is.null(tdomvarlst)) {
      ## get tdomvarlst
      tdomvarlst <- tdoms
    } else { 
      if (any(!tdomvarlst %in% tdoms)) {
        tdom.miss <- tdomvarlst[which(!tdomvarlst %in% tdoms)]
        if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
          message("tdomvarlst domain values not in tree table: ", toString(tdom.miss))
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
    
    ## GETS name for tdomvar
    #####################################################################
    ## If tdomvar2 exists, concatenate the columns to one column (if pivot=TRUE)
    ## treex is the tree table after filtered tree domains
    
    flag <- ifelse(datSum_opts$NAto0, "0", "")
    if (FIAname) {
      # if (tdomvar == "SPCD") {
      #   tdomdata <- datLUTspp(x = tdomtree, spcdname = spcd_name)
      #   ref_spcd <- tdomdata$ref_spcd
      # } else {    
      tdomdata <- datLUTnm(tdomtree, xvar=tdomvar, LUTvar="VALUE", FIAname=TRUE)
      #}
      tdomtree <- tdomdata$xLUT
      tdomvarnm <- tdomdata$xLUTnm
      setkeyv(tdomtree, tsumuniqueid) 
      tdomvarlut <- unique(tdomtree[,c(tdomvar, tdomvarnm), with=FALSE]) 
      
      tdomvarlst2 <- tdomvarlut[match(tdomvarlst, tdomvarlut[[tdomvar]]), 
                                tdomvarnm, with=FALSE][[1]]
      
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
        # if (tdomvar2 == "SPCD") {
        #   tdomdata <- datLUTspp(x=tdomtree, spcdname=spcd_name)
        #   ref_spcd <- tdomdata$ref_spcd
        # } else {
        tdomdata <- datLUTnm(tdomtree, xvar=tdomvar2, LUTvar="VALUE", FIAname=TRUE)
        # }
        tdomtree <- tdomdata$xLUT
        tdomvar2nm <- tdomdata$xLUTnm
      } 
      
      if (is.numeric(tdomtree[[tdomvar2]])) {
        maxchar2 <- max(sapply(tdomvar2lst, function(x) {nchar(x)}))
      }
      
      if (pivot) {
        concatvar <- paste0(tdomvar, "#", tdomvar2)
        tdomtree[, (concatvar) := paste0(tdomtree[[tdomvarnm]], "#", tdomtree[[tdomvar2]])] 
        sumbyvars <- unique(c(tsumuniqueid, pcdomainlst, concatvar))
        tdomtotnm <- tsumvarnm
      } else {
        sumbyvars <- unique(c(sumbyvars, tdomvar2nm))
      }
    } else {
      if (pivot) {
        tdomtotnm <- tsumvarnm
      }
    }
    
    ## Sum tree (and seed) by tdomvarnm
    #####################################################################
    tdomtreef <- tdomtree[, lapply(.SD, sum, na.rm=TRUE), by=sumbyvars, .SDcols=tsumvarnm]
    setkeyv(tdomtreef, tsumuniqueid)
    
    ## Generate tree domain look-up table (tdomvarlut)
    #####################################################################
    nvar <- ifelse(bycond, "NBRCONDS", "NBRPLOTS")
    tsumnm <- tsumvarnm
    
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
    
    
    ######################################################################## 
    ## If pivot=FALSE
    ######################################################################## 
    if (!pivot) {
      treesum <- tdomtreef
      tdomscolstot <- tsumvarnm
      tdomscols <- sort(unique(tdomtreef[[tdomvarnm]]))
      
    } else {
      
      ######################################################################## 
      ## If pivot=TRUE, aggregate tree domain data
      ######################################################################## 
      yvar <- ifelse (is.null(tdomvar2), tdomvarnm, concatvar)
      treesum <- datPivot(tdomtreef, pvar = tsumnm, 
                          xvar = c(tsumuniqueid, pcdomainlst), yvar = yvar,
                          pvar.round = tround, returnDT = TRUE)
      treesum <- setDT(treesum)
      
      ## check if tree domain in tdomlst.. if not, create column with all 0 values
      tdomscols <- colnames(treesum)[!colnames(treesum) %in% sumbyvars]
      if (is.null(concatvar)) {
        UNMATCH <- tdomvarlst2[is.na(match(tdomvarlst2, tdomscols))] 
        if (length(UNMATCH) > 0) {
          treesum[, (UNMATCH) := 0]
          tdomvarlst2 <- c(tdomvarlst2, UNMATCH)
        }
      } else {
        tdomvarlst2 <- tdomscols
      }
      
      ## ADD TOTAL OF TREE DOMAINS IN tdomvarlst 
      if (tdomtot) {
        ## Sum the total tree domains in tdomvarlst after any filtering by plot
        treesum[, (tdomtotnm) := round(rowSums(.SD, na.rm=TRUE), tround), .SDcols=tdomvarlst2]
        tdomscolstot <- c(tdomvarlst2, tdomtotnm)
      } else {
        tdomscolstot <- tdomvarlst2
      }
    } 
    
    ## Merge if keepall
    if (keepall) {
      
      NAcols <- c(tdomscols, tdomtotnm)
      if (pltsp) {
        treesum <- merge(sppltx, treesum, by=tsumuniqueid, all.x=TRUE)
        if (NAto0) {
          for (col in NAcols) treesum[is.na(treesum[[col]]), col] <- 0
        }
      } else {
        ## Check if class of tsumuniqueid matches class of tsumuniqueid
        tabs <- check.matchclass(dat, treesum, tsumuniqueid, key(treesum))
        dat <- tabs$tab1
        treesum <- tabs$tab2
        
        treesum <- treesum[dat]
        if (NAto0) {
          treesum <- DT_NAto0(treesum, cols=NAcols)
        }
        setcolorder(treesum, c(names(treesum)[!names(treesum) %in% NAcols], NAcols))
      }
    }
  } else { 
    
    ## Join tables
    #############################################################
    if (keepall) {
      if (!bycond && !is.null(pltx)) {
        if (pltsp) {
          treesum <- merge(sppltx, sumdat, by.x=puniqueid, by.y=uniqueid, all.x=TRUE)
          tsumuniqueid <- puniqueid
          returnDT <- FALSE
          if (NAto0) {
            for (col in tdomscols) treesum[is.na(treesum[[col]]), col] <- 0
          }
          
        } else {
          ## set key
          pltx <- setDT(pltx)
          setkeyv(pltx, puniqueid)
          
          treesum <- sumdat[pltx]
          #sumdat <- merge(pltx, sumdat, all.x=TRUE)
        }
        
        #if (NAto0) {
        #  sumdat <- DT_NAto0(sumdat, cols=tcols)
        #}
      }
      if (bycond && !is.null(condx)) {
        
        ## Check if class of tsumuniqueid matches class of tsumuniqueid
        tabs <- check.matchclass(sumdat, condx, tsumuniqueid)
        sumdat <- tabs$tab1
        condx <- tabs$tab2
        
        treesum <- sumdat[condx]
        if (NAto0) {
          treesum <- DT_NAto0(treesum, cols=tdomscols)
        }
      }
      setcolorder(treesum, c(names(treesum)[!names(treesum) %in% tdomscols], tdomscols))
    } else {
      treesum <- sumdat
    }
  } 
  
  
  #### WRITE TO FILE
  #############################################################
  if (savedata) {
    message("saving ", out_layer, "...")
    
    if (pltsp) {
      spExportSpatial(treesum,
                      savedata_opts = outlst)
    } else {
      datExportData(treesum,
                    savedata_opts = outlst)
      
    }
  }
  
  if (returnDT) {
    if ("sf" %in% class(treesum)) {
      treesum <- setDT(sf::st_drop_geometry(treesum))
    } else {
      treesum <- setDT(treesum)
    }
  }
  returnlst <- list(tsumdat = treesum,
                    sumvar = tsumvarnm,
                    tsumuniqueid = uniqueid,
                    pltsp = pltsp)
  

  if (!is.null(pcdomainlst)) {
    returnlst$pcdomainlst <- pcdomainlst
  }
  if (!is.null(tdomvar)) {
    returnlst$tdomlst <- tdomscols
    returnlst$tdomainlst <- tdomainlst
    returnlst$tdomvarlst <- tdomvarlst
    if (!is.null(tdomvarlst2)) {
      returnlst$tdomvarlst2 <- tdomvarlst2
    }
    if (tdomtot && !is.null(tdomtotnm)) {
      returnlst$tdomtotnm <- tdomtotnm
    }
  }
  if (!is.null(domclassify)) {
    returnlst$classifynmlst <- classifynmlst
  }
  if (!is.null(tround)) {
    returnlst$tround <- tround
  }
  returnlst$grmqry <- grm.qry
  
  
  ## Disconnect open connection
  if (!dbconnopen && !is.null(dbconn)) {
    DBI::dbDisconnect(dbconn)
  }
  
  
  return(returnlst)
  
}
