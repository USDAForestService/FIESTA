#' Data - Aggregates numeric cover data to the plot or condition-level.
#'
#' Aggregates percent cover data (e.g., COVER_PCT) to plot or condition,
#' including options for filtering.
#'
#' If you want to adjust plot-level or subplot-level information by condition
#' proportions (adjplot), you need to include CONDID & CONDPROP_UNADJ in cond
#' or tree table and COND_STATUS_CD. \cr
#'
#' @param tab Dataframe or comma-delimited file (*.csv). The table with percent 
#' cover.
#' @param tabnm String. Name of input table. If tabnm = NULL and tab is a
#' string, tabnm = tab, otherwise tabnm = 'tab'.
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
#' @param tabid String. Name of variable in tab defining unique identifier of plot.
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
#' @param covtype String. Cover type for estimates (e.g. 'P2VEG')
#' @param tsumvar String. Variable (or derivation) in tab to summarize.
#' If covtype = 'P2VEG' and summing by plot or condition, the COVER_PCT
#' variable in table is divided by 4 number of subplots and by 100 to convert
#' percent to proportion ('SUM(COVER_PCT) / 4').
#' @param tsumunits String. c('prop', 'pct'). If 'prop', the output is 
#' proportion, if 'pct', the output is as a percent.
#' @param tfilter String. Filter to subset the tree data before aggregating
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
#' @param pivot Logical. If TRUE, tdomvar data are transposed (pivoted) to
#' separate columns.
#' @param getadjplot Logical. If TRUE, and adj='plot', adjfactors are
#' calculated for nonsampled conditions at plot-level.
#' @param domclassify List. List for classifying domain variables in bydomainlst
#' (e.g., DIA = c(10,20,30)).
#' @param pltidsWITHqry SQL query. A query identifying plots to sum (e.g.,
#' 'WITH pltids AS (SELECT cn AS PLT_CN FROM plot WHERE statecd=49 and INVYR=2018)')
#' @param pltidsid Sting. Name of unique identifier in pltidsWITHqry.
#' @param pcwhereqry String. Plot/Condition filter if plot and/or cond table is
#' included.
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
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'condsum'.
#'
#' @return A list of the following items: \item{tdat}{ Data frame. Plot or
#' condition-level table with aggregated tree attributes. } \item{sumvars}{
#' String vector. Name(s) of the output aggregated tree attributes. }
#'
#' If savedata=TRUE\cr - treedat will be saved to the outfolder. \cr - a text
#' file of input parameters is saved to outfolder
#' ('outfn'_parameters_'date'.txt).
#' @note If a dat table is provided, the aggregated data will be merged to
#' table and NULL values will be output as 0.
#' @author Tracey S. Frescino
#' @keywords data
#' @export datSumCoverDom
datSumCoverDom <- function(tab = NULL,
                           tabnm = NULL,
                           cond = NULL,
                           plt = NULL,
                           subp_cond = NULL,
                           subplot = NULL,
                           tabid = "PLT_CN",
                           datsource = "obj",
                           dbconn = NULL,
                           dsn = NULL,
                           bycond = FALSE,
                           bysubp = FALSE,
                           bydomainlst = NULL,
                           covtype = "P2VEG",
                           tsumvar = "COVER_PCT",
                           tsumunits = "pct",
                           tfilter = NULL, 
                           tdomvar = NULL, 
                           tdomvarlst = NULL, 
                           tdomvar2 = NULL, 
                           tdomvar2lst = NULL, 
                           pivot = TRUE,
                           getadjplot = FALSE,
                           domclassify = NULL,
                           pltidsWITHqry = NULL,
                           pltidsid = NULL,
                           pcwhereqry = NULL,
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
    condx=pltx=sppltx=pcflds=tdomscols=tdomtotnm=classifynmlst=
    pltflds=condflds <- NULL
  
  #ref_estvar <- FIESTAutils::ref_estvar
  twhereqry=swhereqry=tfromqry=sfromqry=pcfromqry=pcselectvars=tpavarnm=pcdomainlst=
    tdomvarlst=tdomvarlst2=tsumvar=tsumvarnm=tdomtotnm=concatvar <- NULL
  
  datindb <- FALSE
  pltassgnid <- "PLT_CN"
  SCHEMA. <- ""
  checkNA = FALSE   
  keepall = FALSE   ## if TRUE, keeps all variables in table, if included as inputs
  returnDT = TRUE
  FIAname <- FALSE
  tdomtot <- TRUE

  ## Query alias.
  talias. <- "t."

  
  ## For documentation
  # subplot Dataframe or comma-delimited file (*.csv). If getadjplot=TRUE,
  # The subplot-level table with SUBP_STATUS_CD variable for calculating
  # adjustment factors by subplot.
  adjvarlst <- unlist(list(COND="ADJ_FACTOR_COND", SUBP="ADJ_FACTOR_SUBP",
                           MICR="ADJ_FACTOR_MICR", MACR="ADJ_FACTOR_MACR",
                           P2VEG="ADJ_FACTOR_P2VEG_SUBP"))
  
  ## Check PROP names and build query for calculating adjustment factors
  propvars <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")

  
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
    dbconnopen <- dbinfo$dbconnopen
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

  ## Check checkNA
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?",
                          first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE

  ## Check pivot
  pivot <- pcheck.logical(pivot, varnm="pivot", title="Pivot columns?", 
                          first="NO", gui=gui)
  
  ## Check checkNA
  checkNA <- pcheck.logical(checkNA, varnm="checkNA", title="Check NA values?",
                            first="YES", gui=gui)
  if (is.null(checkNA)) checkNA <- FALSE
  
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

  ## 3.1. Check tab
  tablst <- datTabchk(tab = tab, tabtext = "tab", 
                      tabid = "PLT_CN",
                      dbconn = dbconn, schema = schema, 
                      dbtablst = dbtablst,
                      bycond = bycond,
                      bysubp = bysubp) 
  tabflds <- tablst$tabflds
  tuniqueid <- tablst$uniqueid
  tabx <- tablst$tabx
  tabindb <- tablst$indb

  if (!is.null(tabx) && is.data.frame(tabx)) {
    tabnm <- "tabx"
  } else if (is.null(tabnm)) {
     tabnm <- plotlst$tabnm
  }
  

  if (bycond) {
    condid <- tablst$condid
  }
  if (bysubp) {
    subpid <- tablst$subpid
  }

  ## 3.2 Check plot, cond, and subplot tables (if bysubp)
  ###############################################################################
  plotnm <- condnm <- NULL
  pltindb <- condindb <- FALSE
  
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
  
  if (bysubp) {
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
    subpcondlst <- datTabchk(tab = subp_cond, tabtext = "subplot", 
                             dbconn = dbconn, schema = schema, 
                             dbtablst = dbtablst) 
    subpcondflds <- subpcondlst$tabflds
    subpcondid <- subpcondlst$uniqueid
    subpcondx <- subpcondlst$tabx
    if (is.data.frame(subpcondx)) {
      subp_condnm <- "subplotx"
    } else {
      subp_condnm <- subpcondlst$tabnm
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
      stop("invalid pltidsid... make sure it is in pltidsWITHqry")
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
    subpidchk <- findnm(subpid, tabflds, returnNULL = TRUE)
    if (is.null(subpidchk)) {
      stop(subpid, " not in tab")
    }
    tsumuniqueid <- c(tsumuniqueid, subpid)
  }
  
  ## check if cond uniqueid is in tab... if it is not, append CONDID = 1
  if (bycond) {
    condidchk <- findnm(condid, tabflds, returnNULL = TRUE)
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
  
  ## make uniqueids uppercase
  tsumuniqueid <- toupper(tsumuniqueid)
  
  
  ###############################################################################
  ### 6. Check tsumvar
  ###############################################################################
  tsumvarnew <- NULL
  if (is.null(tsumvar)) {
    tsumvar <- "COVER_PCT"
  }
  if (covtype == "P2VEG") {
    tsumvar <- "COVER_PCT"
    if (bysubp) {
      tsumvarnew <- tsumvar
    } else {
      tsumvarnew <- paste0(tsumvar, " * 1.0 / 4 / 100")
    }
  }
  if (is.null(tsumvarnm)) {
    tsumvarnm <- paste0(tsumvar, "_SUM")
    if (adjtree) {
      tsumvarnm <- paste0(tsumvarnm, "_ADJ")
    }
  }
  
  
  ###############################################################################
  ## 7. Check domclassify
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
  ## 8. Check bydomainlst
  ###############################################################################
  pdomainlst <- cdomainlst <- tdomainlst <- NULL
  domainlst <- bydomainlst
  
  
  ## check if all variables in bydomainlst are in the input tables
  if (!all(bydomainlst %in% c(tabflds, pcflds))) {
    missdomain <- bydomainlst[!bydomainlst %in% c(tabflds, pcflds)]
    stop("invalid variable in bydomainlst: ", toString(missdomain))
  }
  
  ## Check if domain variables are in tab or cond
  if (!is.null(domainlst)) {
    if (any(bydomainlst %in% tabflds)) {
      tdomainlst <- bydomainlst[bydomainlst %in% tabflds]
      pcdomainlst <- bydomainlst[!bydomainlst %in% tdomainlst]
    } else {
      pcdomainlst <- bydomainlst
    }
  }
  if (length(pcdomainlst) == 0) pcdomainlst <- NULL
  
  
  ## Check if domain variables are in the plot or cond table
  if (!is.null(pcdomainlst)) {
    if (!is.null(plotnm) && any(pcdomainlst %in% pltflds)) {
      pdomainlst <- pcdomainlst[pcdomainlst %in% pltflds]
    } else if (!is.null(condnm) && any(pcdomainlst %in% condflds)) {
      cdomainlst <- pcdomainlst[pcdomainlst %in% condflds]
    }
  }
  
  if (!is.null(pcdomainlst) && !bycond) {
    warning("variables in bydomainlst may result in condition-level data summaries")
  }
  
  ## Define variables to classify from domainlst or domclassify list
  tclassifyvars <- classifyvars[classifyvars %in% tdomainlst]
  pcclassifyvars <- classifyvars[classifyvars %in% pcdomainlst]


  ###############################################################################
  ## 9. Check pwhereqry and ACI
  ###############################################################################
  pwhereqry = cwhereqry <- NULL
  
  if (!is.null(pcwhereqry)) {
    if (is.null(pcflds)) {
      stop("must include plot and/or cond if including pcwhereqry")
    } else {
      pcwhereqry <- check.logic(pcflds, pcwhereqry)
      pcwhereqry <- RtoSQL(pcwhereqry)
      
      if (!is.null(plotnm)) {
        pwhereqry <- check.logic(pltflds, pcwhereqry, stopifinvalid = FALSE)
        
        ## Add alias to plot filters
        if (!is.null(pwhereqry)) {
          pchkvars <- check.logic.vars(pltflds, pcwhereqry, returnVars = TRUE)
          for (pchkvar in pchkvars) {
            if (!grepl(paste0("pc.", pchkvar), pcwhereqry)) {
              pcwhereqry <- sub(pchkvar, paste0("pc.", pchkvar), pcwhereqry)
            }
          }
          pvars <- c(pvars, pchkvars)
        }
      }

      if (!is.null(condnm)) {
        cwhereqry <- check.logic(condflds, pcwhereqry, stopifinvalid = FALSE)
        
        ## Add alias to cond filters
        if (!is.null(cwhereqry)) {
          cchkvars <- check.logic.vars(condflds, pcwhereqry, returnVars = TRUE)
          for (cchkvar in cchkvars) {
            if (!grepl(paste0("pc.", cchkvar), pcwhereqry)) {
              pcwhereqry <- sub(cchkvar, paste0("pc.", cchkvar), pcwhereqry)
            }
          }
          cvars <- c(cvars, cchkvars)
        }
      }
      
      if (!(startsWith(gsub(" ", "", pcwhereqry), "\nWHERE"))) {
        if (startsWith(gsub(" ", "", pcwhereqry), "WHERE")) {
          pcwhereqry <- paste0("\n ", pcwhereqry)
        } else {
          pcwhereqry <- paste0("\nWHERE ", pcwhereqry)
        }
      }
    }
  }
  
  ## If ACI, include COND_STATUS_CD = 1 to exclude conditions measured on ACI plots
  if (!ACI) {
    if (is.null(condflds)) {
      message("must include cond to exclude ACI plots... assuming data has no ACI plots")
    } else {
      cond_status_cdnm <- findnm("COND_STATUS_CD", condflds, returnNULL = TRUE)
      if (is.null(cond_status_cdnm)) {
        message("must include COND_STATUS_CD in cond to exclude ACI plots... assuming data has no ACI plots")
      }
      if (!is.null(cwhereqry)) {
        if (!(grepl("COND_STATUS_CD", cwhereqry, ignore.case = TRUE) &&
              (grepl("COND_STATUS_CD=1", gsub(" ", "", cwhereqry), ignore.case = TRUE) ||
               grepl("COND_STATUS_CDin(1)", gsub(" ", "", cwhereqry), ignore.case = TRUE)))) {
          pcwhereqry <- paste0(cwhereqry, " AND ", cond_status_cdnm, " = 1")
        }
      } else if (!is.null(pltidsWITHqry)) {
        if (!(grepl("COND_STATUS_CD", pltidsWITHqry, ignore.case = TRUE) &&
              (grepl("COND_STATUS_CD=1", gsub(" ", "", pltidsWITHqry), ignore.case = TRUE) ||
               grepl("COND_STATUS_CDin(1)", gsub(" ", "", pltidsWITHqry), ignore.case = TRUE)))) {
          cwhereqry <- pcwhereqry <- paste0("\n WHERE ", cond_status_cdnm, " = 1")
        }
      } else {
        cwhereqry <- pcwhereqry <- paste0("\n WHERE ", cond_status_cdnm, " = 1")
      }
      cvars <- unique(c(cvars, cond_status_cdnm))
      
      if (!is.null(pcwhereqry) && !grepl(paste0("pc.", cond_status_cdnm), pcwhereqry)) {
        pcwhereqry <- sub(cond_status_cdnm, paste0("pc.", cond_status_cdnm), pcwhereqry)
      }
    }
  }
  

  ###############################################################################
  ## 10. Check if adjustment variable is in tab
  ###############################################################################
  ## if adjtree = TRUE, first check if 'tadjfac' is in tabflds
  ## If adjvar is not in tabflds, check if getadjplot = TRUE or pltidsWITHqry is not NULL
  adjvar <- datSum_opts$adjvar
  if (adjtree) {
    adjvarchk <- findnm(adjvar, tabflds, returnNULL = TRUE)
    
    if (is.null(adjvarchk)) {
      if (is.null(condnm) && is.null(pltidsWITHqry)) {
        if (bysubp) {
          msg <- paste0("must include cond, subplot, subp_cond tables or pltidsWITHqry or ",
                        adjvar, " in tab when adj != 'none'")
        } else {
          msg <- paste0("must include cond or pltidsWITHqry or ",
                        adjvar, " in tab when adj != 'none'")
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
  
  ###############################################################################
  ## 11. Get plot and cond if keepall = TRUE
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
  ## 12. Build WITH query to get cond data (pltcondx)
  #################################################################################
  message("building query for plot/cond data...")
  
  pjoin <- ifelse((!is.null(pdomainlst) || !is.null(pwhereqry)), TRUE, FALSE)
  cjoin <- ifelse((!is.null(cdomainlst) || !is.null(cwhereqry) || getadjplot), TRUE, FALSE)
  
  if (!condinWITHqry && condnm != "pltcondx") {
    
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
  ## 13. Build query for adjustment factors (if getadjplot = TRUE)
  #################################################################################
  pca. <- "pc."
  
  if (adjtree) {
    if (getadjplot) {
      
      message("building query for plot-level adjustments...")
      adjjoinid <- cuniqueid
      
      ## Build WHERE query to filter nonsampled plots
      pcADJwhereqry <- getADJwherePLOT(condflds, conda. = pca.)
      
      
      if (bysubp) {
        adjjoinid <- subplotid
        
        ## Build WHERE query for removing nonsampled subplots
        subpwhereqry <- getADJwhereSUBP(subplotflds, 
                                        adjwhereqry = pcADJwhereqry)
        
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
        if (!is.null(pltidsWITHqry)) {
          
          pltidsWITHqry <- paste0(
            pltidsWITHqry, ", ",
            "\n----- sum sampled subplot proportions",
            "\nsubpcprop AS ",
            "\n(", sumpropqry, "),",
            "\n----- adjustment factors",
            "\npltidsadj AS ",
            "\n(", ADJqrySUBP, ")")
          #message(pltidsWITHqry)
          
        } else {
          
          pltidsWITHqry <- paste0(
            "\n----- sum sampled subplot proportions",
            "\nsubpcprop AS ",
            "\n(", sumpropqry, "),",
            "\n----- adjustment factors",
            "\npltidsadj AS ",
            "\n(", ADJqrySUBP, ")")
        }
        
      } else {   ## bysubp = FALSE
      
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
        if (!is.null(pltidsWITHqry)) {
          
          pltidsWITHqry <- paste0(
            pltidsWITHqry, ", ",
            "\n----- adjustment factors",
            "\npltidsadj AS ",
            "\n(", ADJqry, ")")
          #message(pltidsWITHqry)
        }
      }
    } else { ## END getadjplot
      adjjoinid <- pltidsid
    }
      
  
    ## Build query for select CASE statement to add adjfactors
    ######################################################################################
    if (!is.null(adjvarchk)) {
      tadjcase <- paste0("t.", adjvarchk)
    } else {
      tadjcase <- paste0("adj.", adjvarlst[["P2VEG"]], " AS ", adjvar)
    }
    
    ## Define name - add _ADJ to name if adjusting
    tsumvarnm <- paste0(tsumvar, "_ADJ")
  }  

    
  #################################################################################
  #################################################################################
  ## 14. Build WITH query to get tree data (tdat)
  #################################################################################
  #################################################################################
  message("building query for tree data...")
  
  adjalias. <- "adj."
  twithalias <- "tdat"
    
  
  ## 14.1. FROM statement for tdat WITH query
  ##########################################################################
  if (tabindb) {
    twithfromqry <- paste0("\n FROM ", SCHEMA., tabnm, " t")
  } else {
    twithfromqry <- paste0("\n FROM ", tabnm, " t")
  }
  
  if (adjtree) {
    if (is.null(adjvarchk)) {
      tadjjoinqry <- getjoinqry(adjjoinid, cuniqueid, adjalias., talias.)
      twithfromqry <- paste0(twithfromqry,
                             "\n JOIN pltidsadj adj ", tadjjoinqry)
    }
  } else {
    
    if (!is.null(pltidsWITHqry)) {
      tjoinqry <- getjoinqry(tuniqueid, pltidsid, talias., "pltids.")
      twithfromqry <- paste0(twithfromqry,
                             "\n JOIN pltids ", tjoinqry)
    }
  }
  
  
  ## 14.2. WHERE statement for tdat WITH query - tfilter
  ##########################################################################
  if (!is.null(tfilter)) {
    twhereqry <- paste0("\n WHERE ", RtoSQL(tfilter, x=tabflds))
    
    ## Add alias to tree filters
    if (!is.null(twhereqry)) {
      tchkvars <- check.logic.vars(tabflds, twhereqry, returnVars = TRUE)
      for (tchkvar in tchkvars) {
        twhereqry <- sub(tchkvar, paste0(talias., tchkvar), twhereqry)
      }
    }
  }
 
  
  ## 14.3. SELECT statement for tdat WITH query
  #######################################################################
  
  ## SELECT variables
  twithSelect <- paste0(tsumvar, " AS ", tsumvarnm)

   
  ## Build twithqry
  twithqry <- paste0("SELECT")
  twithvars <- c("CONDID", "SUBP")
  
  ## Build twithSelect
  twithSelect <- toString(paste0(talias., c(unique(c(tsumuniqueid, twithvars)), tdomainlst, tsumvar)))
  

  ## Build final SELECT statement 
  twithqry <- paste(twithqry, toString(twithSelect))

  ## Add adjustment variables
  if (adjtree) {
    twithqry <- paste0(twithqry, ", ", tadjcase)
    
    ## Define name - add _ADJ to name if adjusting
    tumvarnm <- sub("_UNADJ", "_ADJ", tsumvar)
  }
  

  ## WHERE statement
  twithwhereqry <- twhereqry

  ## Build final WITH query
  twithqry <- paste0(twithqry,
                     twithfromqry,
                     twithwhereqry)
  
   

  ## Append tdat WITH query to pltidsWITHqry
  ##########################################################################
  if (!is.null(pltidsWITHqry)) {
    if (!is.null(condnm)) {
      uniqueid <- cuniqueid
    } else {
      uniqueid <- pltidsid
    }
    if (bysubp) {
      uniqueid <- unique(c(uniqueid, subpid))
    }
    if (bycond) {
      uniqueid <- c(uniqueid, condid)
    }
    pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                            "\n----- get data",
                            "\ntdat AS",
                            "\n(", twithqry, ")")
    
  } else {
    uniqueid <- tuniqueid
    if (bysubp) {
      uniqueid <- unique(c(uniqueid, subpid))
    }
    if (bycond) {
      uniqueid <- c(uniqueid, condid)
    }
    
    grpby. <- "tdat."
    pltidsWITHqry <- paste0("WITH tdat AS",
                            "\n(", twithqry, ")")
  }

  
  #################################################################################
  #################################################################################
  ## 15. Build query for summarizing tree data
  #################################################################################
  #################################################################################
  message("building query for summarizing tree data...\n")
  
  
  ## 15.1. FROM statement 
  ####################################################################
  tfromqry <- paste0("\nFROM tdat")
  
  
  ## Add pltcondx if pjoin or cjoin variables 
  if (pjoin || cjoin) {
    tjoinqry <- getjoinqry(c(cuniqueid, condid), c(tuniqueid, condid), "pc.", "tdat.")
    tfromqry <- paste0(tfromqry,
                       "\nJOIN pltcondx pc ", tjoinqry)
  }
  
  

  ## 15.2. SELECT statement for tab query
  ###########################################################################
  if (adjtree) {
    tsumvarnew <- paste0("COALESCE(SUM(tdat.", tsumvarnew, " * tdat.", adjvar, "), 0)")
  } else {
    tsumvarnew <- paste0("COALESCE(SUM(tdat.", tsumvarnew, "), 0)")
  }


  ## Build SELECT statement
  ####################################################################

  #define grpby variables
  tgrpbyvars <- paste0("tdat.", uniqueid)

  ## add grpby variable to SELECT statement
  tselectqry <- paste0("\nSELECT ", toString(tgrpbyvars))

  
  ## Add classifications to SELECT statement
  domclassifyqry <- NULL
  
  if (!is.null(domclassify)) {
    for (i in 1:length(domclassify)) {
      classifyvar <- names(domclassify)[i]
      classifylut <- domclassify[[i]]
      if (is.vector(classifylut)) {
        classifynm <- paste0(classifyvar, "CL")
        if (classifyvar %in% pcclassifyvars) {
          classvar. <- "pc."
        } else {
          classvar. <- "tdat."
        }

        ## Get classification query
        cutbreaks <- domclassify[[classifyvar]]
        fill <- NULL
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
        } else {
          classvar. <- "tdat."
        }

        ## Get classification query
        domclassqry <- classqry(classifyvar, fromval, toval,
                                  classnm = classifynm,
                                  class. = classvar.,
                                  fill = NULL)
      }
      tgrpbyvars <- c(tgrpbyvars, domainlst)
      classifynmlst[[classifyvar]] <- classifynm
      
      ## change names in domain lists to classified variable name
      domainlst <- domainlst[domainlst != classifyvar]
      if (classifyvar %in% pcdomainlst) {
        pcdomainlst <- pcdomainlst[pcdomainlst != classifyvar]
        pcdomainlst <- c(pcdomainlst, classifynm)
      }
      if (classifyvar %in% tdomainlst) {
        tdomainlst <- tdomainlst[tdomainlst != classifyvar]
        tdomainlst <- c(tdomainlst, classifynm)
      }

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
    
    if (length(tdomainlst) > 0) {
      tselectqry <- paste0(tselectqry, ", ", toString(paste0("tdat.", tdomainlst)))
      tgrpbyvars <- unique(c(tgrpbyvars, paste0("tdat.", tdomainlst)))
    }
    if (length(pcdomainlst) > 0) {
      tselectqry <- paste0(tselectqry, ", ", toString(paste0("pc.", pcdomainlst)))
      tgrpbyvars <- unique(c(tgrpbyvars, paste0(pca., pcdomainlst)))
    }
  }

  ## Build select query
  tselectqry <- paste0(tselectqry,
                       ",\n   ", tsumvarnew, " AS ", tsumvarnm)

  
  ## Build query to summarize data, including pcwhereqry
  ################################################################
  tqry <- paste0(tselectqry,
                 tfromqry,
                 pcwhereqry,
                 "\nGROUP BY ", toString(tgrpbyvars))

  ## Build final query to summarize data including WITH queries
  ################################################################
  if (!is.null(pltidsWITHqry)) {
    tab.qry <- paste0(pltidsWITHqry,
                       "\n-------------------------------------------",
                       tqry)
  }

  message("running query...")
  if (datindb) {
    sumdat <- tryCatch(
      DBI::dbGetQuery(dbconn, tab.qry),
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  } else {
    sumdat <- tryCatch(
      sqldf::sqldf(tab.qry),
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  }
  if (is.null(sumdat)) {
    message("query is invalid...")
    message(tab.qry)
    stop()
  } else {
    names(sumdat) <- toupper(names(sumdat))
  }
  
  ## set key
  setkeyv(setDT(sumdat), uniqueid)

  if (tsumunits == "pct" && !bysubp) {
    sumdat[, (tsumvarnm) := .SD * 100, .SDcols=tsumvarnm]
  }
  ## Round digits
  if (!is.null(tround)) {
    sumdat[, (tsumvarnm) := round(.SD, tround), .SDcols=tsumvarnm]
  }

  
  ######################################################################## 
  ## Check tdomvar and tdomvar2
  ######################################################################## 
  
  if (!is.null(tdomvar)) {
    tdomtab <- sumdat
  
    if (!is.null(classifynmlst[[tdomvar]])) {
      tdomvar <- classifynmlst[[tdomvar]]
    }
    if (!is.null(tdomvar2) && !is.null(classifynmlst[[tdomvar2]])) {
      tdomvar2 <- classifynmlst[[tdomvar2]]
    }

    ## Get unique values of tdomvar
    tdoms <- sort(unique(tdomtab[[tdomvar]]))
 
    ## Check tdomvarlst
    nbrtdoms <- length(tdoms)
    if (is.null(tdomvarlst)) {
      ## get tdomvarlst
      tdomvarlst <- tdoms
    } else { 
      if (any(!tdomvarlst %in% tdoms)) {
        tdom.miss <- tdomvarlst[which(!tdomvarlst %in% tdoms)]
        if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
          message("tdomvarlst domain values not in table: ", toString(tdom.miss))
        }
        if (length(tdomvarlst) == 0) {
          stop("")
        }
        if (is.numeric(tdoms)) {
          tdomvarlst <- as.numeric(tdomvarlst) 
        }
      }  
      tdomtab <- tdomtab[tdomtab[[tdomvar]] %in% tdomvarlst,]
    }
    
    ## GETS name for tdomvar
    #####################################################################
    ## If tdomvar2 exists, concatenate the columns to one column (if pivot=TRUE)
    flag <- ifelse(datSum_opts$NAto0, "0", "")
    if (FIAname) {
      # if (tdomvar == "SPCD") {
      #   tdomdata <- datLUTspp(x = tdomtab, spcdname = spcd_name)
      #   ref_spcd <- tdomdata$ref_spcd
      # } else {    
        tdomdata <- datLUTnm(tdomtab, xvar=tdomvar, LUTvar="VALUE", FIAname=TRUE)
      #}
      tdomtab <- tdomdata$xLUT
      tdomvarnm <- tdomdata$xLUTnm
      setkeyv(tdomtab, tsumuniqueid) 
      tdomvarlut <- unique(tdomtab[,c(tdomvar, tdomvarnm), with=FALSE]) 
      
      tdomvarlst2 <- tdomvarlut[match(tdomvarlst, tdomvarlut[[tdomvar]]), 
                                tdomvarnm, with=FALSE][[1]]
      
    } else {
      tdomvarnm <- tdomvar
      #tdomvarlut <- data.frame(tdomvarlst, stringsAsFactors=FALSE)
      #names(tdomvarlut) <- tdomvarnm
      tdomvarlst2 <- as.character(tdomvarlst) 
    }
    sumbyvars <- unique(c(tsumuniqueid, pcdomainlst, tdomvarnm))
    

    ## GET tdomvarlst2 or CHECK IF ALL domains IN tdomvar2lst ARE INCLUDED IN tdomvar2.
    if (!is.null(tdomvar2)) {
      tdoms2 <- sort(unique(tdomtab[[tdomvar2]]))
      
      if (is.null(tdomvar2lst)) {
        ## GET tdomvar2lst
        if (gui) {
          tdomvar2lst <- select.list(as.character(tdoms2), title="Table domains?", multiple=TRUE)
          if (length(tdomvar2lst) == 0) stop("")
          if (is.numeric(tdoms2))  tdomvar2lst <- as.numeric(tdomvar2lst)
        }else{
          tdomvar2lst <- tdoms2
        }
      } else { 
        if (any(!tdomvar2lst %in% unique(tdomtab[[tdomvar2]]))) {
          tdom.miss <- tdomvar2lst[!tdomvar2lst %in% unique(tdomtab[[tdomvar2]])]
          if (!is.null(tdom.miss) || length(tdom.miss) > 0) {
            message("tdomvar2lst domain values not in table: ", toString(tdom.miss))
          }
          if (gui) {
            tdomvar2lst <- select.list(as.character(tdoms2), title="Tab domain(s)", multiple=TRUE)
          }
          if (length(tdomvar2lst) == 0) {
            stop("")
          }
          if (is.numeric(tdoms2))  {
            tdomvar2lst <- as.numeric(tdomvar2lst)
          }
        }
      }
      
      tdomtab <- tdomtab[tdomtab[[tdomvar2]] %in% tdomvar2lst,]
      tdomvar2nm <- tdomvar2
      if (FIAname) {
        # if (tdomvar2 == "SPCD") {
        #   tdomdata <- datLUTspp(x=tdomtab, spcdname=spcd_name)
        #   ref_spcd <- tdomdata$ref_spcd
        # } else {
          tdomdata <- datLUTnm(tdomtab, xvar=tdomvar2, LUTvar="VALUE", FIAname=TRUE)
        # }
        tdomtab <- tdomdata$xLUT
        tdomvar2nm <- tdomdata$xLUTnm
      } 
      
      if (is.numeric(tdomtab[[tdomvar2]])) {
        maxchar2 <- max(sapply(tdomvar2lst, function(x) {nchar(x)}))
      }
      
      if (pivot) {
        concatvar <- paste0(tdomvar, "#", tdomvar2)
        tdomtab[, (concatvar) := paste0(tdomtab[[tdomvarnm]], "#", tdomtab[[tdomvar2]])] 
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

    ## Sum tab by tdomvarnm
    #####################################################################
    tdomtabf <- tdomtab[, lapply(.SD, sum, na.rm=TRUE), by=sumbyvars, .SDcols=tsumvarnm]
    setkeyv(tdomtabf, tsumuniqueid)

    ## Generate tab domain look-up table (tdomvarlut)
    #####################################################################
    nvar <- ifelse(bycond, "NBRCONDS", "NBRPLOTS")
    tsumnm <- tsumvarnm
    
    if (!is.null(concatvar)) {
      tdomvarlut <- tdomtabf[, list(sum(.SD, na.rm=TRUE), .N), by=concatvar, .SDcols = tsumnm]
      names(tdomvarlut) <- c(concatvar, tsumnm, nvar)
      tdomvarlut <- tdomvarlut[, (c(tdomvarnm, tdomvar2nm)) := tstrsplit(get(concatvar), "#", fixed=TRUE)]
      tdomvarlut[[concatvar]] <- NULL
    } else {
      tdomvarlut <- tdomtabf[, list(sum(.SD, na.rm=TRUE), .N), by=tdomvarnm, .SDcols = tsumnm]
      names(tdomvarlut) <- c(tdomvarnm, tsumnm, nvar)
      
      if (!is.null(tdomvar2)) {
        tdomvar2lut <- tdomtabf[, list(sum(.SD, na.rm=TRUE), .N), by=tdomvar2nm, .SDcols = tsumnm]
        names(tdomvar2lut) <- c(tdomvar2nm, tsumnm, nvar)
      }
    }

    
    ######################################################################## 
    ## If pivot=FALSE
    ######################################################################## 
    if (!pivot) {
      tabsum <- tdomtabf
      tdomscolstot <- tsumvarnm
      tdomscols <- sort(unique(tdomtabf[[tdomvarnm]]))
      
    } else {

      ######################################################################## 
      ## If pivot=TRUE, aggregate tab domain data
      ######################################################################## 
      yvar <- ifelse (is.null(tdomvar2), tdomvarnm, concatvar)
      tabsum <- datPivot(tdomtabf, pvar = tsumnm, 
                        xvar = c(tsumuniqueid, pcdomainlst), yvar = yvar,
                        pvar.round = tround, returnDT = TRUE)
      tabsum <- setDT(tabsum)

      ## check if domain in tdomlst.. if not, create column with all 0 values
      tdomscols <- colnames(tabsum)[!colnames(tabsum) %in% sumbyvars]
      if (is.null(concatvar)) {
        UNMATCH <- tdomvarlst2[is.na(match(tdomvarlst2, tdomscols))] 
        if (length(UNMATCH) > 0) {
          tabsum[, (UNMATCH) := 0]
          tdomvarlst2 <- c(tdomvarlst2, UNMATCH)
        }
      } else {
        tdomvarlst2 <- tdomscols
      }

      ## Add total of defined table domains in tdomvarlst 
      if (tdomtot) {
        ## Sum the total tree domains in tdomvarlst after any filtering by plot
        tabsum[, (tdomtotnm) := round(rowSums(.SD, na.rm=TRUE), tround), .SDcols=tdomvarlst2]
        tdomscolstot <- c(tdomvarlst2, tdomtotnm)
      } else {
        tdomscolstot <- tdomvarlst2
      }
    } 

    ## Merge if keepall
    if (keepall) {
      
      NAcols <- c(tdomscols, tdomtotnm)
      if (pltsp) {
        tabsum <- merge(sppltx, tabsum, by=tsumuniqueid, all.x=TRUE)
        if (NAto0) {
          for (col in NAcols) tabsum[is.na(tabsum[[col]]), col] <- 0
        }
      } else {
        ## Check if class of tsumuniqueid matches class of tsumuniqueid
        tabs <- check.matchclass(dat, tabsum, tsumuniqueid, key(tabsum))
        dat <- tabs$tab1
        tabsum <- tabs$tab2
        
        tabsum <- tabsum[dat]
        if (NAto0) {
          tabsum <- DT_NAto0(tabsum, cols=NAcols)
        }
        setcolorder(tabsum, c(names(tabsum)[!names(tabsum) %in% NAcols], NAcols))
      }
    }
  } else { 
    
    ## Join tables
    #############################################################
    if (keepall) {
      if (!bycond && !is.null(pltx)) {
        if (pltsp) {
          tabsum <- merge(sppltx, sumdat, by.x=puniqueid, by.y=uniqueid, all.x=TRUE)
          tsumuniqueid <- puniqueid
          returnDT <- FALSE
          if (NAto0) {
            for (col in tdomscols) tabsum[is.na(tabsum[[col]]), col] <- 0
          }
          
        } else {
          ## set key
          pltx <- setDT(pltx)
          setkeyv(pltx, puniqueid)
          
          tabsum <- sumdat[pltx]
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
        
        tabsum <- sumdat[condx]
        if (NAto0) {
          tabsum <- DT_NAto0(tabsum, cols=tdomscols)
        }
      }
      setcolorder(tabsum, c(names(tabsum)[!names(tabsum) %in% tdomscols], tdomscols))
    } else {
      tabsum <- sumdat
    }
  } 
  

  #### WRITE TO FILE
  #############################################################
  if (savedata) {
    message("saving ", out_layer, "...")

    if (pltsp) {
      spExportSpatial(tabsum,
                      savedata_opts = outlst)
    } else {
      datExportData(tabsum,
                    savedata_opts = outlst)

    }
  }

  if (returnDT) {
    if ("sf" %in% class(tabsum)) {
      tabsum <- setDT(sf::st_drop_geometry(tabsum))
    } else {
      tabsum <- setDT(tabsum)
    }
  }
  returnlst <- list(tsumdat = tabsum,
                    sumvar = tsumvarnm,
                    tsumuniqueid = uniqueid,
                    pltsp = pltsp)
  
  if (!is.null(tfilter)) {
    returnlst$tfilter <- tfilter
  }

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
  returnlst$treeqry <- tab.qry
  
  
  ## Disconnect open connection
  if (dbconnopen && !is.null(dbconn)) {
    DBI::dbDisconnect(dbconn)
  }
  
  
  return(returnlst)

}
