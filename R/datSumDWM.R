#' Data - Aggregates numeric cover data to the plot or condition-level.
#'
#' Aggregates percent cover data (e.g., COVER_PCT) to plot or condition,
#' including options for filtering.
#'
#' If you want to adjust plot-level or subplot-level information by condition
#' proportions (adjplot), you need to include CONDID & CONDPROP_UNADJ in cond
#' or tree table and COND_STATUS_CD. \cr
#'
#' @param dwm Dataframe or comma-delimited file (*.csv). The table with  
#' down woody material (dwm) data.
#' @param dwmsumvar String. Name of estimation variable in dwm (e.g., CWD_DRYBIO_UNDAJ)
#' @param cond Dataframe or comma-delimited file (*.csv). Condition-level table
#' to join the aggregated tree data to, if bycond=TRUE. This table also may be
#' used for condition proportion or strata variables used if adjcond or
#' adjstrata = TRUE (See details below).  This table is optional.
#' @param plt Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Plot-level table to join the aggregated tree data to, if bycond=FALSE. This
#' table is optional.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param dbconn Open database connection.
#' @param dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). 
#' @param bydomainlst String (vector). Categorical domain variables for
#' summing tree data by (e.g., SPCD). Variables must be in tree table or
#' plt/cond table if tables are provided.
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
#' @param tabIDs List of unique IDs corresponding to the tables. See
#' See help(tableIDs) for a list of options.
#' @param datSum_opts List. Options for summarizing tree data, such as TPA,
#' rounding, and adjusting TPA. See help(datSum_options()) for a list of
#' options.
#' @param database_opts List. Options for database, such as schema and
#' password. See help(database_options()) for a list of options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#'
#' @return A list of the following items: \item{dwmdat}{ Data frame. Plot or
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
#' @export datSumDWM
datSumDWM <- function(dwm,
                      dwmsumvar,
                      plt = NULL,
                      cond = NULL,
                      datsource = "obj",
                      dbconn = NULL,
                      dsn = NULL,
                      bycond = FALSE,
                      bydomainlst = NULL,
                      getadjplot = FALSE,
                      domclassify = NULL,
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
    condx=pltx=sppltx=pcflds=tdomscols=tdomtotnm=classifynmlst=
    pltflds=condflds <- NULL
  
  #ref_estvar <- FIESTAutils::ref_estvar
  dwmwhereqry=dwmfromqry=pcfromqry=pcselectvars=tpavarnm=pcdomainlst=adjvar=
    dwmsumvarnm <- NULL
  
  datindb <- FALSE
  pltassgnid <- "PLT_CN"
  SCHEMA. <- ""
  checkNA = FALSE   
  keepall = FALSE   ## if TRUE, keeps all variables in table, if included as inputs
  returnDT = TRUE
  FIAname <- FALSE

  ## Query alias.
  #dwmalias. <- "dwm."
  
  
  ## For documentation
  # subplot Dataframe or comma-delimited file (*.csv). If getadjplot=TRUE,
  # The subplot-level table with SUBP_STATUS_CD variable for calculating
  # adjustment factors by subplot.
  adjvarlst <- c("ADJ_FACTOR_COND", "ADJ_FACTOR_SUBP",
                  "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR")
  dwmadjvarlst <- c("ADJ_FACTOR_CWD", "ADJ_FACTOR_FWD_SM", "ADJ_FACTOR_FWD_MD", 
                    "ADJ_FACTOR_FWD_LG", "ADJ_FACTOR_DUFF", "ADJ_FACTOR_PILE")
  
  ## Check PROP names and build query for calculating adjustment factors
  propvars <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ", "MACRPROP_UNADJ")
  dwmpropvars <- list(CWD="CONDPROP_CWD", FWD_SM="CONDPROP_FWD_SM", FWD_MD="CONDPROP_FWD_MD",
                      FWD_LG="CONDPROP_FWD_LG", DUFF="CONDPROP_DUFF", PILE="CONDPROP_PILE")
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumDWM)) 
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

  ## Check getadjplot
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot",
                               title="Get plot adjustment?", first="NO", gui=gui)
  
  ## Check adjtree
  adjtree <- pcheck.logical(adjtree, varnm="adjtree", title="Adjust trees",
                            first="NO", gui=gui)
  if (getadjplot) adjtree <- TRUE
  
  ## Check metric
  metric <- pcheck.logical(metric, varnm="metric", title="Metric?",
                           first="NO", gui=gui, stopifnull=TRUE)
  
  ## Check checkNA
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?",
                          first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE
  
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
  dwmnm <- NULL
 
  ## 3.1. Check tab
  ###############################################################################
  dwmlst <- datTabchk(tab = dwm, tabtext = "dwm", 
                      tabid = "PLT_CN",
                      dbconn = dbconn, schema = schema, 
                      dbtablst = dbtablst,
                      bycond = bycond) 
  dwmflds <- dwmlst$tabflds
  dwmuniqueid <- dwmlst$uniqueid
  dwmx <- dwmlst$tabx
  dwmindb <- dwmlst$indb
  if (is.data.frame(dwmx)) {
    dwmnm <- "dwmx"
  } else {
    dwmnm <- dwmlst$tabnm
  }
  if (bycond) {
    condid <- dwmlst$condid
  }

  
  ## 3.2 Check plot, cond
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
  
  ###############################################################################
  ## 4. Check if condition table is in WITH query (e.g., pltcondx)
  ###############################################################################
  if (!dwmindb && !is.null(pltidsWITHqry)) {
    
    chk <- check.logic.vars(dwmnm, pltidsWITHqry)
    #if (!check.logic.vars("pltids", pltidsWITHqry, returnVars=TRUE))
    if (!chk) {
      stop("must include dwm table...")
    }
    
    dwmuniqueid <- "PLT_CN"
    
    dwmflds.qry <- paste0(
      pltidsWITHqry,
      "\nSELECT * FROM ", dwmnm, " LIMIT 0"
    )
    dwmfldsdf <- tryCatch(
      DBI::dbGetQuery(dbconn, dwmflds.qry),
      error = function(e) {
        return(NULL)
      })
    if (is.null(dwmfldsdf)) {
      message("pltidsWITHqry is invalid...")
      message(dwmflds.qry)
      stop()
    } else {
      dwmflds <- toupper(names(dwmfldsdf))
    }
    
  }  
  
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
    pltids <- "pltids"
    
    ## check if condnm in pltidsWITHqry
    if (!is.null(condnm) && !condindb &&
        check.logic.vars(condnm, pltidsWITHqry, ignore.case=TRUE)) {
      condinWITHqry <- TRUE
    
      condflds.qry <- paste0(
        pltidsWITHqry,
        "\nSELECT * FROM ", condnm, " LIMIT 0"
      )
      condfldsdf <- tryCatch(
        DBI::dbGetQuery(dbconn, condflds.qry),
        error = function(e) {
          return(NULL)
        })
      if (is.null(condfldsdf)) {
        message("pltidsWITHqry is invalid...")
        message(condflds.qry)
        stop()
      } else {
        condflds <- toupper(names(condfldsdf))
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
  dwmsumuniqueid <- dwmuniqueid
  
  ## check if cond uniqueid is in tab... if it is not, append CONDID = 1
  if (bycond) {
    condidchk <- findnm(condid, dwmflds, returnNULL = TRUE)
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
    dwmsumuniqueid <- c(dwmsumuniqueid, condid)
  }
  
  ## make uniqueids uppercase
  dwmsumuniqueid <- toupper(dwmsumuniqueid)
  
  
  ###############################################################################
  ### 6. Check dwmsumvar
  ###############################################################################
  dwmsumvar <- findnm(dwmsumvar, dwmflds, returnNULL = TRUE)
  
  vars2convert <- c("DRYBIO", "CARBON")
  if (any(sapply(vars2convert, function(x) grepl(x, dwmsumvar)))) {
    
    if (metric) {
      dwmsumvarnew <- paste0("COALESCE(dwmdat.", dwmsumvar, ", 0) / 2000 * 0.90718474")
      estunits <- "metric tons"
    } else {   
      dwmsumvarnew <- paste0("COALESCE(dwmdat.", dwmsumvar, ", 0) / 2000")
      estunits <- "tons"
    }
  } else {
    if (metric) {
      dwmsumvarnew <- paste0("COALESCE(dwmdat.", dwmsumvar, ", 0)")
      estunits <- "cubic meters"
    } else {
      dwmsumvarnew <- paste0("COALESCE(dwmdat.", dwmsumvar, ", 0) * 0.02831685")
      estunits = "cubic feet"
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
  pdomainlst <- cdomainlst <- NULL
  domainlst <- bydomainlst
  
  ## check if all variables in bydomainlst are in the input tables
  if (!all(bydomainlst %in% c(dwmflds, pcflds))) {
    missdomain <- bydomainlst[!bydomainlst %in% c(dwmflds, pcflds)]
    stop("invalid variable in bydomainlst: ", toString(missdomain))
  }
  
  ## Check if domain variables are in tree or seedling table
  if (!is.null(domainlst)) {
    if (any(bydomainlst %in% dwmflds)) {
      dwmdomain <- bydomainlst[bydomainlst %in% dwmflds]
      pcdomainlst <- bydomainlst[!bydomainlst %in% dwmdomain]
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
  ## if adjtree = TRUE, first check if 'tadjfac' is in dwmflds
  ## If adjvar is not in dwmflds, check if getadjplot = TRUE or pltidsWITHqry is not NULL
  if (adjtree) {
    adjvarchk <- findnm(adjvar, dwmflds, returnNULL = TRUE)
    
    if (is.null(adjvarchk)) {
      if (is.null(condnm) && is.null(pltidsWITHqry)) {
        msg <- paste0("must include cond or pltidsWITHqry or ",
                      adjvar, " in tab when adj != 'none'")
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
  if (!is.null(plotnm) && pltindb && keepall && !bycond) {
    
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
      
      
      ## Build FROM query
      conda. <- "c."
      pcfromqry <- paste0("\n FROM ", SCHEMA., condnm, " c")
      
      ADJqry <-
        getADJqry(popType = "VOL",
                  adj = "plot",
                  propvars = c(propvars, dwmpropvars),
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
      
      
    } else { ## END getadjplot
      #adjjoinid <- cuniqueid
      adjjoinid <- pltidsid
    }
    
    ## Build query for select CASE statement to add adjfactors
    ######################################################################################
    if (!is.null(adjvarchk)) {
      dwmadjcase <- paste0("dwm.", adjvarchk)
    } else {
      adjvarchk2 <- findnm(adjvar, c(adjvarlst, dwmadjvarlst), returnNULL = TRUE)
      if (!is.null(adjvarchk2)) {
        dwmadjcase <- paste0("adj.", adjvarchk2)
      } else {
        stop("invalid adjvar: ", adjvar)
      }
    }
    
    ## Define name - add _ADJ to name if adjusting
    dwmsumvarnm <- sub("_UNADJ", "_ADJ", dwmsumvar)
    
  }  
  
  
  #################################################################################
  #################################################################################
  ## 14. Build WITH query to get tree data (tdat)
  #################################################################################
  #################################################################################
  message("building query for tree data...")
  
  adjalias. <- "adj."
  dwmwithalias <- "dwmdat"
  dwmalias. <- "dwm."
  
  
  ## 14.1. FROM statement for tdat WITH query
  ##########################################################################
  if (dwmindb) {
    dwmwithfromqry <- paste0("\n FROM ", SCHEMA., dwmnm, " dwm")
  } else {
    dwmwithfromqry <- paste0("\n FROM ", dwmnm, " dwm")
  }
  
  if (adjtree) {
    if (is.null(adjvarchk)) {
      dwmadjjoinqry <- getjoinqry(adjjoinid, dwmuniqueid, adjalias., dwmalias.)
      dwmwithfromqry <- paste0(dwmwithfromqry,
                               "\n JOIN pltidsadj adj ", dwmadjjoinqry)
    }
  } else {
    
    if (!is.null(pltidsWITHqry)) {
      dwmjoinqry <- getjoinqry(dwmuniqueid, pltidsid, dwmalias., "pltids.")
      dwmwithfromqry <- paste0(dwmwithfromqry,
                               "\n JOIN pltids ", dwmjoinqry)
    }
  }
  
  
  ## 14.2. SELECT statement for tdat WITH query
  #######################################################################
  
  ## SELECT variables
  dwmwithSelect <- paste0(dwmsumvar, " AS ", dwmsumvarnm)
  
  ## Build dwmwithqry
  dwmwithqry <- paste0("SELECT")
  dwmwithvars <- c("CONDID")
  
  ## Build dwmwithSelect
  dwmwithSelect <- toString(paste0(dwmalias., c(dwmsumuniqueid, dwmsumvar)))
  
 
  ## Build final SELECT statement 
  dwmwithqry <- paste(dwmwithqry, toString(dwmwithSelect))

  ## Add adjustment variables
  if (adjtree) {
    dwmwithqry <- paste0(dwmwithqry, ", ", dwmadjcase)
    
    ## Define name - add _ADJ to name if adjusting
    dwmsumvarnm <- sub("_UNADJ", "_ADJ", dwmsumvar)
  }
  
 

  ## WHERE statement
  dwmwithwhereqry <- dwmwhereqry
  
  ## Build final WITH query
  dwmwithqry <- paste0(dwmwithqry,
                       dwmwithfromqry,
                       dwmwithwhereqry)
  
  
  ## Append dwmdat WITH query to pltidsWITHqry
  ##########################################################################
  if (!is.null(pltidsWITHqry)) {
    if (!is.null(condnm)) {
      uniqueid <- cuniqueid
    } else {
      uniqueid <- pltidsid
    }
    if (bycond) {
      uniqueid <- c(uniqueid, condid)
    }
    pltidsWITHqry <- paste0(pltidsWITHqry, ", ",
                            "\n----- get data",
                            "\ndwmdat AS",
                            "\n(", dwmwithqry, ")")
  } else {
    uniqueid <- dwmuniqueid
    if (bycond) {
      uniqueid <- c(uniqueid, condid)
    }
    
    pltidsWITHqry <- paste0("WITH dwmdat AS",
                            "\n(", dwmwithqry, ")")
  }
  uniqueid <- toupper(uniqueid)
  
  
  #################################################################################
  #################################################################################
  ## 15. Build query for summarizing tree data
  #################################################################################
  #################################################################################
  message("building query for summarizing tree data...\n")
  
  
  ## 15.1. FROM statement 
  ####################################################################
  dwmfromqry <- paste0("\nFROM dwmdat")
  
  
  ## Add pltcondx if pjoin or cjoin variables 
  if (pjoin || cjoin) {
    dwmjoinqry <- getjoinqry(c(cuniqueid, condid), c(dwmuniqueid, condid), "pc.", "dwmdat.")
    dwmfromqry <- paste0(dwmfromqry,
                       "\nJOIN pltcondx pc ", dwmjoinqry)
  }
  
  
  ## 15.2. SELECT statement for tab query
  ###########################################################################
  if (adjtree) {
    dwmsumvarnew <- paste0("SUM(", dwmsumvarnew, " * dwmdat.", adjvar, ")")
  } else {
    dwmsumvarnew <- paste0("SUM(", dwmsumvarnew, ")")
  }

  
  # ## Build FROM statement
  # ################################################################
  # 
  # ## use LEFT JOIN for dwmdat to get all records, no data filled with 0
  # dwmjointype <- ifelse((is.null(pcdomainlst) || length(pcdomainlst) == 0), "JOIN", "LEFT JOIN")
  # 
  # if (!is.null(condnm)) {
  #   conda. <- "pc."
  #   dwmfromqry <- paste0("\nFROM ", condnm, " pc")
  #   dwmjoinid <- getjoinqry(c(dwmuniqueid, condid), c(cuniqueid, condid), "dwmdat.", conda.)
  #   dwmfromqry <- paste0(dwmfromqry,
  #                      "\n", dwmjointype, " dwmdat ", dwmjoinid)
  #   
  # } else if (!is.null(pltidsnm)) {
  #   dwmfromqry <- paste0("\nFROM ", pltidsnm, " pltids")
  #   dwmjoinid <- getjoinqry(dwmuniqueid, pltidsid, "dwmdat.", pltidsa.)
  #   dwmfromqry <- paste0(dwmfromqry,
  #                      "\n", dwmjointype, " dwmdat ", dwmjoinid)
  #   
  # } else if (!is.null(plotnm)) {
  #   dwmfromqry <- paste0("\nFROM ", plotnm)
  #   dwmjoinid <- getjoinqry(dwmuniqueid, pltidsid, "dwmdat.", pltidsa.)
  #   dwmfromqry <- paste0(dwmfromqry,
  #                      "\n", dwmjointype, " dwmdat ", dwmjoinid)
  #   
  # } else {
  #   dwmfromqry <- paste0("\nFROM ", dwmwithalias)
  # }
  
  ## Build SELECT statement
  ####################################################################
  
  #define grpby variables
  dwmgrpbyvars <- paste0("dwmdat.", uniqueid)
  #tgrpbyvars <- paste0("dwmdat.", dwmsumuniqueid)
  
  ## add grpby variable to select qry query
  dwmselectqry <- paste0("\nSELECT ", toString(dwmgrpbyvars))
  

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
        } else {
          classvar. <- "dwmdat."
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
          classvar. <- "dwmdat."
        }
        
        ## Get classification query
        domclassqry <- classqry(classifyvar, fromval, toval,
                                classnm = classifynm,
                                class. = classvar.,
                                fill = NULL)
      }
      dwmgrpbyvars <- c(dwmgrpbyvars, domainlst)
      classifynmlst[[classifyvar]] <- classifynm
      
      ## change names in domain lists to classified variable name
      domainlst <- domainlst[domainlst != classifyvar]
      if (classifyvar %in% pcdomainlst) {
        pcdomainlst <- pcdomainlst[pcdomainlst != classifyvar]
        pcdomainlst <- c(pcdomainlst, classifynm)
      }

      domclassifyqry <- paste0(domclassifyqry, "\n", domclassqry)
      if (length(domclassify) > 1 && i < length(domclassify)) {
        domclassifyqry <- paste0(domclassifyqry, ",")
      }
    }
    if (length(domainlst) > 0) {
      dwmselectqry <- paste0(dwmselectqry, ", ", toString(domainlst), ", ", domclassifyqry)
    } else {
      dwmselectqry <- paste0(dwmselectqry, ", ", domclassifyqry)
    }
  } else if (!is.null(domainlst) && length(domainlst) > 0) {
    if (length(pcdomainlst) > 0) {
      dwmselectqry <- paste0(dwmselectqry, ", ", toString(paste0("pc.", pcdomainlst)))
      dwmgrpbyvars <- unique(c(dwmgrpbyvars, pcdomainlst))
    }
  }
  
  ## Build select query
  dwmselectqry <- paste0(dwmselectqry,
                       ",\n   ", dwmsumvarnew, " AS ", dwmsumvarnm)

  
  ## Build query to summarize data, including pcwhereqry
  ################################################################
  dwmqry <- paste0(dwmselectqry,
                   dwmfromqry,
                   pcwhereqry,
                   "\nGROUP BY ", toString(dwmgrpbyvars))
  
  ## Build final query to summarize data including WITH queries
  ################################################################
  if (!is.null(pltidsWITHqry)) {
    dwm.qry <- paste0(pltidsWITHqry,
                      "\n-------------------------------------------",
                      dwmqry)
  }

  if (datindb) {
    sumdat <- tryCatch(
      DBI::dbGetQuery(dbconn, dwm.qry),
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  } else {
    sumdat <- tryCatch(
      sqldf::sqldf(dwm.qry),
      error = function(e) {
        message(e, "\n")
        return(NULL)
      }
    )
  }
  if (is.null(sumdat)) {
    message("query is invalid...")
    message(dwm.qry)
    stop()
  } else {
    names(sumdat) <- toupper(names(sumdat))
  }
  
  ## set key
  setkeyv(setDT(sumdat), uniqueid)
  
  ## Round digits
  if (!is.null(tround)) {
    sumdat[, (dwmsumvarnm) := round(.SD, tround), .SDcols=dwmsumvarnm]
  }
  
  
  ## Join tables
  #############################################################
  if (keepall) {
    if (!bycond && !is.null(pltx)) {
      if (pltsp) {
        tabsum <- merge(sppltx, sumdat, by.x=puniqueid, by.y=uniqueid, all.x=TRUE)
        dwmsumuniqueid <- puniqueid
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
      
      ## Check if class of dwmsumuniqueid matches class of dwmsumuniqueid
      tabs <- check.matchclass(sumdat, condx, dwmsumuniqueid)
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
  returnlst <- list(sumdat = tabsum,
                    sumvar = dwmsumvarnm,
                    uniqueid = uniqueid,
                    pltsp = pltsp,
                    estunits = estunits)
  
  # if (!is.null(dwmfilter)) {
  #   returnlst$dwmfilter <- dwmfilter
  # }
  
  if (!is.null(pcdomainlst)) {
    returnlst$pcdomainlst <- pcdomainlst
  }
  
  if (!is.null(domclassify)) {
    returnlst$classifynmlst <- classifynmlst
  }
  if (!is.null(tround)) {
    returnlst$tround <- tround
  }
  returnlst$dwmqry <- dwm.qry
  
  return(returnlst)
  
}
