#' Data - Aggregates numeric condition data to plot level.
#' 
#' Aggregates CONDPROP_UNADJ variable or other continuous condition variables
#' to plot level with option to apply condition filters. If condition variable
#' is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ for
#' weighted sum.
#' 
#' If variable = NULL, then it will prompt user for input.
#' 
#' @param cond Data frame or comma-delimited file (*.csv). Condition-level
#' table with aggregate variable and CONDPROP_UNADJ.
#' @param datsource String. Source of data ('obj', 'csv', 'sqlite', 'gdb').
#' @param dbconn Open database connection.
#' @param dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param plt Data frame, comma-delimited file (*.csv), shapefile (*.shp), or
#' database file. Plot-level table to join the aggregated tree data to (if
#' bycond=FALSE). Nonsampled plots (PLOT_STATUS_CD = 3) are removed. Optional.
#' @param subp_cond Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot condition-level table to use to sum condition proportions, 
#' if bysubp=TRUE. 
#' @param subplot Dataframe, comma-delimited file (*.csv), or shapefile (*.shp).
#' Subplot-level table to used to calculate adjustment factors, to remove 
#' nonsampled conditions (SUBP_STATUS_CD = 3). This table is optional.
#' @param bycond Logical. If TRUE, the data are aggregated to the condition
#' level (by: cuniqueid, condid). If FALSE, the data are aggregated to the plot
#' level (by: puniqueid). 
#' @param bysubp Logical. If TRUE, data are aggregated to the subplot level.
#' @param csumvar String. One or more variable names to sum to plot level.
#' @param csumvarnm String. Name of the resulting aggregated plot-level
#' variable(s).  Default = csumvar + '_PLT'.
#' @param cfilter String. A filter to subset the cond data before aggregating
#' (e.g., "COND_STATUS_CD == 1"). Must be R syntax.
#' @param getadjplot Logical. If TRUE, adjustments are calculated for
#' nonsampled conditions on plot.
#' @param adjcond Logical. If TRUE, csumvar condition variables are adjusted
#' for nonsampled conditions by plot.
#' @param pltidsWITHqry SQL query. A query identifying plots to sum (e.g.,
#' 'WITH pltids AS (SELECT cn AS PLT_CN FROM plot WHERE statecd=49 and INVYR=2018)')
#' @param pltidsid Sting. Name of unique identifier in pltidsWITHqry.
#' @param pcwhereqry String. Plot/Condition filter if plot and/or cond table is
#' included. 
#' @param cround Number. The number of digits to round to. If NULL, default=5.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param dbconnopen Logical. If TRUE, keep database connection open.
#' @param tabIDs List of unique IDs corresponding to the tables. See
#' See help(tableIDs) for a list of options.
#' @param datSum_opts List. Options for summarizing tree data, such as TPA,
#' rounding, and adjusting TPA. See help(datSum_options()) for a list of
#' options.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'condsum'.
#' @param database_opts List. Options for database, such as schema and
#' password. See help(database_options()) for a list of options.
#' 
#' @return A list of the following items: \item{condsum}{ Data frame.
#' Plot-level table with aggregated condition attribute. } \item{cfilter}{
#' Condition filter. }
#' 
#' If savedata=TRUE, condsum is saved to the outfolder.
#' @note Nonsampled plots are removed from table.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' # Aggregate LIVE_CANOPY_CVR_PCT to plot, weighted by CONDPROP_UNADJ
#' condsum <- datSumCond(cond = FIESTA::WYcond,
#'                       csumvar = "LIVE_CANOPY_CVR_PCT")$condsum
#' 
#' # Check results
#' condsum[condsum$PLT_CN == 40404737010690,]
#' FIESTA::WYcond[FIESTA::WYcond$PLT_CN == 40404737010690,]
#' 
#' @export datSumCond
datSumCond <- function(cond, 
                       datsource = "obj", 
                       dbconn = NULL,
                       dsn = NULL, 
                       plt = NULL, 
                       subp_cond = NULL,  
                       subplot = NULL, 
                       bycond = FALSE,                        
                       bysubp = FALSE, 
                       csumvar = NULL, 
                       csumvarnm = NULL, 
                       cfilter = NULL, 
                       getadjplot = FALSE,
                       pltidsWITHqry = NULL,
                       pltidsid = NULL,
                       pcwhereqry = NULL,
                       adjcond = FALSE, 
                       cround = 5, 
                       savedata = FALSE, 
                       dbconnopen = TRUE,
                       tabIDs = tableIDs(),
                       datSum_opts = datSum_options(),
                       savedata_opts = NULL,
                       database_opts = NULL){
  
  #####################################################################################
  ## DESCRIPTION: Aggregates CONDPROP_UNADJ variable or other continuous condition 
  ##	variables to plot level with option to apply condition filters. If condition 
  ##	variable is not CONDPROP_UNADJ the variable is multiplied by CONDPROP_UNADJ  
  ##	for weighted sum.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #gui <- ifelse(nargs() == 0, TRUE, FALSE)
  gui <- FALSE

  ## If gui.. set variables to NULL
  if(gui){ puniqueid=cuniqueid=csumvarnm=savedata <- NULL }

  ## Set global variables
  CONDPROP_ADJ=CONDPROP_UNADJ=NF_COND_STATUS_CD=propvars=pltflds <- NULL
  ACI <- FALSE
  checkNA = FALSE
  returnDT = TRUE
  NAto0 <- TRUE
  propvars <- "CONDPROP_UNADJ"
  pltassgnid <- "PLT_CN"
  condid <- "CONDID"
  subpid <- "SUBP"
  

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(datSumCond)) 
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
                         tabIDs = tabIDs))
  savedata_opts <- optslst$savedata_opts
  database_opts <- optslst$database_opts
  tabIDs <- optslst$tabIDs

    
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  noplt <- TRUE
  nocond=pltsp <- FALSE

  
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

  ## Check checkNA
  NAto0 <- pcheck.logical(NAto0, varnm="NAto0", title="Convert NA to 0?", 
		first="YES", gui=gui)
  if (is.null(NAto0)) NAto0 <- FALSE
  
  ## Check getadjplot
  getadjplot <- pcheck.logical(getadjplot, varnm="getadjplot", 
                               title="Get plot adjustment?", first="NO", gui=gui)
  if (getadjplot && is.null(condx)) {
    stop("must include condx to adjust to plot")
  }
  
  ## Check adjcond
  adjcond <- pcheck.logical(adjcond, varnm="adjcond", 
                            title="Adjust conditions?", first="NO", gui=gui)
  if (getadjplot && !adjcond) {
    message("getadjplot=TRUE, and adjcond=FALSE... setting adjcond=TRUE")
    adjcond <- TRUE
  }
  
  ## Check cround
  if (is.null(cround) || !is.numeric(cround) || (cround %% 1 != 0)) {
    warning("cround is invalid.. rounding to 5 digits")
    cround <- 5
  }
  
  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data table?",
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (savedata) {
    outlst <- pcheck.output(savedata_opts = savedata_opts)
    outlst$add_layer <- TRUE
    out_layer <- outlst$out_layer
  }
  
  
  ###############################################################################
  ## 3. Check tables
  ###############################################################################
  plotnm <- condnm <- NULL
  pltindb <- condindb <- FALSE
  
  ## Check plt table
  if (!is.null(plt)) {
    plotlst <- datTabchk(tab = plt, tabtext = "plt", 
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
  csumuniqueid <- cuniqueid
  
  
  ## check if subplot uniqueid is in tab
  if (bysubp) {
    subpidchk <- findnm(subpid, condflds, returnNULL = TRUE)
    if (is.null(subpidchk)) {
      stop(subpid, " not in tab")
    }
    csumuniqueid <- c(csumuniqueid, subpid)
  }
  
  ## check if cond uniqueid is in tab... if it is not, append CONDID = 1
  if (bycond) {
    condidchk <- findnm(condid, condflds, returnNULL = TRUE)
    if (is.null(condidchk)) {
      message(condid, " not in tab... assuming only 1 condition")
      if (is.data.frame(condx)) {
        condx[[condid]] <- 1
      } else {
        stop()
      }
    } else {
      condid <- condidchk
    }
    csumuniqueid <- c(csumuniqueid, condid)
  }
  
  ## make uniqueids uppercase
  tcsumuniqueid <- toupper(csumuniqueid)
  
  
  ###############################################################################
  ### 6. Check csumvar
  ###############################################################################
  condnmlst <- names(condx)
  
  ## Check csumvar
  csumvar <- pcheck.varchar(var2check = csumvar, varnm = "csumvar", 
		                        checklst = condnmlst, 
		                        caption = "csumvar(s)", 
		                        multiple = TRUE,
		                        stopifnull = TRUE, gui = gui)

  ## Check csumvarnm
  if (is.null(csumvarnm)) csumvarnm <- paste(csumvar, "PLT", sep="_")
  condnmlst <- sapply(csumvarnm, checknm, condnmlst)

  
  ###############################################################################
  ## 7. Check pwhereqry and ACI
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
  ## 8. Check if adjustment variable is in tab
  ###############################################################################
  ## if adjtree = TRUE, first check if 'cadjfac' is in condflds
  ## If 'cadjfac' is not in condflds, check if getadjplot = TRUE or pltidsWITHqry is not NULL
  adjvar <- "cadjvar"
  
  adjvarchk <- findnm(adjvar, condflds, returnNULL = TRUE)
  if (is.null(adjvarchk)) {
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
  
  
  ###############################################################################
  ## 9. Get plot and cond if keepall = TRUE
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
  
  
  ## 10. Build query for adjustment factors (if getadjplot = TRUE)
  #################################################################################
  pca. <- "pc."
  
  
  if (getadjplot) {
    
    message("building query for plot-level adjustments...")
    adjjoinid <- cuniqueid
    
    ## Build FROM query including subplot and subp_cond
    pcfromqry <- "\n FROM pltcondx pc"
    
    ## Build WHERE query to filter nonsampled plots
    pcADJwhereqry <- getADJwherePLOT(condflds, conda. = pca.)
    
    
    if (bysubp) {
      adjjoinid <- subplotid
      
      ## Build WHERE query for removing nonsampled subplots
      subpwhereqry <- getADJwhereSUBP(subplotflds, 
                                      adjwhereqry = pcADJwhereqry)
      
      subpa. <- "subp."
      subpca. <- "subpc."
      subpjoinqry <- getjoinqry(subplotid, cuniqueid, subpa., pca.)
      subpfromqry <- paste0(
        pcfromqry,
        "\n JOIN ", SCHEMA., subplotnm, " subp ", subpjoinqry,
        "\n JOIN ", SCHEMA., subp_condnm, " subpc ON (", subpca., subplotid, " = ", pca., cuniqueid,
        " AND ", subpca., condid, " = ", pca., condid,
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
                  adjfromqry = "\n FROM subpcprop",
                  pwhereqry = NULL,
                  pltidsid = subplotid,
                  pltassgnid = c(pltassgnid, subpid),
                  pltidsa. = NULL)
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
      
      ADJqry <-
        getADJqry(popType = "VOL",
                  adj = "plot",
                  propvars = propvars,
                  adjfromqry = pcfromqry,
                  pwhereqry = pcADJwhereqry,
                  pltidsid = cuniqueid,
                  pltassgnid = pltassgnid,
                  pltidsa. = "pc.")
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
  


  ################################################################################  
  ### DO WORK
  ################################################################################  

  if (getadjplot) {

    if (bysubp) {
      ## Remove nonsampled conditions by subplot and summarize to condition-level
      subpcx <- subpsamp(cond = condx, 
                         subp_cond = subpcondx, 
                         subplot = subplotx, 
                         subpuniqueid = subplotid, 
                         subpid = subpid)

      adjfacdata <- getadjfactorPLOT(condx = subpcx, 
		                          cuniqueid = c(subplotid, subpid), 
                                     areawt = csumvar)
      subpcx <- adjfacdata$condx
      mergecols <- unique(c(cuniqueid, condid, names(condx)[!names(condx) %in% names(subpcx)]))
      condx <- merge(condx[, mergecols, with=FALSE], subpcx, 
                     by.x=c(cuniqueid, condid), by.y=c(subplotid, condid))
      

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
      condx <- datFilter(x = condx, 
                         xfilter = cond.nonsamp.filter, 
		              title.filter = "cond.nonsamp.filter")$xf


      adjfacdata <- getadjfactorPLOT(condx = condx, 
                                     cuniqueid = cuniqueid, 
                                     areawt = csumvar)
      condx <- adjfacdata$condx
    }
  }
 
  ### Filter cond data 
  ###########################################################  
  cdat <- datFilter(x = condx, 
                    xfilter = cfilter, 
                    title.filter = "cfilter",
                    stopifnull = TRUE, 
                    gui = gui)
  condf <- cdat$xf
  cfilter <- cdat$xfilter


  if (getadjplot) {
    csumvaradj <- ifelse(csumvar == "CONDPROP_UNADJ", "CONDPROP_ADJ", paste0(csumvar, "_ADJ"))
    csumvarnm <- paste0(csumvarnm, "_ADJ")
    condf.sum <- condf[, lapply(.SD, sum, na.rm=TRUE), by=csumuniqueid, .SDcols=csumvaradj]
  } else {
    condf.sum <- condf[, lapply(.SD, sum, na.rm=TRUE), by=csumuniqueid, .SDcols=csumvar]
  }
  names(condf.sum) <- c(csumuniqueid, csumvarnm)


  ######################################################################## 
  ######################################################################## 
 
  ## Merge to cond or plot
  ###################################
  if (bycond && !nocond) {
    ## Merge to cond
    sumdat <- merge(condx, condf.sum, by=csumuniqueid, all.x=TRUE)
  } else if (!noplt) {
    if (is.data.table(pltx)) {
      setkeyv(condf.sum, cuniqueid)
      setkeyv(pltx, puniqueid)
    }
    sumdat <- merge(pltx, condf.sum, by.x=puniqueid, by.y=cuniqueid, all.x=TRUE)
  } else if (bysubp && !is.null(subplotx)) {
    sumdat <- merge(subplotx, condf.sum, by=csumuniqueid, all.x=TRUE)
  } else {
    sumdat <- condf.sum
  }

  ## Change NA values TO 0
  if (NAto0) {
    for (col in csumvarnm) set(sumdat, which(is.na(sumdat[[col]])), col, 0)
  }


  #### WRITE TO FILE 
  #############################################################
  if (savedata) {
    if (pltsp) {
      spExportSpatial(sumdat, 
                      savedata_opts = outlst)
    } else {
      datExportData(sumdat,
                    savedata_opts = outlst) 
    }
  }  

  ## Round values
  sumdat[,(csumvarnm) := lapply(.SD, round, cround), .SDcols=csumvarnm]

  if (!returnDT) {     
    sumdat <- data.frame(sumdat)
  }
  sumcondlst <- list(condsum=sumdat, csumvarnm=csumvarnm)
  if (!is.null(cfilter)) {
    sumcondlst$cfilter <- cfilter
  }
  
  ## Disconnect open connection
  if (dbconnopen && !is.null(dbconn)) {
    DBI::dbDisconnect(dbconn)
  }
  
  
  return(sumcondlst)
} 
