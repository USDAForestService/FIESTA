#' Database - Extracts actual plot coordinates from the FIA Oracle database.
#' 
#' Extracts actual plot coodinates for the most current measurement of each
#' plot in the database.
#' 
#' 
#' @param states String or numeric vector. Name (e.g., 'Arizona','New Mexico')
#' or code (e.g., 4, 35) of state(s) for evalid. If all states in one or more
#' FIA Research Station is desired, set states=NULL and use RS argument to
#' define RS.
#' @param RS String vector. Name of research station(s) to get public XY
#' coordinates for ('RMRS','SRS','NCRS','NERS','PNWRS'). Do not use if states 
#' is populated. See FIESTA::ref_statecd for reference to RS and states.
#' @param datsource Source of data ('datamart', 'sqlite').
#' @param dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.sqlite).
#' @param xy sf R object or String. Table with xy coordinates. Can be a spatial
#' polygon object, data frame, full pathname to a shapefile, or name of a layer
#' within a database.
#' @param xy_opts List of xy data options to specify if xy is NOT NULL. 
#' See xy_options (e.g., xy_opts = list(xvar='LON', yvar='LAT').
#' @param dbTabs List of database tables the user would like returned.
#'  See help(dbTables) for a list of options.
#' @param eval String. Type of evaluation time frame for data extraction 
#' ('FIA', 'custom'). See eval_opts for more further options. 
#' @param eval_opts List of evaluation options for 'FIA' or 'custom'
#' evaluations to determine the set of data returned. See help(eval_options)
#' for a list of options.
#' @param pjoinid String. Variable in plt to join to XY data. Not necessary to
#' be unique. If using most current XY coordinates, use identifier for a plot
#' (e.g., PLOT_ID).
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL').  Only one inventory type (PERIODIC/ANNUAL) at a time.
#' @param intensity1 Logical. If TRUE, includes only XY coordinates where 
#' INTENSITY = 1 (FIA base grid).
#' @param issp Logical. If TRUE, returns spatial XY data as a list object with
#' query.
#' @param addINTENSITY Logical. If TRUE, appends INTENSITY variable to dataframe.
#' @param addINVYR Logical. If TRUE, appends INVYR variable to dataframe.
#' @param addMEASYEAR Logical. If TRUE, appends INTENSITY variable to dataframe.
#' @param returndata Logical. If TRUE, returns XY data as a list object with
#' query.
#' @param savedata Logical. If TRUE, saves XY data to outfolder as comma-delimited
#' file (*.csv).
#' @param exportsp Logical. If TRUE, exports data as spatial. 
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed. 
#'
#' @return if returndata=TRUE, a list of the following objects: 
#' \item{xy*_PUBLIC}{ Data frame. XY data from FIA's public database. If 
#' measCur=TRUE, named xyCur_PUBLIC, else named xy_PUBLIC. The data frame 
#' has 10 columns ('PLT_CN', 'LON_PUBLIC', 'LAT_PUBLIC', 'STATECD', 'UNITCD',
#' 'COUNTYCD', 'PLOT', 'INTENSITY', 'PLOT_ID' (ID+STATECD+UNTCD+COUNTYCD+PLOT), 
#' 'COUNTYFIPS'. If issp=TRUE, returns an sf object. }
#' \item{xyqry}{ String. Query to extract coordinates. }
#' \item{xvar}{ String. Name of X variable in xy*_PUBLIC. }
#' \item{yvar}{ String. Name of Y variable in xy*_PUBLIC. }
#' 
#' If savedata=TRUE, outputs the xy*_PUBLIC as out_fmt to outfolder. 
#' If exportsp=TRUE, the output xy data are saved as spatial layer.
#' @note
#' 
#' If no parameters are included, the user is prompted for input. If partial
#' parameters, the default parameter values are used for those not specified.
#' 
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \dontrun{
#' # Most current evaluation and shapefile with public coordinates
#' COxylst <- DBgetXY(states = "Colorado",
#'                    eval_opts=eval_options(measCur = TRUE))
#' names(COxylst)
#' 
#' head(COxylst$xyCur_ACTUAL)
#' COxylst$xyqry
#' }
#' @export DBgetXY
DBgetXY <- function (states = NULL, 
                     RS = NULL, 
                     datsource = "datamart", 
                     dsn = NULL, 
                     xy = "PLOT", 
                     xy_opts = xy_options(xy.uniqueid="CN", 
 	                               xvar="LON", yvar="LAT"),
                     dbTabs = dbTables(),
                     eval = "FIA",
                     eval_opts = eval_options(),
                     pjoinid = "CN", 
                     invtype = "ANNUAL", 
                     intensity1 = FALSE, 
                     issp = FALSE, 
                     addINTENSITY = TRUE,
                     addINVYR = TRUE,
                     addMEASYEAR = FALSE,
                     returndata = TRUE, 
                     savedata = FALSE, 
                     exportsp = FALSE,
                     savedata_opts = NULL,
                     POP_PLOT_STRATUM_ASSGN = NULL,
                     dbconn = NULL, 
                     dbconnopen = FALSE
                     ) {

  ## DESCRIPTION: Get the most current coordinates in the FIA database
  
  gui <- FALSE
  if (gui) {
    intensity1=savedata=parameters=out_fmt=overwrite <- NULL
  }
    
  
  ## Set global variables
  parameters <- FALSE
  coords <- "PUBLIC"
  SCHEMA.=invyrtab=evalEndyr=plotnm=ppsanm <- NULL
  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check arguments
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetXY)))) {
    miss <- input.params[!input.params %in% formals(DBgetXY)]
    stop("invalid parameter: ", toString(miss))
  } 
 
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts, eval_opts=eval_opts,
				xy_opts=xy_opts)


  ## Set eval_options defaults
  eval_defaults_list <- formals(eval_options)[-length(formals(eval_options))] 
  for (i in 1:length(eval_defaults_list)) {
    assign(names(eval_defaults_list)[[i]], eval_defaults_list[[i]])
  } 
  ## Set user-supplied eval_opts values
  if (length(eval_opts) > 0) {
    for (i in 1:length(eval_opts)) {
      if (names(eval_opts)[[i]] %in% names(eval_defaults_list)) {
        assign(names(eval_opts)[[i]], eval_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(eval_opts)[[i]]))
      }
    }
  } else {
    stop("must specify an evaluation timeframe for data extraction... \n", 
	"...see eval_opts parameter, (e.g., eval_opts=list(Cur=TRUE))")
  }

  ## Set xy_options defaults
  xy_defaults_list <- formals(xy_options)[-length(formals(xy_options))]
  for (i in 1:length(xy_defaults_list)) {
    assign(names(xy_defaults_list)[[i]], xy_defaults_list[[i]])
  }
  ## Set user-supplied xy_opts values
  if (length(xy_opts) > 0) {
    for (i in 1:length(xy_opts)) {
      if (names(xy_opts)[[i]] %in% names(xy_defaults_list)) {
        assign(names(xy_opts)[[i]], xy_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(xy_opts)[[i]]))
      }
    }
  } 

  ## Set dbTables defaults
  dbTables_defaults_list <- formals(dbTables)[-length(formals(dbTables))]
  for (i in 1:length(dbTables_defaults_list)) {
    assign(names(dbTables_defaults_list)[[i]], dbTables_defaults_list[[i]])
  }
  ## Set user-supplied dbTables values
  if (length(dbTabs) > 0) {
    for (i in 1:length(dbTabs)) {
      if (names(dbTabs)[[i]] %in% names(dbTables_defaults_list)) {
        assign(names(dbTabs)[[i]], dbTabs[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(dbTabs)[[i]]))
      }
    }
  }

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
 
  ########################################################################
  ### GET PARAMETER INPUTS 
  ########################################################################
  iseval <- FALSE

  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- pcheck.states(states, statereturn="ABBR")
  stcdlst <- pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)

  ## Check database connection
  ########################################################
  if (!is.null(dbconn) && DBI::dbIsValid(dbconn)) {
    datsource == "sqlite"
    dbtablst <- DBI::dbListTables(dbconn)
    if (length(dbtablst) == 0) {
      stop("no data in database")
    }
  } else {
    datsourcelst <- c("sqlite", "datamart", "csv", "obj")
    datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		gui=gui, checklst=datsourcelst, caption="Data source?")

    if (datsource == "sqlite" && !is.null(dsn)) {
      dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
      if (length(dbtablst) == 0) {
        stop("no data in database")
      }
    }
  }

  ## Check eval
  ###########################################################
  evallst <- c('FIA', 'custom')
  eval <- pcheck.varchar(eval, varnm="eval", checklst=evallst, 
		caption="Evaluation Type", gui=gui)

  ## Check invtype
  ###########################################################
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  ## Check intensity1
  ###########################################################
  intensity1 <- pcheck.logical(intensity1, varnm="intensity1", 
		title="Single intensity?", first="YES", gui=gui)


  ## Get DBgetEvalid parameters from eval_opts
  ################################################
  if (eval == "FIA") {
    evalCur <- ifelse (Cur, TRUE, FALSE) 
    evalAll <- ifelse (All, TRUE, FALSE) 
    evalEndyr <- Endyr
    measCur=allyrs <- FALSE
    measEndyr <- NULL
  } else {
    measCur <- ifelse (Cur, TRUE, FALSE) 
    allyrs <- ifelse (All, TRUE, FALSE) 
    if (length(Endyr) > 1) {
      stop("only one Endyr allowed for custom estimations")
    }
    measEndyr <- Endyr
    evalCur=evalAll <- FALSE
    evalEndyr <- NULL
  }
 
  ## Get states, Evalid and/or invyrs info
  ##########################################################
  evalInfo <- tryCatch( DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource,
                          data_dsn = dsn,
                          dbconn = dbconn,
                          dbconnopen = TRUE,
                          invtype = invtype, 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll,
                          evalType = evalType,
                          dbTabs = dbTabs,
                          gui = gui),
			error = function(e) {
                  message(e)
                  return(NULL) })
  if (is.null(evalInfo)) {
    iseval <- FALSE
  } else {
    if (is.null(evalInfo)) stop("no data to return")
    states <- evalInfo$states
    rslst <- evalInfo$rslst
    evalidlist <- evalInfo$evalidlist
    invtype <- evalInfo$invtype
    invyrtab <- evalInfo$invyrtab
    if (length(evalidlist) > 0) {
      invyrs <- evalInfo$invyrs
      iseval <- TRUE
    }
    SURVEY <- evalInfo$SURVEY
    POP_PLOT_STRATUM_ASSGN <- evalInfo$POP_PLOT_STRATUM_ASSGN
    dbconn <- evalInfo$dbconn
  }
 
  ## Check plot_layer
  #####################################################
  PLOT <- pcheck.table(plot_layer, tab_dsn=dsn)

  if (datsource == "datamart") {
    PLOT <- tryCatch( DBgetCSV(plot_layer, stabbrlst, 
                      returnDT=TRUE, stopifnull=FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
    if (!is.null(PLOT)) {
      plotnm <- "PLOT"
      pltflds <- names(PLOT)
    }
  } else if (datsource == "sqlite") {
    plotnm <- chkdbtab(dbtablst, plot_layer, stopifnull=FALSE)
    if (!is.null(plotnm)) {
      pltflds <- DBI::dbListFields(dbconn, plotnm)
    }
  } else {
    if (!is.null(PLOT)) {
      plotnm <- "PLOT"
      pltflds <- names(PLOT)
    }
  }
  if (datsource != "sqlite") {
    if (!is.null(PLOT) && nrow(PLOT) > 0) {
      plotnm <- "PLOT"
      pltflds <- names(PLOT)
      pjoinid <- findnm(pjoinid, pltflds, returnNULL=TRUE)
      if (is.null(pjoinid)) {
        if (xy.uniqueid %in% pltflds) {
          pjoinid <- xy.uniqueid
        }
      }
    } else {
      plotnm <- NULL
    }
  }
 
  ## Check xy
  #####################################################
   xyisplot <- ifelse (identical(xy, plot_layer), TRUE, FALSE)

  if (xyisplot) {
    xyflds <- pltflds
    xynm <- plotnm
  } else {
    if (datsource == "datamart") {
      XY <- tryCatch( DBgetCSV(xy, stabbrlst, 
                      returnDT=TRUE, stopifnull=FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
      if (!is.null(XY)) {
        xynm <- "XY"
        xyflds <- names(XY)
      }
    } else if (datsource == "sqlite") {
      xynm <- chkdbtab(dbtablst, xy, stopifnull=FALSE)
      if (!is.null(xynm)) {
        xyflds <- DBI::dbListFields(dbconn, xynm)
      }
    } else {
      if (!is.null(XY)) {
        xynm <- "XY"
        name(XY) <- toupper(names(XY))
        xyflds <- names(XY)
      }
    }
  }
  xy.uniqueid <- findnm(xy.uniqueid, xyflds, returnNULL=FALSE)
  if (is.null(xyjoinid)) {
    xyjoinid <- xy.uniqueid
  } else {
    xyjoinid <- findnm(xyjoinid, xyflds, returnNULL=FALSE)
  }
 
  ## If using EVALID, you don't need to get INVYRS, intensity
  if (!iseval) { 

    ## Check custom Evaluation data
    ##########################################################
    evalchk <- customEvalchk(states = states, 
                    measCur = measCur, 
                    measEndyr = measEndyr, 
                    allyrs = allyrs, 
                    invyrs = invyrs, 
                    measyrs = measyrs,
                    invyrtab = invyrtab)

    if (is.null(evalchk)) {
      stop("must specify an evaluation timeframe for data extraction... \n", 
		"...see eval_opts parameter, (e.g., eval_opts=eval_options(evalCur=TRUE))")
    }
    measCur <- evalchk$measCur
    measEndyr <- evalchk$measEndyr
    allyrs <- evalchk$allyrs
    invyrs <- evalchk$invyrs
    measyrs <- evalchk$measyrs
  } else {
    allyrs <- FALSE
  }


  ## Check savedata
  ###########################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check outfolder, outfn.date, overwrite_dsn
  ###########################################################
  if (savedata) {
    outlst <- pcheck.output(outfolder = outfolder, 
                            out_dsn = out_dsn,
                            out_fmt = out_fmt,
                            outfn.pre = outfn.pre, 
                            outfn.date = outfn.date, 
                            overwrite_dsn = overwrite_dsn, 
                            overwrite_layer = overwrite_layer,
                            add_layer = add_layer, 
                            append_layer = append_layer, 
                            gui = gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre

    if (is.null(out_layer)) {
      out_layer <- "spxy"
    }
  } 

  ###########################################################################
  ## Build filter
  ###########################################################################
  ## Create filter for state
  stcds <- pcheck.states(states, "VALUE")
  stFilter <- paste0("p.STATECD IN(", toString(stcds), ")")
  evalFilter=xyfromqry <- NULL
  stabbr <- pcheck.states(states, "ABBR")

  ## PLOT from/join query
  if (iseval) {
    ## Check if evalid in plot table
    evalidnm <- findnm("EVALID", pltflds, returnNULL=TRUE)
    evalid <- unlist(evalidlist) 

    if (is.null(POP_PLOT_STRATUM_ASSGN) && is.null(evalidnm)) {
      ## Check ppsa_layer
      if (datsource == "sqlite") {
        ppsanm <- chkdbtab(dbtablst, ppsa_layer)
        ppsaflds <- DBI::dbListFields(dbconn, ppsanm)
        evalidnm <- findnm("EVALID", ppsaflds) 

        ## Check evalids 
        evalid.qry <- paste("select distinct evalid from ", ppsanm) 
        if (datsource == "sqlite") {
          evalidindb <- DBI::dbGetQuery(dbconn, evalid.qry)[[1]]
        } else {
          evalidindb <- sqldf::sqldf(evalid.qry)
        }
        if (!all(evalid %in% evalidindb)) {
          missevalid <- sort(!evalid[evalid %in% evalidindb])
          stop(ppsa_layer, " is missing evalids: ", toString(missevalid))
          ppsanm <- NULL
        }

      } else if (datsource == "datamart") {
        POP_PLOT_STRATUM_ASSGN <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", stabbrlst, 
			returnDT=TRUE, stopifnull=FALSE)
        if (!all(evalid %in% unique(POP_PLOT_STRATUM_ASSGN$EVALID))) {
          miss <- sort(evalid[!evalid %in% 
				unique(POP_PLOT_STRATUM_ASSGN$EVALID)])
          stop("evalid not in POP_PLOT_STRATUM_ASSGN: ", toString(sort(evalid)))
        }
        ppsanm <- "POP_PLOT_STRATUM_ASSGN"

      } else {
        POP_PLOT_STRATUM_ASSGN <- pcheck.table(ppsa_layer, stopifnull=TRUE, stopifinvalid=TRUE)
        evalidnm <- findnm("EVALID", names(POP_PLOT_STRATUM_ASSGN))
        if (!all(evalid %in% unique(POP_PLOT_STRATUM_ASSGN[[evalidnm]]))) {
          miss <- sort(evalid[!evalid %in% 
				unique(POP_PLOT_STRATUM_ASSGN[[evalidnm]])])
          stop("evalid not in POP_PLOT_STRATUM_ASSGN: ", toString(sort(evalid)))
        }
      }
    }       

    pfromqry <- paste0(SCHEMA., ppsanm, " ppsa")
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = ppsa.PLT_CN)")
    } else {
      xyfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., xynm, 
			" p ON (p.", xy.uniqueid, " = ppsa.PLT_CN)")
    }
    evalFilter <- paste0("ppsa.EVALID IN(", toString(unlist(evalidlist)), ")")

  } else if (length(unlist(invyrs)) > 1) {
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(SCHEMA., xynm, " xy")
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = xy.", xy.uniqueid, ")")
    } else {
      xyfromqry <- paste0(SCHEMA., xynm, " p")
    }
    evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(unlist(invyrs)), ")")

  } else if (length(unlist(measyrs)) > 1) {
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(SCHEMA., xynm, " xy")
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = xy.", xy.uniqueid, ")")
    } else {
      xyfromqry <- paste0(SCHEMA., xynm, " p")
    }
    evalFilter <- paste0(stFilter, " and p.MEASYEAR IN(", toString(unlist(measyrs)), ")")

  } else {
    if (measCur && !is.null(plotnm)) {
      popSURVEY <- ifelse(is.null(SURVEY), FALSE, TRUE)
      xyfromqry <- getpfromqry(Endyr = measEndyr, 
                               SCHEMA. = SCHEMA., 
                               intensity1 = intensity1, 
                               popSURVEY = popSURVEY, 
                               plotnm = plotnm,
                               surveynm = "SURVEY")
    } else {
      plotfromnm <- ifelse(is.null(plotnm), xynm, plotnm)
      xyfromqry <- paste0(SCHEMA., plotfromnm, " p")
    }
    evalFilter <- stFilter 
  }
  if (intensity1) {
    evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
  } 

  ##################################################################################
  ##################################################################################
  ## Generate queries
  ##################################################################################
  XYvarlst <- c(xy.uniqueid, xvar, yvar)
  pvarlst <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
  if (addINTENSITY) { 
    pvarlst <- c(pvarlst, "INTENSITY")
  }
  if (addINVYR) {
    pvarlst <- c(pvarlst, "INVYR")
  }
  if (addMEASYEAR) {
    pvarlst <- c(pvarlst, "MEASYEAR")
  }
  xyvars <- c(XYvarlst, pvarlst)

  if (is.null(plotnm) || xyisplot) {
    if (!all(xyvars %in% pltflds)) {
      xypmiss <- xyvars[!xyvars %in% pltflds]
      if (length(xypmiss) > 0 && any(xypmiss %in% XYvarlst)) {
        xymiss <- xypmiss[xypmiss %in% XYvarlst]
        stop("xy table is missing essential variables: ", toString(xymiss))
      } else if (length(xypmiss) > 0) {
        message("missing plot variables: ", toString(xypmiss))
        xyvars <- xyvars[xyvars %in% pltflds]
      }
    }
    varsA <- paste0("p.", xyvars)
  } else {
    if (!all(xyvars %in% xyflds)) {
      xypmiss <- xyvars[!xyvars %in% xyflds]
      if (length(xypmiss) > 0 && any(xypmiss %in% XYvarlst)) {
        xymiss <- xypmiss[xypmiss %in% XYvarlst]
        stop("xy table is missing essential variables: ", toString(xymiss))
      } else if (length(xypmiss) > 0) {
        message("missing xy variables: ", toString(xypmiss))
        xyvars <- xyvars[xyvars %in% xyflds]
      }
      varsA <- paste0("xy.", xyvars)
      pltvars <- xyvars
      pmiss <- pltvars[!pltvars %in% pltflds]
      if (length(pmiss) > 0) {
        message("missing plot variables: ", toString(pmiss))
        pltvars <- pltvars[pltvars %in% pltflds]
      }
      if (length(pltvars) > 0) {
        varsA <- toString(c(varsA, paste0("p.", pltvars)))
      }
    } else {
      varsA <- toString(paste0("xy.", xyvars))
    }    
  }

  ## Create xy query
  ###########################################################
  if (iseval) {
    if (evalidnm %in% pltflds) {
      evalidA <- paste0("p.EVALID")
    } else {
      evalidA <- paste0("ppsa.EVALID")
    }      
    varsA <- toString(c(varsA, evalidA))
  }
  xycoords.qry <- paste0("select ", toString(varsA), 
		" from ", xyfromqry,
		" where ", evalFilter)

  ## Create invyrtab query 
  ###########################################################
  if (!iseval) {
    yrvar <- ifelse(!is.null(invyrs), "INVYR", "MEASYEAR")
    yrvarnm <- findnm(yrvar, pltflds, returnNULL=TRUE)
    stcdnm <- findnm("STATECD", pltflds, returnNULL=TRUE)
    if (!is.null(yrvarnm) && !is.null(stcdnm)) {
      invarsA <- toString(paste0("p.", c(stcdnm, yrvarnm)))
      invyrtab.qry <- paste0("SELECT ", invarsA, ", COUNT(*)", 
		           " from ", xyfromqry,
				" GROUP BY statecd, ", yrvar, 
				" ORDER BY statecd, ", yrvar) 
    }
  }

  if (datsource == "sqlite") {
    xyx <- tryCatch( DBI::dbGetQuery(dbconn, xycoords.qry),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
    if (!iseval && is.null(invyrtab)) {
      invyrtab <- tryCatch( DBI::dbGetQuery(dbconn, invyrtab.qry),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) }) 
    }      
  } else {
    xyx <- tryCatch( sqldf::sqldf(xycoords.qry, stringsAsFactors = FALSE), 
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
    if (!iseval && is.null(invyrtab)) {
      invyrtab <- tryCatch( sqldf::sqldf(invyrtab.qry, stringsAsFactors = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) }) 
    }      
  }
  if (is.null(xyx) || nrow(xyx) == 0) {
    message("invalid xy query\n")
    message(xycoords.qry)
    stop()
  }

  xyx <- xyx[!xyx$CN %in% FIESTAutils::kindcd3old$CN, ]
  setnames(xyx, "CN", "PLT_CN")
    
  if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% names(xyx))) {
    xyx[["PLOT_ID"]] <- paste0("ID", 
		formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$UNITCD, width=2, digits=2, flag=0),
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(xyx$PLOT, width=5, digits=5, flag=0)) 
    setnames(xyx, c("LON", "LAT"), c("LON_PUBLIC", "LAT_PUBLIC"), skip_absent=TRUE)
  }
  if (all(c("STATECD", "COUNTYCD") %in% names(xyx))) {
    xyx$COUNTYFIPS <- paste0(formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          		formatC(xyx$COUNTYCD, width=3, digits=3, flag=0))
  }
  if (Cur) {
    xyoutnm <- paste0("xyCur_", coords)
    assign(xyoutnm, xyx)
  } else {
    xyoutnm <- paste0("xy_", coords)
    assign(xyoutnm, xyx)
  } 
  if (is.null(out_layer)) {
    out_layer <- xyoutnm
  }
 
  if (issp) {
    spxyoutnm <- paste0("sp", xyoutnm)
     
    if (all(c("LON_PUBLIC", "LAT_PUBLIC") %in% names(xyx))) {

      ## Generate shapefile
      assign(spxyoutnm, spMakeSpatialPoints(xyplt=xyx, xvar="LON_PUBLIC", 
		        yvar="LAT_PUBLIC", xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE, 
		        exportsp=exportsp, 
		        savedata_opts=list(out_dsn=out_dsn, 
                               out_fmt=outsp_fmt,
                               outfolder=outfolder, out_layer=spxyoutnm, 
		                    outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer, 
		                    append_layer=append_layer, outfn.pre=outfn.pre) ))
    } else { 
      message("need LON_PUBLIC and LAT_PUBLIC variables to generate spatial xy")
    }
  }
     
  ###############################################################################
  ## SAVE data
  ###############################################################################
  if (savedata) {
    index.unique.xyplt <- "PLT_CN"
    
    datExportData(get(xyoutnm),  
          index.unique = index.unique.xyplt,
          savedata_opts = list(outfolder = outfolder, 
                              out_fmt = out_fmt, 
                              out_dsn = out_dsn, 
                              out_layer = out_layer,
                              outfn.pre = outfn.pre, 
                              outfn.date = outfn.date, 
                              overwrite_layer = overwrite_layer,
                              append_layer = append_layer, 
                              add_layer = TRUE))
  }

  ## GENERATE RETURN LIST
  ###########################################################
  if (returndata) {
    returnlst <- list()
    if (issp) {
      returnlst$spxy <- get(spxyoutnm)
    } 
    returnlst[[xyoutnm]] <- get(xyoutnm)
    returnlst[["xyqry"]] <- xycoords.qry
    returnlst$xvar <- "LON_PUBLIC"
    returnlst$yvar <- "LAT_PUBLIC"
    returnlst$xy.uniqueid <- xy.uniqueid
    returnlst$pjoinid <- pjoinid
    
    if (dbconnopen) {
      returnlst$dbconn <- dbconn
    }

    ## Return data list
    return(returnlst)
  } 

  if (datsource == "sqlite" && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  } 

}
