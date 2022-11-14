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
#' @param xy_datsource Source of XY data ('obj', 'csv', 'datamart', 'sqlite').
#' @param xy_dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.sqlite) where XY data are.
#' @param xy sf R object or String. Table with xy coordinates. Can be a spatial
#' polygon object, data frame, full pathname to a shapefile, or name of a layer
#' within a database.
#' @param xy_opts List of xy data options to specify if xy is NOT NULL. 
#' See xy_options (e.g., xy_opts = list(xvar='LON', yvar='LAT').
#' @param datsource String. Source of FIA data ("obj", "csv", "datamart",
#' "sqlite").  If datsource="sqlite", specify database name in data_dsn and
#' layers in *_layer arguments.  If datsource="datamart", files are downloaded
#' and extracted from FIA DataMart
#' (http://apps.fs.usda.gov/fia/datamart/datamart.html). See details for more
#' information about plot coordinates.  If datsource="csv", specify *.csv file
#' names in *_layer arguments.
#' @param data_dsn String. Name of database where *_layers reside.
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
#' @param POP_PLOT_STRATUM_ASSGN Data frame. The POP_PLOT_STRATUM_ASSGN table
#' if already downloaded. 
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
#'                    eval = "FIA",
#'                    eval_opts=eval_options(Endyr = 2019))
#' names(COxylst)
#' 
#' head(COxylst$xyCur_ACTUAL)
#' COxylst$xyqry
#' }
#' @export DBgetXY
DBgetXY <- function (states = NULL, 
                     RS = NULL, 
                     xy_datsource = "datamart", 
                     xy_dsn = NULL, 
                     xy = "PLOT", 
                     xy_opts = xy_options(xy.uniqueid="CN", 
 	                               xvar="LON", yvar="LAT"),
                     datsource = NULL,
                     data_dsn = NULL, 
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
  SCHEMA.=invyrtab=evalEndyr=plotnm=ppsanm=pvars <- NULL
  
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



  ###########################################################################
  ## Check xy database
  ###########################################################################
  xyisplot <- ifelse (identical(xy, plot_layer), TRUE, FALSE)

  ## Check database connection - xy_dsn
  ########################################################
  xy_datsourcelst <- c("sqlite", "datamart", "csv", "obj")
  xy_datsource <- pcheck.varchar(var2check=xy_datsource, varnm="xy_datsource", 
		gui=gui, checklst=xy_datsourcelst, caption="XY data source?")

  if (xy_datsource == "sqlite" && !is.null(xy_dsn)) {
    xyconn <- DBtestSQLite(xy_dsn, dbconnopen=TRUE, showlist=FALSE)
    xytablst <- DBI::dbListTables(xyconn)
    if (length(xytablst) == 0) {
      stop("no data in ", xy_datsource)
    }
  }
 
  ###########################################################################
  ## Check plot database (if xyisplot = FALSE)
  ###########################################################################
  if (xyisplot) {
    datsource <- xy_datsource
    data_dsn <- xy_dsn
  } else {
    if (is.null(datsource)) {
      datsource <- xy_datsource
      data_dsn <- xy_dsn
    } else if (!identical(xy_datsource, datsource) || !identical(xy_dsn, data_dsn)) {
 
      ## Check database connection - data_dsn
      ########################################################
      datsourcelst <- c("sqlite", "datamart", "csv", "obj")
      datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		gui=gui, checklst=datsourcelst, caption="Plot data source?",
           stopifnull=TRUE, stopifinvalid=TRUE)

      if (datsource == "sqlite" && !is.null(data_dsn)) {
        dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
        dbtablst <- DBI::dbListTables(dbconn)
        if (length(dbtablst) == 0) {
          stop("no data in ", datsource)
        }
      }
    } else {
      datsource <- xy_datsource
      data_dsn <- xy_dsn

      if (datsource == "sqlite" && !is.null(data_dsn)) {
        dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
        dbtablst <- DBI::dbListTables(dbconn)
        if (length(dbtablst) == 0) {
          stop("no data in ", datsource)
        }
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

  ###########################################################################
  ## Get states, Evalid and/or invyrs info
  ###########################################################################
  evalInfo <- tryCatch( DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource,
                          data_dsn = data_dsn,
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
    surveynm <- evalInfo$surveynm
    POP_PLOT_STRATUM_ASSGN <- evalInfo$POP_PLOT_STRATUM_ASSGN
    dbconn <- evalInfo$dbconn
  }

  ## If using EVALID, you don't need to get INVYRS, intensity, or subcycle
  if (!iseval) {
  
    ## Check custom Evaluation data
    #############################################
    evalchk <- customEvalchk(states = states, 
                             measCur = measCur, 
                             measEndyr = measEndyr, 
                             allyrs = allyrs, 
                             invyrs = invyrs, 
                             measyrs = measyrs,
                             invyrtab = invyrtab)
    if (is.null(evalchk)) {
      stop("must specify an evaluation timeframe for data extraction... \n", 
		"...see eval_opts parameter, (e.g., eval_opts=eval_options(Cur=TRUE))")
    }
    measCur <- evalchk$measCur
    measEndyr <- evalchk$measEndyr
    allyrs <- evalchk$allyrs
    invyrs <- evalchk$invyrs
    measyrs <- evalchk$measyrs
  }

  ## Check xy table
  ########################################################
  if (xy_datsource == "datamart") {
    XY <- tryCatch( DBgetCSV(xy, 
                             stabbrlst,
                             returnDT = TRUE, 
                             stopifnull = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
    if (!is.null(XY)) {
      xynm <- "XY"
      xyflds <- names(XY)
    }
  } else if (xy_datsource == "sqlite") {
    xynm <- chkdbtab(xytablst, xy, stopifnull=FALSE)
    if (!is.null(xynm)) {
      xyflds <- DBI::dbListFields(xyconn, xynm)
    }
  } else {
    XY <- pcheck.table(xy, stopifnull=TRUE, stopifinvalid=TRUE)
    xynm <- "XY"
    names(XY) <- toupper(names(XY))
    xyflds <- names(XY)
  }

  ## Check uniqueid and joinid
  xy.uniqueid <- findnm(xy.uniqueid, xyflds, returnNULL=FALSE)


  ## Check XY variables
  #################################################################
  XYvarlst <- unique(c(xy.uniqueid, xvar, yvar))
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
  if (!all(xyvars %in% xyflds)) {
    xypmiss <- xyvars[!xyvars %in% xyflds]
    if (any(xypmiss %in% XYvarlst)) {
      xymiss <- xypmiss[xypmiss %in% XYvarlst]
      stop("xy table is missing essential variables: ", toString(xymiss))
    } else {
      pvars <- xypmiss
    }
    xyvars <- xyvars[!xyvars %in% xypmiss]
  }


  ###########################################################################
  ###########################################################################
  ## Check plot
  ###########################################################################
  ###########################################################################
  if (!is.null(pvars)) {    
    ## Check database connection - data_dsn
    ########################################################
    datsourcelst <- c("sqlite", "datamart", "csv", "obj")
    datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		gui=gui, checklst=datsourcelst, caption="Plot data source?")

    if (datsource == "sqlite" && !is.null(data_dsn)) {
      dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      dbtablst <- DBI::dbListTables(dbconn)
      if (length(dbtablst) == 0) {
        stop("no data in ", datsource)
      }
    }

    ## Check plot table
    ########################################################
    if (datsource == "datamart") {
      PLOT <- tryCatch( DBgetCSV(plot_layer, 
                             stabbrlst,
                             returnDT = TRUE, 
                             stopifnull = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
      if (!is.null(PLOT)) {
        plotnm <- "PLOT"
        pltflds <- names(PLOT)
      }
    } else if (xy_datsource == "sqlite") {
      plotnm <- chkdbtab(dbtablst, plot_layer, stopifnull=FALSE)
      if (!is.null(plotnm)) {
        pltflds <- DBI::dbListFields(dbconn, plotnm)
      }
    } else {
      PLOT <- pcheck.table(plot_layer, stopifnull=TRUE, stopifinvalid=TRUE)
      plotnm <- "PLOT"
      names(PLOT) <- toupper(names(PLOT))
      pltflds <- names(PLOT)
    }

    ## Check plot variables
    ########################################################
    if (!all(pvars %in% pltflds)) {
      pmiss <- pvars[!pvars %in% pltflds]
      message("missing plot variables: ", toString(pmiss))
      if (length(pmiss) < length(pvars)) {
        pvars <- pvars[!pvars %in% pmiss]
      }
    }

    if (!is.null(plotnm) && length(pvars) > 0) {
      ## Check xyjoinid
      xyjoinid <- findnm(xyjoinid, xyflds, returnNULL=TRUE)
      if (is.null(xyjoinid)) {
        xyjoinid <- xy.uniqueid
      }

      ## Check pjoinid
      pjoinid <- findnm(pjoinid, pltflds, returnNULL=TRUE)
      if (is.null(pjoinid)) {
        pjoinid <- findnm(xyjoinid, pltflds, returnNULL=TRUE)
        if (is.null(pjoinid) && xyjoinid == "PLT_CN" && "CN" %in% pltflds) {
          pjoinid <- "CN"
        } else {
          stop("pjoinid is invalid")
        }
      }

      ## Add alias to variables
      xyvarsA <- paste0("xy.", unique(c(xyjoinid, xyvars))) 
      if (!all(pvars == pjoinid)) {
        pvarsA <- paste0("p.", unique(pvars)) 
        xyvarsA <- toString(c(xyvarsA, pvarsA))
      } else {
        plotnm <- NULL
      }
    }
  } else {
    plotnm <- NULL
    xyvarsA <- paste0("xy.", unique(xyvars)) 
  }


  ###########################################################################
  ## Build filter
  ###########################################################################
  ## Create filter for state
  stcds <- pcheck.states(states, "VALUE")
  stFilter <- paste0("p.STATECD IN(", toString(stcds), ")")
  evalFilter=xyfromqry <- NULL
  stabbr <- pcheck.states(states, "ABBR")


  ## If iseval = TRUE 
  if (iseval) {
    evalid <- unlist(evalidlist) 

    if (is.null(POP_PLOT_STRATUM_ASSGN)) {
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
    xyfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., xynm, 
			" xy ON (xy.", xy.uniqueid, " = ppsa.PLT_CN)")

    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = ppsa.PLT_CN)")
    } 
    evalFilter <- paste0("ppsa.EVALID IN(", toString(unlist(evalidlist)), ")")

  } else if (length(unlist(invyrs)) > 1) {
    xyfromqry <- paste0(SCHEMA., xynm, " xy")
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = xy.", xy.uniqueid, ")")
    } 
    evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(unlist(invyrs)), ")")

  } else if (length(unlist(measyrs)) > 1) {
    xyfromqry <- paste0(SCHEMA., xynm, " xy")
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = xy.", xy.uniqueid, ")")
    }
    evalFilter <- paste0(stFilter, " and p.MEASYEAR IN(", toString(unlist(measyrs)), ")")

  } else {
 
    if (!is.null(plotnm)) {
      if (measCur) {
        popSURVEY <- ifelse(is.null(SURVEY), FALSE, TRUE)
        xyfromqry <- getpfromqry(Endyr = measEndyr, 
                               SCHEMA. = SCHEMA., 
                               intensity1 = intensity1, 
                               popSURVEY = popSURVEY, 
                               plotnm = plotnm,
                               surveynm = "SURVEY")
      }
      xyfromqry <- paste0(SCHEMA., xynm, " xy")
      if (!xyisplot) {
        xyfromqry <- paste0(xyfromqry, 
            " JOIN ", SCHEMA., plotnm, " p ON(xy.", xyjoinid, " = p.", pjoinid, ")")
      } 
    } else {
      xyfromqry <- paste0(SCHEMA., xynm, " xy")
    }
    evalFilter <- stFilter 
  }
  if (intensity1) {
    intensitynm <- findnm("INTENSITY", xyflds, returnNULL=TRUE)
    if (!is.null(intensitynm)) {
      intensityA <- paste0("xy.INTENSITY")
    } else if (!is.null(plotnm)) {
      intensitynm <- findnm("INTENSITY", pltflds, returnNULL=TRUE)
      intensityA <- paste0("p.INTENSITY")
    } else {
      message("the INTENSITY variable is not in dataset... ",
              "assuming all data are single intensity")
    }
    if (!is.null(intensitynm)) {
      evalFilter <- paste(evalFilter, "and ", intensityA, " = '1'")
    }
  } 

  ##################################################################################
  ##################################################################################
  ## Generate queries
  ##################################################################################

  ## Create invyrtab table 
  ###########################################################
  invyrtab.qry <- NULL

  ## Append EVALID 
  if (iseval) {
    evalidnm <- findnm("EVALID", xyflds, returnNULL=TRUE)
    if (!is.null(evalidnm)) {
      evalidA <- paste0("xy.EVALID")
    } else {
      if (!is.null(ppsanm)) {
        evalidnm <- findnm("EVALID", ppsaflds, returnNULL=TRUE)
        if (!is.null(evalidnm)) {
          evalidA <- paste0("ppsa.EVALID")
        }
      } else if (!is.null(plotnm)) {
        evalidnm <- findnm("EVALID", pltflds, returnNULL=TRUE)
        if (!is.null(evalidnm)) {
          evalidA <- paste0("p.EVALID")
        }
      }
    }
  }

  ## Inventory year table query
  yrvar <- ifelse(!is.null(invyrs), "INVYR", "MEASYEAR")
  if (!is.null(plotnm)) {
    yrvarnm <- findnm(yrvar, pltflds, returnNULL=TRUE)
    stcdnm <- findnm("STATECD", pltflds, returnNULL=TRUE)
  } else {
    yrvarnm <- findnm(yrvar, xyflds, returnNULL=TRUE)
    stcdnm <- findnm("STATECD", xyflds, returnNULL=TRUE)
  }
  if (!is.null(yrvarnm) && !is.null(stcdnm)) {
    invarsA <- toString(paste0("p.", c(stcdnm, yrvarnm)))
    invyrtab.qry <- paste0("SELECT ", invarsA, ", COUNT(*)", 
		           " from ", xyfromqry,
				" GROUP BY statecd, ", yrvar, 
				" ORDER BY statecd, ", yrvar) 
  }


  ## Create invyrtab query 
  ###########################################################
  xycoords.qry <- paste0("select ", toString(xyvarsA), 
		" from ", xyfromqry,
		" where ", evalFilter)

  if (datsource == "sqlite") {
    xyx <- tryCatch( DBI::dbGetQuery(dbconn, xycoords.qry),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
    if (!iseval && is.null(invyrtab) && !is.null(invyrtab.qry)) {
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
    if (!iseval && is.null(invyrtab) && !is.null(invyrtab.qry)) {
      invyrtab <- tryCatch( sqldf::sqldf(invyrtab.qry, stringsAsFactors = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) }) 
    } 
    xyx <- setDT(xyx)     
  }
  if (is.null(xyx) || nrow(xyx) == 0) {
    message("invalid xy query\n")
    message(xycoords.qry)
    stop()
  }

  ## Change CN to PLT_CN if exists
  if ("CN" %in% names(xyx) && !"PLT_CN" %in% names(xyx)) {
    setnames(xyx, "CN", "PLT_CN")
    xy.uniqueid <- "PLT_CN"
    xyjoinid <- "PLT_CN"
  }

  ## Remove KNOWN plots that are no longer in inventory
  if (measCur || !is.null(measEndyr)) {
    xyx <- xyx[!xyx$PLT_CN %in% FIESTAutils::kindcd3old$CN, ]
  }
   
  if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% names(xyx))) {
    xyx[["PLOT_ID"]] <- paste0("ID", 
		formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$UNITCD, width=2, digits=2, flag=0),
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(xyx$PLOT, width=5, digits=5, flag=0)) 
  }

  ## Change names of X/Y variables to *_PUBLIC
  if (xvar == "LON" && yvar == "LAT") {
    setnames(xyx, c("LON", "LAT"), c("LON_PUBLIC", "LAT_PUBLIC"))
    xvar <- "LON_PUBLIC"
    yvar <- "LAT_PUBLIC"
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
     
    if (all(c(xvar, yvar) %in% names(xyx))) {

      ## Generate shapefile
      assign(spxyoutnm, spMakeSpatialPoints(xyplt=xyx, xvar=xvar, 
		        yvar=yvar, xy.uniqueid=xy.uniqueid, xy.crs=4269, addxy=TRUE, 
		        exportsp=exportsp, 
		        savedata_opts=list(out_dsn=out_dsn, 
                               out_fmt=outsp_fmt,
                               outfolder=outfolder, out_layer=spxyoutnm, 
		                    outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer, 
		                    append_layer=append_layer, outfn.pre=outfn.pre) ))
    } else { 
      message("need ", xvar, " and ", yvar, " variables to generate spatial xy")
    }
  }
     
  ###############################################################################
  ## SAVE data
  ###############################################################################
  if (savedata) {
    index.unique.xyplt <- xy.uniqueid
    
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
    returnlst$xvar <- xvar
    returnlst$yvar <- yvar
    returnlst$xy.uniqueid <- xy.uniqueid
    returnlst$xyjoinid <- xyjoinid
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

