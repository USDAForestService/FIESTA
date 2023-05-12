#' Database - Extracts plot coordinates.
#' 
#' Extracts public plot coodinates for an FIA evaluation or a custom 
#' evaluation. Plots are extracted from FIA's public Datamart 
#' (https://apps.fs.usda.gov/fia/datamart/datamart.html) or other defined
#' datasource. 
#' 
#' 
#' @param states String or numeric vector. Name (e.g., 'Arizona','New Mexico')
#' or code (e.g., 4, 35) of state(s) for evalid. If all states in one or more
#' FIA Research Station is desired, set states=NULL and use RS argument to
#' define RS.
#' @param RS String vector. Name of research station(s) to get public XY
#' coordinates for ('RMRS','SRS','NCRS','NERS','PNWRS'). Do not use if states 
#' is populated. See FIESTA::ref_statecd for reference to RS and states.
#' @param xy_datsource Source of XY data ('datamart', 'sqlite', 'obj', 'csv').
#' @param xy_dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.db) where XY data reside.
#' @param xy sf R object or String. If xy_dsn = 'datamart', name of xy table 
#' in FIA DataMart. If xy_dsn = 'sqlite', name of xy layer in database. If 
#' datsource = 'csv', full pathname of xy CSV file(s). If datsource = 'obj', 
#' name of xy R object. If datsource = 'shp', full pathname of shapefile.
#' @param xy_opts List of xy data options for xy (e.g., xy_opts = list(xvar='LON', 
#' yvar='LAT'). See xy_options() for more options and defaults.
#' @param datsource String. Source of FIA data for defining FIA evalutions or 
#' appending variables ('datamart', 'sqlite', 'obj', 'csv'). If datsource = NULL, 
#' datsource = xy_datsource. If datsource = 'datamart', data are downloaded
#' extracted from FIA DataMart (http://apps.fs.usda.gov/fia/datamart/datamart.html). 
#' If datsource='sqlite', specify database name(s) in data_dsn and table name(s) 
#' in dbTabs() argument. If datsource = ('obj','csv'), specify *.csv file name in 
#' dbTabs argument.
#' @param data_dsn String. Name of database with plot_layer and/or ppsa_layer.
#' @param dbTabs String or R Object. If data_dsn = 'datamart', name of table(s) 
#' in FIA DataMart. If data_dsn = 'sqlite', name of layer(s) in database. If 
#' datsource = 'csv', name of CSV file(s). If datsource = 'obj', name of R object.
#' @param pjoinid String. Variable in plot table to join to XY data, if 
#' plot_layer is not NULL. Not necessary to be unique. If using most current 
#' XY coordinates, use identifier for a plot (e.g., PLOT_ID).
#' @param eval String. Type of evaluation time frame for data extraction 
#' ('FIA', 'custom'). See eval_opts for more further options. 
#' @param eval_opts List of evaluation options for 'FIA' or 'custom'
#' evaluations to determine the set of data returned. See help(eval_options)
#' for a list of options.
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL').  Only one inventory type (PERIODIC/ANNUAL) at a time.
#' @param coordType String. c('PUBLIC', 'ACTUAL'). Defines type of coordinates and is
#' used for the output name.
#' @param intensity1 Logical. If TRUE, includes only XY coordinates where 
#' INTENSITY = 1 (FIA base grid).
#' @param pvars2keep String vector. One or more variables in plot_layer to append 
#' to output.
#' @param issp Logical. If TRUE, returns spatial XY data as a list object with
#' query.
#' @param returndata Logical. If TRUE, returns XY data as a list object with
#' query.
#' @param savedata Logical. If TRUE, saves XY data. Specify outfolder and 
#' format using savedata_opts. 
#' @param exportsp Logical. If TRUE, exports data as spatial. 
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE or exportsp = TRUE.
#' @param SURVEY Data frame. The name of the SURVEY data frame object
#' if it has been already downloaded and stored in environment.
#' @param POP_PLOT_STRATUM_ASSGN Data frame. The name of the 
#' PLOT data frame object if it is already downloaded and stored in
#' environment. 
#' @param PLOT Data frame. The name of the PLOT data frame object if 
#' it is already downloaded and stored in environment. 
#' @param POP_PLOT_STRATUM_ASSGN Data frame. The name of the 
#' POP_PLOT_STRATUM_ASSGN data frame object if it is already downloaded 
#' and stored in environment. 
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed. 
#' @param evalInfo List. List object output from DBgetEvalid or DBgetXY 
#' FIESTA functions. 
#'
#' @return if returndata=TRUE, a list of the following objects: 
#' \item{xy}{ Data frame. XY data from database. The output name is based on
#' coordType parameter (e.g., xy_PUBLIC). the data frame include xy.uniqueid,
#' xvar, yvar and appended plot variables in pvars2keep if plot_layer is not 
#' NULL. The default plot variables included are 'STATECD','UNITCD','COUNTYCD',
#' 'PLOT','PLOT_ID' (ID+STATECD+UNTCD+COUNTYCD+PLOT), 'COUNTYFIPS'. 
#' If issp=TRUE, returns an sf object. }
#' \item{xyqry}{ String. Query to extract coordinates. }
#' \item{xvar}{ String. Name of X variable in xy*. }
#' \item{yvar}{ String. Name of Y variable in xy*. }
#' 
#' If savedata=TRUE, outputs the xy* based on savedata_opts. 
#' If exportsp=TRUE, the output xy saved as spatial layer based on savedata_opts.
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
#' head(COxylst$xy_PUBLIC)
#' COxylst$xyqry
#' }
#' @export DBgetXY
DBgetXY <- function (states = NULL, 
                     RS = NULL, 
                     xy_datsource = "datamart", 
                     xy_dsn = NULL, 
                     xy = "PLOT", 
                     xy_opts = xy_options(),
                     datsource = NULL,
                     data_dsn = NULL, 
                     dbTabs = dbTables(),
                     pjoinid = "CN", 
                     eval = "FIA",
                     eval_opts = eval_options(),
                     invtype = "ANNUAL", 
                     coordType = "PUBLIC",
                     intensity1 = FALSE, 
                     pvars2keep = NULL,
                     issp = FALSE, 
                     returndata = TRUE, 
                     savedata = FALSE, 
                     exportsp = FALSE,
                     savedata_opts = NULL,
                     PLOT = NULL,
                     POP_PLOT_STRATUM_ASSGN = NULL,
                     SURVEY = NULL,
                     dbconnopen = FALSE,
                     evalInfo = NULL
                     ) {

  ## DESCRIPTION: Get the most current coordinates in the FIA database
  
  gui <- FALSE
  istree=isseed=isveg=ischng=isdwm <- FALSE
  if (gui) {
    intensity1=savedata=parameters=out_fmt=overwrite <- NULL
  }
    
  
  ## Set global variables
  parameters <- FALSE
  SCHEMA.=invyrtab=evalEndyr=plotnm=ppsanm=pvars=dbconn=XYdf <- NULL

  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check arguments
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% c(names(formals(DBgetXY)), "istree", "isseed", "isveg"))) {
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
    message("no evaluation timeframe specified...")
    message("see eval and eval_opts parameters (e.g., eval='custom', eval_opts=eval_options(Cur=TRUE))\n")
    stop()
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

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst,
		caption="Inventory Type", gui=gui)

  ## Check intensity1
  ####################################################################
  intensity1 <- pcheck.logical(intensity1, varnm="intensity1", 
		title="Single intensity?", first="YES", gui=gui)


  ###########################################################################
  ## Check XY database 
  ###########################################################################
  xy_datsourcelst <- c("datamart", "sqlite", "csv", "obj")
  xy_datsource <- pcheck.varchar(var2check=xy_datsource, varnm="xy_datsource", 
		gui=gui, checklst=xy_datsourcelst, caption="XY data source?")

  if (xy_datsource == "sqlite" && !is.null(xy_dsn)) {
    xyconn <- DBtestSQLite(xy_dsn, dbconnopen=TRUE, showlist=FALSE)
    xytablst <- DBI::dbListTables(xyconn)
    if (length(xytablst) == 0) {
      stop("no data in ", xy_datsource)
    }
  }

  ## Check xy database
  ####################################################################
  if (all(list(class(xy), class(plot_layer)) == "character") && 
		(is.null(datsource) || xy_datsource == datsource)) {
    xyisplot <- ifelse (identical(tolower(xy), tolower(plot_layer)), TRUE, FALSE)
  } else if (!identical(xy_datsource, datsource)) {
    xyisplot <- FALSE
  } else {
    xyisplot <- ifelse (identical(xy, plot_layer), TRUE, FALSE)
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
      datsourcelst <- c("datamart", "sqlite", "csv", "obj", "shp")
      datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		gui=gui, checklst=datsourcelst, caption="Plot data source?",
           stopifnull=TRUE, stopifinvalid=TRUE)
    } else {
      datsource <- xy_datsource
      data_dsn <- xy_dsn
    }
  }
  if (datsource == "sqlite" && !is.null(data_dsn)) {
    dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
    dbtablst <- DBI::dbListTables(dbconn)
    if (length(dbtablst) == 0) {
      stop("no data in ", datsource)
    }
  }
 
  ## Check eval
  ####################################################################
  evallst <- c('FIA', 'custom')
  eval <- pcheck.varchar(eval, varnm="eval", checklst=evallst, 
		caption="Evaluation Type", gui=gui)

  ## Check coordType
  ####################################################################
  coordTypelst <- c("PUBLIC", "ACTUAL")
  coordType <- pcheck.varchar(var2check=coordType, varnm="coordType", 
		gui=gui, checklst=coordTypelst, caption="Coordinate Type?")


  ## Check savedata
  ####################################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data to outfolder?", first="YES", gui=gui)

  ## Check exportsp
  ####################################################################
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial?", first="YES", gui=gui)
  if (exportsp) {
    issp <- TRUE
  }

  ## Check outfolder, outfn.date, overwrite_dsn
  ####################################################################
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


  ## GETS DATA TABLES (OTHER THAN PLOT/CONDITION) IF NULL
  ###########################################################
  if (gui) {
    Typelst <- c("ALL", "CURR", "VOL", "P2VEG", "DWM", "INV", "GROW", "MORT", "REMV", "GRM")
    Type <- select.list(Typelst, title="eval type", 
		preselect="VOL", multiple=TRUE)
    if (length(Type)==0) Type <- "VOL"
  } 

  if (any(Type == "P2VEG")) {
    # understory vegetation tables 
    # (P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE, INVASIVE_SUBPLOT_SPP)
    isveg=issubp <- TRUE
  } 
  if (any(Type == "DWM")) {
    # summarized condition-level down woody debris table (COND_DWM_CALC)
    isdwm <- TRUE
  }
  if (any(Type == "CHNG")) {
    # current and previous conditions, subplot-level - sccm (SUBP_COND_CHNG_MTRX) 
    ischng=issubp <- TRUE
  }
  if (any(Type == "GRM")) {
    ischng=issubp=isgrm <- TRUE
  }
  if (isveg && invtype == "PERIODIC") {
    message("understory vegetation data only available for annual data\n")
    isveg <- FALSE
  } 

  if (isveg && invtype == "PERIODIC") {
    message("understory vegetation data only available for annual data\n")
    isveg <- FALSE
  } 

  ########################################################################
  ### DBgetEvalid()
  ########################################################################

  ## Get DBgetEvalid parameters from eval_opts
  ####################################################################
  if (eval == "FIA") {
    evalCur <- ifelse (Cur || !is.null(Endyr), TRUE, FALSE) 
    evalAll <- ifelse (All, TRUE, FALSE) 
    evalEndyr <- Endyr
    measCur=allyrs <- FALSE
    measEndyr <- NULL

  } else {
    measCur <- ifelse (Cur || !is.null(Endyr), TRUE, FALSE) 
    allyrs <- ifelse (All, TRUE, FALSE) 
    if (length(Endyr) > 1) {
      stop("only one Endyr allowed for custom estimations")
    }
    measEndyr <- Endyr
    evalCur=evalAll <- FALSE
    evalEndyr <- NULL
  }
 
  ####################################################################
  ## Get states, Evalid and/or invyrs info
  ####################################################################
  ## Get states, Evalid and/or invyrs info
  ##########################################################
  if (!is.null(evalInfo)) {
    list.items <- c("states", "invtype")
    evalInfo <- pcheck.object(evalInfo, "evalInfo", list.items=list.items)

  } else {
    evalInfo <- tryCatch( DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource,
                          data_dsn = data_dsn,
                          dbTabs = list(plot_layer=plot_layer),
                          dbconn = dbconn,
                          dbconnopen = TRUE,
                          invtype = invtype, 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll,
                          evalType = evalType,
                          gui = gui),
			error = function(e) {
                  message(e,"\n")
                  return(NULL) })
    if (is.null(evalInfo)) {
      iseval <- FALSE
    }
  }
 
  if (is.null(evalInfo)) stop("no data to return")
  states <- evalInfo$states
  rslst <- evalInfo$rslst
  evalidlist <- evalInfo$evalidlist
  invtype <- evalInfo$invtype
  invyrtab <- evalInfo$invyrtab
  if (length(evalidlist) > 0) {
    invyrlst <- evalInfo$invyrs
    iseval <- TRUE
    savePOP <- TRUE
  }
  dbconn <- evalInfo$dbconn
  SURVEY <- evalInfo$SURVEY
  if (!is.null(SURVEY)) {
    surveynm <- "SURVEY"
  }

  if (!is.null(PLOT)) {
    plotnm <- "PLOT"
  } else if (!is.null(evalInfo$PLOT)) {
    PLOT <- evalInfo$PLOT
    plotnm <- "PLOT"
  }
 
  if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
    ppsanm <- "POP_PLOT_STRATUM_ASSGN"
  } else if (!is.null(evalInfo$POP_PLOT_STRATUM_ASSGN)) {
    POP_PLOT_STRATUM_ASSGN <- evalInfo$POP_PLOT_STRATUM_ASSGN
    ppsanm <- "POP_PLOT_STRATUM_ASSGN"
  }

  ####################################################################
  ## Check custom Evaluation data
  ####################################################################
  if (!iseval) {
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
    invyrlst <- evalchk$invyrlst
    measyrlst <- evalchk$measyrlst
  }

  ## Define variables
  ###########################################################
  XYvarlst <- unique(c(xy.uniqueid, xyjoinid, xvar, yvar))
  if (any(XYvarlst %in% "")) {
    XYempty <- XYvarlst[XYvarlst %in% ""] 
    XYvarlst <- XYvarlst[!XYvarlst %in% XYempty]
  }
  pvars2keep <- unique(c("STATECD", "UNITCD", "COUNTYCD", "PLOT", 
					pvars2keep))
  if (!iseval) {
    pvars2keep <- unique(pvars2keep, "SRV_CN")
  }    

  if (!is.null(measyrs) || measCur) {
    XYvarlst <- unique(c(XYvarlst, "MEASYEAR", "PLOT_STATUS_CD", "INVYR")) 
  }
  if (!is.null(invyrs)) {
    XYvarlst <- unique(c(XYvarlst, "INVYR")) 
  }
  if (intensity1) {
    XYvarlst <- unique(c(XYvarlst, "INTENSITY")) 
  }
 
  ####################################################################
  ## Check xy table
  ####################################################################
  if (xy_datsource == "datamart") {
    if (iseval && is.null(POP_PLOT_STRATUM_ASSGN)) {
      POP_PLOT_STRATUM_ASSGN <- tryCatch( DBgetCSV("POP_PLOT_STRATUM_ASSGN", 
                             stabbrlst,
                             returnDT = TRUE, 
                             stopifnull = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
      ppsanm <- "POP_PLOT_STRATUM_ASSGN"
    }

    XYdf <- pcheck.table(xy, stopifnull=FALSE)
    if (is.null(XYdf)) {
      if (is.character(xy) && exists(xy) && !is.null(get(xy))) {
        XYdf <- get(xy)
      } else { 
        XYdf <- tryCatch( DBgetCSV(xy, 
                             stabbrlst,
                             returnDT = TRUE, 
                             stopifnull = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
      }
    } 
    if (!is.null(XYdf)) {
      xynm <- "XYdf"
      xyflds <- names(XYdf)
    }
  } else if (xy_datsource == "sqlite") {
    if (!is.character(xy)) {
      stop("invalid xy")
    }
    xynm <- chkdbtab(xytablst, xy, stopifnull=FALSE)
    if (!is.null(xynm)) {
      xyflds <- DBI::dbListFields(xyconn, xynm)
    } else {
      stop(xy, " does not exist in database\n ", toString(xytablst))
    }
  } else {
    if (iseval && is.null(POP_PLOT_STRATUM_ASSGN)) {
      ppsaqry <- paste("select * from POP_PLOT_STRATUM_ASSGN where evalid IN(",
               toString(unlist(evalidlist)), ")")
			
      POP_PLOT_STRATUM_ASSGN <- tryCatch( DBI::dbGetQuery(dbconn, ppsaqry),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
      ppsanm <- "POP_PLOT_STRATUM_ASSGN"
    }

    XYdf <- pcheck.table(xy, stopifnull=TRUE, stopifinvalid=TRUE)
    xynm <- "XYdf"
    names(XYdf) <- toupper(names(XYdf))
    xyflds <- names(XYdf)
  }

  ## Check xy.uniqueid
  xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", 
		gui=gui, checklst=xyflds, caption="UniqueID variable of xy",
		warn=paste(xy.uniqueid, "not in xy table"), stopifnull=TRUE)

  ## Check XYdf variables
  ####################################################################
  xyvars <- unique(c(XYvarlst, pvars2keep))
  if (!all(xyvars %in% xyflds)) {
    xypmiss <- xyvars[!xyvars %in% xyflds]
    if (length(xypmiss) > 0) {
      pvars2keep <- xypmiss
    } else {
      pvars2keep <- NULL
    }
    xyvars <- xyvars[!xyvars %in% xypmiss]
  } else {
    pvars2keep <- NULL
  }

 
  ####################################################################
  ## Check plot table
  ####################################################################
  if (xyisplot && !is.null(pvars2keep)) {
    pmiss <- pvars2keep[!pvars2keep %in% xyflds]
    if (any(pmiss %in% XYvarlst)) {
      xymiss <- pmiss[pmiss %in% XYvarlst]
      if (length(xymiss) > 0) {
        if (all(c("LON", "LAT") %in% xymiss) && all(c("LON_PUBLIC", "LAT_PUBLIC") %in% xyflds)) {
          xyvars <- c(xyvars[!xyvars %in% c("LON", "LAT")], "LON_PUBLIC", "LAT_PUBLIC")
          xymiss <- xymiss[!xymiss %in% c("LON", "LAT")]
          xvar <- "LON_PUBLIC"
          yvar <- "LAT_PUBLIC"
        } 
        if (length(xymiss) > 0) {
          stop("missing essential variables: ", toString(xymiss))
        }
      } else {
        message("missing plot variables: ", toString(pmiss))
      }
    }
    if (measCur) {
      xyvarsA <- paste0("p.", unique(xyvars)) 
    } else {
      xyvarsA <- paste0("xy.", unique(xyvars)) 
    }

  } else if (!xyisplot && !is.null(pvars2keep)) {    
 
    ## Check plot table
    ########################################################
    if (datsource == "datamart") {
      if (xy_datsource == "sqlite") {
        statenm <- findnm("STATECD", xyflds, returnNULL=TRUE)
        if (is.null(statenm)) {
          stop("must include STATECD in xy dataset")
        } else {
          xy.qry <- paste("select", toString(xyvars), "from", xy, 
				"where STATECD in(", toString(stcdlst), ")")
          XYdf <- DBI::dbGetQuery(xyconn, xy.qry)
          xynm <- "XYdf"
          xy_datsource <- "datamart"
        }
      }
      if (is.null(PLOT)) {
        PLOT <- tryCatch( DBgetCSV("PLOT", 
                             stabbrlst,
                             returnDT = TRUE, 
                             stopifnull = FALSE),
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })
      }
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
      PLOT <- pcheck.table(plot_layer, stopifnull=TRUE, stopifinvalid=TRUE)
      plotnm <- "PLOT"
      names(PLOT) <- toupper(names(PLOT))
      pltflds <- names(PLOT)
    }

    ## Check plot variables
    ########################################################
    if (!all(pvars2keep %in% pltflds)) {
      pmiss <- pvars2keep[!pvars2keep %in% pltflds]
      if (any(pmiss %in% XYvarlst)) {
        xymiss <- pmiss[pmiss %in% XYvarlst]
        if (length(xymiss) > 0) {
          stop("missing essential variables: ", toString(xymiss))
        } else {
          message("missing plot variables: ", toString(pmiss))
        }
        if (length(pmiss) < length(pvars2keep)) {
          pvars <- pvars2keep[!pvars2keep %in% pmiss]
        }
      }
    } else {
      pvars <- pvars2keep
    }
    if (!is.null(plotnm) && length(pvars) > 0) {
      ## Check xyjoinid
      xyjoinid <- findnm(xyjoinid, xyflds, returnNULL=TRUE)
      if (is.null(xyjoinid)) {
        message("xyjoinid is NULL... using ", xy.uniqueid, " to join to plot table")
        xyjoinid <- xy.uniqueid
      }

      ## Check pjoinid
      pjoinid <- findnm(pjoinid, pltflds, returnNULL=TRUE)
      if (is.null(pjoinid)) {
        pjoinid <- findnm(xyjoinid, pltflds, returnNULL=TRUE)
        if (is.null(pjoinid)) {
          if (xyjoinid == "PLT_CN" && "CN" %in% pltflds) {
            pjoinid <- "CN"
          } else {
            stop("pjoinid is invalid")
          }
        }
      }

      if (datsource == "sqlite") {
        if ("STATECD" %in% pltflds) {
          plot.qry <- paste("select", toString(unique(c(pjoinid, pvars))), 
				"from", plotnm, 
				"where STATECD in(", toString(stcdlst), ")")
        } else {
          plot.qry <- paste("select", toString(unique(c(pjoinid, pvars))), 
				"from", plotnm)
        }
        PLOT <- DBI::dbGetQuery(dbconn, plot.qry)
        plotnm <- "PLOT"
        datsource <- "datamart"

        if (xy_datsource == "sqlite" && is.null(XYdf)) {
          if ("STATECD" %in% xyflds) {
            xy.qry <- paste("select", toString(unique(c(xyjoinid, xyvars))), 
				"from", xynm, 
				"where STATECD in(", toString(stcdlst), ")")
          } else {
            xy.qry <- paste("select", toString(unique(c(xyjoinid, xyvars))), 
				"from", xynm)
          }
          XYdf <- DBI::dbGetQuery(dbconn, xy.qry)
          xynm <- "XYdf"
          xy_datsource <- "datamart"
        }

        ## Check if class of xyjoinid in XYdf matches class of pjoinid in PLOT
        tabchk <- check.matchclass(XYdf, PLOT, xyjoinid, pjoinid)
        XYdf <- tabchk$tab1
        PLOT <- tabchk$tab2

        XYPLOT <- merge(XYdf, PLOT, by.x=xyjoinid, by.y=pjoinid)
        if (length(XYPLOT) == 0) {
          message("invalid join... check xyjoinid and pjoinid") 
        }
        xyisplot <- TRUE
        xyvars <- unique(c(xyvars, pvars))
        xynm <- "XYPLOT"
        pvars=plotnm <- NULL
        if (measCur) {
          xyvarsA <- paste0("p.", unique(xyvars)) 
        } else {
          xyvarsA <- paste0("xy.", unique(xyvars)) 
        }
      } else {
        ## Add alias to variables
        xyvarsA <- paste0("xy.", unique(c(xyjoinid, xyvars))) 
        if (!all(pvars == pjoinid)) {
          pvarsA <- paste0("p.", unique(pvars)) 
          xyvarsA <- toString(c(xyvarsA, pvarsA))
        } else {
          plotnm <- NULL
        }
      }
    }
  } else {
    plotnm <- NULL
    if (measCur) {
      xyvarsA <- paste0("p.", unique(xyvars)) 
    } else {
      xyvarsA <- paste0("xy.", unique(xyvars)) 
    }
  }
 
  ###########################################################################
  ## Build filter
  ###########################################################################
  ## Create filter for state
  stcds <- pcheck.states(states, "VALUE")
  statecdnm <- findnm("STATECD", xyvars, returnNULL=TRUE)
  if (!is.null(statecdnm)) {
    if (measCur) {
      statecdA <- paste0("p.", statecdnm)
    } else {
      statecdA <- paste0("xy.", statecdnm)
    }
  } else if (!is.null(plotnm)) {
    statecdnm <- findnm("STATECD", pvars, returnNULL=TRUE)
    if (is.null(statecdnm)) {
      stop("STATECD is missing from xy and plot data tables")
    } else {
      statecdA <- paste0("p.", statecdnm)
    }
  }
  stFilter <- paste0(statecdA, " IN(", toString(stcds), ")")

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
        ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)

      } else {
        POP_PLOT_STRATUM_ASSGN <- pcheck.table(ppsa_layer, stopifnull=TRUE, stopifinvalid=TRUE)
        evalidnm <- findnm("EVALID", names(POP_PLOT_STRATUM_ASSGN))
        if (!all(evalid %in% unique(POP_PLOT_STRATUM_ASSGN[[evalidnm]]))) {
          miss <- sort(evalid[!evalid %in% 
				unique(POP_PLOT_STRATUM_ASSGN[[evalidnm]])])
          stop("evalid not in POP_PLOT_STRATUM_ASSGN: ", toString(sort(evalid)))
        }
        ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
      }
    } else {
      ppsaflds <- names(POP_PLOT_STRATUM_ASSGN)
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
   
    invyrnm <- findnm("INVYR", xyvars, returnNULL=TRUE)
    if (!is.null(invyrnm)) {
      invyrA <- paste0("xy.", invyrnm)  
    } else if (!is.null(plotnm)) {
      invyrnm <- findnm("INVYR", pvars, returnNULL=TRUE)
      if (is.null(invyrnm)) {
        stop("INVYR is missing from xy and plot data tables")
      } else {
        invyrA <- paste0("p.", invyrnm)
      }
    }
    xyfromqry <- paste0(SCHEMA., xynm, " xy")
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = xy.", xy.uniqueid, ")")
    } 
    evalFilter <- paste0(stFilter, " and ", invyrA, " IN(", toString(unlist(invyrs)), ")")

  } else if (length(unlist(measyrs)) > 1) {

    measyearnm <- findnm("MEASYEAR", xyvars, returnNULL=TRUE)
    if (!is.null(measyearnm)) {
      measyearA <- paste0("xy.", measyearnm)  
    } else if (!is.null(plotnm)) {
      measyearnm <- findnm("MEASYEAR", pvars, returnNULL=TRUE)
      if (is.null(measyearnm)) {
        stop("MEASYEAR is missing from xy and plot data tables")
      } else {
        measyearA <- paste0("p.", measyearnm)
      }
    }

    xyfromqry <- paste0(SCHEMA., xynm, " xy")
    if (!is.null(plotnm) && !xyisplot) {
      xyfromqry <- paste0(xyfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.", pjoinid, " = xy.", xy.uniqueid, ")")
    }
    evalFilter <- paste0(stFilter, " and ", measyearA, " IN(", toString(unlist(measyrs)), ")")

  } else {

    if (measCur) {
      if (xyisplot || is.null(plotnm)) {
        pnm <- xynm
        pid <- xy.uniqueid
      } else {
        pnm <- plotnm
        pid <- xyjoinid
      }
      popSURVEY <- FALSE
      if (!is.null(SURVEY) && "SRV_CN" %in% names(get(pnm))) {
        popSURVEY <- TRUE
      }
        
      pfromqry <- getpfromqry(Endyr = measEndyr, 
                            SCHEMA. = SCHEMA., 
                            intensity1 = intensity1, 
                            popSURVEY = popSURVEY, 
                            plotnm = pnm,
                            pjoinid = pid,
                            surveynm = surveynm,
                            plotobj = get(pnm))
      if (xyisplot || is.null(plotnm)) {
        xyfromqry <- pfromqry
      } else {
        xyfromqry <- paste0(pfromqry, 
            " JOIN ", SCHEMA., xynm, " xy ON(xy.", xyjoinid, " = p.", pjoinid, ")")
      }
    } else {
      xyfromqry <- paste0(SCHEMA., xynm, " xy")
      if (!xyisplot && !is.null(plotnm)) {
        xyfromqry <- paste0(xyfromqry, 
		 " JOIN ", SCHEMA., plotnm, " p ON(xy.", xyjoinid, " = p.", pjoinid, ")")
      }
    }
    evalFilter <- stFilter 
  }

  if (intensity1) {
    intensitynm <- findnm("INTENSITY", xyvars, returnNULL=TRUE)
    if (!is.null(intensitynm)) {
      if (measCur) {
        intensityA <- paste0("p.", intensitynm)
      } else {
        intensityA <- paste0("xy.", intensitynm)
      }
    } else if (!is.null(plotnm)) {
      intensitynm <- findnm("INTENSITY", pvars, returnNULL=TRUE)
      intensityA <- paste0("p.", intensitynm)
    } else {
      message("the INTENSITY variable is not in dataset... ",
              "assuming plots are single intensity")
      intensity1 <- FALSE
    }
    if (!is.null(intensitynm)) {
      evalFilter <- paste(evalFilter, "and", intensityA, "= '1'")
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
    evalidnm <- findnm("EVALID", xyvars, returnNULL=TRUE)
    if (!is.null(evalidnm)) {
      evalidA <- paste0("xy.EVALID")
    } else {
      if (!is.null(ppsanm)) {
        evalidnm <- findnm("EVALID", ppsaflds, returnNULL=TRUE)
        if (!is.null(evalidnm)) {
          evalidA <- paste0("ppsa.EVALID")
        }
      } else if (!is.null(plotnm)) {
        evalidnm <- findnm("EVALID", pvars, returnNULL=TRUE)
        if (!is.null(evalidnm)) {
          evalidA <- paste0("p.EVALID")
        }
      }
    }
  }

  ## Inventory year table query
  yrvar <- ifelse(!is.null(invyrs), "INVYR", "MEASYEAR")
  if (!is.null(plotnm)) {
    yrvarnm <- findnm(yrvar, pvars, returnNULL=TRUE)
    stcdnm <- findnm("STATECD", pvars, returnNULL=TRUE)
  } else {
    yrvarnm <- findnm(yrvar, xyvars, returnNULL=TRUE)
    stcdnm <- findnm("STATECD", xyvars, returnNULL=TRUE)
  }
  if (!is.null(yrvarnm) && !is.null(stcdnm)) {
    invarsA <- toString(paste0("p.", c(stcdnm, yrvarnm)))
    invyrtab.qry <- paste0("SELECT distinct ", invarsA, ", COUNT(*)", 
		           " from ", xyfromqry,
				" GROUP BY statecd, ", yrvar, 
				" ORDER BY statecd, ", yrvar) 
  }
 
  ## Create invyrtab query 
  ###########################################################
  xycoords.qry <- paste0("select distinct ", toString(xyvarsA), 
		" from ", xyfromqry,
		" where ", evalFilter)
  message(xycoords.qry)

  if (xy_datsource == "sqlite") {
    if (is.null(dbconn)) dbconn <- xyconn
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
    xyx <- tryCatch( sqldf::sqldf(xycoords.qry, 
						stringsAsFactors = FALSE), 
			error = function(e) {
                  message(e, "\n")
                  return(NULL) })

    if (!iseval && is.null(invyrtab) && !is.null(invyrtab.qry)) {
      invyrtab <- tryCatch( sqldf::sqldf(invyrtab.qry, 
						stringsAsFactors = FALSE),
			error = function(e) {
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
    xyoutnm <- paste0("xyCur_", coordType)
    assign(xyoutnm, xyx)
  } else {
    xyoutnm <- paste0("xy_", coordType)
    assign(xyoutnm, xyx)
  } 

  if (is.null(out_layer) || out_layer == "outdat") {
    out_layer <- xyoutnm
    if (issp) {
      outsp_layer <- paste0("sp", xyoutnm)
    }
  }

  if (issp) {
    spxyoutnm <- paste0("sp", xyoutnm)
     
    if (all(c(xvar, yvar) %in% names(xyx))) {

      ## Generate shapefile
      assign(spxyoutnm, spMakeSpatialPoints(xyplt = xyx, 
                     xvar = xvar, yvar = yvar, xy.uniqueid = xy.uniqueid, 
                     xy.crs = 4269, addxy = TRUE, 
                     exportsp = exportsp,
                     savedata_opts=list(out_dsn=out_dsn, 
                               out_fmt=outsp_fmt,
                               outfolder=outfolder, out_layer=outsp_layer, 
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

  ## Set xyjoinid
  if (is.null(xyjoinid)) {
    xyjoinid <- xy.uniqueid
  }
 
  ## GENERATE RETURN LIST
  ###########################################################
  if (returndata) {
    returnlst <- list()
    if (issp) {
      returnlst$spxy <- get(spxyoutnm)
    } 
    returnlst[[xyoutnm]] <- get(xyoutnm)
    returnlst$xyqry <- xycoords.qry
    returnlst$xy_opts <- list(xy.uniqueid=xy.uniqueid, 
                             xvar=xvar, yvar=yvar, 
                             xy.crs=xy.crs, xyjoinid=xyjoinid)
    returnlst$pjoinid <- pjoinid
    returnlst$invyrlst <- invyrlst
    
    if (dbconnopen) {
      returnlst$dbconn <- dbconn
    }
    returnlst$evalInfo <- evalInfo
 
    if (!is.null(ppsanm) && exists(ppsanm)) {
      returnlst$pop_plot_stratum_assgn <- get(ppsanm)
    }

    ## Return data list
    return(returnlst)
  } 

  if (datsource == "sqlite" && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  } 

}

