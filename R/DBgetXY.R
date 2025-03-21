#' Database - Extracts plot coordinates.
#' 
#' Extracts public plot coordinates for an FIA evaluation or a custom 
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
#' @param datsource String. Source of FIA data for defining FIA evaluations or 
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
#' @param dbconn Open database connection.
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
                     xy_datsource = NULL, 
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
                     dbconn = NULL,
                     dbconnopen = TRUE,
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
  SCHEMA.=invyrtab=evalEndyr=plotnm=ppsanm=ppsanm=pltflds=
    ppsaflds=pvars=XYdf=invyrtab.qry <- NULL


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
  pcheck.params(input.params, 
                savedata_opts = savedata_opts, 
                eval_opts = eval_opts,
				        xy_opts = xy_opts)

  
  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
                         savedata_opts = savedata_opts,
                         eval_opts = eval_opts, 
                         xy_opts = xy_opts))
  savedata_opts <- optslst$savedata_opts  
  eval_opts <- optslst$eval_opts
  xy_opts <- optslst$xy_opts  

  for (i in 1:length(eval_opts)) {
    assign(names(eval_opts)[[i]], eval_opts[[i]])
  }
  for (i in 1:length(xy_opts)) {
    assign(names(xy_opts)[[i]], xy_opts[[i]])
  }
  for (i in 1:length(savedata_opts)) {
    assign(names(savedata_opts)[[i]], savedata_opts[[i]])
  }
  
  ## Set user-supplied dbTabs options
  dbTables_defaults_list <- formals(dbTables)[-length(formals(dbTables))]
  dbTabs2 <- dbTables_defaults_list
  if (length(dbTabs) > 0) {
    for (i in 1:length(dbTabs)) {
      if (names(dbTabs)[[i]] %in% names(dbTables_defaults_list)) {
        if (!is.null(dbTabs[[i]])) {
          dbTabs2[[names(dbTabs)[[i]]]] <- dbTabs[[i]]
        }
      }
    }
  }
  for (i in 1:length(dbTabs2)) {
    assign(names(dbTabs2)[[i]], dbTabs2[[i]])
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
                   intensity <- NULL
  if (intensity1) {
	  intensity <- 1
  }

  ###########################################################################
  ## Check XY database 
  ###########################################################################
  xyindb=plotindb=ppsaindb <- FALSE
  if (!is.null(dbconn) && DBI::dbIsValid(dbconn)) {
    xyconn <- dbconn
    xyisplot <- TRUE
    xy_datsource=datsource <- "sqlite"
    xytablst <- DBI::dbListTables(xyconn)
  } else {
    xy_datsourcelst <- c("datamart", "sqlite", "csv", "obj")
    xy_datsource <- pcheck.varchar(var2check = xy_datsource, 
                                   varnm = "xy_datsource", gui=gui, 
                                   checklst = xy_datsourcelst, 
                                   caption="XY data source?")
    if (is.null(xy_datsource)) {
      if (!is.null(xy_dsn) && !is.null(datsource)) {
        xy_datsource <- datsource
      } else {
        stop("xy_datsource is invalid")
      }
    }
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
    } else if (xy_datsource == "datamart" && datsource == "datamart") {
      xyisplot <- TRUE
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
        datsource <- pcheck.varchar(var2check = datsource, 
                                    varnm = "datsource", gui=gui, 
                                    checklst = datsourcelst, 
                                    caption = "Plot data source?",
                                    stopifnull = TRUE, stopifinvalid = TRUE)
      } else {
        datsource <- xy_datsource
        data_dsn <- xy_dsn
      }
    }

    if (datsource == "sqlite" && !is.null(data_dsn)) {
      if (!is.null(xy_dsn) && data_dsn == xy_dsn) {
        dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      } else {
        dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
      }
      dbtablst <- DBI::dbListTables(dbconn)
      if (length(dbtablst) == 0) {
        message("no data in ", datsource)
	      return(NULL)
      }
    }
    if (datsource == "sqlite" && is.null(data_dsn)) {
      message("datsource=", datsource, " but data_dsn=NULL... returning NULL")
	    return(NULL)
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
  ##########################################################
  if (!is.null(evalInfo)) {
    list.items <- c("states", "invtype")
    evalInfo <- pcheck.object(evalInfo, "evalInfo", list.items=list.items)

  } else {
    evalInfo <- tryCatch(
				DBgetEvalid(states = states, 
                    RS = RS, 
                    datsource = datsource,
                    data_dsn = data_dsn,
                    dbTabs = dbTabs,
                    dbconn = dbconn,
                    dbconnopen = TRUE,
                    invtype = invtype, 
                    evalid = evalid, 
                    evalCur = evalCur, 
                    evalEndyr = evalEndyr, 
                    evalAll = evalAll,
                    evalType = Type,
						        returnPOP = TRUE,
                    gui = gui),
			error = function(e) {
                  message(e,"\n")
                  return(NULL) })
    if (is.null(evalInfo)) {
      iseval <- FALSE
    }
  }
  if (is.null(evalInfo)) {
    message("no data to return")
    return(NULL)
  }
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
  surveynm <- evalInfo$surveynm
  SURVEY <- evalInfo$SURVEY
  ppsanm <- evalInfo$ppsanm
  POP_PLOT_STRATUM_ASSGN <- evalInfo$POP_PLOT_STRATUM_ASSGN
  PLOT <- evalInfo$PLOT
  plotnm <- evalInfo$plotnm

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
    #XYvarlst <- unique(c(XYvarlst, "MEASYEAR", "PLOT_STATUS_CD", "INVYR")) 
    XYvarlst <- unique(c(XYvarlst, "INVYR")) 
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
  
    ## Check pop_plot_stratum_assgn
    if (iseval) {
      if (is.null(POP_PLOT_STRATUM_ASSGN) || !is.data.frame(POP_PLOT_STRATUM_ASSGN)) {
        POP_PLOT_STRATUM_ASSGN <- tryCatch( DBgetCSV("POP_PLOT_STRATUM_ASSGN", 
                             stabbrlst,
                             returnDT = TRUE, 
                             stopifnull = FALSE),
			            error = function(e) {
                  message(e, "\n")
                  return(NULL) }) 
      }				  
    }
	  if (!is.null(POP_PLOT_STRATUM_ASSGN)) {
	    ppsanm <- "POP_PLOT_STRATUM_ASSGN"
	  }
	
    ## Check xy
    XYdf <- pcheck.table(xy, stopifnull=FALSE)
    if (is.null(XYdf)) {
      if (is.character(xy) && exists(xy) && !is.null(get(xy))) {
        XYdf <- get(xy)
      } else { 
        XYdf <- tryCatch( 
            DBgetCSV(xy, 
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
      message("invalid xy")
	    return(NULL)
    }
    ## Check pop_plot_stratum_assgn
  	if (iseval) {
	    if (!is.null(POP_PLOT_STRATUM_ASSGN) && is.data.frame(POP_PLOT_STRATUM_ASSGN)) {
	      ppsanm <- "POP_PLOT_STRATUM_ASSGN"
	    } else if (is.null(POP_PLOT_STRATUM_ASSGN) || !is.character(POP_PLOT_STRATUM_ASSGN)) {
	      ppsanm <- chkdbtab(xytablst, "POP_PLOT_STRATUM_ASSGN", stopifnull=FALSE)
		    if (!is.null(ppsanm)) {
		      ppsaindb <- TRUE
		      ppsaflds <- DBI::dbListFields(xyconn, ppsanm)
		    }
      } else if (is.character(POP_PLOT_STRATUM_ASSGN)) {
	      ppsanm <- POP_PLOT_STRATUM_ASSGN
	    } 	    
	  }
    xynm <- chkdbtab(xytablst, xy, stopifnull=FALSE)
    if (!is.null(xynm)) {
      xyflds <- DBI::dbListFields(xyconn, xynm)
    } else {
      stop(xy, " does not exist in database\n ", toString(xytablst))
    }
	  xyindb <- TRUE
	
  } else {
    if (iseval && is.null(POP_PLOT_STRATUM_ASSGN)) {
	    ppsanm <- dbTables$ppsa_layer
      ppsaqry <- paste0("SELECT * FROM ", ppsanm, " WHERE evalid IN(",
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
		gui=gui, checklst=xyflds, caption="UniqueID variable in xy",
		warn=paste(xy.uniqueid, "not in xy table"), stopifnull=TRUE)

  ## Check xyjoinid
  xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
		              gui=gui, checklst=xyflds, caption="Join variable in xy",
		              stopifnull=FALSE)
  if (is.null(xyjoinid)) {
    xyjoinid <- xy.uniqueid
  }

  ## Check XYdf variables
  ####################################################################
  xyvars <- unique(c(XYvarlst, pvars2keep))
  
  xyvarschk <- unlist(sapply(xyvars, findnm, xyflds, returnNULL = TRUE))
  if (length(xyvarschk) < length(xyvars)) {
    xypmiss <- xyvars[!xyvars %in% names(xyvarschk)]
    if (length(xypmiss) > 0) {
      pvars2keep <- xypmiss
    } else {
      pvars2keep <- NULL
    }
    xyvars <- xyvarschk
  } else {
    pvars2keep <- NULL
  }

  ####################################################################
  ## Check plot table
  ####################################################################
  if (xyisplot || is.null(pvars2keep)) {
    
    ## Check plot table
    if (is.null(xynm) || !is.null(pvars2keep)) {
      if (datsource == "datamart") {
        PLOT <- tryCatch( DBgetCSV("PLOT", 
                                   stabbrlst,
                                   returnDT = TRUE, 
                                   stopifnull = FALSE),
                          error = function(e) {
                            message(e, "\n")
                            return(NULL) })
        if (!is.null(PLOT)) {
          plotnm=xynm <- "PLOT"
          pltflds=xyflds <- names(PLOT)
        }
      } else if (datsource == "sqlite") {
        ## If XY and plot data are from the same databases, 
        ## we will query both
        if (!plotindb) {
          plotnm <- chkdbtab(dbtablst, plot_layer, stopifnull=FALSE)
        }
        if (!is.null(plotnm)) {
          xyindb <- TRUE	 
          xynm <- plotnm
          pltflds=xyflds <- DBI::dbListFields(dbconn, xynm)     
        }
      } else {
        if (!is.null(plotnm)) {
          PLOT <- pcheck.table(plot_layer, stopifnull=TRUE, stopifinvalid=TRUE)
          plotnm=xynm <- "PLOT"
        }
        names(PLOT) <- toupper(names(PLOT))
        pltflds=xyflds <- names(PLOT)
      }
    } else {
      plotnm <- xynm
      pltflds <- xyflds
    }
  
    ## check pjoinid
    pjoinid <- findnm(pjoinid, xyflds, returnNULL=TRUE)
    if (is.null(pjoinid)) {
      pjoinid <- findnm(xyjoinid, xyflds, returnNULL=TRUE)
      if (is.null(pjoinid)) {
        if (xyjoinid == "PLT_CN" && "CN" %in% pltflds) {
          pjoinid <- "CN"
        } else if (xyjoinid == "plt_cn" && "cn" %in% pltflds) {
          pjoinid <- "cn"
        } else {
          stop("pjoinid is invalid")
        }
      }
    }

    if (!is.null(pvars2keep)) {
      pvars2keepchk <- unlist(sapply(pvars2keep, findnm, xyflds, returnNULL = TRUE))
      if (length(pvars2keepchk) < length(pvars2keep)) {
        pmiss <- pvars2keep[!pvars2keep %in% names(pvars2keepchk)]
        pmisschk <- unlist(sapply(pmiss, findnm, XYvarlst, returnNULL = TRUE))
        if (!is.null(pmisschk)) {
          xymiss <- pmisschk
          
          if (length(xymiss) > 0) {
            if (all(c("LON", "LAT") %in% xymiss) && all(c("LON_PUBLIC", "LAT_PUBLIC") %in% xyflds)) {
              xyvars <- c(xyvars[!xyvars %in% c("LON", "LAT")], "LON_PUBLIC", "LAT_PUBLIC")
              xymiss <- xymiss[!xymiss %in% c("LON", "LAT")]
              xvar <- "LON_PUBLIC"
              yvar <- "LAT_PUBLIC"
            } else if (all(c("LON", "LAT") %in% xymiss) && all(c("lon_public", "lat_public") %in% xyflds)) {
              xyvars <- c(xyvars[!xyvars %in% c("lon", "lat")], "lon_public", "lat_public")
              xymiss <- xymiss[!xymiss %in% c("LON", "LAT")]
              xvar <- "lon_public"
              yvar <- "lat_public"
            }
            if (length(xymiss) > 0) {
              stop("missing essential variables: ", toString(xymiss))
            }
          }
        } else {
          message("missing plot variables: ", toString(pmiss))
        }
      }
	  }

	  ## Define xyfromqry
	  xyfromqry <- paste0("\nFROM ", SCHEMA., xynm, " xy")

	  ## Define xyfromqry2
	  if (measCur) {
	    groupvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
	    groupvars <- sapply(groupvars, findnm, xyvars, returnNULL = TRUE)
	    if (any(is.null(groupvars))) {
	      missvars <- groupvars[is.null(groupvars)]
        if (length(missvars) > 1 || missvars != "unitcd") {
		      stop("dataset must include statecd, countycd, and plot")		  
        }
	    } else {
	      groupvars <- as.vector(groupvars)
	    }
	    groupvars <- groupvars[groupvars %in% xyvars]
	
	    xyfromqry2 <- paste0("\nINNER JOIN maxyear ON (")
	    for (i in 1:length(groupvars)) {
	      gvar <- groupvars[i]
	      xyfromqry2 <- paste0(xyfromqry2, "xy.", gvar, " = maxyear.", gvar)	   
	      if (i < length(groupvars)) {
	        xyfromqry2 <- paste0(xyfromqry2, " and ")
	      }
	    }
	    xyfromqry2 <- paste0(xyfromqry2, " and xy.", varCur, " = maxyear.maxyr)")
	  
	  } else {
	    xyfromqry2 <- paste0("\nINNER JOIN p ON (xy.", xyjoinid, " = p.", pjoinid, ")")
	  }
	
#	  xyfromqry <- paste0("\nFROM ", SCHEMA., xynm, " xy",
#                  "\nINNER JOIN p ON(xy.", pjoinid, " = p.", pjoinid, ")")	

    xyfromqry <- paste0(xyfromqry, xyfromqry2)
    xyqry <- paste0("SELECT ", toString(paste0("xy.", xyvars)), 
                  xyfromqry)
    
  } else if (!xyisplot && !is.null(pvars2keep)) {    

    ## Check plot table
    ########################################################
    if (datsource == "datamart") {
	    ## If XY is from database and plot data are from datamart, 
	    ## we will extract xy data by state first
      if (xy_datsource == "sqlite") {
        statenm <- findnm("STATECD", xyflds, returnNULL=TRUE)
        if (is.null(statenm)) {
          stop("must include STATECD in xy dataset")
        } else {
          xy.qry <- paste("SELECT DISTINCT", toString(xyvars), 
                          "\nFROM", xy, 
				                  "\nWHERE statecd in(", toString(stcdlst), ")")
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

	    ## If XY and plot data are from the same databases, 
	    ## we will query both
	    if (xy_datsource == "sqlite" && all.equal(dbconn, xyconn)) {
	      if (!plotindb) {
          plotnm <- chkdbtab(dbtablst, plot_layer, stopifnull=FALSE)
	      }
        if (!is.null(plotnm)) {
          plotindb <- TRUE	  
          pltflds <- DBI::dbListFields(dbconn, plotnm)     
	  
	        ## Check for indices
          if (evalCur || measCur) { 
            pltindx <- sapply(c("STATECD", "UNITCD", "COUNTYCD", "PLOT"), findnm, pltflds, returnNULL = TRUE)
            chk <- checkidx(dbconn, plotnm, pltindx)
            if (is.null(chk)) {
              message("to speed query... add an index to the plot table")
              message("createidx(dbconn, '", plotnm, 
                 "', c('statecd', 'unitcd', 'countycd', 'plot'))")
	          }
          }
        }
      } else {

	      ## If XY and plot data are from different databases, 
		    ## extract both first by state before querying
		    if (!is.null(plotnm)) {
          plot.qry <- paste0("SELECT DISTINCT ", toString(unique(c(pjoinid, pvars2keep))), 
		                         "\nFROM ", plotnm, 
				                     "\nWHERE statecd IN (", toString(stcdlst), ")")
          PLOT <- tryCatch( DBI::dbGetQuery(dbconn, plot.qry),
                            error = function(e) {
                              message(e, "\n")
                              return(NULL) })
          if (is.null(PLOT)) {
            message("invalid query: \n", plot.qry)
            stop()
          }
          PLOT <- setDT(PLOT)
          plotnm <- "PLOT"
		      pltflds <- names(PLOT)
		      datsource <- xy_datsource
		      plotindb <- FALSE
        }
          
        if (datsource == "sqlite") {
          xy.qry <- paste0("SELECT ", toString(xyvars), 
		                     "\nFROM ", xy, 
				                 "\nWHERE statecd IN (", toString(stcdlst), ")")
          XYdf <- tryCatch( DBI::dbGetQuery(xyconn, xy.qry),
                            error = function(e) {
                              message(e, "\n")
                              return(NULL) })
          if (is.null(XYdf)) {
            message("invalid query: \n", xy.qry)
            stop()
          }
          XYdf <- setDT(XYdf)
	      } else {
	        xynm <- "XYdf"
	      }
	    }
    } else {
	    if (!is.null(plotnm)) {
        PLOT <- pcheck.table(plot_layer, stopifnull=TRUE, stopifinvalid=TRUE)
        plotnm <- "PLOT"
	    }
      names(PLOT) <- toupper(names(PLOT))
      pltflds <- names(PLOT)
    }

    ## Check plot variables
    ########################################################
    pvars2keepchk <- unlist(sapply(pvars2keep, findnm, xyflds, returnNULL = TRUE))
    if (length(pvars2keepchk) < length(pvars2keep)) {
      pmiss <- pvars2keep[!pvars2keep %in% names(pvars2keepchk)]

      pmisschk <- unlist(sapply(pmiss, findnm, XYvarlst, returnNULL = TRUE))
      if (!is.null(pmisschk)) {
        xymiss <- pmisschk
        if (length(xymiss) > 0) {
          pltindx <- sapply(c("STATECD", "UNITCD", "COUNTYCD", "PLOT"), findnm, names(XYdf), returnNULL = TRUE)
          if (!is.null(pltindx) && !any(duplicated(XYdf[, pltindx, with=FALSE])) && measCur) {
            measCur <- FALSE
            allyrs <- TRUE
            plotnm=pvars <- NULL
            pjoinid <- xy.uniqueid
          } else {
            #stop("missing essential variables: ", toString(xymiss))
            stop("missing essential variables: ", toString(xymiss))
          }
        } else {
          warning("missing plot variables: ", toString(pmiss))
        }
        if (length(pmiss) < length(pvars2keep)) {
          pvars <- pvars2keep[!pvars2keep %in% pmiss]
        }
      } else {

	      message("missing variable: ", toString(pmiss))
		    pvars <- pvars2keep[!pvars2keep %in% pmiss] 
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
	  }

    ## Define xyfromqry
	  xyfromqry <- paste0("\nFROM ", SCHEMA., xynm, " xy")
	  xyfromqry <- paste0(xyfromqry, "\nINNER JOIN ", plotnm, " p ON(xy.", xyjoinid, " = p.", pjoinid, ")")
	
	  ## Define xyfromqry2
	  if (measCur) {
	    inxy <- TRUE
	    groupvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
	    groupvars <- sapply(groupvars, findnm, xyvars, returnNULL = TRUE)

	    if (any(is.null(groupvars))) {
	      missvars <- groupvars[is.null(groupvars)]
        if (length(missvars) > 1 || missvars != "unitcd") {
		      inxy <- FALSE	
          groupvars <- sapply(groupvars, findnm, pvars, returnNULL = TRUE)	
	        if (any(is.null(groupvars))) {
	          missvars <- groupvars[is.null(groupvars)]
            if (length(missvars) > 1 || missvars != "unitcd") {
			        stop("dataset must include statecd, countycd, and plot")
		        }
		      }
        }
	    } else {
	      groupvars <- as.vector(groupvars)
	    }
	    groupvars <- groupvars[groupvars %in% xyvars]
	  
	    alias. <- ifelse (inxy, "xy.", "p.")
	    xyfromqry2 <- paste0("\nINNER JOIN maxyear ON (")
	    for (i in 1:length(groupvars)) {
	      gvar <- groupvars[i]
	      xyfromqry2 <- paste0(xyfromqry2, alias., gvar, " = maxyear.", gvar)	   
	      if (i < length(groupvars)) {
	        xyfromqry2 <- paste0(xyfromqry2, " and ")
	      }
	    }
	    valias. <- ifelse (varCur %in% xyvars, "xy.", "p.")
	    xyfromqry2 <- paste0(xyfromqry2, " and ", valias., varCur, " = maxyear.maxyr)")
      #xyfromqry <- paste0(xyfromqry, xyfromqry2)	  
	  } 
	  if (is.null(pvars)) {
	    xyqry <- paste0("SELECT ", toString(c(paste0("xy.", xyvars))), 
	                    xyfromqry)
	  } else {
	    xyqry <- paste0("SELECT ", toString(c(paste0("xy.", xyvars), paste0("p.", pvars))), 
             xyfromqry)	
	  }
    ## Inventory year table query
    yrvar <- ifelse(!is.null(invyrs), "INVYR", "MEASYEAR")
	  yrvarnmA=stcdnmA <- NULL
    if (!is.null(pvars)) {
      yrvarnm <- findnm(yrvar, pvars, returnNULL=TRUE)
	    if (!is.null(yrvarnm)) {
	      yrvarnmA <- paste0("pp.", yrvarnm)
	    }
      stcdnm <- findnm("STATECD", pvars, returnNULL=TRUE)
	    if (!is.null(stcdnm)) {
	      stcdnmA <- paste0("pp.", stcdnm)
	    }
    }
	  if (!is.null(xyvars)) {
	    if (is.null(yrvarnmA)) {
        yrvarnm <- findnm(yrvar, xyvars, returnNULL=TRUE)
	      if (!is.null(yrvarnm)) {
	        yrvarnmA <- paste0("pp.", yrvarnm)
	      }
	    }
 	    if (is.null(stcdnmA)) {
        stcdnm <- findnm("STATECD", xyvars, returnNULL=TRUE)
	      if (!is.null(stcdnm)) {
	        stcdnmA <- paste0("pp.", stcdnm)
	      }
	    }
    }

    if (!all(is.null(yrvarnmA), is.null(stcdnmA))) {
      invarsA <- toString(c(stcdnmA, yrvarnmA))
      invyrtab.qry <- paste0("SELECT distinct ", invarsA, ", COUNT(*)", 
		                         xyfromqry,
				                    "\n GROUP BY ", invarsA, 
				                    "\n ORDER BY ", invarsA) 
    }
  }

  ###########################################################################
  ## Build filter
  ###########################################################################
  if (xyisplot || is.null(plotnm)) {
    pnm <- xynm
    pid <- xy.uniqueid
    pflds <- xyflds
	  pltvars <- xyvars
  } else {
    pnm <- plotnm
    pid <- pjoinid
	  pflds <- pltflds
	  pltvars <- pvars
  }
  popSURVEY <- FALSE
  if (!is.null(SURVEY) && "SRV_CN" %in% pltflds) {
    popSURVEY <- TRUE
  }

  ## If iseval = TRUE 
  if (iseval) {
  
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

	  if (exists(pnm) && !is.function(get(pnm)) &&!is.null(get(pnm))) {
      setkeyv(get(pnm), pid)
	  } 
    withqry <- getpwithqry(evalid = unlist(evalidlist), 
                           pjoinid = pid,
                           intensity = intensity,
                           plotnm = pnm,
		                       pltflds = pflds,
                           ppsanm = ppsanm,
		                       ppsaflds = ppsaflds,
		                       pvars = pvars)
  } else {

    ## Get statecd for filter
    stcds <- pcheck.states(states, "VALUE")
    statecdnm <- findnm("STATECD", xyvars, returnNULL=TRUE)
    stabbr <- pcheck.states(states, "ABBR")
 
    if (length(unlist(invyrs)) > 1) {
	
	    if (exists(pnm) && !is.function(get(pnm)) &&!is.null(get(pnm))) {
		    setkeyv(get(pnm), pid)
	    } 
	    withqry <- getpwithqry(states = stcds, 
                           pjoinid = pid,
                           intensity = intensity,
                           plotnm = pnm,
		                       pltflds = pflds,
                           invyrs = unlist(invyrs),
		                       pvars = pltvars,
		                       popSURVEY = popSURVEY,
		                       surveynm = surveynm,
		                       Type = Type)

    } else if (length(unlist(measyrs)) > 1) {

	    if (exists(pnm) && !is.function(get(pnm)) &&!is.null(get(pnm))) {
		    setkeyv(get(pnm), pid)
	    } 
	
      withqry <- getpwithqry(states = stcds, 
                             pjoinid = pid,
                             intensity = intensity,
                             plotnm = pnm,
		                         pltflds = pflds,
                             measyears = unlist(measyrs),
		                         pvars = pltvars,
		                         popSURVEY = popSURVEY,
		                         surveynm = surveynm,
		                         Type = Type) 
		   
    } else if (measCur) {

      ## Set key variable in pnm
	    if (exists(pnm) && !is.function(get(pnm)) &&!is.null(get(pnm))) {
	      keyvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR")
        keyvars <- keyvars[keyvars %in% xyflds]
		    setkeyv(get(pnm), keyvars)
	    } 
      withqry <- getpwithqry(states = stcds, 
	                           plotCur = TRUE,
	                           Endyr = measEndyr,	                        
                             varCur = varCur, 
                             SCHEMA. = SCHEMA., 
                             intensity = intensity, 
                             plotnm = pnm,
                             pjoinid = pid,
                             surveynm = surveynm,
                             popSURVEY = popSURVEY,
							               pltflds = pflds,
							               pvars = pltvars,
                             Type = Type)

    } else if (allyrs) {

	    if (exists(pnm) && !is.function(get(pnm)) &&!is.null(get(pnm))) {
		    setkeyv(get(pnm), pid)
	    } 
	
      withqry <- getpwithqry(states = stcds, 
                             pjoinid = pid,
                             intensity = intensity,
                             plotnm = pnm,
		                         pltflds = pflds,
                             allyrs = TRUE,
		                         pvars = pltvars,
		                         popSURVEY = popSURVEY,
		                         surveynm = surveynm,
		                         Type = Type) 
	  }
  }
 
  ##################################################################################
  ##################################################################################
  ## Generate queries
  ##################################################################################

  ## Create invyrtab query 
  ###########################################################
  xycoords.qry <- paste0(withqry, "\n", xyqry)
  message(xycoords.qry)

 if (xy_datsource == "sqlite") {
    xyx <- tryCatch( DBI::dbGetQuery(xyconn, xycoords.qry),
			          error = function(e) {
                  message(e, "\n")
                  return(NULL) })
    if (!iseval && is.null(invyrtab) && !is.null(invyrtab.qry)) {
      invyrtab <- tryCatch( DBI::dbGetQuery(xyconn, invyrtab.qry),
			           error = function(e) {
                  message(e, "\n")
                  return(NULL) }) 
    }      
  } else {

    xyx <- tryCatch( sqldf::sqldf(xycoords.qry, connection = NULL,
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
    warning("invalid xy query\n")
    message(xycoords.qry)
    stop()
  }

  if (nrow(xyx) > length(unique(xyx[[xy.uniqueid]]))) {
    xyx <- unique(xyx)
  }

  ## Change CN to PLT_CN if exists
  if ("CN" %in% names(xyx) && !"PLT_CN" %in% names(xyx)) {
    setnames(xyx, "CN", "PLT_CN")
    xy.uniqueid <- "PLT_CN"
    xyjoinid <- "PLT_CN"
  } else if ("cn" %in% names(xyx) && !"plt_cn" %in% names(xyx)) {
    setnames(xyx, "cn", "plt_cn")
    xy.uniqueid <- "plt_cn"
    xyjoinid <- "plt_cn"
  }

  ## Remove KNOWN plots that are no longer in inventory
  if (measCur || !is.null(measEndyr)) {
    xyx <- xyx[!xyx$PLT_CN %in% FIESTAutils::kindcd3old$CN, ]
  }
   
  if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% names(xyx))) {
    xyx[["PLOT_ID"]] <- paste0("PID", 
		formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$UNITCD, width=2, digits=2, flag=0),
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(xyx$PLOT, width=5, digits=5, flag=0)) 
  } else if (all(c("statecd", "unitcd", "countycd", "plot") %in% names(xyx))) {
    xyx[["plot_id"]] <- paste0("pid", 
            formatC(xyx$statecd, width=2, digits=2, flag=0), 
            formatC(xyx$unitcd, width=2, digits=2, flag=0),
            formatC(xyx$countycd, width=3, digits=3, flag=0),
            formatC(xyx$plot, width=5, digits=5, flag=0)) 
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
  } else {
    if (issp) {
	  outsp_layer <- paste0("sp", out_layer)
	}
  }

  if (issp) {
    spxyoutnm <- paste0("sp", xyoutnm)
     
    if (all(c(xvar, yvar) %in% names(xyx))) {

      ## Generate shapefile
      assign(spxyoutnm, spMakeSpatialPoints(xyplt = xyx, 
                      xvar = xvar, yvar = yvar, xy.uniqueid = xy.uniqueid, 
                      xy.crs = 4269, addxy = FALSE, 
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

  ## Disconnect database
  if (!is.null(dbconn) && !dbconnopen && DBI::dbIsValid(dbconn)) {
    DBI::dbDisconnect(dbconn)
  } 

}

