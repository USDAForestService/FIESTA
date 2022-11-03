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
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed.
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL').  Only one inventory type (PERIODIC/ANNUAL) at a time.
#' @param evalid Integer. Inventory span defining variable. Extract public 
#' XY coordinates for a specific FIA Evaluation (See details for more 
#' information about FIA Evaluations).
#' @param evalCur Logical. Inventory span defining variable. If TRUE, extract
#' public XY coordinates for the most current FIA Evaluation for each state.
#' @param evalEndyr YYYY. Inventory span defining variable. Extract public XY 
#' coordinates for the FIA Evaluation(s) ending in the specified evalEndyr(s). 
#' If more than one state and different Evaluations by state are desired, 
#' input a named list object with evalEndyr by state (e.g., list(Utah=2014, 
#' Colorado=2013).
#' @param evalAll Logical. Inventory span defining variable. If TRUE, extract
#' public XY coordinates for all FIA Evaluations for each state.
#' @param evalType String vector. The type(s) of FIA Evaluation of interest 
#' ('ALL', 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN'). 
#' The evalType 'ALL' includes nonsampled plot coordinates; 'CURR' includes 
#' plot coordinates used for area estimates; 'VOL' includes plot coordinates 
#' used for area and/or tree estimates; 'GRM' includes plot coordinates used 
#' for growth, removals, mortality, and change estimates (evalType %in% 
#' c(GROW, MORT, REMV, CHNG)). Multiple types are accepted. See details below 
#' and FIA database manual for regional availability and/or differences.
#' @param measCur Logical. Inventory span defining variable. If TRUE, extract
#' public XY coordinates with most current sampled measurement for state(s).
#' @param measEndyr Logical. Inventory span defining variable. If TRUE, extract
#' XY public coordinates with most current sampled measurement for state(s) 
#' for years measured in or before measEndyr.
#' @param allyrs Logical. Inventory span defining variable. If TRUE, extract
#' all annual inventory years in database for each state.
#' @param invyrs YYYY vector. Inventory span defining variable. Extract public
#' XY coordinates by state for the specified inventory year(s) (e.g., 
#' c(2000, 2001, 2002)). If more than one state and different inventory years 
#' are desired, input a named list object with years labeled by state 
#' (e.g., list(Utah=2000:2009, Colorado=c(2002,2003,2005)).
#' @param measyrs YYYY vector. Measurement year span defining variable. Extract
#' public XY coordinates by state for the specified measurement year(s) 
#' (e.g., c(2000, 2001, 2002)). If more than one state and different 
#' measurement years are desired, input a named list object with years labeled 
#' by state (e.g., list(Utah=2000:2009, Colorado=c(2002,2003,2005)).
#' @param intensity1 Logical. If TRUE, includes only XY coordinates where 
#' INTENSITY = 1 (FIA base grid).
#' @param issp Logical. If TRUE, returns spatial XY data as a list object with
#' query.
#' @param returndata Logical. If TRUE, returns XY data as a list object with
#' query.
#' @param savedata Logical. If TRUE, saves XY data to outfolder as comma-delimited
#' file (*.csv).
#' @param exportsp Logical. If TRUE, exports data as spatial. 
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
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
#'                    measCur = TRUE)
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
                         dbconn = NULL, 
                         dbconnopen = FALSE,
                         invtype = "ANNUAL", 
                         evalid = NULL, 
                         evalCur = FALSE, 
                         evalEndyr = NULL, 
                         evalAll = FALSE, 
                         evalType = "ALL", 
                         measCur = FALSE, 
                         measEndyr = NULL, 
                         allyrs = FALSE, 
                         invyrs = NULL, 
                         measyrs = NULL, 
                         intensity1 = FALSE, 
                         issp = FALSE, 
                         returndata = TRUE, 
                         savedata = FALSE, 
                         exportsp = FALSE,
                         savedata_opts = NULL){

  ## DESCRIPTION: Get the most current coordinates in the FIA database
  
  gui <- FALSE
  if (gui) {
    evalCur=evalAll=measCur=allyrs=intensity1=
	savedata=parameters=out_fmt=overwrite <- NULL
  }
    
  
  ## Set global variables
  parameters <- FALSE
  xymeasCur <- FALSE
  coords <- "PUBLIC"
  ppsanm <- NULL
  
  
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
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
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

  if (!is.null(dbconn) && DBI::dbIsValid(dbconn)) {
    datsource == "sqlite"
    dbtablst <- DBI::dbListTables(dbconn)
    if (length(dbtablst) == 0) {
      stop("no data in database")
    }
  } else {
    datsourcelst <- c("sqlite", "datamart")
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

  ## Get PLOT name or data tables 
  if (datsource == "sqlite") {
    plotnm <- chkdbtab(dbtablst, "PLOT")
  } else {
    PLOT <- DBgetCSV("PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)
    plotnm <- "PLOT"
  }
  ## Get pop_plot_stratum_assgn
  if (iseval) {
    if (datsource == "sqlite") {
      ppsanm <- chkdbtab(dbtablst, "POP_PLOT_STRATUM_ASSGN")
      if (is.null(ppsanm)) {
        ppsanm <- chkdbtab(dbtablst, "ppsa", stopifnull=TRUE)
      }
    } else {
      POP_PLOT_STRATUM_ASSGN <- DBgetCSV("POP_PLOT_STRATUM_ASSGN", 
		stabbr, returnDT=TRUE, stopifnull=FALSE)
      ppsanm <- "POP_PLOT_STRATUM_ASSGN"
    }
  }


  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)
 
  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states = states, 
                          RS = RS, 
                          datsource = datsource,
                          data_dsn = dsn,
                          dbconn = dbconn,
                          invtype = invtype, 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll,
                          evalType = evalType,
                          ppsanm = ppsanm)
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
  survey <- evalInfo$SURVEY

  ## Get state abbreviations and codes 
  ###########################################################
  stabbrlst <- pcheck.states(states, statereturn="ABBR")
  stcdlst <- pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)


  ## If using EVALID, you don't need to get INVYRS, intensity
  if (!iseval) { 
    ## Check custom Evaluation data
    #############################################
    evalchk <- customEvalchk(states = states, 
                    measCur = measCur, 
                    measEndyr = measEndyr, 
                    measEndyr.filter = measEndyr.filter, 
                    allyrs = allyrs, 
                    invyrs = invyrs, 
                    measyrs = measyrs, 
                    intensity = intensity)
    measCur <- evalchk$measCur
    measEndyr <- evalchk$measEndyr
    measEndyr.filter <- evalchk$measEndyr.filter
    allyrs <- evalchk$allyrs
    invyrs <- evalchk$invyrs
    measyrs <- evalchk$measyrs
  } else {
    allyrs <- FALSE
  }

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


  ###########################################################################
  ## Build filter
  ###########################################################################
  ## Create filter for state
  stcds <- pcheck.states(states, "VALUE")
  stFilter <- paste0("p.STATECD IN(", toString(stcds), ")")
  evalFilter=xyfromqry <- NULL
  stabbr <- pcheck.states(states, "ABBR")
  SCHEMA. <- NULL

  ## PLOT from/join query
  if (iseval) {
    pfromqry <- paste0(SCHEMA., ppsanm, " ppsa")
    xyfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., plotnm, 
			" p ON (p.CN = ppsa.PLT_CN)")
    evalFilter <- paste0("ppsa.EVALID IN(", toString(unlist(evalidlist)), ")")

  } else if (length(unlist(invyrs)) > 1) {
    xyfromqry <- paste0(SCHEMA., plotnm, " p")
    evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(unlist(invyrs)), ")")

  } else if (length(unlist(measyrs)) > 1) {
    xyfromqry <- paste0(SCHEMA., plotnm, " p")
    evalFilter <- paste0(stFilter, " and p.MEASYEAR IN(", toString(unlist(measyrs)), ")")

  } else {
    if (measCur) {
      xyfromqry <- getpfromqry(Endyr = measEndyr, 
                               SCHEMA. = SCHEMA., 
                               intensity1 = intensity1, 
                               popSURVEY = TRUE, 
                               plotnm = plotnm)
    } else {
      xyfromqry <- paste0(SCHEMA., plotnm, " p")
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
  ## VARIABLES
  XYvarlst <- c("CN", "LON", "LAT", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "INTENSITY")
  xycoords.qry <- paste0("select ", 
		toString(paste0("p.", XYvarlst)), 
		" from ", xyfromqry,
		" where ", evalFilter)
  if (datsource == "sqlite") {
    xyx <- tryCatch( DBI::dbGetQuery(dbconn, xycoords.qry),
			error = function(e) {
                  message(e)
                  return(NULL) })
  } else {
    xyx <- tryCatch( sqldf::sqldf(xycoords.qry, stringsAsFactors = FALSE), 
			error = function(e) {
                  message(e)
                  return(NULL) })
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
    setnames(xyx, c("LON", "LAT"), c("LON_PUBLIC", "LAT_PUBLIC"))
  }
  if (all(c("STATECD", "COUNTYCD") %in% names(xyx))) {
    xyx$COUNTYFIPS <- paste0(formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          		formatC(xyx$COUNTYCD, width=3, digits=3, flag=0))
  }
  if (measCur) {
    xynm <- paste0("xyCur_", coords)
    assign(xynm, xyx)
  } else if (evalCur) {
    xynm <- paste0("xy_", coords)
    assign(xynm, xyx)
  } else {
    xynm <- paste0("xy_", coords)
    assign(xynm, xyx)
  } 
  if (is.null(out_layer)) {
    out_layer <- xynm
  }
 
  if (issp) {
    spxynm <- paste0("sp", xynm)
     
    if (all(c("LON_PUBLIC", "LAT_PUBLIC") %in% names(xyx))) {

      ## Generate shapefile
      assign(spxynm, spMakeSpatialPoints(xyplt=xyx, xvar="LON_PUBLIC", 
		        yvar="LAT_PUBLIC", xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE, 
		        exportsp=exportsp, 
		        savedata_opts=list(out_dsn=out_dsn, 
                               out_fmt=outsp_fmt,
                               outfolder=outfolder, out_layer=spxynm, 
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
    
    datExportData(get(xynm),  
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
    fiadatlst <- list()
    if (issp) {
      fiadatlst[[xynm]] <- get(spxynm)
    } else {
      fiadatlst[[xynm]] <- get(xynm)
    }
    fiadatlst[["xyqry"]] <- xycoords.qry
    fiadatlst$xvar <- "LON_PUBLIC"
    fiadatlst$yvar <- "LAT_PUBLIC"
    
    ## Return data list
    return(fiadatlst)
  } 

  if (datsource == "sqlite" && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
 
}
