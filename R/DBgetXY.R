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

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states = states, 
                          RS = RS, 
                          invtype = invtype, 
                          evalid = evalid, 
                          evalCur = evalCur, 
                          evalEndyr = evalEndyr, 
                          evalAll = evalAll,
                          evalType = evalType)
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

    ### Check measCur
    ###########################################################
    measCur <- pcheck.logical(measCur, varnm="measCur", 
			title="Current measyear?", first="YES", gui=gui)

    ### Check measEndyr
    ###########################################################
    measEndyr.filter <- NULL
    if (!is.null(measEndyr)) {
      minyr <- min(invyrtab$INVYR)
      if (!is.numeric(measEndyr) || measEndyr < minyr)
        stop("measEndyr must be yyyy format and greater than minimum inventory year: ", minyr)
      measCur <- TRUE
      measEndyr.filter <- paste0(" and MEASYEAR < ", measEndyr)
    }
    if (measCur) {
      xymeasCur <- TRUE
      allyrs <- FALSE
    }  

    ### GET allyrs
    ###########################################################
    allyrs <- pcheck.logical(allyrs, varnm="allyrs", 
		title="All years?", first="YES", gui=gui)
    if (allyrs) {
      ## xymeasCur
      xymeasCur <- pcheck.logical(xymeasCur, varnm="xymeasCur", 
                                  title="Most current XY?", first="YES", gui=gui)
      measCur <- FALSE
      measEndyr=measEndyr.filter <- NULL
    }
 
    ## Check INVYR(S) 
    ###########################################################
    if (!measCur) {
      if ((is.null(invyrs) || length(invyrs) == 0) && 
			(is.null(measyrs) || length(measyrs) == 0)) {
        invyrs <- sapply(states, function(x) NULL)
        for (state in states) { 
          stabbr <- pcheck.states(state, "ABBR")
          stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])

          if (allyrs) {
            invyr <- stinvyrlst
          } else {
            if (!gui) stop("need to specify a timeframe for plot data")

            ## GET INVENTORY YEAR(S) FROM USER
            invyr <- select.list(as.character(stinvyrlst), 
                        title=paste("Inventory year(s) -", stabbr), multiple=TRUE)
            if (length(invyr) == 0) stop("")
          }
          invyrs[[state]] <- as.numeric(invyr)
        }
      } else if (!is.null(invyrs)) {
        if (!is(invyrs, "list")) {
          if (is.vector(invyrs) && is.numeric(invyrs)) {
            invyrs <- list(invyrs)
            if (length(states) == 1) {
              names(invyrs) <- states
            } else {
              warning("using specified invyrs for all states")
              yrs <- invyrs
              invyrs <- sapply(states, function(x) NULL)
              for (st in states) invyrs[st] <- yrs
            } 
          }
        } else if (length(invyrs) != length(states)) {
          stop("check invyrs list.. does not match number of states")
        }
        ## Check inventory years
        for (state in states) {
          stinvyrlst <- invyrtab[invyrtab$STATENM == state, "INVYR"]
          if (!all(invyrs[[state]] %in% stinvyrlst))
            stop("inventory years do not match database")
        }
      } else if (!is.null(measyrs)) {
        if (!is(measyrs, "list")) {
          if (is.vector(measyrs) && is.numeric(measyrs)) {
            measyrs <- list(measyrs)
            if (length(states) == 1) {
              names(measyrs) <- states
            } else {
              warning("using specified invyrs for all states")
              yrs <- measyrs
              measyrs <- sapply(states, function(x) NULL)
              for (st in states) measyrs[st] <- yrs
            } 
          }
        } else if (length(measyrs) != length(states)) {
          stop("check measyrs list.. does not match number of states")
        }
        ## Check inventory years
        for (state in states) {
          stinvyrlst <- invyrtab[invyrtab$STATENM == state, "INVYR"]
          if (!all(measyrs[[state]] %in% stinvyrlst))
            stop("measurement years do not match database")
        }
      }
    }

    ## Check intensity1
    ###########################################################
    ## For periodic data, the INTENSITY variable does not equal 1
    if (invtype == "ANNUAL") {
      intensity1 <- pcheck.logical(intensity1, varnm="intensity1", 
                            title="Intensity = 1?", first="YES", gui=gui)
    } else {
      intensity1 <- FALSE
    }
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
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
          out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
          overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
          add_layer=add_layer, append_layer=append_layer, gui=gui)
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
    pfromqry <- paste0(SCHEMA., "POP_PLOT_STRATUM_ASSGN ppsa")
    xyfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
			"PLOT p ON (p.CN = ppsa.PLT_CN)")
    evalFilter <- paste0("ppsa.EVALID IN(", toString(unlist(evalidlist)), ")")

  } else if (length(unlist(invyrs)) > 1) {
    xyfromqry <- paste0(SCHEMA., "PLOT p")
    evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(unlist(invyrs)), ")")

  } else if (length(unlist(measyrs)) > 1) {
    xyfromqry <- paste0(SCHEMA., "PLOT p")
    evalFilter <- paste0(stFilter, " and p.MEASYEAR IN(", toString(unlist(measyrs)), ")")

  } else {
    if (measCur) {
      xyfromqry <- getpfromqry(Endyr=measEndyr, 
                               SCHEMA.=SCHEMA., 
                               intensity1=intensity1, 
                               popSURVEY=TRUE, plotnm="PLOT")
    } else {
      xyfromqry <- paste0(SCHEMA., "PLOT p")
    }
    evalFilter <- stFilter 
  }
  if (intensity1) {
    evalFilter <- paste(evalFilter, "and p.INTENSITY = '1'")
  } 

  ##################################################################################
  ##################################################################################

  ## Get PLOT table  
  PLOT <- FIESTA::DBgetCSV("PLOT", stabbr, returnDT=TRUE, stopifnull=FALSE)

  if (iseval) {
    POP_PLOT_STRATUM_ASSGN <- FIESTA::DBgetCSV("POP_PLOT_STRATUM_ASSGN", 
		stabbr, returnDT=TRUE, stopifnull=FALSE)
  }

  ## Generate queries
  ##################################################################################
  ## VARIABLES
  XYvarlst <- c("CN", "LON", "LAT", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "INTENSITY")
  xycoords.qry <- paste0("select ", 
		toString(paste0("p.", XYvarlst)), 
		" from ", xyfromqry,
		" where ", evalFilter)
  xyx <- tryCatch( sqldf::sqldf(xycoords.qry), 
			error=function(e) {
                  message(e)
                  return(NULL) })
  if (is.null(xyx)) {
    message("invalid xy query\n")
    message(xycoords.qry)
    stop()
  }
  xyx <- xyx[!xyx$CN %in% FIESTAutils::kindcd3old$CN, ]
    
  setnames(xyx, "CN", "PLT_CN")
  xyx[["PLOT_ID"]] <- paste0("ID", 
		formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$UNITCD, width=2, digits=2, flag=0),
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(xyx$PLOT, width=5, digits=5, flag=0)) 
  setnames(xyx, c("LON", "LAT"), c("LON_PUBLIC", "LAT_PUBLIC"))
  xyx$COUNTYFIPS <- paste0(formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          		formatC(xyx$COUNTYCD, width=3, digits=3, flag=0))

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
    
    ## Generate shapefile
    assign(spxynm, spMakeSpatialPoints(xyplt=xyx, xvar="LON_PUBLIC", 
		        yvar="LAT_PUBLIC", xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE, 
		        exportsp=exportsp, 
		        savedata_opts=list(out_dsn=out_dsn, out_fmt=outsp_fmt, 
		                  outfolder=outfolder, out_layer=spxynm, 
		                  outfn.date=outfn.date, overwrite_layer=overwrite_layer, 
		                  append_layer=append_layer, outfn.pre=outfn.pre) ))
  }
     
  ###############################################################################
  ## SAVE data
  ###############################################################################
  if (savedata) {
    index.unique.xyplt <- "PLT_CN"
    
    datExportData(get(xynm),  
          index.unique = index.unique.xyplt,
          savedata_opts = list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer=out_layer,
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer, 
                              add_layer=TRUE))
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
  ## Close database connection
  #DBI::dbDisconnect(dbconn)
 
}
