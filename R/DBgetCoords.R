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
#' @param RS String vector. Name of research station(s)
#' ('RMRS','SRS','NCRS','NERS','PNWRS').  Do not use if states is populated.
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL').  Only one inventory type (PERIODIC/ANNUAL) at a time.
#' @param evalid Integer. Inventory span defining variable. Extract data for a
#' specific evaluation period (See details for more information about FIA
#' Evaluations).
#' @param evalCur Logical. Inventory span defining variable. If TRUE, extract
#' data for the most current FIA Evalidation for each state.
#' @param evalEndyr YYYY. Inventory span defining variable. Extract data for
#' the Evaluation(s) ending in the specified evalEndyr(s). If more than one
#' state and different Evaluations by state are desired, input a named list
#' object with evalEndyr by state (e.g., list(Utah=2014, Colorado=2013).
#' @param evalAll Logical. Inventory span defining variable. If TRUE, extract
#' data for all Evaluations for each state.
#' @param measCur Logical. Inventory span defining variable. If TRUE, extract
#' plots with most current measurement for state(s).
#' @param measEndyr Logical. Inventory span defining variable. If TRUE, extract
#' plots with most current measurement for state(s) for years measured in or
#' before measEndyr.
#' @param allyrs Logical. Inventory span defining variable. If TRUE, extract
#' all annual inventory years in database for each state.
#' @param invyrs YYYY vector. Inventory span defining variable. Extract data by
#' state for the specified inventory year(s) (e.g., c(2000, 2001, 2002)). If
#' more than one state and different inventory years are desired, input a named
#' list object with years labeled by state (e.g., list(Utah=2000:2009,
#' Colorado=c(2002,2003,2005)).
#' @param measyrs YYYY vector. Measurement year span defining variable. Extract
#' data by state for the specified measurement year(s) (e.g., c(2000, 2001,
#' 2002)). If more than one state and different inventory years are desired,
#' input a named list object with years labeled by state (e.g.,
#' list(Utah=2000:2009, Colorado=c(2002,2003,2005)).
#' @param intensity1 Logical. If TRUE, includes only plots where INTENSITY = 1.
#' @param issp Logical. If TRUE, returns spatial data as a list object with
#' query.
#' @param returndata Logical. If TRUE, returns data as a list object with
#' query.
#' @param savedata Logical. If TRUE, saves data to outfolder as comma-delimited
#' file (*.csv).
#' @param outfolder String. The output folder path. If NULL and savedata=TRUE
#' or parameters=TRUE or isshp=TRUE, outfolder is the working directory.
#' @param out_fmt String. File format for output ('csv', 'sqlite','gpkg',
#' 'gdb').  If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be
#' installed. If out_fmt='gdb', arcgisbinding package and R-Bridge must be
#' installed.
#' @param out_dsn String. Data source name for output. If extension is not
#' included, out_fmt is used. Use full path if outfolder=NULL.
#' @param out_layer String. Name of file, out_fmt = 'csv', or name of layer in
#' out_dsn, if out_fmt != 'csv'.
#' @param append_layer Logical. If TRUE, appends to existing out_dsn. The
#' out_dsn a database or shapefile. If FALSE, the out_dsn will be overwritten
#' if exists.
#' @param outfn.pre String. The name used for prefix of outfiles (e.g.,
#' outfn.pre'_plt*'.)
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param overwrite_dsn Logical. If TRUE and out_fmt = 'sqlite', the out_dsn is
#' overwritten.
#' @param overwrite_layer Logical. If TRUE and out_fmt = 'csv', files are
#' overwritten.  If out_fmt != 'csv', the layer in database is overwritten.
#' @return fiadat - a list of the following objects: \item{xy*_ACTUAL}{ Data
#' frame. XY data from FS_FIADB_NIMS_*.SDS_PLOT.  xyCur_ACTUAL - if
#' measCur=TRUE, xy_ACTUAL otherwise. } \item{xyqry}{ String. Query to extract
#' coordinates }
#' 
#' If savedata=TRUE, outputs data as out_fmt to outfolder.  If out_fmt =
#' 'sqlite' and issp = TRUE, the output will be a SpataiLite database.
#' @note
#' 
#' If no parameters are included, the user is prompted for input. If partial
#' parameters, the default parameter values are used for those not specified.
#' This function will not function without a working ODBC connection to FIA's
#' Oracle database or
#' 
#' \bold{Data Access}
#' 
#' Access to Oracle is only available through FIA's security policies and
#' select permission must be granted to query Oracle's tables. Contact your
#' local Oracle database manager to grant access. See details for more
#' information.
#' 
#' Access to FIA's Oracle database also requires a compliant ODBC (Open
#' Database Connectivity) connection with the local TNS (Transparent Network
#' Substrate) name.  ODBC refers to a database driver on the client computer
#' which translates queries from client applications into commands the database
#' understands.  Use DBtestOracle() to test your connection to the Oracle
#' database. See your local IT or Oracle administrator if connection is
#' unsuccessful.
#' 
#' If states intersect more than one FIA Research Station (RS), you must have
#' accesss to FS_FIADB::SDS_PLOT to extract coordinates. Include RS unit to use
#' regional SDS tables ('FS_NIMS_FIADB_', RS, '.SDS_PLOT')
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' 
#' 
#'   # Most current evaluation and shapefile with public coordinates
#'   COxylst <- DBgetCoords(states="Colorado", measCur=TRUE, RS="RMRS")
#'   names(COxylst)
#' 
#'   head(COxylst$xyCur_ACTUAL)
#'   COxylst$xyqry
#' 
#' @export DBgetCoords
DBgetCoords <- function (states=NULL, RS=NULL, invtype="ANNUAL", 
	evalid=NULL, evalCur=FALSE, evalEndyr=NULL, evalAll=FALSE, 
	measCur=FALSE, measEndyr=NULL, allyrs=FALSE, invyrs=NULL,
	measyrs=NULL, intensity1=FALSE, issp=FALSE, returndata=TRUE, 
	savedata=FALSE, outfolder=NULL, out_fmt="csv", out_dsn=NULL, 
	out_layer="xyplt", append_layer=FALSE, outfn.pre=NULL, 
	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE) {

  ## DESCRIPTION: Get the most current coordinates in the FIA database
  
  gui <- FALSE
  if (gui) {
    evalCur=evalAll=measCur=allyrs=intensity1=
	savedata=parameters=out_fmt=overwrite <- NULL
  }

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  if (!"sqldf" %in% rownames(installed.packages())) {
    message("the sqldf package is required when datsource='ORACLE'")
  }

  ## SET OPTIONS
  options.old <- options()
  options(scipen=8) # bias against scientific notation
  on.exit(options(options.old), add=TRUE) 


  ## Check arguments
  ###########################################################
  input.params <- names(as.list(match.call()))[-1]
  if (!all(input.params %in% names(formals(DBgetCoords)))) {
    miss <- input.params[!input.params %in% formals(DBgetCoords)]
    stop("invalid parameter: ", toString(miss))
  } 

  ## Define variables
  parameters <- FALSE
  xymeasCur <- FALSE
  coords <- "PUBLIC"

  ########################################################################
  ### GET PARAMETERS 
  ########################################################################
  iseval <- FALSE

  ## Check invtype
  invtypelst <- c('ANNUAL', 'PERIODIC')
  invtype <- pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states=states, RS=RS, invtype=invtype, 
	evalid=evalid, evalCur=evalCur, evalEndyr=evalEndyr, evalAll=evalAll)
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
        if (class(invyrs) != "list") {
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
        if (class(measyrs) != "list") {
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
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, append_layer=append_layer, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
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
      xyfromqry <- getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA., 
				intensity1=intensity1, popSURVEY=TRUE, plotnm="PLOT")
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
  setnames(xyx, "CN", "PLT_CN")
  xyx[["PLOT_ID"]] <- paste0("ID", 
		formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          	formatC(xyx$UNITCD, width=2, digits=2, flag=0),
          	formatC(xyx$COUNTYCD, width=3, digits=3, flag=0),
          	formatC(xyx$PLOT, width=5, digits=5, flag=0)) 
  setnames(xyx, c("LON", "LAT"), c("LON_PUBLIC", "LAT_PUBLIC"))
  xyx$COUNTYFIPS <- paste0(formatC(xyx$STATECD, width=2, digits=2, flag=0), 
          		formatC(xyx$COUNTYCD, width=3, digits=3, flag=0))

  if (xymeasCur) {
    xynm <- paste0("xyCur_", coords)
    assign(xynm, xyx) 
  } else {
    xynm <- paste0("xy_", coords)
    assign(xynm, xyx)
  } 
  if (is.null(out_layer)) {
    out_layer <- xynm
  }
 
  if (issp) {
    spxynm <- paste0("sp_", out_layer)

    ## Generate shapefile
    out_fmt_sp <- ifelse(out_fmt == "csv", "shp", out_fmt)
    assign(spxynm, spMakeSpatialPoints(xyplt=xyx, xvar="LON_PUBLIC", 
		yvar="LAT_PUBLIC", xy.uniqueid="PLT_CN", xy.crs=4269, addxy=TRUE, 
		exportsp=savedata, out_dsn=out_dsn, out_fmt=out_fmt_sp, 
		outfolder=outfolder, out_layer=spxynm, outfn.date=outfn.date,
 		overwrite_layer=overwrite_layer, append_layer=TRUE,
		outfn.pre=outfn.pre))

  }
     
  ###############################################################################
  ## SAVE data
  ###############################################################################
  if (savedata) {
 
    index.unique.xyplt <- "PLT_CN"
    FIESTA::datExportData(get(xynm), outfolder=outfolder, 
		out_fmt=out_fmt, out_dsn=out_dsn, out_layer=out_layer, 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		index.unique=index.unique.xyplt, append_layer=append_layer,
		outfn.pre=outfn.pre)
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

    ## Return data list
    return(fiadatlst)
  } else {
    return(NULL)
  }
  ## Close database connection
  #DBI::dbDisconnect(dbconn)
 
}
