DBgetCoords <- function (states=NULL, invtype="ANNUAL", 
	evalid=NULL, evalCur=FALSE, evalEndyr=NULL, evalAll=FALSE, 
	measCur=FALSE, measEndyr=NULL, allyrs=FALSE, 
	invyrs=NULL, intensity1=FALSE, issp=FALSE, returndata=TRUE, 
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
  invtype <- FIESTA::pcheck.varchar(invtype, varnm="invtype", checklst=invtypelst, 
		caption="Inventory Type", gui=gui)

  ## Get states, Evalid and/or invyrs info
  evalInfo <- DBgetEvalid(states=states, invtype=invtype, 
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
  stabbrlst <- FIESTA::pcheck.states(states, statereturn="ABBR")
  stcdlst <- FIESTA::pcheck.states(states, statereturn="VALUE")

  ## Get number of states 
  nbrstates <- length(states)


  ## If using EVALID, you don't need to get INVYRS, intensity
  if (!iseval) { 

    ### Check measCur
    ###########################################################
    measCur <- FIESTA::pcheck.logical(measCur, varnm="measCur", 
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
    allyrs <- FIESTA::pcheck.logical(allyrs, varnm="allyrs", 
		title="All years?", first="YES", gui=gui)
    if (allyrs) {
      ## xymeasCur
      xymeasCur <- FIESTA::pcheck.logical(xymeasCur, varnm="xymeasCur", 
		title="Most current XY?", first="YES", gui=gui)
      measCur <- FALSE
      measEndyr=measEndyr.filter <- NULL
    }
 
    ## Check INVYR(S) 
    ###########################################################
    if (!measCur) {
      if ((is.null(invyrs) || length(invyrs) == 0)) {
        invyrs <- sapply(states, function(x) NULL)
        for (state in states) { 
          stabbr <- FIESTA::pcheck.states(state, "ABBR")
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
      }
    }

    ## Check intensity1
    ###########################################################
    ## For periodic data, the INTENSITY variable does not equal 1
    if (invtype == "ANNUAL") {
      intensity1 <- FIESTA::pcheck.logical(intensity1, varnm="intensity1", 
		title="Intensity = 1?", first="YES", gui=gui)
    } else {
      intensity1 <- FALSE
    }
  } else {
    allyrs <- FALSE
  }
    

  ## Check savedata
  ###########################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
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
  stcds <- FIESTA::pcheck.states(states, "VALUE")
  stFilter <- paste0("p.STATECD IN(", toString(stcds), ")")
  evalFilter=xyfromqry <- NULL
  stabbr <- FIESTA::pcheck.states(states, "ABBR")
  SCHEMA. <- NULL

  ## PLOT from/join query
  if (iseval) {
    pfromqry <- paste0(SCHEMA., "POP_PLOT_STRATUM_ASSGN ppsa")
    xyfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., 
			"PLOT p ON (p.PLT_CN = ppsa.PLT_CN)")
    evalFilter <- paste0("p.EVALID IN(", toString(unlist(evalidlist)), ")")

  } else if (length(invyrs) > 1) {
    xyfromqry <- paste0(SCHEMA., "PLOT p")
    evalFilter <- paste0(stFilter, " and p.INVYR IN(", toString(invyrs), ")")

  } else {
    if (measCur) {
      xyfromqry <- FIESTA::getpfromqry(Endyr=measEndyr, SCHEMA.=SCHEMA., 
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


  ## Generate queries
  ##################################################################################
  ## VARIABLES
  XYvarlst <- c("CN", "LON", "LAT", "STATECD", "UNITCD", "COUNTYCD", "PLOT")
  xycoords.qry <- paste0("select ", 
		toString(paste0("p.", XYvarlst)), 
		" from ", xyfromqry,
		" where ", evalFilter)
  xyx <- tryCatch( sqldf::sqldf(xycoords.qry), 
			error=function(e) {
                  message("invalid xy query\n")
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
