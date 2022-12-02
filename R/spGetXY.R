#' Spatial wrapper - Extracts XY coordinates within a given boundary.
#' 
#' Wrapper to get FIA plots within the boundary population (area of interest) -
#' Intersect with state boundary - Get FIA plots for intersected states,
#' including tree, and spatial - Clip spatial coordinates and other tables to
#' boundary (spClipPoint)
#' 
#' 
#' \bold{datsource}
#' 
#' Plots are extracted from 3 different data sources:\cr 1) CSV - data have
#' previously been extracted from the FIA database and stored as CSV files.\cr
#' 2) datamart - data are extracted from FIA's publically-available
#' datamart.\cr 3) sqlite - data have previously been extracted from the FIA
#' database and stored within a SQLite database.\cr
#' 
#' \bold{Selection parameters}
#' 
#' FIA plots are selected based on the following parameters:\cr \tabular{ll}{
#' \tab evalid - the FIA evaluation identifier\cr \tab evalCur - the most
#' current FIA evaluation in database\cr \tab evalEndyr - the FIA evaluation
#' ending in evalEndyr\cr \tab evalType - the FIA evaluation type ('ALL',
#' 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN')\cr \tab measCur -
#' the most current measurement of each plot in database\cr \tab measEndyr -
#' the most current measuremtn of each plot in database in or prior to
#' measEndyr\cr \tab Endyr.filter - a filter for bnd that specifies the
#' boundary where measEndyr should be applied\cr }
#' 
#' @param bnd sf R object, Area of Interest (AOI) boundary. Can be a spatial sf
#' object, full pathname to a shapefile, or name of a layer within a database.
#' @param bnd_dsn String. Data source name (dsn; e.g., SQLite database or shapefile
#' pathname) of bnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd is an R object.
#' @param bnd.filter String. Filter to subset bnd spatial layer.
#' @param states String. The name of state(s) for tables (e.g., "Vermont",
#' "Utah").
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
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
#' @param pvars2keep String vector. One or more variables in pltTab to append to output.
#' @param clipxy Logical. If TRUE, clips xy data to bnd.
#' @param showsteps Logical. If TRUE, display data in device window.
#' @param returnxy Logical. If TRUE, returns XY coordinates.
#' @param savedata Logical. If TRUE, saves data to outfolder. Note:
#' includes XY data if returnxy = TRUE.
#' @param exportsp Logical. If savedata = TRUE and returnxy = TRUE, 
#' if TRUE, exports XY data as spatial data.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#'
#' @return \item{spxy}{ sf. If returnxy=TRUE, spatial xy point data. }
#' \item{pltids}{ data frame. A table of pltids that are within bnd. }
#' \item{spxy}{ sf data frame. If returnxy, a simple feature with pltids within bnd. }
#' \item{bndx}{ sf object. Input bnd. } 
#' \item{xy.uniqueid}{ String. Unique identifier of plots in xy. } 
#' \item{states}{ String. Vector of states that intersect bnd. } 
#' \item{countyfips}{ String. Vector of countyfips values that intersect bnd. } 
#' \item{stbnd.att}{ String. Name of state attribute used to select plots. }
#' 
#' If savedata=TRUE and returnxy=TRUE, the plt data frame, including XY 
#' coordinates is saved to outfolder (xyplt).\cr 
#' If savedata=TRUE and returnxy=FALSE, the plt data frame, without XY 
#' coordinates is saved to outfolder (pltids).\cr
#' If savedata=TRUE and returnxy=TRUE and exportsp=TRUE, the spxy sf object is 
#' exported as shapefile to outfolder.\cr
#' @note
#' 
#' If savebnd=TRUE:\cr If out_fmt=c('csv','shp'), the writeOGR (rgdal) function
#' is called. The ArcGIS driver truncates variable names to 10 characters or
#' less. Variable names are changed before export using an internal function
#' (trunc10shp). If Spatial object has more than 1 record, it will be returned
#' but not exported.
#' 
#' If datsource="datmart", (default), data are imported from FIA DataMart.  The
#' plot coordinates have been altered for privacy (See
#' https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php for details).
#' The zip files are extracted on-the-fly from the online website. Web server
#' connections will affect download speeds.
#' 
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \dontrun{
#' # Set up data from FIESTA
#' WYbhfn <- system.file("extdata",
#'                       "sp_data/WYbighorn_adminbnd.shp",
#'                       package = "FIESTA")
#' # Use spGetXY
#' WYbhxy <- spGetXY(bnd = WYbhfn,
#'                   xy_datsource = "datamart",
#'                   eval = "custom",
#'                   eval_opts = list(Cur = TRUE),
#'                   returnxy = TRUE)
#' }
#' @export spGetXY
spGetXY <- function(bnd, 
                    bnd_dsn = NULL, 
                    bnd.filter = NULL, 
                    states = NULL, 
                    RS = NULL, 
                    xy_datsource = "datamart", 
                    xy_dsn = NULL, 
                    xy = "PLOT",
                    xy_opts = xy_options(),
                    datsource = NULL,
                    data_dsn =NULL, 
                    dbTabs = dbTables(),
                    eval = "FIA",
                    eval_opts = NULL,
                    pjoinid = "CN",
                    invtype = "ANNUAL", 
                    intensity1 = FALSE, 
                    pvars2keep = NULL, 
                    clipxy = TRUE, 
                    showsteps = FALSE, 
                    returnxy = TRUE, 
                    savedata = FALSE, 
                    exportsp = FALSE, 
                    savedata_opts = NULL){
  ##############################################################################
  ## DESCRIPTION
  ## Get FIA plots within the boundary population (area of interest)
  ## 1) Reproject state boundary to bnd projection (nolonglat=TRUE)
  ## 2) Intersect with state boundary 
  ## 5) Clip spatial coordinates and other tables to boundary
  ##
  ## ARGUMENTS
  ## xy - file name, R object, or layer in SQLitfn
  ##
  ## VALUE
  ## List of clipped data frames
  ##############################################################################

  ## Set global variables
  xydat=stateFilter=countyfips=stcds=dbconn=intensitynm <- NULL
  returnlst <- {}

  gui <- FALSE
  coordtype <- "public"
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spGetXY))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts, eval_opts=eval_opts)
  

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
    ## Append eval_options defaults not specified to pass on to DBgetXY()
    if (any(names(eval_defaults_list) %in% names(eval_opts))) {
      eval_opts <- append(eval_opts, 
		eval_defaults_list[!names(eval_defaults_list) %in% names(eval_opts)])
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
    ## Append xy_options defaults not specified to pass on to DBgetXY()
    if (any(names(xy_defaults_list) %in% names(xy_opts))) {
      xy_opts <- append(xy_opts, 
		xy_defaults_list[!names(xy_defaults_list) %in% names(xy_opts)])
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
    ## Append eval_options defaults not specified to pass on to DBgetXY()
    if (any(names(eval_defaults_list) %in% names(eval_opts))) {
      eval_opts <- append(eval_opts, 
                          eval_defaults_list[!names(eval_defaults_list) %in% names(eval_opts)])
    }
  } else {
    message("no evaluation timeframe specified... using all data in database...\n")
    eval_opts$allyrs <- TRUE
    
    #    stop("must specify an evaluation timeframe for data extraction... \n", 
    #	"...see eval_opts parameter, (e.g., eval_opts=eval_options(evalCur=TRUE))")
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
    ## Append xy_options defaults not specified to pass on to DBgetXY()
    if (any(names(xy_defaults_list) %in% names(xy_opts))) {
      xy_opts <- append(xy_opts, 
                        xy_defaults_list[!names(xy_defaults_list) %in% names(xy_opts)])
    }
  } 
 
  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################
  
  ## Import boundary
  ########################################################
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
 
  if (!is.null(bndx)) {
    ## bnd.filter
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf
    clipxy <- TRUE
  } else {
    clipxy <- FALSE
  }
    
  ## Check intensity1
  #############################################################################
  intensity1 <- pcheck.logical(intensity1, varnm="intensity1", 
                              title="Intensity 1?", first="NO", gui=gui) 

  ## Check showsteps
  #############################################################################
  showsteps <- pcheck.logical(showsteps, varnm="showsteps", 
                             title="Show steps?", first="NO", gui=gui) 
  
  ## Check returnxy
  #############################################################################
  returnxy <- pcheck.logical(returnxy, varnm="returnxy", 
                             title="Return XY?", first="NO", gui=gui) 

  ## Check savedata
  #############################################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
                             title="Save data?", first="NO", gui=gui) 
  if (savedata && returnxy) {
    ## Check exportsp
    exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
                             title="Export spatial XY?", first="NO", gui=gui) 
  }
  
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
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
  }
 
  ########################################################################
  ### DO THE WORK
  ########################################################################
  if (!is.null(evalid)) {
    evalid <- unlist(evalid)
    stcds <- unique(as.numeric(substr(evalid, nchar(evalid)-6, nchar(evalid)-4))) 
    savePOP <- TRUE
    statenames <- pcheck.states(stcds, "MEANING")
  } else if (!is.null(states)) {
    if (!all(states %in% FIESTAutils::ref_statecd$MEANING))
      stop("states is invalid")
    statenames <- states
    stcds <- pcheck.states(states, "VALUE")

  } else if (!is.null(bndx)) {
 
    ## Get intersecting states
    statedat <- spGetStates(bndx, 
                            stbnd.att = "COUNTYFIPS", 
                            RS = RS, 
                            states = states, 
                            showsteps = showsteps)
    bndx <- statedat$bndx
    stbnd.att <- statedat$stbnd.att
    statenames <- statedat$statenames
 
    if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
      countyfips <- statedat$states
      countyfips <- formatC(as.numeric(countyfips), width=5, digits=5, flag="0")
      stcds <- sort(unique(as.numeric(sapply(countyfips, 
				substr, nchar(countyfips)-5, nchar(countyfips)-3))))
    } else {
      stcds <- ref_statecd$VALUE[ref_statecd$MEANING %in% statedat$states]
    }
    message("boundary intersected states: ", toString(statenames))
  } else {
    stop("must include bndx or states")
  }
 
  #############################################################################
  ## If xy is separate file or database, and clipxy=TRUE, import first
  #############################################################################
  if (!is.null(xy) && "sf" %in% class(xy)) {
    spxy <- xy

  } else if (xy_datsource == "gdb") {
    stop("cannot write to geodatabases")
#
#    ## Check for data tables in database
#    ###########################################################
#    gdbpath <- suppressWarnings(DBtestESRIgdb(xy_dsn, showlist=FALSE))
#    layerlst <- sf::st_layers(gdbpath)
#    tablst <- layerlst$name
#    if (!xy %in% tablst) {
#      stop(xy, " not in ", gdbpath)
#    }
#    geomtype <- layerlst$geomtype[layerlst$name == xy][[1]]
#    xyopen <- arcgisbinding::arc.open(paste0(xy_dsn, "/", xy))
#    statenm <- findnm("STATECD", names(xyopen@fields), returnNULL=TRUE) 
#    sql <- getfilter(statenm, stcds, syntax="sql")
#    spxy <- pcheck.spatial(xy, xy_dsn, sql=sql)
#    if (is.na(geomtype)) { 
#      spxy <- spMakeSpatialPoints(xyplt=spxy, 
#                                  xy.uniqueid=xy.uniqueid, 
#                                  xvar=xvar, 
#                                  yvar=yvar, 
#                                  xy.crs=xy.crs)
#    }

  } else { 
    xydat <- DBgetXY(states = stcds,
                     xy_datsource = xy_datsource,
                     xy_dsn = xy_dsn,
                     xy = xy,
                     xy_opts = xy_opts,
                     eval = eval,
                     eval_opts = eval_opts,
                     datsource = datsource,
                     data_dsn = data_dsn,
                     dbTabs = dbTabs,
                     pjoinid = pjoinid,
                     invtype = invtype,
                     intensity1 = intensity1,
                     issp = TRUE)
    spxy <- xydat$spxy
    xy.uniqueid <- "PLT_CN"
    xyjoinid <- xydat$xyjoinid
    pjoinid <- xydat$pjoinid 
    evalInfo <- xydat$evalInfo
  }

  if (clipxy) {
    xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", gui=gui, 
		checklst=names(spxy), caption="UniqueID variable of xy data", 
		warn=paste(xy.uniqueid, "not in xy data"), stopifnull=TRUE)

    clipdat <- spClipPoint(spxy, 
                           clippolyv = bndx, 
                           uniqueid = xy.uniqueid)
    spxy <- clipdat$clip_xyplt 
    if (length(spxy) == 0) stop("xy does not overlap bndx")
    bndx <- clipdat$clip_polyv

    if (showsteps) {
      plot(sf::st_geometry(bndx), border="black")
      plot(sf::st_geometry(spxy), add=TRUE, col="blue")
    }
  } 
 
  ## Add a STATECD variable to spxy if not already there
  stunitco.names <- c("STATECD", "UNITCD", "COUNTYCD", "COUNTYFIPS")
  statevars <- stunitco.names[!stunitco.names %in% names(spxy)]
  if (length(statevars) > 0) {
    spxy <- spExtractPoly(spxy, 
                          xy.uniqueid = xy.uniqueid, 
                          polyvlst = FIESTAutils::stunitco, 
                          polyvarlst = statevars)$spxyext

    ## Check projections of inlayer point layer vs. polygon layer. 
    ## If different, reproject sppltx to polygon projection.
    prjdat <- crsCompare(spxy, bndx) 
    spxy <- prjdat$x
  }

  ## Subset columns of spxy
  #spxy <- spxy[, unique(c(xy.uniqueid, xyjoinid, stunitco.names))]


  #############################################################################
  ## Save tables
  #############################################################################
  pltids <- sf::st_drop_geometry(spxy)
  pltids <- pltids[,which(!names(pltids) %in% c(xvar, yvar))]
  
  if (savedata) {
    if (returnxy) {
      datExportData(sf::st_drop_geometry(spxy), 
         savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="xyplt",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            add_layer=TRUE)) 
   
      if (exportsp) {
        spExportSpatial(spxy, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="spxyplt",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer, 
                            add_layer=TRUE))
      }
    } else {
      datExportData(pltids, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="pltids",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            add_layer=TRUE)) 
    }
  }

  if (showsteps) {
    ## Retain par parameters
    mar <-  graphics::par("mar")
    on.exit(graphics::par(mar=mar))
    par(mar=c(1,1,1,1))

    plot(sf::st_geometry(spxy), col="blue", cex=.5)
    if (!is.null(bndx)) {
      plot(sf::st_geometry(bndx), add=TRUE, border="black", lwd=0.75)
    }
    #par(mar=mar)
  }

  if (returnxy) {
    returnlst$spxy <- spxy
  } 
  if (!is.null(evalInfo)) {
    returnlst$evalInfo <- evalInfo
  }
#  if (!is.null(evalchk)) {
#    returnlst$evalchk <- evalchk
#  }     
  returnlst$pltids <- pltids
  returnlst$bndx <- bndx
  returnlst$xy.uniqueid <- xy.uniqueid
  returnlst$pjoinid <- pjoinid
  returnlst$xyjoinid <- xyjoinid
  returnlst$states <- statenames
  returnlst$countyfips <- countyfips
  return(returnlst)
}
