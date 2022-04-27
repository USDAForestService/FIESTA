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
#' @param xy_datsource String. Source of XY data ("obj", "csv", "datamart",
#' "sqlite").  If datsource=NULL, checks extension of xy_dsn or xy to identify
#' datsource.
#' @param xy sf R object or String. Table with xy coordinates. Can be a spatial
#' polygon object, data frame, full pathname to a shapefile, or name of a layer
#' within a database.
#' @param xy_dsn String. Data source name (dsn; i.e., pathname or database
#' name) of xy. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd_layer is an R
#' object.
#' @param xy.uniqueid String. Unique identifier of xy.
#' @param xvar String. Name of variable in xyplt defining x coordinate.
#' @param yvar String. Name of variable in xyplt defining y coordinate.
#' @param xy.crs PROJ.4 String or CRS object or Integer EPSG code defining
#' Coordinate Reference System.
#' @param xyjoinid String. Variable in xy to join to plot data. If NULL,
#' xyjoinid=xy.uniqueid.
#' @param pjoinid String. Variable in plt to join to XY data. Not necessary to
#' be unique. If using most current XY coordinates, use identifier for a plot
#' (e.g., PLOT_ID).
#' @param clipxy Logical. If TRUE, clips xy data to bnd.
#' @param plot_layer String. Name of plot_layer in database.
#' @param evalid Integer. To extract data for a specific evaluation period. See
#' notes for more information about FIA Evaluations.
#' @param evalCur Logical. If TRUE, extract plots with most current FIA
#' Evalidation for state(s).
#' @param evalEndyr Integer. Defining end year of Evaluation (yyyy).
#' @param measCur Logical. If TRUE, extract plots with most current measurement
#' for state(s).
#' @param measEndyr Integer year (YYYY). If measCur=TRUE, extract plots with
#' most current measurement for state(s) for years measured before measEndyr.
#' @param measEndyr.filter Filter. If measCur=TRUE and measEndyr != NULL, a
#' filter for bnd to identify and area to use measEndyr, such as disturbed
#' areas where you want to exclude plots measured after disturbance.
#' @param invyrs Integer vector. Defining specific inventory years of data
#' (e.g., 2010:2015).
#' @param measyrs Integer vector. Defining specific measurement years of data
#' (e.g., 2010:2015).
#' @param allyrs Logical. If TRUE, selects all years (annual inventory) in
#' database.
#' @param intensity1 Logical. If TRUE, include only single intensity plots
#' (i.e., INTENSITY = 1).
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
#'                   evalCur = TRUE,
#'                   returnxy = TRUE)
#' }
#' @export spGetXY
spGetXY <- function(bnd, 
                    bnd_dsn = NULL, 
                    bnd.filter = NULL, 
                    states = NULL, 
                    RS = NULL, 
                    xy_datsource = "datamart", 
                    xy = NULL, 
                    xy_dsn = NULL, 
                    xy.uniqueid = "PLT_CN", 
                    xvar = NULL, 
                    yvar = NULL, 
                    xy.crs = 4269, 
                    xyjoinid = NULL, 
                    pjoinid = "CN", 
                    clipxy = TRUE, 
                    plot_layer = "plot",
                    evalid = NULL, 
                    evalCur = FALSE, 
                    evalEndyr = NULL, 
                    measCur = FALSE, 
                    measEndyr = NULL, 
                    measEndyr.filter = NULL, 
                    invyrs = NULL, 
                    measyrs = NULL, 
                    allyrs = FALSE, 
                    intensity1 = FALSE, 
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
 
  ## Set xy_datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "datamart", "sqlite", "shp", "gdb")
  xy_datsource <- pcheck.varchar(var2check=xy_datsource, varnm="xy_datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (is.null(xy_datsource)) {
    if (!is.null(xy) && "sf" %in% class(xy)) {
      xy_datsource <- "obj"
    } else if (!is.null(xy_dsn)) {
      dsn.ext <- getext(xy_dsn)
      if (!is.na(dsn.ext) && dsn.ext != "") {
        xy_datsource <- ifelse(dsn.ext == "gdb", "gdb", 
		ifelse(dsn.ext %in% c("db", "db3", "sqlite", "sqlite3"), "sqlite", 
             ifelse(dsn.ext == "csv", "csv",
			ifelse(dsn.ext == "shp", "shp", "datamart")))) 
      }
    } else if (!is.null(xy)) {
      xy.ext <- getext(xy)
      if (!is.na(xy.ext) && xy.ext != "") {
        xy_datsource <- ifelse(xy.ext == "shp", "shp", 
             ifelse(xy.ext == "csv", "csv", "datamart")) 
      }
    } else {
      stop("must include xy_datsource")
    }
  } 
 
  ## Check measEndyr.filter
  #############################################################################
  measEndyr.filter <- check.logic(bnd, measEndyr.filter)

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
                            stbnd.att="COUNTYFIPS", 
                            RS=RS, 
                            states=states, 
                            showsteps=showsteps)
    bndx <- statedat$bndx
    stbnd.att <- statedat$stbnd.att
    statenames <- statedat$statenames
    if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
      countyfips <- statedat$states
      countyfips <- formatC(as.numeric(countyfips), width=5, digits=5, flag="0")
      stcds <- sort(unique(as.numeric(sapply(countyfips, 
				substr, nchar(countyfips)-5, nchar(countyfips)-3))))
    } else {
      stcds <- FIESTAutils::ref_statecd$VALUE[FIESTAutils::ref_statecd$MEANING %in% statedat$states]
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

  } else if (xy_datsource %in% c("obj", "csv")) {

    ####################################################################
    ## 1) Import file(s)
    ## 2) Clip xy (for all states) to boundary
    ####################################################################

    ## Check xy table
    xyplt <- pcheck.table(xy)

    ## Subset if STATECD in table
    statenm <- findnm("STATECD", names(xyplt), returnNULL=TRUE)
    if (!is.null(statenm)) {
      xyplt <- datFilter(xyplt, getfilter(statenm, stcds))$xf
    }

    ## Make spatial
    if (!"sf" %in% class(xyplt)) {
      spxy <- spMakeSpatialPoints(xyplt=xyplt, xy.uniqueid=xy.uniqueid, 
		xvar=xvar, yvar=yvar, xy.crs=xy.crs)
    }

  } else if (xy_datsource == "shp") {
    #sqlatt <- paste("select * from ", basename.NoExt(xy), "limit 0")

    if (!is.na(getext(xy)) && getext(xy) == "shp" && 
		"STATECD" %in% names(st_read(xy, quiet=TRUE))) {
      #where <- getfilter("STATECD", stcds, syntax="sql")
      #sql <- paste("select * from", basename.NoExt(xy), "where", where)
      #spxy <- pcheck.spatial(xy, sql=sql)
      spxy <- pcheck.spatial(xy)
      spxy <- spxy[spxy$STATECD %in% stcds, ]

    } else {
      spxy <- pcheck.spatial(xy)
    }
  } else {			## xy_datsource in('datamart', 'sqlite')
    if (xy_datsource == "datamart") {
      spxy <- DBgetXY(states=stcds, 
                          evalid=evalid, 
                          evalCur=evalCur, 
                          evalEndyr=evalEndyr, 
                          measCur=measCur, 
                          measEndyr=measEndyr, 
                          allyrs=allyrs, 
                          invyrs=invyrs, 
                          measyrs=measyrs, 
                          intensity1=intensity1, 
                          issp=TRUE)[[1]]
      xy.uniqueid <- "PLT_CN"
      xyjoinid <- "PLT_CN"
  
    } else if (xy_datsource == "sqlite") {

      ####################################################################
      ## 1) Check if data for all states is in database
      ## 1) Get most current plots from xy database that intersect state
      ## 2) Clip xy (for state) to boundary
      ## 3) Subset other data with clipped xy joinid
      ####################################################################
      xyindb <- FALSE
      plot_layer <- NULL

      ## Check for data tables in database
      ###########################################################
      dbconn <- DBtestSQLite(xy_dsn, dbconnopen=TRUE, 
			showlist=FALSE, createnew=FALSE, stopifnull=TRUE)
      tablst <- DBI::dbListTables(dbconn)

      if (is.null(xy)) {
        xytabs <- tablst[grepl("xy", tablst)]
        if (length(xytabs) == 0) {
          stop("no xy in ", xy_dsn)
        }
        if (length(findnm("ACTUAL", xytabs, returnNULL=TRUE)) == 1) {
          xy <- xytabs[grepl("ACTUAL", xytabs)]
          message("xy is NULL...  using ", xy)
        } else if (length(findnm("PUBLIC", xytabs, returnNULL=TRUE)) == 1) {
          xy <- xytabs[grepl("PUBLIC", xytabs)]
          message("xy is NULL...  using ", xy)
        } else {
          plot_layer <- findnm("plot", xytabs, returnNULL=TRUE)
          if (!is.null(plot_layer) && length(plot_layer) == 1) {
            message("xy is NULL...  using ", plot_layer, " table")
            xy <- plot_layer
            pfields <- DBI::dbListFields(dbconn, xy)
            if ("LON_PUBLIC" %in% pfields) {
              xvar <- "LON_PUBLIC"
              if ("LAT_PUBLIC" %in% pfields) {
                yvar <- "LAT_PUBLIC"
              }
            }
          } else {
            stop(xy, " not in ", xy_dsn) 
          }
        } 
      } else {
        if (!xy %in% tablst) {
          stop(xy, " not in database")
        }
      }
      xyfields <- DBI::dbListFields(dbconn, xy)
      xystatenm <- findnm("STATECD", xyfields, returnNULL=TRUE)
 
      if (is.null(xvar) || !xvar %in% names(xyfields)) {
        if (grepl("ACTUAL", xy)) {
          xvar <- "LON_ACTUAL"
          yvar <- "LAT_ACTUAL"
        } else if (grepl("PUBLIC", xy)) {
          xvar <- "LON_PUBLIC"
          yvar <- "LAT_PUBLIC"
        }
      }

      ## Check xvar
      if (!is.null(xvar) && !xvar %in% xyfields) {
        stop(xvar, " is not in table")
      }
      ## Check yvar
      if (!is.null(yvar) && !yvar %in% xyfields) {
        stop(yvar, " is not in table")
      }
 
      ## check xy.uniqueid (unique identifier of xy)
      xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", 
		gui=gui, checklst=xyfields, caption="xy uniqueid", stopifnull = FALSE)

      ## check xyjoinid (variable to join to plot table)
      xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
		      gui=gui, checklst=xyfields, caption="xyjoinid", stopifnull = FALSE)
      if (is.null(xyjoinid)) {
        xyjoinid <- xy.uniqueid
      }
      if (intensity1) {
        intensitynm <- findnm("INTENSITY", xyfields, returnNULL=TRUE)
      }

      if (!is.null(xystatenm) && !intensity1) {
        stfilter <- paste("where ", xystatenm, " IN(", toString(stcds), ")")

        sql <- paste0("select * from ", xy, " where ", xystatenm, " IN(", 
			    toString(stcds), ")")
        xyplt <- suppressMessages(pcheck.table(xy, tab_dsn=xy_dsn, tabqry=sql))

        ## Make spatial
        spxy <- spMakeSpatialPoints(xyplt=xyplt, 
                                    xy.uniqueid=xy.uniqueid, 
                                    xvar=xvar, 
                                    yvar=yvar, 
                                    xy.crs=xy.crs) 
      } else {

        plot_layer <- findnm("plot", tablst, returnNULL=TRUE)
        if (!is.null(plot_layer) && length(plot_layer) == 1) {
          pltfields <- DBI::dbListFields(dbconn, plot_layer)
          pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", 
			      gui=gui, checklst=pltfields, caption="plot joinid")
          if (is.null(pjoinid)) {
            if (xyjoinid %in% pltfields) {
              pjoinid <- xyjoinid
            } else if (xyjoinid == "PLT_CN" && "CN" %in% pltfields) {
              pjoinid <- "CN"
            } else {
              stop("invalid pjoinid")
            }
          }
 
          pstatenm <- findnm("STATECD", pltfields, returnNULL=TRUE)
          if (!is.null(pstatenm)) {
            stfilter <- paste0("p.", pstatenm, " IN(", toString(stcds), ")")
            xyfromqry <- getpfromqry(plotCur=measCur, 
                                     invyrs=invyrs, 
                                     allyrs=allyrs, 
                                     intensity1=intensity1, 
                                     syntax="R", 
                                     plotnm=plot_layer)
            sql <- paste0("select xy.* from ", xyfromqry, 
				" JOIN ", xy, " xy ON (p.", pjoinid, " = xy.", xyjoinid, ")",
				" where ", stfilter) 
#            sql <- paste0("select xy.* from ", xy, " xy join ", 
#			plot_layer, " p ON(xy.", xyjoinid, " = p.", pjoinid, ") where p.", 
#			pstatenm, " IN(", toString(stcds), ")")

            xyplt <- pcheck.table(xy, tab_dsn=xy_dsn, tabqry=sql)
            if (nrow(xyplt) == 0) {
              if (!is.null(xyjoinid) && pjoinid != xyjoinid) {
                message("check if xyjoinid (", xyjoinid, ") in ", xy, 
				" matches pjoinid (", pjoinid, ") in ", plot_layer)
              }     
              stop("invalid xy query")
            }
 
            ## Make spatial
            spxy <- spMakeSpatialPoints(xyplt=xyplt, 
                                        xy.uniqueid=xy.uniqueid, 
                                        xvar=xvar, 
                                        yvar=yvar, 
                                        xy.crs=xy.crs)            
          } else {
            stop("STATECD not in tables")
          }
        } else {
          message("no plot layer in database")
          spxy <- pcheck.spatial(xy, dsn=xy_dsn)
        }
      } 

      if (!is.null(dbconn)) {
        DBI::dbDisconnect(dbconn)
      }
    }   # xy_datsource == "sqlite"
  }

  if (clipxy) {
    xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", gui=gui, 
		checklst=names(spxy), caption="UniqueID variable of xy data", 
		warn=paste(xy.uniqueid, "not in xy data"), stopifnull=TRUE)

    clipdat <- spClipPoint(spxy, clippolyv=bndx, uniqueid=xy.uniqueid)
    spxy <- clipdat$clip_xyplt 
    if (length(spxy) == 0) stop("xy does not overlap bndx")
    bndx <- clipdat$clip_polyv

    if (showsteps) {
      plot(st_geometry(bndx), border="black")
      plot(st_geometry(spxy), add=TRUE, col="blue")
    }
  } 
 
  ## Add a STATECD variable to spxy if not already there
  stunitco.names <- c("STATECD", "UNITCD", "COUNTYCD", "COUNTYFIPS")
  statevars <- stunitco.names[!stunitco.names %in% names(spxy)]
  if (length(statevars) > 0) {
    spxy <- spExtractPoly(spxy, 
                          xy.uniqueid=xy.uniqueid, 
                          polyvlst=FIESTAutils::stunitco, 
                          polyvarlst=statevars)$spxyext

    ## Check projections of inlayer point layer vs. polygon layer. 
    ## If different, reproject sppltx to polygon projection.
    prjdat <- crsCompare(spxy, bndx) 
    spxy <- prjdat$x
  }

  ## Subset columns of spxy
  #spxy <- spxy[, unique(c(xy.uniqueid, xyjoinid, stunitco.names))]


  #############################################################################
  ## measEndyr.filter
  #############################################################################
  if (!is.null(measEndyr.filter)) {
    filternames <- check.logic(bnd, measEndyr.filter, returnvar=TRUE)
    if (length(filternames) > 0) {
      spxy <- spExtractPoly(spxy, polyvlst=bndx, polyvarlst=filternames)$spxyext
    } else {
      spxy <- spExtractPoly(spxy, polyvlst=bndx)$spxyext
    }
  }

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
      plot(st_geometry(bndx), add=TRUE, border="black", lwd=0.75)
    }
    #par(mar=mar)
  }

  if (returnxy) {
    returnlst$spxy <- spxy
  } 
  returnlst$pltids <- pltids
  returnlst$bndx <- bndx
  returnlst$xy.uniqueid <- xy.uniqueid
  returnlst$states <- statenames
  returnlst$countyfips <- countyfips
  return(returnlst)
}
