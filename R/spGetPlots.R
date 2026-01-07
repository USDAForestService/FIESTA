#' Spatial wrapper - Extracts plot data within a given boundary.
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
#' @param states String. The name of state(s) for tables (e.g., "Vermont", "Utah").
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
#' @param pltids Data frame. Non-spatial plot identifiers within bnd).
#' @param xy_datsource String. Source of XY data ("obj", "csv", "datamart",
#' "sqlite").  If datsource=NULL, checks extension of xy_dsn or xy to identify
#' datsource.
#' @param xy_dsn String. Data source name (dsn; i.e., pathname or database
#' name) of xy. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd_layer is an R
#' object.
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
#' @param puniqueid String. Name of unique identifier of plt.
#' @param invtype String. Type of FIA inventory to extract ('PERIODIC',
#' 'ANNUAL').  Only one inventory type (PERIODIC/ANNUAL) at a time.
#' @param intensity1 Logical. If TRUE, includes only XY coordinates where 
#' INTENSITY = 1 (FIA base grid).
#' @param clipxy Logical. If TRUE, clips xy data to bnd.
#' @param pjoinid String. Variable in plt to join to XY data. Not necessary to
#' be unique. If using most current XY coordinates, use identifier for a plot
#' (e.g., PLOT_ID).
#' @param showsteps Logical. If TRUE, display data in device window.
#' @param returnxy Logical. If TRUE, save xy coordinates to outfolder.
#' @param returndata Logical. If TRUE, returns data objects.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savexy Logical. If TRUE, saves XY data to outfolder.
#' @param savebnd Logical. If TRUE, and savedata=TRUE, saves bnd. If 
#' out_fmt='sqlite', saves to a SpatiaLite database.
#' @param exportsp Logical. If TRUE, and savexy=TRUE, saves xy data as 
#' spatial data. If FALSE, saves xy data as table.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param dbconn Open database connection.
#' @param dbconnopen Logical. If TRUE, keep database connection open.
#' @param database_opts List. See help(database_options()) for a list
#' of options. Only used when datsource = 'postgres'.  
#' @param spXYdat R list object. Output from spGetXY().
#' @param ... parameters passed to DBgetPlots().
#' 
#' @return \item{xypltx}{ sf object. Input xy data clipped to boundary. }
#' \item{bndx}{ sf object. Input bnd. } \item{tabs}{ list object. List of input
#' layers clipped to boundary (pltx,condx,etc.). } \item{xy.uniqueid}{ String.
#' Name of unique identifier of xy. } \item{puniqueid}{ String. Name of unique
#' identifier of plot in plt. } \item{pjoinid}{ String. Name of unique
#' identifier of plot in plt. }
#' 
#' If savedata=TRUE, outdat data frame is saved to outfolder.
#' @note
#' 
#' If savebnd=TRUE:\cr If out_fmt=c('csv','shp'), the st_write (sf) function
#' is called. The ArcGIS driver truncates variable names to 10 characters or
#' less. Variable names are changed before export using an internal function
#' (trunc10shp). If Spatial object has more than 1 record, it will be returned
#' but not exported.
#' 
#' If datsource="datmart", data are imported from FIA DataMart.  The plot
#' coordinates have been altered for privacy (See
#' https://www.fia.fs.fed.us/tools-data/spatial/Policy/index.php for details).
#' The zip files are extracted on-the-fly from the online website. Web server
#' connections will affect download speeds.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \dontrun{
#' # Get polygon vector layer from FIESTA external data
#' WYbhfn <- system.file("extdata",
#'                       "sp_data/WYbighorn_adminbnd.shp",
#'                       package = "FIESTA")
#' 
#' # Extract data from FIA datamart for measurement years 2013 thru 2015
#' dat <- spGetPlots(bnd = WYbhfn,
#'                   datsource = "datamart",
#'                   eval = "custom",
#'                   eval_opts = list(measyrs = 2013:2015))
#' names(dat)
#' tabs <- dat$tabs
#' names(tabs)
#' head(tabs$plt)
#' 
#' table(tabs$plt$MEASYEAR)
#' 
#' # Extract data from FIA datamart for most current evaluation
#' datCur <- spGetPlots(bnd = WYbhfn,
#'                      datsource = "datamart",
#'                        eval = "FIA",
#'                      eval_opts = list(Cur = TRUE))
#' names(datCur)
#' tabsCur <- datCur$tabs
#' names(tabsCur)
#' head(tabsCur$plt)
#' 
#' table(tabsCur$plt$MEASYEAR)
#' } 
#' @export spGetPlots
spGetPlots <- function(bnd = NULL, 
                       bnd_dsn = NULL, 
                       bnd.filter = NULL, 
                       states = NULL, 
                       RS = NULL, 
                       pltids = NULL, 
                       xy_datsource = NULL, 
                       xy_dsn = NULL,
                       xy = "PLOT",
                       xy_opts = xy_options(),
                       datsource = NULL,
                       data_dsn =NULL, 
                       dbTabs = dbTables(),
                       eval = "FIA",
                       eval_opts = NULL,
                       puniqueid = "CN",
                       invtype = "ANNUAL", 
                       intensity1 = FALSE,  
                       clipxy = TRUE, 
                       pjoinid = NULL, 
                       showsteps = FALSE,
                       returnxy = TRUE,
                       returndata = TRUE,
                       savedata = FALSE,
                       savexy = FALSE,
                       savebnd = FALSE, 
                       exportsp = FALSE, 
                       savedata_opts = NULL,
                       dbconn = NULL,
                       dbconnopen = TRUE,
                       database_opts = database_options(),
                       spXYdat = NULL,
                       ...) {

  ##############################################################################
  ## DESCRIPTION
  ## Get FIA plots within the boundary population (area of interest)
  ## 1) Reproject state boundary to bnd projection (nolonglat=TRUE)
  ## 2) Intersect with state boundary 
  ## 3) Get FIA plots for intesected states (including tree, shp, INTENSITY=1)
  ## 4) Merge coordinate data if included separately (e.g., coordinates from SDS)
  ## 5) Clip spatial coordinates and other tables to boundary
  ##
  ## ARGUMENTS
  ## xy - file name, R object, or layer in SQLitfn
  ##
  ## VALUE
  ## List of clipped data frames
  ##############################################################################

  ## Set global variables
  xydat=stateFilter=stateFilterDB=countyfips=xypltx=evalidst=PLOT_ID=INVYR=
	othertabnms=stcds=spxy=stbnd=invasive_subplot_spp=subp=subpc=dbconn=
	bndx=evalInfo=plt=pltu=bndvars2keep <- NULL
  isveg=ischng=isdwm <- FALSE
  cuniqueid=tuniqueid=duniqueid <- "PLT_CN"
  stbnd.att <- "COUNTYFIPS"
  returnlst <- list()
  evalidlst <- {}
  #clipdat <- list()

  gui <- FALSE
  coordtype <- "public"
  iseval <- FALSE 
  pltassgnid <- "PLT_CN"
  savePOP <- FALSE

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  args <- as.list(match.call()[-1], list(...))

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- unique(c(names(formals(spGetPlots)), names(formals(DBgetPlots)), 
			"isveg", "ischng", "isdwm"))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, 
                savedata_opts = savedata_opts, 
                eval_opts = eval_opts,
			          xy_opts = xy_opts)

  if ("stateFilter" %in% names(args)) {
    stop("cannot use stateFilter parameter at this time in spGetPlots")
  }
  if ("getxy" %in% input.params) {
    stop("cannot use getxy parameter at this time in spGetPlots")    
  }
  if ("evalInfo" %in% input.params) {
    stop("cannot use evalInfo parameter at this time in spGetPlots")    
  }

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
  
  if ("isveg" %in% names(args)) {
    message("the parameter isveg is deprecated... use eval_options(Type='P2VEG'))\n")
    isveg <- args$isveg
  }

  
  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################      
 
  ## Define list of pop_tables (without PLT_CN)
  pop_tables <- c("POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", 
	"POP_EVAL_TYP", "POP_STRATUM", "SURVEY") 

  ## Check clipxy
  clipxy <- pcheck.logical(clipxy, varnm="clipxy", 
                            title="Clip xy?", first="NO", gui=gui)  


  ## Check xy_datsource and datsource
  ########################################################
  
  ## Check xy_datsource
  xydbinfo <- pcheck.datsource(dbconn = dbconn, 
                               datsource = xy_datsource, 
                               dsn = xy_dsn, 
                               database_opts = database_opts)
  if (is.null(xydbinfo)) {
    stop()
  } else {
    xyindb <- xydbinfo$indb
    xy_datsource <- xydbinfo$datsource
    xy_dbtablst <- xydbinfo$dbtablst
    xy_schema <- xydbinfo$schema
    xy_SCHEMA. <- xydbinfo$SCHEMA.
    xy_dbconn <- xydbinfo$dbconn
  }

  ## Check datsource
  dbinfo <- pcheck.datsource(dbconn = dbconn, 
                             datsource = datsource, 
                             dsn = data_dsn, 
                             database_opts = database_opts)
  if (is.null(dbinfo)) {
    stop()
  } else {
    datindb <- dbinfo$indb
    datsource <- dbinfo$datsource
    dbtablst <- dbinfo$dbtablst
    schema <- dbinfo$schema
    SCHEMA. <- dbinfo$SCHEMA.
    dbconn <- dbinfo$dbconn
  }

  ## Check if xy_datsource and datsource are the same or is null
  if (is.null(xy_datsource) && is.null(datsource) && is.null(dbconn)) {
    stop("xy_datsource and/or datsource are invalid")
  } else if (is.null(xy_datsource)) {
    xy_datsource <- datsource
    xy_dsn <- data_dsn
  } else if (is.null(datsource)) {
    datsource <- xy_datsource
    data_dsn <- xy_dsn
  }

  ## Message for data sources
  if (datsource == xy_datsource) {
    message("source of xy data and plot data is ", datsource)
  } else {
    message("source of xy data is ", xy_datsource, " and plot data is ", datsource) 
  }

  ## check showsteps
  showsteps <- pcheck.logical(showsteps, varnm="showsteps", 
                              title="Show steps?", first="NO", gui=gui) 
  ## check returnxy
  returnxy <- pcheck.logical(returnxy, varnm="returnxy", 
                             title="Return XY?", first="NO", gui=gui)  
  ## check returndata
  returndata <- pcheck.logical(returndata, varnm="returndata", 
                               title="Return data?", first="YES", gui=gui)  
  ## check savexy
  savexy <- pcheck.logical(savexy, varnm="savexy", 
                           title="Save XY data?", first="NO", gui=gui)
  ## check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", 
                             title="Save data?", first="NO", gui=gui)  
  ## check exportsp
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
                             title="Export spatial", first="YES", gui=gui)
  
  ## check savebnd
  #############################################################################
  if (!is.null(bndx)) {
    savebnd <- pcheck.logical(savebnd, varnm="savebnd",
                              title="Save spatial bnd?", first="NO", gui=gui)
  } else {
    savebnd <- FALSE
  }
  
  if (!returndata && savedata && !exportsp) {
    exportsp <- TRUE
  }
  
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || savebnd) {
    outlst <- pcheck.output(savedata_opts = savedata_opts,
                            createSQLite = TRUE)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    outlst$add_layer <- TRUE
    outlst$dbconnopen <- TRUE
    append_layer <- savedata_opts$append_layer
    overwrite_layer <- savedata_opts$overwrite_layer
    if (!is.null(outlst$out_conn) && is.null(outlst$outconn)) {
      outlst$outconn <- outlst$outconn
    }
    outlst$out_conn <- NULL
  }
 

  ## Get DBgetEvalid parameters from eval_opts
  ################################################
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


  ## get XY coordinates inside boundary
  if (is.null(spXYdat)) {
    
    ## Check pltids
    pltids <- pcheck.table(pltids)

    if (!is.null(pltids)) {
      Endyr.filter <- check.logic(pltids, Endyr.filter, stopifnull=FALSE)

      ## Check xyjoinid
      xyjoinidchk <- findnms(xyjoinid, names(pltids))
      if (length(xyjoinidchk) < length(xyjoinid)) {
        message("no xyjoinid defined... using the xy.uniqueid: ", xy.uniqueid)
        xyjoinid <- xy.uniqueid         
      } else {
        xyjoinid <- xyjoinidchk
      }		  
 
      ## Check stbnd.att
      stbnd.att <- pcheck.varchar(var2check = stbnd.att, 
                                  varnm = "stbnd.att", 
		                              checklst = names(pltids), 
		                              caption="State attribute?",
		                              gui=gui) 
      
      ## Get state codes
      if (is.null(stbnd.att)) {
        stbnd.att <- findnm("COUNTYFIPS", names(pltids), returnNULL=TRUE)
        if (is.null(stbnd.att)) {
          stbnd.att <- findnm("STATECD", names(pltids), returnNULL=TRUE)
          if (is.null(stbnd.att)) {
            stbnd.att <- findnm("STATE", names(pltids), returnNULL=TRUE)
          }
        }
      }
      if (stbnd.att == "COUNTYFIPS") {
        countyfips <- sort(unique(pltids[[stbnd.att]]))
        if (sum(is.na(suppressWarnings(as.numeric(countyfips)))) > 0) {
          stop("invalid countyfips")
        }
        countyfips <- formatC(as.numeric(countyfips), width=5, digits=5, flag="0")
        stcds <- sort(unique(as.numeric(sapply(countyfips, 
				substr, nchar(countyfips)-5, nchar(countyfips)-3))))
      } else {
        stcds <- sort(unique(pcheck.states(pltids[[stbnd.att]], statereturn="VALUE")))
      }
      states <- pcheck.states(as.numeric(stcds))

      if (!is.null(Endyr.filter)) {
        Endyr.filter <- check.logic(pltids, Endyr.filter, stopifnull=FALSE)

        ## Split pltids
        pltids1 <- datFilter(pltids, xfilter = Endyr.filter)$xf
        nbrxy <- ifelse (is.null(pltids1), 0, nrow(pltids1)) 
        message ("there are ", nbrxy, " plots where ", Endyr.filter)
        pltids2 <- datFilter(pltids, xfilter=paste0("!", Endyr.filter))$xf
      }

    } else { 	## is.null(pltids)

      ## Import boundary
      bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
      if (!is.null(bndx)) {
        bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf
      } 
      if (!is.null(Endyr.bnd)) {
        ## Import boundary
        Endyr.bndx <- pcheck.spatial(layer=Endyr.bnd, caption="boundary")
        if (!is.null(Endyr.bndx)) {
          Endyr.bndx <- spClipPoly(Endyr.bndx, clippolyv = bndx)
          Endyr.bndx$EndyrAOI <- 1
          bndx <- spUnionPoly(polyv1=bndx, polyv2=Endyr.bndx)
          bndx[is.na(bndx$EndyrAOI), "EndyrAOI"] <- 0
          Endyr.filter <- "EndyrAOI == 1"
        }
      }
      
      if (!is.null(Endyr.filter)) {
        Endyr.filter <- check.logic(bndx, Endyr.filter, stopifnull=FALSE)

        ## split boundaries... 
        ## bndx1 - boundary to use for Endyr.filter
        bndx1 <- datFilter(bndx, xfilter = Endyr.filter)$xf
        if (nrow(bndx1) == 0) {
          stop("invalid Endyr.filter: ", Endyr.filter) 
        }
        bndx1$EndyrAOI <- 1
        
        ## bndx2 - boundary to use for eval_opts
        bndx2 <- datFilter(bndx, xfilter = paste0("!", Endyr.filter))$xf
        if (nrow(bndx2) == 0) {
          stop("invalid Endyr.filter: ", Endyr.filter) 
        }
        bndx2$EndyrAOI <- 0
        bndx <- rbind(bndx1, bndx2)
        
        if (is.null(measEndyr) && is.null(evalEndyr)) {
          stop("must include measEndyr or evalEndyr") 
        } else {
          eval_opts$Cur <- TRUE
          if (!is.null(measEndyr) && measCur) {
            varEndyr <- "measEndyr"
          } else {
            varEndyr <- "evalEndyr"
          }
          eval_opts1 <- eval_opts
          eval_opts2 <- eval_opts
          eval_opts2[["Endyr"]] <- NULL
          eval_opts2[["Endyr.filter"]] <- NULL
        }
        
        ## Define xyjoinid to join all coordinates with different boundary time frames
        xyjoinid=pjoinid <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
        bndvars2keep <- "EndyrAOI"
      } else {
        eval_opts2 <- eval_opts
      }

      ## Check states
      if (!is.null(states)) {
        states <- pcheck.states(states)
      }

      ###########################################################################
      ## Get XY data for all plots within boundary
      ###########################################################################
      if (clipxy) {
        message("getting xy data...")
        
        if (is.null(pjoinid)) {
          pjoinid <- puniqueid
        }
        spXYdat <- spGetXY(bnd = bndx, 
                           states = states, 
                           RS = RS, 
                           xy_datsource = xy_datsource, 
                           xy_dsn = xy_dsn, 
                           xy = xy,
                           xy_opts = xy_opts,
                           eval = eval,
                           eval_opts = eval_opts2,
                           datsource = datsource,
                           data_dsn = data_dsn,
                           dbTabs = dbTabs,
                           pjoinid = NULL,
                           invtype = invtype, 
                           intensity1 = intensity1, 
                           clipxy = clipxy, 
                           bndvars2keep = bndvars2keep,
                           showsteps = FALSE, 
                           returnxy = TRUE,
                           dbconn = dbconn)

        if (is.null(spXYdat)) {
          return(NULL)
        }
        spxy <- spXYdat$spxy
        pltids <- spXYdat$pltids
        states <- spXYdat$states
        #countyfips <- spXYdat$countyfips
        stbnd.att <- spXYdat$stbnd.att
        bndx <- spXYdat$bndx
        xy.uniqueid <- spXYdat$xy.uniqueid
        evalInfo <- spXYdat$evalInfo  
        dbconn <- spXYdat$dbconn
        countyfips <- sort(unique(pltids$COUNTYFIPS))
        xyqry <- spXYdat$xyqry

        if (!is.null(bndvars2keep)) {
          pltids1 <- pltids[pltids$EndyrAOI == 1,]
          countyfips1 <- sort(unique(pltids1$COUNTYFIPS))
          pltids2 <- pltids[pltids$EndyrAOI == 0,]
          countyfips2 <- sort(unique(pltids2$COUNTYFIPS))
        }

        ## Check xyjoinid
        xyjoinidchk <- unlist(sapply(xyjoinid, findnm, names(pltids), returnNULL = TRUE)) 
        if (is.null(xyjoinidchk) || length(xyjoinidchk) < length(xyjoinid)) {
          message("no xyjoinid defined... using the xy.uniqueid: ", xy.uniqueid)
          xyjoinid <- xy.uniqueid         
        } else {
          xyjoinid <- xyjoinidchk
        }

        stcds <- pcheck.states(states, statereturn="VALUE")
        if (is.null(spxy) || nrow(spxy) == 0) {
          stop("spxy is null")
        }

      } else {
        if ("sf" %in% class(xy)) {
          spxy <- xy
        } else {
          spxy <- pcheck.spatial(xy, dsn=xy_dsn)
        }
      
        ## Check xyjoinid
        xyjoinidchk <- unlist(sapply(xyjoinid, findnm, names(spxy), returnNULL = TRUE)) 
        if (is.null(xyjoinidchk) || length(xyjoinidchk) < length(xyjoinid)) {
          message("no xyjoinid defined... using the xy.uniqueid: ", xy.uniqueid)
          xyjoinid <- xy.uniqueid         
        } else {
          xyjoinid <- xyjoinidchk
        }

        ## Check projections. Reproject points to clippolyv projection.
        prjdat <- crsCompare(spxy, bndx, nolonglat=TRUE)
        spxy <- prjdat$x
        bndx <- prjdat$ycrs

        ## Check if extents overlap... if not and stopifnotin=TRUE, return NULL
        chk <- check.extents(sf::st_bbox(bndx), sf::st_bbox(spxy), 
			      layer1nm="bndx", layer2nm="spxy", stopifnotin=TRUE, quiet=TRUE)
        if (is.null(chk)) return(NULL)

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
          stcds <- FIESTAutils::ref_statecd$VALUE[FIESTAutils::ref_statecd$MEANING %in% statedat$states]
        }
        message("boundary intersected states: ", toString(statenames))
        pltids <- sf::st_drop_geometry(spxy)
      } 
    }

    ## Check for duplicate pltids
    if (nrow(pltids) > nrow(unique(pltids[,xyjoinid, drop=FALSE]))) {
      message("pltids have duplicate xyjoinids")
    }
  }

  ########################################################################
  ### DO THE WORK
  ########################################################################
 
  #############################################################################
  ## If xy is separate file or database, and clipxy=TRUE, import first
  #############################################################################

  ## Initialize lists
  tabs2save <- {}

  msg <- "getting plot data for... "
  if (!is.null(evalid)) {
    iseval=savePOP <- TRUE
    evalid <- unlist(evalid)
    msg <- paste0(msg, "for evaluation: ", toString(evalid))
    savePOP <- TRUE
  } else if (allyrs) {
    msg <- paste0(msg, "for all years in database")
  } else if (measCur) {
    msg <- paste0(msg, "for most currently measured plots")
    if (!is.null(measEndyr)) {
      msg <- paste0(msg, ", from year ", measEndyr, " or before")
      if (!is.null(Endyr.filter)) {
        msg <- paste0(msg, ", ", Endyr.filter)
      }
    }
  } else if (evalCur) {
    iseval=savePOP <- TRUE
    msg <- paste0(msg, "for most current evaluation")
    if (!is.null(evalEndyr)) {
      iseval=savePOP <- TRUE
      msg <- paste0(msg, ", ending in ", evalEndyr)
    }
  } else if (!is.null(invyrs)) {
    msg <- paste0(msg, "for inventory years ", min(invyrs), " to ", max(invyrs))
  } else if (!is.null(measyrs)) {
    msg <- paste0(msg, "for measurement years ", min(measyrs), " to ", max(measyrs))
  } else {
    msg <- "using all years in database"
    allyrs <- TRUE
  }
  message(paste(msg, "\n"))
  if (savePOP) {
    pop_plot_stratum_assgnx <- {} 
  }

  
  #######################################################################
  ## Loop through states
  #######################################################################
  for (i in 1:length(states)) { 
    stcliptabs <- list()
    state <- states[i]
    stcd <- pcheck.states(state, statereturn="VALUE")
    stabbr <- pcheck.states(stcd, statereturn="ABBR") 
    message(paste0("\n", state, "..."))

    if ("STATECD" %in% names(pltids)) {
      stpltids <- pltids[pltids$STATECD == stcd, ]
      if (!is.null(Endyr.filter)) {
        stpltids1 <- pltids1[pltids1$STATECD == stcd, ]
        stpltids2 <- pltids2[pltids2$STATECD == stcd, ]
      }
    } else {
      stpltids <- pltids
      if (!is.null(Endyr.filter)) {
        stpltids1 <- pltids1
        stpltids2 <- pltids2
      }
    } 
  
    if (!is.null(evalid)) {
      evalidst <- evalid[unique(as.numeric(substr(evalid, nchar(evalid)-6, 
  					nchar(evalid)-4))) == stcd]
    } 

    ## Get plot data
    ###############################
    if (!is.null(Endyr.filter)) {
 
      ## Get plots inside filter
      #######################################
      if (nrow(stpltids1) > 0) {
        countyfips1 <- formatC(as.numeric(countyfips1), width=5, digits=5, flag="0")
        stcnty1 <- countyfips1[startsWith(countyfips1, formatC(stcd, width=2, flag="0"))]
        countycds1 <- sort(as.numeric(unique(substr(stcnty1, 3, 5))))
        stateFilterDB1 <- paste("p.countycd IN(", toString(countycds1), ")")
      
        if ("stateFilter" %in% input.params) {
          stateFilterDB1 <- paste(stateFilter, "&", stateFilterDB1) 
          #rm(stateFilter)
        }
        dat1 <- DBgetPlots(states = stcd, 
                         datsource = datsource,
                         data_dsn = data_dsn, 
                         dbTabs = dbTabs,
                         eval = eval,
                         eval_opts = eval_opts1,
                         puniqueid = puniqueid, 
                         pjoinid = pjoinid, 
                         getxy = FALSE,
                         stateFilter = stateFilterDB1, 
                         returndata = TRUE,
                         dbconn = dbconn,
                         ...
                         )
        tabs1 <- dat1$tabs
        tabIDs <- dat1$tabIDs
        PLOT1 <- tabs1$plt
        pop_plot_stratum_assgn1 <- dat1$pop_plot_stratum_assgn
        evalid1 <- dat1$evalid
        puniqueid <- dat1$puniqueid
        dbqueries <- dat1$dbqueries
        dbconn <- dat1$dbconn
      
        evalidlst <- unique(c(evalidlst, evalid1))

        ## Check pjoinid
        ##############################################
        pltfields <- names(PLOT1)
        pjoinidchk <- unlist(sapply(pjoinid, findnm, pltfields, returnNULL = TRUE))
        if (is.null(pjoinidchk)) {
          if (all(xyjoinid %in% pltfields)) {
            pjoinid  <- xyjoinid
          } else {
            if (puniqueid %in% names(pltids)) {
              pjoinid <- pjoinid
            } else if (xyjoinid == "PLT_CN" && "CN" %in% pltfields) {
              pjoinid <- "CN"
            } else {
              stop(xyjoinid, " not in plt")
            }
          }
        } else {
          pjoinid <- pjoinidchk
        }

        ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
        if (nrow(PLOT1) > length(unique(PLOT1[[puniqueid]]))) {
          if ("INVYR" %in% names(PLOT1)) {
            setorder(PLOT1, -INVYR)
          } else {
            setorderv(PLOT1, -puniqueid)
          }
          PLOT1 <- PLOT1[, head(.SD, 1), by=pjoinid]
        }

        ## Subset data to stpltids
        #idmatch <- PLOT1[[pjoinid]] %in% stpltids1[[xyjoinid]]
        #plt1 <- PLOT1[idmatch,]
        if (length(xyjoinid) > 1) {
          PLOT1$MATCH <- apply(PLOT1[,pjoinid], 1, paste, collapse = "-")
          stpltids1$MATCH <- apply(stpltids1[,pjoinid], 1, paste, collapse = "-")
          plt1 <- PLOT1[PLOT1[["MATCH"]] %in% stpltids1[["MATCH"]],]
        } else {
          plt1 <- PLOT1[PLOT2[[pjoinid]] %in% stpltids1[[xyjoinid]],]
        }
        if (nrow(plt1) != nrow(stpltids1)) {
          message("plots where ", Endyr.filter, " do not match pltids")
        }
        pids1 <- plt1[[puniqueid]]
		    ppltidnm1 <- findnm("PREV_PLT_CN", names(plt1), returnNULL = TRUE)
		    if (any(Type %in% c("CHNG", "GRM"))) { 
	         ppltids1 <- plt1[plt1[[puniqueid]] %in% pids1, ppltidnm1] 
		      pids1 <- c(pids1, ppltids1)
		    }

        ## Subset other tables in list
        stcliptabs$plt <- plt1
		    for (tabnm in names(tabs1)[names(tabs1) != "plt"]) {
          if (tabIDs[[tabnm]] %in% names(tabs1[[tabnm]])) {
            stcliptabs[[tabnm]] <- tabs1[[tabnm]][tabs1[[tabnm]][[tabIDs[[tabnm]]]] %in% pids1, ]			
		      } else {
            stcliptabs[[tabnm]] <- rbind(stcliptabs[[tabnm]], 
				                  tabs1[[tabnm]][tabs1[[tabnm]][[tabIDs[[tabnm]]]] %in% pids1, ])
          }
        }
      }  
        
      ## Get plots outside filter
      #######################################
      if (nrow(stpltids2) > 0) {
        countyfips2 <- formatC(as.numeric(countyfips2), width=5, digits=5, flag="0")
        stcnty2 <- countyfips2[startsWith(countyfips2, formatC(stcd, width=2, flag="0"))]
        countycds2 <- sort(as.numeric(unique(substr(stcnty2, 3, 5))))
        stateFilterDB2 <- paste("p.countycd IN(", toString(countycds2), ")")
      
        if ("stateFilter" %in% input.params) {
          stateFilterDB2 <- paste(stateFilter, "&", stateFilterDB2) 
          rm(stateFilter)
        }

        dat2 <- DBgetPlots(states = stcd, 
                         datsource = datsource,
                         data_dsn = data_dsn, 
                         dbTabs = dbTabs,
                         eval = eval,
                         eval_opts = eval_opts2,
                         puniqueid = puniqueid,
                         pjoinid = pjoinid, 
                         getxy = FALSE,
                         stateFilter = stateFilterDB2, 
                         returndata = TRUE,
                         dbconn = dbconn,
                         ...
                         )
        tabs2 <- dat2$tabs
        tabIDs <- dat2$tabIDs
        pop_plot_stratum_assgn2 <- dat2$pop_plot_stratum_assgn
        evalid2 <- dat2$evalid
        PLOT2 <- tabs2$plt
	      PLOT2u <- tabs2$pltu

	      evalidlst <- unique(c(evalidlst, evalid2))

        if (nrow(PLOT2) > length(unique(PLOT2[[puniqueid]]))) {
          if ("INVYR" %in% names(PLOT2)) {
            setorder(PLOT2, -INVYR)
          } else {
            setorderv(PLOT2, -puniqueid)
          }
          PLOT2 <- PLOT2[, head(.SD, 1), by=pjoinid]
        }

        ## Subset data to stpltids
	      if (length(xyjoinid) > 1) {
	        PLOT2$MATCH <- apply(PLOT2[,pjoinid], 1, paste, collapse = "-")
	        stpltids2$MATCH <- apply(stpltids2[,pjoinid], 1, paste, collapse = "-")
	        plt2 <- PLOT2[PLOT2[["MATCH"]] %in% stpltids2[["MATCH"]],]
	      } else {
          plt2 <- PLOT2[PLOT2[[pjoinid]] %in% stpltids2[[xyjoinid]],]
	      }
        if (nrow(plt2) != nrow(stpltids2)) {
          message("plots outside filter do not match pltids")
        }
        pids2 <- plt2[[puniqueid]]
		    ppltidnm2 <- findnm("PREV_PLT_CN", names(plt2), returnNULL = TRUE)
		    if (any(Type %in% c("CHNG", "GRM"))) { 
	        ppltids2 <- plt2[plt2[[puniqueid]] %in% pids2, ppltidnm2] 
		      pids2 <- c(pids2, ppltids2)
		    }
		    
		    if (nrow(stpltids1) > 0) {
          ## rbind tables from dat1 and dat2
          stcliptabs$plt <- rbind(stcliptabs$plt, plt2)
          pop_plot_stratum_assgn <- rbind(pop_plot_stratum_assgn1, pop_plot_stratum_assgn2)
          
		    } else {
		      ## Subset other tables in list
		      stcliptabs$plt <- plt2
		      pop_plot_stratum_assgn <- pop_plot_stratum_assgn2
		    }

        for (tabnm in names(tabs2)[names(tabs2) != "plt"]) {
          if (tabIDs[[tabnm]] %in% names(tabs2[[tabnm]])) {
            stcliptabs[[tabnm]] <- tabs2[[tabnm]][tabs2[[tabnm]][[tabIDs[[tabnm]]]] %in% pids2, ]			
		      } else {
            stcliptabs[[tabnm]] <- rbind(stcliptabs[[tabnm]], 
				                  tabs2[[tabnm]][tabs2[[tabnm]][[tabIDs[[tabnm]]]] %in% pids2, ])
		      }
        }
      }
    } else {   ## Endyr.filter is null

      ## Check for counties
      if (!is.null(countyfips)) {
        countyfips <- formatC(as.numeric(countyfips), width=5, digits=5, flag="0")
        stcnty <- countyfips[startsWith(countyfips, formatC(stcd, width=2, flag="0"))]
        countycds <- sort(as.numeric(unique(substr(stcnty, 3, 5))))
        stateFilterDB <- paste("p.countycd IN(", toString(countycds), ")")
      }
 
      if (!is.null(evalInfo)) {
        evalInfost <- evalInfo
        if (!state %in% evalInfost$states) {
          stop("invalid evalInfo")
        }
        evalInfost$states <- state
        if (!is.null(evalInfost$evalidlist)) {
          evalInfost$evalidlist <- evalInfost$evalidlist[names(evalInfost$evalidlist) == state]
        }        
        if (!is.null(evalInfost$invyrtab)) {
          if ("STATECD" %in% names(evalInfost$invyrtab)) {
            evalInfost$invyrtab <- evalInfost$invyrtab[evalInfost$invyrtab$STATECD == stcd,]
          } else if ("STATENM" %in% names(evalInfost$invyrtab)) {
            evalInfost$invyrtab <- evalInfost$invyrtab[evalInfost$invyrtab$STATECD == state,]
          }
        }
        if (!is.null(evalInfost$SURVEY)) {
          evalInfost$SURVEY <- evalInfost$SURVEY[evalInfost$SURVEY$STATECD == stcd,]
        }
      } else {
        evalInfost <- NULL
      }
 
      if ("stateFilter" %in% input.params) {
        stateFilterDB <- paste(stateFilterDB, "&", stateFilter) 
        rm(stateFilter)
      }
      
     dat <- DBgetPlots(states = stcd, 
                         datsource = datsource,
                         data_dsn = data_dsn, 
                         dbTabs = dbTabs,
                         eval = eval,
                         eval_opts = eval_opts,
                         puniqueid = puniqueid, 
                         pjoinid = pjoinid, 
                         getxy = FALSE,
                         stateFilter = stateFilterDB, 
                         returndata = TRUE,
                         evalInfo = evalInfost,
                         dbconn = dbconn,
                         ...
                         )
      tabs <- dat$tabs
      tabIDs <- dat$tabIDs
      pop_plot_stratum_assgn <- dat$pop_plot_stratum_assgn
      evalid <- dat$evalid
      PLOT <- tabs$plt
	    PLOTu <- tabs$pltu
      puniqueid <- dat$puniqueid
      dbqueries <- dat$dbqueries
      
      evalidlst <- unique(c(evalidlst, evalid))
      

      if (is.null(PLOT)) {
        message("no data for ", stcd)
        break
      }

      ## Check pjoinid
      ##############################################
      pltfields <- names(PLOT)
      pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", 
  		                  checklst=pltfields, gui=gui, caption="Joinid in plot?")  
      if (is.null(pjoinid)) {
        if (xyjoinid %in% pltfields) {
          pjoinid  <- xyjoinid
        } else {
          if (puniqueid %in% names(pltids)) {
            pjoinid <- pjoinid
          } else if (xyjoinid == "PLT_CN" && "CN" %in% pltfields) {
            pjoinid <- "CN"
          } else {
            stop(xyjoinid, " not in plt")
          }
        }
      }
      
      ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
      if (nrow(PLOT) > length(unique(PLOT[[puniqueid]]))) {
        if ("INVYR" %in% names(PLOT)) {
          setorder(PLOT, -INVYR)
        } else {
          setorderv(PLOT, -puniqueid)
        }
        PLOT <- PLOT[, head(.SD, 1), by=pjoinid]
      }

      if (nrow(stpltids) > 0) {
        ## Subset data to stpltids
        if (length(xyjoinid) > 1) {
          PLOT$MATCH <- apply(PLOT[,pjoinid], 1, paste, collapse = "-")
          stpltids$MATCH <- apply(stpltids[,pjoinid], 1, paste, collapse = "-")
          plt <- PLOT[PLOT[["MATCH"]] %in% stpltids[["MATCH"]],]
        } else {
          plt <- PLOT[PLOT[[pjoinid]] %in% stpltids[[xyjoinid]],]
        }
        if (nrow(plt) != nrow(stpltids)) {
          message("there are ", abs(nrow(plt) - nrow(stpltids)), 
		             	" plots in ", state, " that do not match pltids")
          #spxy[!spxy[[xyjoinid]] %in% plt[[pjoinid]],] 
          messagedf(stpltids[[xyjoinid]][!stpltids[[xyjoinid]] %in% PLOT[[pjoinid]]])
        }
        pids <- plt[[puniqueid]]
		    ppltidnm <- findnm("PREV_PLT_CN", names(plt), returnNULL = TRUE)	     
		    if (any(Type %in% c("CHNG", "GRM"))) { 
	        ppltids <- plt[plt[[puniqueid]] %in% pids, ppltidnm] 
		      pids <- c(pids, ppltids)
		    }
		
#print(pids)	
        ## Subset other tables in list
        stcliptabs$plt <- plt
        for (tabnm in names(tabs)[names(tabs) != "plt"]) {
          if (tabIDs[[tabnm]] %in% names(tabs[[tabnm]])) {
            stcliptabs[[tabnm]] <- tabs[[tabnm]][tabs[[tabnm]][[tabIDs[[tabnm]]]] %in% pids, ]			
		      } else {
		        stcliptabs[[tabnm]] <- tabs[[tabnm]]
		      }
        }
      }
    }

    ## if returndata... append all tables
    if (returndata) {
      for (tabnm in names(stcliptabs)) {
        tabs2save[[tabnm]] <- rbind(tabs2save[[tabnm]], stcliptabs[[tabnm]])
      }
    }       

    ###############################################################################
    ## SAVE data
    ###############################################################################
    if (savedata) {
      message("saving data...")
      col.names <- ifelse (i == 1, TRUE, FALSE)
      if (i > 1) { 
        append_layer <- TRUE
      }
      if (append_layer && overwrite_layer) {
        overwrite_layer <- FALSE
      }

      for (tabnm in names(stcliptabs)) {
        tab <- stcliptabs[[tabnm]]
        if (tabnm == "plt") tabnm <- "plot"

        indx <- ifelse(tabnm == "plt", "CN", 
		    ifelse(tabnm %in% c("cond", "vsubpspp", "vsubpstr", 
						         "invsubp", "dwm"), c("PLT_CN", "CONDID"),
		    ifelse(tabnm %in% c("tree", "grm"), c("PLT_CN", "CONDID", "TREE"), 
			  ifelse(tabnm == "subplot", c("PLT_CN", "SUBP"), 
			  ifelse(tabnm == "subpcond", c("PLT_CN", "SUBP", "CONDID"), "PLT_CN")))))
        if (!all(indx %in% names(tab))) {
          indx <- indx[indx %in% names(tab)]
        }

        if (!is.null(tab) && nrow(tab) > 0) {
          assign(paste0("index.unique.", tabnm), NULL)
          if (is.null(tabIDs[[tabnm]]) && i == 1 && length(indx) > 0) {
            assign(paste0("index.unique.", tabnm), indx)
          }
          outlst$out_layer <- tabnm
          datExportData(tab, 
                        index.unique = get(paste0("index.unique.", tabnm)),
                        savedata_opts = outlst)
        }
      } 

      if (savePOP && !is.null(pop_plot_stratum_assgn)) {
        index.unique.ppsa <- NULL
        if (!append_layer) {
          index.unique.ppsa <- "PLT_CN"
        }
        outlst$out_layer <- "pop_plot_stratum_assgn"
        datExportData(pop_plot_stratum_assgn, 
                        index.unique = index.unique.ppsa,
                        savedata_opts = outlst) 
        rm(pop_plot_stratum_assgn)
          # gc()
      }

      if (showsteps && !is.null(spxy) && !is.null(bndx)) {
        ## Set plotting margins
        mar <-  graphics::par("mar")
        on.exit(graphics::par(mar=mar))
        
        if (i == 1) {
          if (!is.null(bndx)) {
            plot(sf::st_geometry(bndx), border="black", lwd=0.75)
          } else {
            plot(sf::st_geometry(spxy[spxy$STATECD == stcd,]), col="transparent", cex=.5)
          }
        }
        plot(sf::st_geometry(spxy[spxy$STATECD == stcd,]), col="blue", cex=.5, add=TRUE)
      } 
    }  ## End of looping thru states
  }  ## datsource
 
      
  #############################################################################
  ## Save tables
  #############################################################################
  if (savedata) {
    if (savebnd) {
      outlst$out_layer <- "bnd"
      spExportSpatial(bndx, 
                      savedata_opts = outlst)   
    }
  }
  if (savexy) {
    if (!is.null(spxy)) {
      if (exportsp) {
        outlst$out_layer <- "spxyplt"
        spExportSpatial(spxy,
                        savedata_opts = outlst)
      } else {
        outlst$out_layer <- "xyplt"
        datExportData(sf::st_drop_geometry(spxy), 
                      savedata_opts = outlst) 
	  }
    } else {
      outlst$out_layer <- "pltids"
      datExportData(pltids, 
                    savedata_opts = outlst) 
    } 
  }
  
  if (returnxy && !is.null(spxy)) {
    returnlst$spxy <- spxy
  }
 
  if (returndata) {
    returnlst$tabs <- tabs2save
    returnlst$tabIDs <- tabIDs
    returnlst$pltids <- pltids
    #returnlst$clip_polyv <- bndx

    if (!is.null(bndx)) {
      returnlst$bnd <- bndx
    }
    returnlst$puniqueid <- puniqueid
    returnlst$xy.uniqueid <- xyjoinid
    returnlst$pjoinid <- pjoinid
    returnlst$states <- states
    returnlst$xyqry <- xyqry

    if ("plt" %in% names(tabs2save) && "INVYR" %in% names(tabs2save$plt)) {
      invyrnm <- findnm("INVYR", names(tabs2save$plt), returnNULL=TRUE)
      if (!is.null(invyrnm)) {
        returnlst$invyrs <- sort(unique(tabs2save$plt$INVYR))
      } else {
        invyrnm <- findnm("INVYR", names(pltids), returnNULL=TRUE)
        if (!is.null(invyrnm)) {
          returnlst$invyrs <- sort(unique(pltids$INVYR))
        }
      }
    }
 
    #if (savePOP && !is.null(pop_plot_stratum_assgn)) {
    #  returnlst$pop_plot_stratum_assgn <- get(ppsanm)
    #}
    if (iseval) {
      returnlst$evalid <- unlist(evalidlst)
    }
    
    ## Disconnect database
    if (!is.null(dbconn) && !dbconnopen && DBI::dbIsValid(dbconn)) {
      DBI::dbDisconnect(dbconn)
    } else {
      returnlst$dbconn <- dbconn
    }
    
    
    if (returndata) {
      returnlst$args <- args
      return(returnlst)
    }
  }
}

