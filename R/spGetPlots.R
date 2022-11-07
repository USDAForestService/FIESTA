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
#' @param xy_opts List of xy data options to specify. See xy_options.
#'  (e.g., xy_opts = list(xy='PLOT', xvar='LON', yvar='LAT').
#' @param eval_opts List of evaluation options to determine the set of data.
#'  returned. See help(eval_options) for a list of options.
#' @param measEndyr.filter Filter. If measCur=TRUE and measEndyr != NULL, a
#' filter for bnd to identify and area to use measEndyr, such as disturbed
#' areas where you want to exclude plots measured after disturbance.
#' @param pjoinid String. Variable in plt to join to XY data. Not necessary to
#' be unique. If using most current XY coordinates, use identifier for a plot
#' (e.g., PLOT_ID).
#' @param clipxy Logical. If TRUE, clips xy data to bnd.
#' @param datsource String. Source of FIA data ("obj", "csv", "datamart",
#' "sqlite").  If datsource="sqlite", specify database name in data_dsn and
#' layers in *_layer arguments.  If datsource="datamart", files are downloaded
#' and extracted from FIA DataMart
#' (http://apps.fs.usda.gov/fia/datamart/datamart.html). See details for more
#' information about plot coordinates.  If datsource="csv", specify *.csv file
#' names in *_layer arguments.
#' @param data_dsn String. Name of database where *_layers reside.
#' @param istree Logical. If TRUE, extract tree data from FIA database.
#' @param isseed Logical. If TRUE, extract seedling data from FIA database.
#' @param isveg Logical. If TRUE, understory vegetation tables are extracted
#' from FIA database (P2VEG_SUBPLOT_SPP, P2VEG_SUBP_STRUCTURE, INVASIVE_SUBPLOT_SPP).
#' @param isdwm Logical. If TRUE, down woody material are extracted.
#' @param dbTabs List of database tables the user would like returned.
#'  See help(dbTables) for a list of options.
#' @param eval_opts List of evaluation options to determine the set of data.
#'  returned. See help(eval_options) for a list of options.
#' @param other_layers String. Other layer(s) in database to clip and/or
#' extract from database (Note: must include PLT_CN variable as unique
#' identifier).
#' @param puniqueid String. Name of unique identifier of plt.
#' @param pltassgnid String. Name of unique identifier of pop_plot_stratum_assgn.
#' @param savePOP Logical. If TRUE, returns and/or saves POP_PLOT_STRATUM_ASSGN
#' table.
#' @param intensity1 Logical. If TRUE, include only single intensity plots
#' (i.e., INTENSITY = 1).
#' @param showsteps Logical. If TRUE, display data in device window.
#' @param returndata Logical. If TRUE, returns data objects.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savebnd Logical. If TRUE, saves bnd. If out_fmt='sqlite', saves to a
#' SpatiaLite database.
#' @param returnxy Logical. If TRUE, save xy coordinates to outfolder.
#' @param exportsp Logical. If returnxy=TRUE, if TRUE, saves xy data as 
#' spatial data. If FALSE, saves xy data as table.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param spXYdat R list object. Output from spGetXY().
#' @param gui Logical. If TRUE, uses gui interface. 
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
#' If savebnd=TRUE:\cr If out_fmt=c('csv','shp'), the writeOGR (rgdal) function
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
#'                   measyrs = 2013:2015)
#' names(dat)
#' tabs <- dat$tabs
#' names(tabs)
#' head(tabs$pltx)
#' 
#' table(tabs$pltx$MEASYEAR)
#' 
#' # Extract data from FIA datamart for most current evaluation
#' datCur <- spGetPlots(bnd = WYbhfn,
#'                      datsource = "datamart",
#'                      evalCur = TRUE)
#' names(datCur)
#' tabsCur <- datCur$tabs
#' names(tabsCur)
#' head(tabsCur$pltx)
#' 
#' table(tabsCur$pltx$MEASYEAR)
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
                       xy_opts = xy_options(),
                       eval_opts = eval_options(),
                       measEndyr.filter = NULL,
                       pjoinid = NULL, 
                       clipxy = TRUE, 
                       datsource = NULL,
                       data_dsn =NULL, 
                       istree = FALSE, 
                       isseed = FALSE, 
                       isveg = FALSE,
                       isdwm = FALSE,
                       dbTabs = dbTables(), 
                       other_layers = NULL, 
                       puniqueid = "CN", 
                       pltassgnid = "PLT_CN",
                       savePOP = FALSE,
                       intensity1 = FALSE, 
                       showsteps = FALSE, 
                       returndata = TRUE,
                       savedata = FALSE, 
                       savebnd = FALSE, 
                       returnxy = TRUE, 
                       exportsp = FALSE, 
                       savedata_opts = NULL,
                       spXYdat = NULL,
                       gui = FALSE) {

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
  xydat=stateFilter=countyfips=xypltx=tabs2save=evalidst=PLOT_ID=INVYR=
	othertabnms=stcds=spxy=stbnd=invasive_subplot_spp=subp=subpc <- NULL
  cuniqueid=tuniqueid=duniqueid <- "PLT_CN"
  stbnd.att <- "COUNTYFIPS"
  returnlst <- list()
  #clipdat <- list()

  gui <- FALSE
  coordtype <- "public"
  evalresp <- FALSE
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(spGetPlots)), 
		names(formals(spGetPlots)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts, eval_opts=eval_opts)

  ## Set dbTables defaults
  dbTables_defaults_list <- formals(FIESTA::dbTables)[-length(formals(FIESTA::dbTables))]
  
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

  ## Set eval_options defaults
  eval_defaults_list <- formals(FIESTAutils::eval_options)[-length(formals(FIESTAutils::eval_options))]

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
    stop("must specify an evaluation timeframe for data extraction... \n", 
	"...see eval_opts parameter, (e.g., eval_opts=eval_options(evalCur=TRUE))")
  }


  ## Set xy_options defaults
  xy_defaults_list <- formals(FIESTAutils::xy_options)[-length(formals(FIESTAutils::xy_options))]
  
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
 
  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################      
 
  ## Define list of pop_tables (without PLT_CN)
  pop_tables <- c("POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", 
	"POP_EVAL_TYP", "POP_STRATUM", "SURVEY") 

  ## Check clipxy
  clipxy <- pcheck.logical(clipxy, varnm="clipxy", 
                            title="Clip xy?", first="NO", gui=gui)  
 
  ## Check spXYdat
  if (!is.null(spXYdat)) {
    spxy <- spXYdat$spxy
    pltids <- spXYdat$pltids
    states <- spXYdat$states
    countyfips <- spXYdat$countyfips
    stbnd.att <- spXYdat$stbnd.att
    xy.uniqueid <- spXYdat$xy.uniqueid
    bndx <- spXYdat$bndx
    stcds <- pcheck.states(states, statereturn="VALUE")
    if (is.null(pltids) && (is.null(spxy) || nrow(spxy)) == 0) {
      stop("spxy is null")
    } 

    ## Check xyjoinid
    xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
	      checklst=names(pltids), gui=gui, caption="JoinID in pltids?", 
		stopifnull=FALSE) 
    if (is.null(xyjoinid)) {
      message("no xyjoinid defined... using xy.uniqueid: ", xy.uniqueid)
      xyjoinid <- xy.uniqueid
    } 

  } else {   ## is.null(spXYdat) 

    ## Check pltids
    pltids <- pcheck.table(pltids)
 
    if (!is.null(pltids)) {
      measEndyr.filter <- check.logic(pltids, measEndyr.filter)

      ## Check xyjoinid
      xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
		checklst=names(pltids), gui=gui, caption="JoinID in pltids?", stopifnull=TRUE)  
 
      ## Check stbnd.att
      stbnd.att <- pcheck.varchar(var2check=stbnd.att, varnm="stbnd.att", 
		checklst=names(pltids), gui=gui, caption="State attribute?") 
      
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

      if (!is.null(measEndyr.filter)) {
        measEndyr.filter <- check.logic(pltids, measEndyr.filter)

        ## Split pltids
        pltids1 <- datFilter(pltids, xfilter = measEndyr.filter)$xf
        nbrxy <- ifelse (is.null(pltids1), 0, nrow(pltids1)) 
        message ("there are ", nbrxy, " plots where ", measEndyr.filter)
        pltids2 <- datFilter(pltids, xfilter=paste0("!", measEndyr.filter))$xf
      }

    } else { 	## is.null(pltids)
      if (!is.null(measEndyr.filter)) {
        if (!is.null(measEndyr.filter)) {
          if (is.null(measEndyr) && is.null(evalEndyr)) {
            stop("must include measEndyr or evalEndyr") 
          } else {
            if (!is.null(measEndyr) && measCur) {
              eval_opts$measCur <- TRUE
              varEndyr <- "measEndyr"
            } else {
              eval_opts$evalCur <- TRUE
              varEndyr <- "evalEndyr"
            }
            eval_opts1 <- eval_opts
            eval_opts2 <- eval_opts
            eval_opts2[[varEndyr]] <- NULL
          }
        }
      } 
 
      ## Import boundary
      bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
      if (!is.null(bndx)) {
        bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf
      } 
      if (!is.null(measEndyr.filter)) {
        measEndyr.filter <- check.logic(bndx, measEndyr.filter)

        ## split boundaries
        bndx1 <- datFilter(bndx, xfilter = measEndyr.filter)$xf
        if (nrow(bndx1) == 0) {
          stop("invalid measEndyr.filter: ", measEndyr.filter) 
        }
        bndx2 <- datFilter(bndx, xfilter=paste0("!", measEndyr.filter))$xf
        if (nrow(bndx2) == 0) {
          stop("invalid measEndyr.filter: ", measEndyr.filter) 
        }
      }
 
      ## Check states
      if (!is.null(states)) {
        if (!all(states %in% FIESTAutils::ref_statecd$MEANING)) stop("states is invalid")
      }

      if (clipxy) {
        ###########################################################################
        ## Get XY
        ###########################################################################
        if (is.null(xy_datsource)) {
          xy_datsource <- datsource
        } 
        if (is.null(xy_dsn)) {
          xy_dsn <- data_dsn
        } 

        if (!is.null(measEndyr.filter)) {

          ## Get XY data inside filter
          #######################################
          xydat1 <- spGetXY(bnd = bndx1, 
                         states = states, 
                         RS = RS, 
                         xy_datsource = xy_datsource, 
                         xy_dsn = xy_dsn, 
                         xy_opts = xy_opts,
                         eval_opts = eval_opts1,
                         pjoinid = pjoinid, 
                         clipxy = clipxy, 
                         intensity1 = intensity1, 
                         showsteps = showsteps, 
                         returnxy = TRUE)
          spxy1 <- xydat1$spxy
          pltids1 <- xydat1$pltids
          states1 <- xydat1$states
          countyfips1 <- xydat1$countyfips
          stbnd.att <- xydat1$stbnd.att
          xy.uniqueid <- xydat1$xy.uniqueid
          bndx1 <- xydat1$bndx

          ## Get plots outside filter
          #######################################
          xydat2 <- spGetXY(bnd = bndx2, 
                         states = states, 
                         RS = RS, 
                         xy_datsource = xy_datsource, 
                         xy_dsn = xy_dsn, 
                         xy_opts = xy_opts,
                         eval_opts = eval_opts2,
                         pjoinid = pjoinid, 
                         clipxy = clipxy, 
                         intensity1 = intensity1, 
                         showsteps = showsteps, 
                         returnxy = TRUE)
          spxy2 <- xydat2$spxy
          pltids2 <- xydat2$pltids
          states2 <- xydat2$states
          countyfips2 <- xydat2$countyfips
          bndx2 <- xydat2$bndx

          ## Combine XYdata inside and outside filter
          spxy <- rbind(spxy1, spxy2)
          pltids <- rbind(pltids1, pltids2)
          states <- unique(states1, states2)
          countyfips <- unique(countyfips1, countyfips2)
          bndx <- rbind(bndx1, bndx2)

        } else {
          xydat <- spGetXY(bnd = bndx, 
                         states = states, 
                         RS = RS, 
                         xy_datsource = xy_datsource, 
                         xy_dsn = xy_dsn, 
                         xy_opts = xy_opts,
                         eval_opts = eval_opts,
                         pjoinid = pjoinid, 
                         clipxy = clipxy, 
                         intensity1 = intensity1, 
                         showsteps = showsteps, 
                         returnxy = TRUE)

          spxy <- xydat$spxy
          pltids <- xydat$pltids
          states <- xydat$states
          countyfips <- xydat$countyfips
          stbnd.att <- xydat$stbnd.att
          bndx <- xydat$bndx
          xy.uniqueid <- xydat1$xy.uniqueid        
        }

        ## Check xyjoinid
        xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
	            checklst=names(pltids), gui=gui, caption="JoinID in pltids?", 
	            stopifnull=FALSE) 
        if (is.null(xyjoinid)) {
          if ("PLOT_ID" %in% names(pltids)) {
            xyjoinid <- "PLOT_ID"
            message("no xyjoinid defined... using the defined uniqueid: PLOT_ID")
          } else {
            message("no xyjoinid defined... using the defined uniqueid: ", xy.uniqueid)
            xyjoinid <- xy.uniqueid
          }
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
        xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
		        checklst=names(spxy), gui=gui, caption="JoinID in xy?", 
		        stopifnull=TRUE)  

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
    if (nrow(pltids) > length(unique(pltids[[xyjoinid]]))) {
      message("pltids have duplicate xyjoinids")
    }
  }

  #############################################################################
  ## Set datsource
  ########################################################
  datsourcelst <- c("obj", "csv", "datamart", "sqlite", "gdb")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
		checklst=datsourcelst, gui=gui, caption="Data source?") 
  if (is.null(datsource)) {
    if (!is.null(data_dsn) && file.exists(data_dsn)) {
      dsn.ext <- getext(data_dsn)
      if (!is.na(dsn.ext) && dsn.ext != "") {
        datsource <- ifelse(dsn.ext == "gdb", "gdb", 
		ifelse(dsn.ext %in% c("db", "db3", "sqlite", "sqlite3"), "sqlite", 
             ifelse(dsn.ext == "csv", "csv",
			ifelse(dsn.ext == "shp", "shp", "datamart"))))
      } 
    } else {
      stop("datsource is invalid")
    }
  }
  if (!is.null(xy_datsource) && datsource != xy_datsource) {
    message("datsource is not the same as xy_datsource")
  }
  if (datsource %in% c("sqlite", "gdb")) {
    if (is.null(data_dsn)) {
      stop("data_dsn is NULL")
    }
    if (!file.exists(data_dsn)) {
      stop(data_dsn, " is invalid")
    }
  }

  ## Check showsteps
  #############################################################################
  showsteps <- pcheck.logical(showsteps, varnm="showsteps", 
                             title="Show steps?", first="NO", gui=gui) 
 
  ## Check returnxy
  #############################################################################
  returnxy <- pcheck.logical(returnxy, varnm="returnxy", 
		title="Return XY?", first="NO", gui=gui)  

  ## Check returndata
  #############################################################################
  returndata <- pcheck.logical(returndata, varnm="returndata", 
                      title="Return data?", first="YES", gui=gui)  

  ## Check savedata
  #############################################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="NO", gui=gui)  

  ## Check savebnd
  #############################################################################
  if (!is.null(bndx)) {
    savebnd <- pcheck.logical(savebnd, varnm="savebnd",
		    title="Save spatial bnd?", first="NO", gui=gui)
  } else {
    savebnd <- FALSE
  }
 
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || savebnd) {
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
 
  #############################################################################
  ## If xy is separate file or database, and clipxy=TRUE, import first
  #############################################################################
  if (datsource %in% c("obj", "csv")) {

    ####################################################################
    ## 1) Import file(s)
    ## 2) Clip xy (for all states) to boundary
    ## 3) Subset other data with clipped xy joinid
    ####################################################################
    tabs2save <- list()

    ## plot data
    obj <- ifelse (datsource == "obj", TRUE, FALSE)
    pltx <- pcheck.table(plot_layer, obj=obj, stopifnull=FALSE)
    if (!is.null(pltx)) {
      pltfields <- names(pltx)
    }

    ## condition data
    condx <- pcheck.table(cond_layer, obj=obj, stopifnull=TRUE)

    ## tree data
    if (istree) {
      treex <- pcheck.table(tree_layer, obj=obj, stopifnull=TRUE)
    }
    ## seed data
    if (isseed) {
      seedx <- pcheck.table(seed_layer, obj=obj, stopifnull=TRUE)
    }
    ## P2 veg data
    if (isveg) {
      p2veg_subplot_sppx <- pcheck.table(vsubpspp_layer, obj=obj, stopifnull=TRUE)
      p2veg_subp_structurex <- pcheck.table(vsubpstr_layer, obj=obj, stopifnull=TRUE)
      invasive_subplot_sppx <- pcheck.table(invsubp_layer, obj=obj, stopifnull=TRUE)
    }
    ## pop_plot_stratam_assgn data
    pop_plot_stratum_assgnx <- tryCatch(pcheck.table(ppsa_layer, obj=obj),
     	 	error=function(e) {
			return(NULL) })
    if (is.null(pop_plot_stratum_assgnx)) {
      if (savePOP) {
        stop("ppsa_layer is invalid")
      }
    }

    ## Check pjoinid
    pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", 
		checklst=names(pltx), gui=gui, caption="Joinid in plot?")  

    ## Define pjoinid
    if (is.null(pjoinid)) {
      if (xyjoinid %in% pltfields) {
        pjoinid  <- xyjoinid
      } else {
        if (xyjoinid == "PLT_CN" && "CN" %in% pltfields) {
          pjoinid <- "CN"
        } else {
          stop(xyjoinid, " not in plt")
        }
      }
    }

    ## Check if class of pjoinid in pltx matches class of xyjoinid in pltids
    tabs <- check.matchclass(pltx, pltids, pjoinid, xyjoinid)
    pltx <- tabs$tab1
    pltids <- tabs$tab2
     
    ## Subset plot data
    pltx <- pltx[pltx[[pjoinid]] %in% pltids[[xyjoinid]],]
    if (nrow(pltx) == 0) stop("xyjoinid invalid")
    tabs2save$plt <- pltx

    ## Get plot ids from pltx
    pltids <- pltx[[puniqueid]]

    ## Subset cond data
    condx <- condx[condx[[cuniqueid]] %in% pltids,]
    if (nrow(condx) > 0) {
      tabs2save$cond <- condx
    }
    ## Subset tree data
    if (istree) {
      treex <- treex[treex[[tuniqueid]] %in% pltids,]
      if (nrow(treex) > 0) {
        tabs2save$tree <- treex
      }
    }
    ## Subset seed data
    if (isseed) {
      seedx <- seedx[seedx[[tuniqueid]] %in% pltids,]
      if (nrow(seedx) > 0) {
        tabs2save$seed <- seedx
      }
    }
    ## Subset P2VEG data
    if (isveg) {
      if (!is.null(p2veg_subplot_sppx)) {
        p2veg_subplot_sppx <- p2veg_subplot_sppx[p2veg_subplot_sppx[[tuniqueid]] %in% pltids,]
        if (nrow(p2veg_subplot_sppx) > 0) {
          tabs2save$p2veg_subplot_spp <- p2veg_subplot_sppx
        }
      }
      if (!is.null(p2veg_subp_structurex)) {
        p2veg_subp_structurex <- p2veg_subp_structurex[p2veg_subp_structurex[[tuniqueid]] %in% pltids,]
        if (nrow(p2veg_subp_structurex) > 0) {
          tabs2save$p2veg_subp_structure <- p2veg_subp_structurex
        }
      }
      if (!is.null(invasive_subplot_sppx)) {
        invasive_subplot_sppx <- invasive_subplot_sppx[invasive_subplot_sppx[[tuniqueid]] %in% pltids,]
        if (nrow(invasive_subplot_sppx) > 0) {
          tabs2save$invasive_subplot_spp <- invasive_subplot_sppx
        }
      }
    }

    ## other data
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
          assign(paste0(layer, "x"), get(layer)[get(layer)[["PLT_CN"]] %in% pltids, ])
        }
      }
    }

    ## Subset pop_plot_stratum_assgn data
    if (savePOP) {
      pop_plot_stratum_assgnx <- 
		pop_plot_stratum_assgnx[pop_plot_stratum_assgnx[[pltassgnid]] %in% pltids,]
    }

    if (!is.null(evalid)) {
      if (length(evalid) > 1) {
        if (!is.null(pop_plot_stratum_assgnx)) {
          evalidnm <- findnm("EVALID", names(pop_plot_stratum_assgnx), returnNULL=TRUE)
          if (is.null(evalidnm)) {
            stop("must include evalid in ppsa_layer or plt_layer")
          } else {
            pop_evalidlst <- unique(pop_plot_stratum_assgnx[[evalidnm]])
            if (!all(evalid %in% pop_evalidlst)) {
              miss <- evalid[!evalid %in% evalidlst]
              stop(miss, " not in dataset")
            }
          }
        } else if (!is.null(pltx)) {
          evalidnm <- findnm("EVALID", names(pltx), returnNULL=TRUE)
          if (is.null(evalidnm)) {
            stop("must include evalid in ppsa_layer or plt_layer")
          } else {
            plt_evalidlst <- unique(pltx[[evalidnm]])
            if (!all(evalid %in% plt_evalidlst)) {
              miss <- evalid[!evalid %in% evalidlst]
              stop(miss, " not in dataset")
            }
          }          
        } else {
          stop("need ppsa_layer with more than one evalid")
        }
      } else {
        message("assuming data is ", evalid)
      }

      evalresp <- TRUE
    }          
    
  } else {			## datsource in('datamart', 'sqlite')

    ## Initialize lists
    tabs2save <- {}

    msg <- "getting data for..."
    if (!is.null(evalid)) {
      evalresp=savePOP <- TRUE
      evalid <- unlist(evalid)
      msg <- paste0(msg, "for evaluation: ", toString(evalid))
      savePOP <- TRUE
    } else if (allyrs) {
      msg <- paste0(msg, "for all years")
    } else if (measCur) {
      msg <- paste0(msg, "for most currently measured plots")
      if (!is.null(measEndyr)) {
        msg <- paste0(msg, ", from year ", measEndyr, " or before")
        if (!is.null(measEndyr.filter)) {
          msg <- paste0(msg, ", ", measEndyr.filter)
        }
      }
    } else if (evalCur) {
      evalresp=savePOP <- TRUE
      msg <- paste0(msg, "for most current evaluation")
    } else if (!is.null(evalEndyr)) {
      evalresp=savePOP <- TRUE
      msg <- paste0("ending in ", evalEndyr)
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
  
    for (i in 1:length(states)) { 
      stcliptabs <- list()
      state <- states[i]
      stcd <- pcheck.states(state, statereturn="VALUE")
      stabbr <- pcheck.states(stcd, statereturn="ABBR") 
      message(paste0("\n", state, "..."))

      if ("STATECD" %in% names(pltids)) {
        stpltids <- pltids[pltids$STATECD == stcd, ]
        if (!is.null(measEndyr.filter)) {
          stpltids1 <- pltids1[pltids1$STATECD == stcd, ]
          stpltids2 <- pltids2[pltids2$STATECD == stcd, ]
        }
      } else {
        stpltids <- pltids
        if (!is.null(measEndyr.filter)) {
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
      if (!is.null(measEndyr.filter)) {
 
        ## Get plots inside filter
        #######################################
        if (!is.null(countyfips1)) {
          countyfips1 <- formatC(as.numeric(countyfips1), width=5, digits=5, flag="0")
          stcnty1 <- countyfips1[startsWith(countyfips1, formatC(stcd, width=2, flag="0"))]
          countycds1 <- sort(as.numeric(unique(substr(stcnty1, 3, 5))))
          stateFilter1 <- paste("p.countycd IN(", toString(countycds1), ")")
        }
        dat1 <- DBgetPlots(states=stcd, datsource=datsource, 
                         data_dsn=data_dsn, 
                         stateFilter=stateFilter1, eval_opts=eval_opts1,
                         istree=istree, isseed=isseed, othertables=other_layers, 
                         intensity1=intensity1, savePOP=savePOP)
        tabs1 <- dat1$tabs
        tabIDs <- dat1$tabIDs
        PLOT1 <- tabs1$plt
        pop_plot_stratum_assgn1 <- tabs1$pop_plot_stratum_assgn

        ## Check pjoinid
        pltfields <- names(PLOT1)
        pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", 
  		                  checklst=pltfields, gui=gui, caption="Joinid in plot?")  
        if (is.null(pjoinid)) {
          if (xyjoinid %in% pltfields) {
            pjoinid  <- xyjoinid
          } else {
            if (xyjoinid == "PLT_CN" && "CN" %in% pltfields) {
              pjoinid <- "CN"
            } else {
              stop(xyjoinid, " not in plt")
            }
          }
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
        if (nrow(stpltids1) > 0) {

          ## Subset data to stpltids
          idmatch <- PLOT1[[pjoinid]] %in% stpltids1[[xyjoinid]]
          plt1 <- PLOT1[idmatch,]
          if (nrow(plt1) != nrow(stpltids1)) {
            message("plots where ", measEndyr.filter, " do not match pltids")
          }
          pids1 <- plt1[[puniqueid]]

          ## Subset other tables in list
          stcliptabs$plt <- plt1
          for (tabnm in names(tabs1)[names(tabs1) != "plt"]) {
            stcliptabs[[tabnm]] <- tabs1[[tabnm]][tabs1[[tabnm]][[tabIDs[[tabnm]]]] %in% pids1, ] 
          }
        }
        ## Get plots outside filter
        #######################################
        if (!is.null(countyfips2)) {
          countyfips2 <- formatC(as.numeric(countyfips2), width=5, digits=5, flag="0")
          stcnty2 <- countyfips2[startsWith(countyfips2, formatC(stcd, width=2, flag="0"))]
          countycds2 <- sort(as.numeric(unique(substr(stcnty2, 3, 5))))
          stateFilter2 <- paste("p.countycd IN(", toString(countycds2), ")")
        }
        dat2 <- DBgetPlots(states=stcd, datsource=datsource, 
                         data_dsn=data_dsn, 
                         stateFilter=stateFilter2, eval_opts=eval_opts2,
                         istree=istree, isseed=isseed, othertables=other_layers, 
                         intensity1=intensity1, savePOP=savePOP)
        tabs2 <- dat2$tabs
        PLOT2 <- tabs2$plt
        pop_plot_stratum_assgn2 <- tabs2$pop_plot_stratum_assgn

        if (nrow(PLOT2) > length(unique(PLOT2[[puniqueid]]))) {
          if ("INVYR" %in% names(PLOT2)) {
            setorder(PLOT2, -INVYR)
          } else {
            setorderv(PLOT2, -puniqueid)
          }
          PLOT2 <- PLOT2[, head(.SD, 1), by=pjoinid]
        }
        if (nrow(stpltids2) > 0) {

          ## Subset data to stpltids
          plt2 <- PLOT2[PLOT2[[pjoinid]] %in% stpltids2[[xyjoinid]],]
          if (nrow(plt2) != nrow(stpltids2)) {
            message("plots outside filter do not match pltids")
          }
          pids2 <- plt2[[puniqueid]]

          ## Subset other tables in list
          stcliptabs$plt <- rbind(stcliptabs$plt, plt2)
          pop_plot_stratum_assgn <- rbind(pop_plot_stratum_assgn1, pop_plot_stratum_assgn2)

          for (tabnm in names(tabs2)[names(tabs2) != "plt"]) {
            stcliptabs[[tabnm]] <- rbind(stcliptabs[[tabnm]], 
				tabs2[[tabnm]][tabs2[[tabnm]][[tabIDs[[tabnm]]]] %in% pids2, ])
          }
        }          
      } else {   ## measEndyr.filter is null

        ## Check for counties
        if (!is.null(countyfips)) {
          countyfips <- formatC(as.numeric(countyfips), width=5, digits=5, flag="0")
          stcnty <- countyfips[startsWith(countyfips, formatC(stcd, width=2, flag="0"))]
          countycds <- sort(as.numeric(unique(substr(stcnty, 3, 5))))
          stateFilter <- paste("p.countycd IN(", toString(countycds), ")")
        }
        dat <- DBgetPlots(states=stcd, datsource=datsource, 
                         data_dsn=data_dsn, 
                         stateFilter=stateFilter1, eval_opts=eval_opts,
                         istree=istree, isseed=isseed, othertables=other_layers, 
                         intensity1=intensity1, savePOP=savePOP)
        tabs <- dat$tabs
        tabIDs <- dat$tabIDs
        pop_plot_stratum_assgn <- tabs$pop_plot_stratum_assgn
      
        ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
        if (nrow(PLOT) > length(unique(PLOT[[puniqueid]]))) {
          if ("INVYR" %in% names(PLOT)) {
            setorder(PLOT, -INVYR)
          } else {
            setorderv(PLOT, -puniqueid)
          }
          PLOT <- PLOT[, head(.SD, 1), by=pjoinid]
        }

        ## Check pjoinid
        pltfields <- names(PLOT1)
        pjoinid <- pcheck.varchar(var2check=pjoinid, varnm="pjoinid", 
  		                  checklst=pltfields, gui=gui, caption="Joinid in plot?")  
        if (is.null(pjoinid)) {
          if (xyjoinid %in% pltfields) {
            pjoinid  <- xyjoinid
          } else {
            if (xyjoinid == "PLT_CN" && "CN" %in% pltfields) {
              pjoinid <- "CN"
            } else {
              stop(xyjoinid, " not in plt")
            }
          }
        }

        if (nrow(stpltids) > 0) {

          ## Subset data to stpltids
          plt1 <- PLOT[PLOT[[pjoinid]] %in% stpltids[[xyjoinid]],]
          if (nrow(plt) != nrow(stpltids)) {
            message("plots where ", measEndyr.filter, " do not match pltids")
          }
          pids <- plt[[puniqueid]]

          ## Subset other tables in list
          stcliptabs$plt <- plt
          for (tabnm in names(tabs)[names(tabs) != "plt"]) {
            stcliptabs[[tabnm]] <- tabs[[tabnm]][tabs[[tabnm]][[tabIDs[[tabnm]]]] %in% pids, ] 
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
      if (savedata && !returndata) {
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

          indx <- ifelse(tabnm == "plt", "CN", 
		  ifelse(tabnm %in% c("cond", "vsubpspp", "vsubpstr", 
						"invsubp", "dwm"), c("PLT_CN", "CONDID"),
		    ifelse(tabnm %in% c("tree", "grm"), c("PLT_CN", "CONDID", "TREE"), 
			 ifelse(tabnm == "subplot", c("PLT_CN", "SUBP"), 
			   ifelse(tabnm == "subpcond", c("PLT_CN", "SUBP", "CONDID"), "PLT_CN")))))
          if (!all(indx %in% names(tab))) {
            indx <- indx[indx %in% names(tab)]
          }

          if (!is.null(tab)) {
            assign(paste0("index.unique.", tabnm), NULL)
            if (is.null(tabIDs[[tabnm]]) && i == 1 && length(indx) > 0) {
              assign(paste0("index.unique.", tabnm), indx)
            }
            savedata_opts = list(outfolder = outfolder, 
                                 out_fmt = out_fmt, 
                                 out_dsn = out_dsn, 
                                 out_layer = "plot",
                                 outfn.pre = outfn.pre, 
                                 overwrite_layer = overwrite_layer,
                                 append_layer = append_layer,
                                 outfn.date = outfn.date, 
                                 add_layer = TRUE)
          }
        } 

        if (savePOP && !is.null(pop_plot_stratum_assgn)) {
          index.unique.ppsa <- NULL
          if (!append_layer) index.unique.ppsa <- "PLT_CN"
          datExportData(pop_plot_stratum_assgn, 
              index.unique = index.unique.ppsa,
              savedata_opts = list(outfolder = outfolder, 
                                out_fmt = out_fmt, 
                                out_dsn = out_dsn, 
                                out_layer = "pop_plot_stratum_assgn",
                                outfn.pre = outfn.pre, 
                                overwrite_layer = overwrite_layer,
                                append_layer = append_layer,
                                outfn.date = outfn.date, 
                                add_layer = TRUE)) 
          rm(pop_plot_stratum_assgn)
          gc()
        } 
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
    DBI::dbDisconnect(dbconn)
  }  ## datsource
     
 
  #############################################################################
  ## Save tables
  #############################################################################
  if (savebnd) {
    spExportSpatial(bndx, 
                    savedata_opts=list(outfolder=outfolder, 
                                       out_fmt=out_fmt, 
                                       out_dsn=out_dsn, 
                                       out_layer="bnd",
                                       outfn.pre=outfn.pre, 
                                       outfn.date=outfn.date, 
                                       overwrite_layer=overwrite_layer,
                                       append_layer=append_layer, 
                                       add_layer=TRUE))   
  }
 
  if (savedata) {
    if (returnxy) {   
      if (!is.null(spxy)) {
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
        } else {
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
        }
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
  
  if (returndata) {
    returnlst$tabs <- tabs2save
    returnlst$tabIDs <- tabIDs
    if (returnxy && !is.null(spxy)) {
      returnlst$spxy <- spxy
    }
    returnlst$pltids <- pltids
    #returnlst$clip_polyv <- bndx

    if (!is.null(bndx)) {
      returnlst$bnd <- bndx
    }
    returnlst$puniqueid <- puniqueid
    returnlst$xy.uniqueid <- xyjoinid
    returnlst$pjoinid <- pjoinid
    returnlst$states <- states

    if (savePOP) {
      returnlst$pop_plot_stratum_assgn <- pop_plot_stratum_assgn
    }
    if (evalresp) {
      returnlst$evalid <- evalid
    }
    return(returnlst)
  } 
}

