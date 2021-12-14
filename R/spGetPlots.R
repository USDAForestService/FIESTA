#' Spatial - Extracts data within a given boundary.
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
#' @param bnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of bnd. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd is an R object.
#' @param bnd.filter String. Filter to subset bnd spatial layer.
#' @param states String. The name of state(s) for tables (e.g., "Vermont", "Utah").
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
#' @param xyids Data frame. Non-spatial plot identificators within bnd).
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
#' @param plot_layer String. Name of layer in database of file name of FIA plot
#' table.
#' @param cond_layer String. Name of layer in database of file name of FIA cond
#' table.
#' @param tree_layer String. Name of layer in database of file name of FIA tree
#' table if istree=TRUE.
#' @param seed_layer String. Name of layer in database of file name of FIA
#' seedling table if isseed=TRUE.
#' @param ppsa_layer String. Name of layer in database of file name of FIA
#' pop_plot_stratum_assgn table, if using evaluations.
#' @param other_layers String. Other layer(s) in database to clip and/or
#' extract from database (Note: must include PLT_CN variable as unique
#' identifier).
#' @param puniqueid String. Name of unique identifier of plt.
#' @param savePOP Logical. If TRUE, returns and/or saves POP_PLOT_STRATUM_ASSGN
#' table.
#' @param evalid Integer. To extract data for a specific evaluation period. See
#' notes for more information about FIA Evaluations.
#' @param evalCur Logical. If TRUE, extract plots with most current FIA
#' Evalidation for state(s).
#' @param evalEndyr Integer. Defining end year of Evaluation (yyyy).
#' @param evalType String vector. The type(s) of evaluation of interest ('ALL',
#' 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN').  The evalType
#' 'ALL' includes nonsampled plots; 'AREAVOL' includes plots used for area or
#' tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 'GRM' includes
#' plots used for growth, removals, mortality, and change estimates (eval_typ
#' %in% c(GROW, MORT, REMV, CHNG)). Multiple types are accepted.  See FIA
#' database manual for regional availability and/or differences.
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
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param savebnd Logical. If TRUE, saves bnd. If out_fmt='sqlite', saves to a
#' SpatiaLite database.
#' @param savexy Logical. If TRUE, save xy coordinates to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param spXYdat R list object. Output from FIESTA::spGetXY().
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
#' 
#' 
#' 
#'   ## Get polygon vector layer from FIESTA external data
#'   WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
#' 
#'   ## Extract data from FIA datamart for measurement years 2013 thru 2015
#'   dat <- spGetPlots(bnd=WYbhfn, datsource="datamart", measyrs=2013:2015)
#'   names(dat)
#'   tabs <- dat$tabs
#'   names(tabs)
#'   head(tabs$pltx)
#' 
#'   table(tabs$pltx$MEASYEAR)
#' 
#' 
#'   ## Extract data from FIA datamart for most current evaluation
#'   datCur <- spGetPlots(bnd=WYbhfn, datsource="datamart", evalCur=TRUE)
#'   names(datCur)
#'   tabsCur <- datCur$tabs
#'   names(tabsCur)
#'   head(tabsCur$pltx)
#' 
#'   table(tabsCur$pltx$MEASYEAR)
#' 
#' 
#' @export spGetPlots
spGetPlots <- function(bnd = NULL, 
                       bnd_dsn = NULL, 
                       bnd.filter = NULL, 
                       states = NULL, 
                       RS = NULL, 
                       xyids = NULL, 
                       xy_datsource = NULL, 
                       xy = NULL, 
                       xy_dsn = NULL, 
                       xy.uniqueid = "PLT_CN", 
                       xvar = NULL, 
                       yvar = NULL, 
                       xy.crs = 4269, 
                       xyjoinid = NULL, 
                       pjoinid = NULL, 
                       clipxy = TRUE, 
                       datsource = "datamart", 
                       data_dsn =NULL, 
                       istree = FALSE, 
                       isseed = FALSE, 
                       plot_layer = "plot", 
                       cond_layer = "cond", 
                       tree_layer = "tree", 
                       seed_layer = "seed", 
                       ppsa_layer = "pop_plot_stratum_assgn", 
                       other_layers = NULL, 
                       puniqueid = "CN", 
                       savePOP = FALSE, 
                       evalid = NULL, 
                       evalCur = FALSE, 
                       evalEndyr = NULL, 
                       evalType = "VOL", 
                       measCur = FALSE, 
                       measEndyr = NULL, 
                       measEndyr.filter = NULL, 
                       invyrs = NULL, 
                       measyrs =NULL, 
                       allyrs = FALSE, 
                       intensity1 = FALSE, 
                       showsteps = FALSE, 
                       savedata = FALSE, 
                       savebnd = FALSE, 
                       savexy = TRUE, 
                       savedata_opts = NULL,
                       spXYdat = NULL) {

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
  xydat=stateFilter=statecnty=xypltx=tabs2save=evalidst=PLOT_ID=INVYR=
	othertabnms=stcds=spxy=stbnd=states <- NULL
  cuniqueid=tuniqueid <- "PLT_CN"
  stbnd.att <- "COUNTYFIPS"
  returnlst <- list()
  #clipdat <- list()

  gui <- FALSE
  coordtype <- "public"
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(spGetPlots)), 
		names(formals(spGetXY)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(FIESTA::savedata_options)[-length(formals(FIESTA::savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    for (i in 1:length(savedata_opts)) {
      assign(names(savedata_opts)[[i]], savedata_opts[[i]])
    }
  }

  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################
  
  ## Define list of pop_tables (without PLT_CN)
  pop_tables <- c("POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", 
	"POP_EVAL_TYP", "POP_STRATUM", "SURVEY") 
 
  ## Check spXYdat
  if (!is.null(spXYdat)) {
    spxy <- spXYdat$spxy
    xyids <- spXYdat$xyids
    states <- spXYdat$states
    statecnty <- spXYdat$statecnty
    stbnd.att <- spXYdat$stbnd.att
    xy.uniqueid <- spXYdat$xy.uniqueid
    bndx <- spXYdat$bndx
    stcds <- pcheck.states(states, statereturn="VALUE")
    if (is.null(xyids) && (is.null(spxy) || nrow(spxy)) == 0) {
      stop("spxy is null")
    } 

    ## Check xyjoinid
    xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
	      checklst=names(xyids), gui=gui, caption="JoinID in xyids?", stopifnull=FALSE) 
    if (is.null(xyjoinid)) {
      message("no xyjoinid defined... using the defined uniqueid: ", xy.uniqueid)
      xyjoinid <- xy.uniqueid
    } 

  } else {   ## is.null(spXYdat) 

    ## Import boundary
    bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
    if (!is.null(bndx)) {
      bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf
    } 

    ## Check xyids
    xyids <- pcheck.table(xyids)

    if (!is.null(xyids)) {
      ## Check xyjoinid
      xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
		checklst=names(xyids), gui=gui, caption="JoinID in xyids?", stopifnull=TRUE)  
 
      ## Check stbnd.att
      stbnd.att <- pcheck.varchar(var2check=stbnd.att, varnm="stbnd.att", 
		checklst=names(xyids), gui=gui, caption="State attribute?") 
      
      ## Get state codes
      if (is.null(stbnd.att)) {
        stbnd.att <- findnm("COUNTYFIPS", names(xyids), returnNULL=TRUE)
        if (is.null(stbnd.att)) {
          stbnd.att <- findnm("STATECD", names(xyids), returnNULL=TRUE)
          if (is.null(stbnd.att)) {
            stbnd.att <- findnm("STATE", names(xyids), returnNULL=TRUE)
          }
        }
      }
      if (stbnd.att == "COUNTYFIPS") {
        statecnty <- sort(unique(xyids[[stbnd.att]]))
        stcds <- as.numeric(sort(unique(substr(statecnty, 1, 2))))
      } else {
        stcds <- sort(unique(pcheck.states(xyids[[stbnd.att]], statereturn="VALUE")))
      }
      states <- pcheck.states(as.numeric(stcds))

    } else { 	## is.null(xyids)

      ## Check states
      if (!is.null(states)) {
        if (!all(states %in% FIESTA::ref_statecd$MEANING)) stop("states is invalid")
      }

      ## Check clipxy
      clipxy <- pcheck.logical(clipxy, varnm="clipxy", 
                            title="Clip xy?", first="NO", gui=gui)  

      if (clipxy) {
        ###########################################################################
        ## Get XY
        ###########################################################################
        if (is.null(xy_datsource)) {
          xy_datsource <- datsource
        } 
        if (is.null(xy) && is.null(xy_dsn)) {
          xy_dsn <- data_dsn
        } 
        xydat <- spGetXY(bnd=bndx, 
                         states=states, RS=RS, 
                         xy=xy, xy_dsn=xy_dsn, 
                         xy.uniqueid=xy.uniqueid, 
                         xvar=xvar, yvar=yvar, xy.crs=xy.crs, 
                         xyjoinid=xyjoinid, pjoinid=pjoinid, 
                         xy_datsource=xy_datsource, 
                         clipxy=clipxy, evalid=evalid, 
                         evalCur=evalCur, evalEndyr=evalEndyr, 
                         measCur=measCur, measEndyr=measEndyr, 
                         measEndyr.filter=measEndyr.filter, 
                         invyrs=invyrs, measyrs=measyrs, allyrs=allyrs, 
                         intensity1=intensity1, showsteps=showsteps, 
                         returnxy=TRUE)
        spxy <- xydat$spxy
        xyids <- xydat$xyids
        states <- xydat$states
        statecnty <- xydat$statecnty
        stbnd.att <- xydat$stbnd.att
        bndx <- xydat$bndx

        ## Check xyjoinid
        xyjoinid <- pcheck.varchar(var2check=xyjoinid, varnm="xyjoinid", 
	            checklst=names(xyids), gui=gui, caption="JoinID in xyids?", 
	            stopifnull=FALSE) 
        if (is.null(xyjoinid)) {
          message("no xyjoinid defined... using the defined uniqueid: ", xy.uniqueid)
          xyjoinid <- xy.uniqueid
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
        statedat <- spGetStates(bndx, stbnd=NULL, stbnd_dsn=NULL, 
			        stbnd.att="COUNTYFIPS", RS=RS, states=states, showsteps=showsteps)
        bndx <- statedat$bndx
        stbnd.att <- statedat$stbnd.att
        statenames <- statedat$statenames
        if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS") {
          statecnty <- statedat$states
          stcds <- unique(as.numeric(substr(statecnty, 1,2)))
        } else {
          stcds <- FIESTA::ref_statecd$VALUE[FIESTA::ref_statecd$MEANING %in% statedat$states]
        }
        message("boundary intersected states: ", toString(statenames))
        xyids <- sf::st_drop_geometry(spxy)
      } 
    }
  }
  #xyids <- spxy[[xyjoinid]]
 
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
  } else if (datsource %in% c("sqlite", "gdb")) {
    if (is.null(data_dsn)) {
      stop("data_dsn is NULL")
    }
    if (!file.exists(data_dsn)) {
      stop(data_dsn, " is invalid")
    }
    if (datsource == "sqlite") {
      if (!all(c("RSQLite", "DBI") %in% rownames(installed.packages()))) {
	   message("RSQLite and DBI packages are required to run SQLite queries")
      }
    }
    if (datsource == "gdb") {
      if (!"arcgisbinding" %in% rownames(installed.packages())) {
	   message("RSQLite and DBI packages are required to run SQLite queries")
      }
    }
  } 
 
  ## Check savedata
  #############################################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data?", first="NO", gui=gui)  
 
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
 
  #############################################################################
  ## If xy is separate file or database, and clipxy=TRUE, import first
  #############################################################################
  if (datsource %in% c("obj", "csv")) {

    ####################################################################
    ## 1) Import file(s)
    ## 2) Clip xy (for all states) to boundary
    ## 3) Subset other data with clipped xy joinid
    ####################################################################
    tabs2save <- {}

    ## plot data
    obj <- ifelse (datsource == "obj", TRUE, FALSE)
    pltx <- pcheck.table(plot_layer, obj=obj, stopifnull=FALSE)
    if (!is.null(pltx)) {
      pltfields <- names(pltx)
      tabs2save <- c(tabs2save, "pltx")
    }

    ## condition data
    condx <- pcheck.table(cond_layer, obj=obj, stopifnull=TRUE)
    if (!is.null(condx)) {
      if (is.null(pltx)) {
        pltx <- condx
        pltfields <- names(condx)
        tabs2save <- c(tabs2save, "pltx")
      } else {
        tabs2save <- c(tabs2save, "condx")
      }
    }

    ## tree data
    if (istree) {
      treex <- pcheck.table(tree_layer, obj=obj, stopifnull=TRUE)
      if (!is.null(treex)) {
        tabs2save <- c(tabs2save, "treex")
      }
    }
    ## seed data
    if (isseed) {
      seedx <- pcheck.table(seed_layer, obj=obj, stopifnull=TRUE)
      if (!is.null(seedx)) {
        tabs2save <- c(tabs2save, "seedx")
      }
    }
    ## pop_plot_stratam_assgn data
    if (savePOP) {
      pop_plot_stratum_assgnx <- pcheck.table(ppsa_layer, obj=obj, stopifnull=TRUE)
      if (!is.null(pop_plot_stratum_assgnx)) {
        tabs2save <- c(tabs2save, "pop_plot_stratum_assgnx")
      }
    }
    ## other data
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), pcheck.table(layer, obj=obj, stopifnull=TRUE))
        tabs2save <- c(tabs2save, layer)
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

    ## Check if class of pjoinid in pltx matches class of xyjoinid in xyids
    tabs <- check.matchclass(pltx, xyids, pjoinid, xyjoinid)
    pltx <- tabs$tab1
    xyids <- tabs$tab2
     

    ## Subset plot data
    pltx2 <- pltx[pltx[[pjoinid]] %in% xyids[[xyjoinid]],]
    if (nrow(pltx) == 0) stop("xyjoinid invalid")
    tabs2save <- c(tabs2save, "pltx")

    ## Get plot ids from pltx
    pltids <- pltx[[puniqueid]]

    ## Subset cond data
    condx <- condx[condx[[cuniqueid]] %in% pltids,]
    tabs2save <- c(tabs2save, "condx")

    ## Subset tree data
    if (istree) {
      treex <- treex[treex[[tuniqueid]] %in% pltids,]
    }
    tabs2save <- c(tabs2save, "treex")

    ## Subset seed data
    if (isseed) {
      seedx <- seedx[seedx[[tuniqueid]] %in% pltids,]
    }
    tabs2save <- c(tabs2save, "seedx")

    ## other data
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
          assign(paste0(layer, "x"), get(layer)[get(layer)[["PLT_CN"]] %in% pltids, ])
        }
        tabs2save <- c(tabs2save, layer)
      }
    }

  } else {			## datsource in('datamart', 'sqlite')
    ## Initialize tables
    condx <- {}
    pltx <- {}
    tabs2save <- c("pltx", "condx")
    if (istree) {
      treex <- {} 
      tabs2save <- c(tabs2save, "treex")
    }
    if (isseed) {
      seedx <- {} 
      tabs2save <- c(tabs2save, "seedx")
    }
    if (!is.null(other_layers)) {
      for (layer in other_layers) {
        assign(paste0(layer, "x"), {})
        tabs2save <- c(tabs2save, paste0(layer, "x"))
      }
    }

    msg <- "getting data for..."
    if (!is.null(evalid)) {
      evalid <- unlist(evalid)
      evalidst <- evalid[unique(as.numeric(substr(evalid, nchar(evalid)-6, 
					nchar(evalid)-4))) == stcd]
      msg <- paste0(msg, "for evaluation: ", toString(evalidst))
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
      msg <- paste0(msg, "for most current evaluation")
      if (!is.null(evalEndyr)) {
        msg <- paste0("ending in ", evalEndyr)
      }
      savePOP <- TRUE
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
      tabs2save <- c(tabs2save, "pop_plot_stratum_assgnx")
    }

    ## Check measEndyr.filter
    ###############################################################
    if (!is.null(measEndyr.filter)) {
      measEndyr.filter <- check.logic(xyids, measEndyr.filter)

      ## Get pltids from xyids
      xyids1 <- datFilter(xyids, xfilter=measEndyr.filter)$xf
      nbrxy <- ifelse (is.null(xyids1), 0, nrow(xyids1)) 
      message ("there are ", nbrxy, " plots where ", measEndyr.filter)
      xyids2 <- datFilter(xyids, xfilter=paste0("!", measEndyr.filter))$xf
    }
  }

  if (datsource == "datamart") {
    for (i in 1:length(states)) { 
      state <- states[i]
      stcd <- pcheck.states(state, statereturn="VALUE")
      stabbr <- pcheck.states(stcd, statereturn="ABBR") 
      message(paste0("\n", state, "..."))

      ## Check for counties
      if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS" && !is.null(statecnty)) {
        stcnty <- statecnty[startsWith(statecnty, formatC(stcd, width=2, flag="0"))]
        countycds <- sort(as.numeric(unique(substr(stcnty, 3, 5))))
        stateFilter <- paste("p.countycd IN(", toString(countycds), ")")
      }
      ## Get plot data
      ###############################
      if (measCur && !is.null(measEndyr) && !is.null(measEndyr.filter)) {
        dat <- DBgetPlots(states=stcd, datsource="datamart", 
                          stateFilter=stateFilter, allyrs=TRUE, 
                          istree=istree, isseed=isseed, othertables=other_layers, 
                          intensity1=intensity1, savePOP=savePOP)
      } else {
        dat <- DBgetPlots(states=stcd, datsource="datamart", 
                          stateFilter=stateFilter, allyrs=allyrs, 
                          evalid=evalid, evalCur=evalCur, 
                          evalEndyr=evalEndyr, evalType=evalType, 
                          measCur=measCur, measEndyr=measEndyr, 
                          invyrs=invyrs, measyrs=measyrs, 
                          istree=istree, isseed=isseed, othertables=other_layers, 
                          intensity1=intensity1, savePOP=savePOP)
      }
      tabs <- dat$tabs
      PLOT <- tabs$plt
      cond <- tabs$cond
      if (istree) 
        tree <- tabs$tree
      if (isseed)
        seed <- tabs$seed
      if (savePOP) {
        pop_plot_stratum_assgn <- tabs[[chkdbtab(names(dat), "POP_PLOT_STRATUM_ASSGN")]]
      }
      puniqueid <- "CN"

      if (!is.null(other_layers)) {
        for (layer in other_layers) {
          assign(layer, dat[[chkdbtab(names(dat), layer)]])
        }
      }

      ## Check pjoinid
      pltfields <- names(PLOT)
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
 
      ## Get most current plots in database for !measEndyr.filter
      #######################################################
      if (measCur && !is.null(measEndyr) && !is.null(measEndyr.filter)) {

        ################################################
        ## Subset FIA plot data to xyids1 
        ################################################
        if (nbrxy) {
 
          ## ## Query plots - measCur=TRUE and measEndyr
          pfromqry <- getpfromqry(plotCur=TRUE, Endyr=measEndyr, syntax="R", plotnm="PLOT")
          plt.qry <- paste0("select distinct p.* from ", pfromqry) 
          PLOT1 <- setDT(sqldf::sqldf(plt.qry))

          ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
          if (nrow(PLOT1) > length(unique(PLOT1[[puniqueid]]))) {
            if ("INVYR" %in% names(PLOT1)) {
              setorder(PLOT1, -INVYR)
            } else {
              setorderv(PLOT1, -puniqueid)
             }
            PLOT1 <- PLOT1[, head(.SD, 1), by=pjoinid]
          }

          ## Subset plot data to xyids1 
          plt1 <- PLOT1[PLOT1[[pjoinid]] %in% xyids1[[xyjoinid]], ]
          pltids1 <- plt1[[puniqueid]]

          cond1 <- cond[cond[["PLT_CN"]] %in% pltids1, ]
          if (istree)
            tree1 <- tree[tree[["PLT_CN"]] %in% pltids1, ]
          if (isseed)
            seed1 <- seed[seed[["PLT_CN"]] %in% pltids1, ]
          if (savePOP) {
            pop_plot_stratum_assgn1 <- 
			pop_plot_stratum_assgn[pop_plot_stratum_assgn[["PLT_CN"]] %in% pltids1, ]
          }
          if (!is.null(other_layers)) {
            for (layer in other_layers) {
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(paste0(layer, "2"), 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids1, ])
              }
            }
          }
        } else {
          plt1=cond1 <- NULL
          if (istree)
            tree1 <- NULL
          if (isseed)
            seed1 <- NULL
          if (savePOP) {
            pop_plot_stratum_assgn1 <- NULL
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(paste0(layer, "1"), NULL)
              }
            }
          }
        }


        ################################################
        ## Subset FIA plot data to xyids2 
        ################################################
        if (nrow(xyids2) > 0) {

          ## ## Query plots - measCur=TRUE
          pfromqry <- getpfromqry(plotCur=TRUE, syntax="R", plotnm="PLOT")
          plt.qry <- paste0("select distinct p.* from ", pfromqry) 
          PLOT2 <- setDT(sqldf::sqldf(plt.qry))

          ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
          if (nrow(PLOT2) > length(unique(PLOT2[[puniqueid]]))) {
            if ("INVYR" %in% names(PLOT2)) {
              setorder(PLOT2, -INVYR)
            } else {
              setorderv(PLOT2, -puniqueid)
            }
            PLOT2 <- PLOT2[, head(.SD, 1), by=pjoinid]
          } 
        
          ## Subset plot data to xyids2 
          plt2 <- PLOT2[PLOT2[[pjoinid]] %in% xyids2[[xyjoinid]], ]
          pltids2 <- plt2[[puniqueid]]

          cond2 <- cond[cond[["PLT_CN"]] %in% pltids2, ]
          if (istree)
            tree2 <- tree[tree[["PLT_CN"]] %in% pltids2, ]
          if (isseed)
            seed2 <- seed[seed[["PLT_CN"]] %in% pltids2, ]
          if (savePOP) {
            pop_plot_stratum_assgn2 <- pop_plot_stratum_assgn[pop_plot_stratum_assgn[["PLT_CN"]] %in% pltids2, ]
          }
          if (!is.null(other_layers)) {
            for (layer in other_layers) {
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(paste0(layer, "2"), 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids2, ])
              }
            }
          }
        } else {
          plt2=cond2 <- NULL
          if (istree)
            tree2 <- NULL
          if (isseed)
            seed2 <- NULL
          if (savePOP) {
            pop_plot_stratum_assgn2 <- NULL
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(paste0(layer, "2"), NULL)
              }
            }
          }
        }
        plt <- rbind(plt1, plt2)
        cond <- rbind(cond1, cond2)
        if (istree)
          tree <- rbind(tree1, tree2)
        if (isseed)
          seed <- rbind(seed1, seed2)
        if (savePOP) {
          pop_plot_stratum_assgn <- rbind(pop_plot_stratum_assgn1, pop_plot_stratum_assgn2)
        }
        if (!is.null(other_layers)) {
          for (i in 1:length(other_layers)) {
            layer <- other_layers[i]
            if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
              assign(paste0(layer), rbind(paste0(layer, "1"), paste0(layer, "2")))
            }
          }
        }
      } else {    ## measEndyr.filter = NULL

        if (nrow(xyids) > 0) {
          ## Subset data to xyids
          plt <- PLOT[PLOT[[pjoinid]] %in% xyids[[xyjoinid]], ]
          pltids <- plt[[puniqueid]]

          cond <- cond[cond[["PLT_CN"]] %in% pltids, ]
          if (istree)
            tree <- tree[tree[["PLT_CN"]] %in% pltids, ]
          if (isseed)
            seed <- seed[seed[["PLT_CN"]] %in% pltids, ]
          if (!is.null(other_layers)) {
            for (layer in other_layers) {
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(layer, 
				get(layer)[get(layer)[["PLT_CN"]] %in% pltids, ])
              }
            }
          }
        } else {
          plt=cond <- NULL
          if (istree)
            tree <- NULL
          if (isseed)
            seed <- NULL
          if (savePOP) {
            pop_plot_stratum_assgn2 <- NULL
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(layer, NULL)
              }
            }
          }
        }
      }  ## if measEndyr.filter is not NULL

      pltx <- rbind(pltx, plt)
      condx <- rbind(condx, cond)
      if (istree)
        treex <- rbind(treex, tree)
      if (isseed)
        seedx <- rbind(seedx, seed)
      if (savePOP) {
        pop_plot_stratum_assgnx <- rbind(pop_plot_stratum_assgnx, pop_plot_stratum_assgn)
      }
      if (!is.null(other_layers)) {
        for (i in 1:length(other_layers)) {
          layer <- other_layers[i]
          assign(paste0(layer, "x"), rbind(paste0(layer, "x"), layer))
        }
      }
      if (showsteps && !is.null(spxy)) {
        ## Set plotting margins
        mar <-  par("mar")
        par(mar=c(1,1,1,1))

        if (i == 1) {
          if (!is.null(bndx)) {
            plot(sf::st_geometry(bndx), border="black", lwd=0.75)
          } else {
            plot(sf::st_geometry(spxy[spxy$STATECD == stcd,]), col="transparent", cex=.5)
          }
        }
        plot(sf::st_geometry(spxy[spxy$STATECD == stcd,]), col="blue", cex=.5, add=TRUE)
        par(mar=mar)
      }
    }  ## End of looping thru states
  }

############################################################
  if (datsource == "sqlite") {
    ####################################################################
    ## 1) Check if data for all states is in database
    ## 1) Get most current plots from xy database that intersect state
    ## 3) Subset other data with clipped xy joinid
    ####################################################################

    ## Check for data tables in database
    ###########################################################
    dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    plot_layer <- chkdbtab(tablst, plot_layer, stopifnull=TRUE)
    cond_layer <- chkdbtab(tablst, cond_layer, stopifnull=TRUE)
    if (istree) {
      tree_layer <- chkdbtab(tablst, tree_layer, stopifnull=TRUE)
    }
    if (isseed) {
      seedchk <- chkdbtab(tablst, seed_layer)
      if (is.null(seedchk) && seed_layer == "seed") {
        seedchk <- chkdbtab(tablst, "seedling")
        if (is.null(seedchk)) {
          message("no seedling data in database...")
          isseed <- FALSE
        } else {
          seed_layer <- seedchk
        }
      } else {
        seed_layer <- seedchk
      }
    }
    if (savePOP) {
      ppsa_layer <- chkdbtab(tablst, ppsa_layer, stopifnull=TRUE)
    }       
    if (!is.null(other_layers) && !any(other_layers %in% tablst)) {
      stop("missing layers in database: ", 
			toString(other_layers[!other_layers %in% tablst]))
    }

    ## Check list of pop_tables
    pop_tables <- unlist(sapply(pop_tables, pcheck.varchar, 
			checklst=tablst, stopifinvalid=FALSE))

    ## Check for state in database
    ###########################################################
    dbstcds <- DBI::dbGetQuery(dbconn, paste("select distinct statecd from", 
			plot_layer))[[1]]

    if (!is.null(stcds) && !all(as.numeric(stcds) %in% as.numeric(dbstcds))) {
      statemiss <- stcds[!stcds %in% dbstcds]
      #message("database does not include all states: ", toString(statemiss))
          
      if (length(stcds) == length(statemiss)) {
        message("no states to include...")
        return(NULL)
      } else {
        message("database does not include all states: ", toString(statemiss))
        stcds <- stcds[!stcds %in% statemiss]
        bndx <- spGetStates(bndx, states=stcds)$bndx
        plot(st_geometry(bndx))
      }  
    }

    ## Get fields in plot table
    pltfields <- DBI::dbListFields(dbconn, "plot")

    ## Check pjoinid
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
 
    for (i in 1:length(stcds)) { 
      stcd <- stcds[i]
      state <- pcheck.states(stcd) 
      message(paste0("\n", state, "..."))

      ## Check for counties
      if (!is.null(stbnd.att) && stbnd.att == "COUNTYFIPS" && !is.null(statecnty)) {
        stcnty <- statecnty[startsWith(as.character(statecnty), 
				formatC(stcd, width=2, flag="0"))]
        countycds <- sort(as.numeric(unique(substr(stcnty, 3, 5))))
        stateFilter <- paste("p.countycd IN(", toString(countycds), ")")
      }
          
      ## Create state filter
      stfilter <- paste("p.statecd IN(", toString(stcd), ")")
      if (!is.null(stateFilter)) { 
        stfilter <- paste(stfilter, "and", stateFilter)
      }
 
      ## Get most current evalid
      if (evalCur) {
        ppsa_layer<- chkdbtab(tablst, ppsa_layer, stopifnull=TRUE)
        evalidlst <- DBgetEvalid(states=stcd, datsource="sqlite", 
			          data_dsn=data_dsn, evalid=evalid, evalEndyr=evalEndyr,
			          evalCur=evalCur, evalType=evalType, ppsanm=ppsa_layer)
        evalidst <- unlist(evalidlst$evalidlist)
      }
 
      ## Get evalid filter
      if (!is.null(evalidst)) {
        stfilter <- paste0("ppsa.evalid IN(", toString(evalidst), ")")
      } else {
        if (intensity1) {
          stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
        }
      }

      ## Print stfilter
      message(stfilter)

      ## get pfromqry
      pfromqry <- getpfromqry(dsn=data_dsn, evalid=evalidst, plotCur=measCur, 
			Endyr=measEndyr, invyrs=invyrs, allyrs=allyrs, 
			intensity1=intensity1, syntax="R", plotnm=plot_layer, 
			ppsanm=ppsa_layer)
      if (is.null(pfromqry)) {
        message("no time frame specified... including all years")
        allyrs <- TRUE
        pfromqry <- getpfromqry(dsn=data_dsn, evalid=evalidst, 
            plotCur=measCur, Endyr=measEndyr, invyrs=invyrs, 
            allyrs=allyrs, intensity1=intensity1, syntax="R", 
            plotnm=plot_layer, ppsanm=ppsa_layer, chk=TRUE)
      }
      ## Set up query for plots
      plt.qry <- paste0("select distinct p.* from ", pfromqry, " where ", stfilter) 
 
      ## Query database for plots
      rs <- DBI::dbSendQuery(dbconn, plt.qry)
      plt <- setDT(DBI::dbFetch(rs))
      DBI::dbClearResult(rs)

      zids <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
      if (!"PLOT_ID" %in% names(plt)) {
        if (!all(zids %in% names(plt))) {
          message("cannot create unique identifier for plot")
          message(toString(zids[which(!zids %in% names(plt))]), " not in plt")
        } else {
          plt[, PLOT_ID := paste0("ID", 
			formatC(plt$STATECD, width=2, digits=2, flag=0), 
          		formatC(plt$UNITCD, width=2, digits=2, flag=0),
          		formatC(plt$COUNTYCD, width=3, digits=3, flag=0),
          		formatC(plt$PLOT, width=5, digits=5, flag=0))] 
        }
      }
      ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
      if (nrow(plt) > length(unique(plt[[pjoinid]]))) {
        if ("INVYR" %in% names(plt)) {
          setorder(plt, -INVYR)
        } else {
          setorderv(plt, -puniqueid)
        }
        plt <- plt[, head(.SD, 1), by=pjoinid]
      }
 
      ## Get most current plots in database for measEndyr.filter & !measEndyr.filter
      #######################################################################
      p2fromqry <- pfromqry
      if (!is.null(measEndyr.filter)) {

        ################################################
        ## Subset FIA plot data to xyids1 
        ################################################
        if (nbrxy > 0) {

          plt1 <- plt[plt[[pjoinid]] %in% xyids1[[xyjoinid]], ]
          pltids1 <- plt1[[puniqueid]]

          cond1.qry <- paste0("select cond.* from ", p2fromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", 
				"p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
          rs <- DBI::dbSendQuery(dbconn, cond1.qry)
          cond1 <- suppressWarnings(DBI::dbFetch(rs))
          DBI::dbClearResult(rs)

          if (istree) {
            tree1.qry <- paste0("select tree.* from ", p2fromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, tree1.qry)
            tree1 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }
          if (isseed) {
            seed1.qry <- paste0("select seed.* from ", p2fromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, seed1.qry)
            seed1 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }
          if (savePOP) {
            ppsa1.qry <- paste0("select ppsa.* from ", p2fromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, ppsa1.qry)
            pop_plot_stratum_assgn1 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }       
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                if (!is.null(evalidst)) {
                  other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                } else {
                  other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                }
              } else {
                ofromqry <- paste(p2fromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids1, quotes=TRUE), ")")
              }
              rs <- DBI::dbSendQuery(dbconn, other.qry)
              assign(paste0(layer, "1"), DBI::dbFetch(rs))
              othertabnms <- c(othertabnms, layer)
              DBI::dbClearResult(rs)
            }
          } 
        } else {
          plt1=cond1 <- NULL
          if (istree)
            tree1 <- NULL
          if (isseed)
            seed1 <- NULL
          if (savePOP) {
            pop_plot_stratum_assgn1 <- NULL
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(paste0(layer, "1"), NULL)
              }
            }
          }
        }

        ################################################
        ## Subset FIA plot data to xyids1 
        ################################################
        if (nrow(xyids2) > 0) {

          ## Get most current evalid
          if (evalCur) {
            evalidst <- getEvalid.ppsa(ppsa=ppsa_layer, states=stcd, 
				        evalCur=evalCur, evalType=evalType)
          }
          ## Get evalid filter
          if (!is.null(evalidst)) {
            stfilter <- paste0("ppsa.evalid IN(", toString(evalidst), ")")
          } else {
            if (intensity1)
              stfilter <- paste(stfilter, "p.intensity == 1", sep=" and ")
          }
          ## get pfromqry
          pfromqry2 <- getpfromqry(evalid=evalidst, plotCur=measCur, 
			          invyrs=invyrs, allyrs=allyrs, intensity1=intensity1, 
			          syntax="R", plotnm=plot_layer, ppsanm=ppsa_layer)
          ## Set up query for plots
          plt2.qry <- paste0("select distinct p.* from ", pfromqry2, " where ", stfilter) 

          ## Query database for plots
          rs <- DBI::dbSendQuery(dbconn, plt2.qry)
          plt2 <- setDT(DBI::dbFetch(rs))
          DBI::dbClearResult(rs)

          if (!"PLOT_ID" %in% names(plt2)) {
            if (all(zids %in% names(plt2))) {
              plt2[, PLOT_ID := paste0("ID", 
				formatC(plt2$STATECD, width=2, digits=2, flag=0), 
          			formatC(plt2$UNITCD, width=2, digits=2, flag=0),
          			formatC(plt2$COUNTYCD, width=3, digits=3, flag=0),
          			formatC(plt2$PLOT, width=5, digits=5, flag=0))] 
            }
          }

          ## If duplicate plots, sort descending based on INVYR or CN and select 1st row
          if (nrow(plt2) > length(unique(plt2[[pjoinid]]))) {
            if ("INVYR" %in% names(plt)) {
              setorder(plt2, -INVYR)
            } else {
              setorderv(plt2, -puniqueid)
            }
            plt2 <- plt2[, head(.SD, 1), by=pjoinid]
          }

          plt2 <- plt2[plt2[[pjoinid]] %in% xyids2[[xyjoinid]], ]
          pltids2 <- plt2[[puniqueid]]

          cond2.qry <- paste0("select cond.* from ", p2fromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
          rs <- DBI::dbSendQuery(dbconn, cond2.qry)
          cond2 <- suppressWarnings(DBI::dbFetch(rs))
          DBI::dbClearResult(rs)

          if (istree) {
            tree2.qry <- paste0("select tree.* from ", p2fromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, tree2.qry)
            tree2 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }
          if (isseed) {
            seed2.qry <- paste0("select seed.* from ", p2fromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, seed2.qry)
            seed2 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }
          if (savePOP) {
            ppsa2.qry <- paste0("select ppsa.* from ", p2fromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, ppsa2.qry)
            pop_plot_stratum_assgn2 <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }       

          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                if (!is.null(evalidst)) {
                  other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                } else {
                  other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                }
              } else {
                ofromqry <- paste(p2fromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids2, quotes=TRUE), ")")
              }
              rs <- DBI::dbSendQuery(dbconn, other.qry)
              assign(paste0(layer, "2"), DBI::dbFetch(rs))
              othertabnms <- c(othertabnms, layer)
              DBI::dbClearResult(rs)
            }
          } 
        } else {
          plt2=cond2 <- NULL
          if (istree)
            tree2 <- NULL
          if (isseed)
            seed2 <- NULL
          if (savePOP) {
            pop_plot_stratum_assgn2 <- NULL
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(paste0(layer, "2"), NULL)
              }
            }
          }
        }
        plt <- rbind(plt1, plt2)
        cond <- rbind(cond1, cond2)
        if (istree)
          tree <- rbind(tree1, tree2)
        if (isseed)
          seed <- rbind(seed1, seed2)
        if (savePOP) {
          pop_plot_stratum_assgn <- rbind(pop_plot_stratum_assgn1, pop_plot_stratum_assgn2)
        }
        if (!is.null(other_layers)) {
          for (i in 1:length(other_layers)) {
            layer <- other_layers[i]
            assign(paste0(layer), rbind(paste0(layer, "1"), paste0(layer, "2")))
          }
        }
      } else {    ## measEndyr.filter = NULL
        if (nrow(xyids) > 0) {
          plt <- plt[plt[[pjoinid]] %in% xyids[[xyjoinid]], ]
          pltids <- plt[[puniqueid]]

          cond.qry <- paste0("select cond.* from ", p2fromqry,
			" join cond on(cond.PLT_CN = p.CN) where ", stfilter, 
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
          rs <- DBI::dbSendQuery(dbconn, cond.qry)
          cond <- suppressWarnings(DBI::dbFetch(rs))
          DBI::dbClearResult(rs)
          if (istree) {
            tree.qry <- paste0("select tree.* from ", p2fromqry,
			" join tree on(tree.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, tree.qry)
            tree <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }
          if (isseed) {
            seed.qry <- paste0("select seed.* from ", p2fromqry,
			" join ", seed_layer, " on(seed.PLT_CN = p.CN) where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, seed.qry)
            seed <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }
          if (savePOP) {
            ppsa.qry <- paste0("select ppsa.* from ", p2fromqry,
			" where ", stfilter, 
			" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
            rs <- DBI::dbSendQuery(dbconn, ppsa.qry)
            pop_plot_stratum_assgn <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
          }       

          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (!is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                if (!is.null(evalidst)) {
                  other.qry <- paste0("select * from ", layer, " ppsa where ", stfilter)
                } else {
                  other.qry <- paste0("select * from ", layer, " p where ", stfilter)
                }
              } else {
                ofromqry <- paste(p2fromqry, "JOIN", layer, "o on(o.PLT_CN=p.CN)")
                other.qry <- paste("select o.* from", ofromqry, "where", stfilter,
				" and p.", puniqueid, " in(", addcommas(pltids, quotes=TRUE), ")")
              }
              rs <- DBI::dbSendQuery(dbconn, other.qry)
              assign(paste0(layer), DBI::dbFetch(rs))
              othertabnms <- c(othertabnms, layer)
              DBI::dbClearResult(rs)
            }
          }
        } else {
          plt=cond <- NULL
          if (istree)
            tree <- NULL
          if (isseed)
            seed <- NULL
          if (savePOP) {
            pop_plot_stratum_assgn2 <- NULL
          }
          if (!is.null(other_layers)) {
            for (i in 1:length(other_layers)) {
              layer <- other_layers[i]
              if (is.null(pcheck.varchar(layer, checklst=pop_tables, stopifinvalid=FALSE))) {
                assign(layer, NULL)
              }
            }
          }
        }
      }  ## if measEndyr.filter is not NULL
      pltx <- rbind(pltx, plt)
      condx <- rbind(condx, cond)

      if (istree) {
        treex <- rbind(treex, tree)
      }
      if (isseed) {
        seedx <- rbind(seedx, seed)
      }
      if (savePOP) {
        pop_plot_stratum_assgnx <- rbind(pop_plot_stratum_assgnx, pop_plot_stratum_assgn)
      }
      if (!is.null(other_layers)) {
        for (i in 1:length(other_layers)) {
          layer <- other_layers[i]
          assign(paste0(layer, "x"), rbind(paste0(layer, "x"), layer))
        }
      }
      if (showsteps && !is.null(spxy) && !is.null(bndx)) {
        ## Set plotting margins
        mar <-  par("mar")
        par(mar=c(1,1,1,1))

        if (i == 1) {
          if (!is.null(bndx)) {
            plot(sf::st_geometry(bndx), border="black", lwd=0.75)
          } else {
            plot(sf::st_geometry(spxy[spxy$STATECD == stcd,]), col="transparent", cex=.5)
          }
        }
        plot(sf::st_geometry(spxy[spxy$STATECD == stcd,]), col="blue", cex=.5, add=TRUE)
        par(mar=mar)
      } 
    }  ## End of looping thru states
    DBI::dbDisconnect(dbconn)
  }  ## datsource
   
  tabs <- lapply(tabs2save, get, envir=environment())
  tabIDs <- list(pltx=puniqueid, condx=cuniqueid)
  if (istree) {
    tabIDs$treex <- tuniqueid
  }
  if (isseed) {
    tabIDs$seedx <- tuniqueid
  }
  miss <- names(tabs)[!names(tabs) %in% names(tabIDs)]
  if (length(miss) > 0) {
    for (m in miss) {
      if ("PLT_CN" %in% names(tabs[[m]])) {
        tabIDs[[m]] <- "PLT_CN"
      } else if ("CN" %in% names(tabs[[m]])) {
        tabIDs[[m]] <- "CN"
      } else {
        tabIDs[[m]] <- NA
      }
    }
  }

  names(tabs) <- sapply(tabs2save, function(x) substr(x, 1, nchar(x)-1))
  names(tabIDs) <- sapply(names(tabIDs), function(x) substr(x, 1, nchar(x)-1))
 
  #############################################################################
  ## Save tables
  #############################################################################
  if (savedata) {
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
    if (savexy) {
      if(!is.null(spxy)) {
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
    
      datExportData(xyids, 
        savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer="xyids",
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE)) 
    }

    for (tab in tabs2save) {
      datExportData(get(tab), 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer=tab,
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer,
                              add_layer=TRUE)) 
    }
  } 
  
#  if (showsteps) {
#    ## Set plotting margins
#    mar <-  par("mar")
#    par(mar=c(1,1,1,1))
#
#    plot(sf::st_geometry(xyids), col="blue", cex=.5)
#    if (!is.null(bndx)) {
#      plot(st_geometry(bndx), add=TRUE, border="black", lwd=0.75)
#    }
#    par(mar=mar)
#  }

#  if (clipxy) {
#    returnlst$clip_tabs <- lapply(tabs2save, get, envir=environment())
#    names(returnlst$clip_tabs) <- paste0("clip_", tabs2save)
#  } else {
    #returnlst$tabs <- lapply(tabs2save, get, envir=environment())
    returnlst$tabs <- tabs
#  } 
 
  returnlst$tabIDs <- tabIDs
  if (savexy && !is.null(spxy)) {
    returnlst$spxy <- spxy
  }
  returnlst$xyplt <- xyids
  #returnlst$clip_polyv <- bndx
  returnlst$bnd <- bndx
  returnlst$puniqueid <- puniqueid
  returnlst$xy.uniqueid <- xyjoinid
  returnlst$pjoinid <- pjoinid
  returnlst$states <- states
  return(returnlst)
}


