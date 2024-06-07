#' Spatial wrapper - Extracts and compiles auxiliary data within a specified
#' boundary.
#' 
#' Wrapper to extract and compile auxiliary data by domain unit (i.e, estimation
#' unit or small area domain). The following information is compiled:\cr -
#' Attribute defining domain (i.e., estimation unit) from domain layer\cr -
#' Area by domain (i.e., estimation unit)\cr - Zonal statistics by domain
#' (i.e., estimation unit) - spZonalRast()\cr
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' If there is a raster and SpatialPolygon layer, and the projection of the
#' SpatialPolygons is different than the projection of the raster, the
#' SpatialPolygons object is reprojected to the projection of raster (See note
#' about on-the-fly projection conversion).
#' 
#' @param xyplt Data frame object or String. Name of layer with xy coordinates
#' and unique identifier. Can be layer with xy_dsn, full pathname, including
#' extension, or file name (with extension) in xy_dsn folder.
#' @param xyplt_dsn String. Name of database where xyplt is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param uniqueid String.* Unique identifier of xyplt records.
#' @param unittype String. Type of spatial layer unit_layer is ("POLY",
#' "RASTER").
#' @param unit_layer sf R object or String. Name of the domain spatial layer.
#' Can be a spatial polygon object, full pathname to a shapefile, name of a
#' polygon layer within a database, or a full pathname to raster file.
#' @param unit_dsn String. The data source name (dsn; i.e., folder or database
#' name) of unit_layer. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional.
#' @param unitvar String. Name of domain variable in domlayer. If NULL,
#' assuming one domain. An attribute names ONEUNIT is added to layer with
#' value=1.
#' @param unitvar2 String. If unittype="POLY", name of attribute in unit_layer
#' defining a second, hierarchical larger, estimation unit (e.g., Statecd).
#' @param rastlst.cont String vector or list. A list of raster(s) with
#' continuous data values (e.g., DEM). The list may include file name of
#' raster(s) or raster objects that are not InMemory.
#' @param rastlst.cont.name String vector. Output names for continuous rasters.
#' Optional. If NULL, name of raster is used as default or name+'_'+layer
#' number for multi-band layers.
#' @param rastlst.cont.stat String. Zonal statistic for continuous rasters.
#' @param rastlst.cont.NODATA Numeric vector. NODATA value for continuous
#' rasters (See notes). These values will be converted to NA and removed from
#' output if keepNA=FALSE. If 1 number, the same value will be used for all
#' categorical rasters. If more than 1 number, the number of values must be
#' equal to the number of rasters in rastlst.cont.
#' @param rastlst.cat String vector or list. A list of raster(s) with thematic
#' (i.e., categorical) data values. The list may include file name of raster(s)
#' or raster objects that are not InMemory.
#' @param rastlst.cat.name String vector. Output names for categorical rasters.
#' If NULL, name of raster is used as default or name+'_'+layer number for
#' multi-band layers.
#' @param rastlst.cat.NODATA Numeric vector. NODATA value for categorical
#' rasters (See notes). These values will be converted to NA and removed from
#' output if keepNA=FALSE. If 1 number, the same value will be used for all
#' categorical rasters. If more than 1 number, the number of values must be
#' equal to the number of rasters in rastlst.cat.
#' @param rastfolder String. Name of the folder with raster layers. Optional.
#' Useful if all raster layers are in same folder.
#' @param asptransform Logical. If TRUE, transforms aspect to Northness and
#' Eastness indices using sin and cosine functions.
#' @param rast.asp String or raster object. The raster in rastlst.cont that is
#' the aspect raster (Note: aspect must have units in degrees).
#' @param rast.lut String. A raster in rastlst.cat to group class values. Only
#' one raster is allowed.
#' @param rastlut String or raster object. The raster look up table used for
#' collapsing rast.lut values.
#' @param extract Logical. If TRUE, extracts values from rastlst.cont and 
#' rastlst.cat along with values from unit_layer. If FALSE, extracts only 
#' values from unit_layer.
#' @param areacalc Logical. If TRUE, returns area by domvar.
#' @param areaunits String. Output area units ("ACRES", "HECTARES",
#' "SQMETERS").
#' @param keepNA Logical. If TRUE, returns data frame of NA values.
#' @param ncores Integer. Number of cores to use for extracting values.
#' @param NAto0 Logical. If TRUE, converts extracted NA values to 0.
#' @param npixels Logical. If TRUE, include number of pixels.
#' @param addN Logical. If TRUE, adds N to unitzonal output with number of 
#' plots by unit.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param returnxy Logical. If TRUE, returns xy data as sf object (spxyplt).
#' @param savedata Logical. If TRUE, the input data with extracted values are
#' saved to outfolder.
#' @param exportsp Logical. If savedata=TRUE and returnxy=TRUE, If TRUE, the  
#' extracted strata point data are exported to outfolder.
#' @param exportNA Logical. If TRUE, NA values are exported to outfolder.
#' @param spMakeSpatial_opts List. See help(spMakeSpatial_options()) for a list
#' of options. Use to convert X/Y values to simple feature (sf) coordinates.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param vars2keep String vector. Attributes in SAdoms, other than domvar to
#' include in unitzonal output and extract to pltassgn points.
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{pltassgn}{ sf object. xyplt data with extracted values from
#' rastlst*. } \item{unitzonal}{ Data frame. Number of pixels and zonal
#' statistics from continuous rasters or zonal proportions from categorical
#' raster for each domain (i.e., estimation unit). } \item{unitvar}{ Data
#' frame. Domain (i.e., estimation unit) name. } \item{inputdf}{ Data frame.
#' Raster information input to zonal summaries. } \item{prednames}{ String
#' vector. Name(s) of predictor variable(s). } \item{zonalnames}{ String
#' vector. Name(s) of zonal variable(s). } \item{predfac}{ String vector.
#' Name(s) of categorical (i.e. factor) variable(s). } \item{npixelvar}{
#' String. Name of variable describing number of pixels. } \item{unitarea}{
#' Data frame. Area by domain (i.e., estimation unit). } \item{areavar}{
#' String. Name of variable describing acres in domarea. } \item{pltassgnid}{
#' String. Unique identifier of plot. } \item{spxy}{ Simple feature. If
#' returnxy=TRUE, Spatial coordinates. } \item{xy.uniqueid}{ String. If
#' returnxy=TRUE, unique identifier of spxy. }
#' 
#' If savedata=TRUE, datstrat and unitarea are saved to outfolder.  If
#' exportsp=TRUE, the sf object is exported to outfolder.
#' @note
#' 
#' rast.NODATA\cr NODATA values are raster pixel values that have no data of
#' interest, including pixels within the extent of the layer, but outside the
#' area of interest. Sometimes these pixels have been defined previously. The
#' defined NODATA pixels are imported to R as NULL values. When not previously
#' defined, the pixels outside the area of interest will be the minimum or
#' maximum value depending on the data type (e.g., 16-bit signed: min=-32,768;
#' max=32,768) or byte size (1 byte: min=0; max=255).  These NODATA values will
#' be added to the zonal statistic calculations if not specified in
#' rast.NODATA.
#' 
#' If exportsp=TRUE:\cr If out_fmt="shp", the st_write (sf) function is
#' called. The ArcGIS driver truncates variable names to 10 characters or less.
#' Variable names are changed before export using an internal function
#' (trunc10shp). If Spatial object has more than 1 record, it will be returned
#' but not exported.
#' 
#' On-the-fly projection conversion\cr The spTransform (sf) method is used
#' for on-the-fly map projection conversion and datum transformation using
#' PROJ.4 arguments. Datum transformation only occurs if the +datum tag is
#' present in the both the from and to PROJ.4 strings. The +towgs84 tag is used
#' when no datum transformation is needed. PROJ.4 transformations assume NAD83
#' and WGS84 are identical unless other transformation parameters are
#' specified.  Be aware, providing inaccurate or incomplete CRS information may
#' lead to erroneous data shifts when reprojecting. See spTransform help
#' documentation for more details.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \donttest{
#' # Get layers from FIESTA external data
#' ## dem (continuous)
#' demfn <- system.file("extdata",
#'                      "sp_data/WYbighorn_dem_250m.img",
#'                      package = "FIESTA")
#'   
#' ## tnt (categorical)                      
#' tntfn <- system.file("extdata",
#'                      "sp_data/WYbighorn_forest_nonforest_250m.tif",
#'                      package = "FIESTA")
#' 
#' ## unit layer
#' WYbhdistfn <- system.file("extdata",
#'                           "sp_data/WYbighorn_districtbnd.shp", 
#'                           package = "FIESTA")
#' # Get Auxiliary data                            
#' spGetAuxiliary(xyplt = FIESTA::WYplt,
#'                uniqueid = "CN",
#'                unit_layer = WYbhdistfn,
#'                unitvar = "DISTRICTNA",
#'                rastlst.cont = demfn,
#'                rastlst.cat = tntfn,
#'                spMakeSpatial_opts = list(xvar = "LON_PUBLIC",
#'                                          yvar = "LAT_PUBLIC"))
#' }
#' @export spGetAuxiliary
spGetAuxiliary <- function(xyplt = NULL, 
                           xyplt_dsn = NULL, 
                           uniqueid = "PLT_CN", 
                           unittype = "POLY", 
                           unit_layer = NULL, 
                           unit_dsn = NULL, 
                           unitvar = NULL, 
                           unitvar2 = NULL,
                           rastlst.cont = NULL, 
                           rastlst.cont.name = NULL, 
                           rastlst.cont.stat = "mean", 
                           rastlst.cont.NODATA = NULL, 
                           rastlst.cat = NULL, 
                           rastlst.cat.name = NULL, 
                           rastlst.cat.NODATA = NULL, 
                           rastfolder = NULL, 
                           asptransform = FALSE, 
                           rast.asp = NULL, 
                           rast.lut = NULL, 
                           rastlut = NULL, 
                           extract = TRUE,
                           areacalc = TRUE, 
                           areaunits = "ACRES", 
                           keepNA = TRUE, 
                           ncores = 1,
                           NAto0 = TRUE, 
                           npixels = TRUE, 
                           addN = FALSE,
                           showext = FALSE, 
                           returnxy = FALSE, 
                           savedata = FALSE, 
                           exportsp = FALSE, 
                           exportNA = FALSE, 
                           spMakeSpatial_opts = NULL,
                           savedata_opts = NULL, 
                           vars2keep = NULL, 
                           gui = FALSE) {

  ##################################################################################
  ## DESCRIPTION: Get data extraction and zonal statistics for Model-assisted or
  ##		Model-based (Small Area) Estimation. The major steps are as follows:
  ## 1) Check parameters 
  ## 2) Extract point values from unit_layer
  ## 3) Set up output data structures
  ## 4) Extract point values and get zonal statistics from continuous raster layers
  ## 5) Extract point values and get zonal statistics from categorical raster layers
  ## 6) Get total acres from unit_layer (if areacalc=TRUE)
  ##################################################################################


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) {uniqueid=savedata <- NULL}


  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters=rbind(Filters,tif=c("Raster tif files (*.tif)", "*.tif"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }

  ## Set global variables
  value=count=ACRES=TOTPIXELCNT=rast.lutfn=predfac=aspfn=prednames.cat=AOI <- NULL
  badrast <- {}
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spGetAuxiliary))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 
  ## Check parameter lists
  pcheck.params(input.params, spMakeSpatial_opts=spMakeSpatial_opts, 
                    savedata_opts=savedata_opts)
  
  
  ## Set spMakeSpatial defaults
  spMakeSpatial_defaults_list <- formals(spMakeSpatial_options)[-length(formals(spMakeSpatial_options))]
  
  for (i in 1:length(spMakeSpatial_defaults_list)) {
    assign(names(spMakeSpatial_defaults_list)[[i]], spMakeSpatial_defaults_list[[i]])
  }
  
  ## Set user-supplied spMakeSpatial values
  if (length(spMakeSpatial_opts) > 0) {
    for (i in 1:length(spMakeSpatial_opts)) {
      if (names(spMakeSpatial_opts)[[i]] %in% names(spMakeSpatial_defaults_list)) {
        assign(names(spMakeSpatial_opts)[[i]], spMakeSpatial_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(spMakeSpatial_opts)[[i]]))
      }
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

  ## Check ncores
  if (!is.null(ncores)) {
    if (length(ncores) != 1) {
      stop("ncores must be integer vector of length = 1")
    } else if (!is.numeric(ncores)) {
      stop("ncores must be integer vector of length = 1")
    } else if (ncores > 1) {
      nbrcores <- parallel::detectCores()
      if (ncores > nbrcores) {
        message("ncores is greater than number of available cores")
        message("using ", nbrcores, " ncores")
        ncores <- nbrcores
      }
    }     
  } else {
    ncores <- 1
  }

  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################

  ## Spatial points for data extraction.. 
  ##################################################################################
  sppltx <- pcheck.table(tab = xyplt, tab_dsn = xyplt_dsn, tabnm = "xyplt",
                         caption = "XY coordinates?", stopifnull = FALSE)
  if (is.null(sppltx) && extract) {
    stop("xyplt is null and extract = TRUE")
  }

  if (extract) {
    sppltx.names <- names(sf::st_drop_geometry(sppltx))

    if (!"sf" %in% class(sppltx)) { 
      ## Create spatial object from xyplt coordinates
      sppltx <- spMakeSpatialPoints(xyplt = sppltx, 
                                    xy.uniqueid = uniqueid, 
                                    xvar = xvar, 
                                    yvar = yvar,
                                    xy.crs = xy.crs)
    } else {
      ## GET uniqueid
      sppltnames <- names(sppltx)
      uniqueid <- pcheck.varchar(var2check = uniqueid, varnm = "uniqueid", gui=gui,
                                 checklst = sppltnames, 
                                 caption = "UniqueID of spplt",
                                 warn = paste(uniqueid, "not in spplt"), 
                                 stopifnull = TRUE)
    }
  }

  ## Check unittype
  ###################################################################
  unittypelst <- c("POLY", "RASTER") 
  unittype <- pcheck.varchar(var2check = unittype, varnm = "unittype", gui=gui,
                             checklst = unittypelst, 
                             caption = "Unit type?", 
                             stopifnull = TRUE)

  ## Check unit_layer and unitvar
  ###################################################################
  if (unittype == "POLY") {
    ## Check unit_layer
    unitlayerx <- pcheck.spatial(layer = unit_layer, dsn = unit_dsn, gui=gui,
                                  caption = "Unit domain spatial polygons?", 
                                  stopifnull = TRUE)
		
    ## Remove empty geometries
 	  if (sum(sf::st_is_empty(unitlayerx)) > 0) {
 	    unitlayerx <- unitlayerx[!sf::st_is_empty(unitlayerx),]
	  }

    ## Check unitvar
    unitvar <- pcheck.varchar(var2check = unitvar, varnm = "unitvar", gui=gui,
                              checklst = names(unitlayerx), 
                              caption = "Unit variable",
                              warn = paste(unitvar, "not in unit_layer"))
    unitvar2 <- pcheck.varchar(var2check = unitvar2, varnm = "unitvar2", gui=gui, 
                               checklst = names(unitlayerx), 
                               caption = "Unit2 variable", 
                               warn = paste(unitvar2, "not in unit_layer"), 
                               multiple = FALSE)
    unitvars <- c(unitvar2, unitvar)
    
    if (is.null(unitvar)) {
      if ("DOMAIN" %in% names(unitlayerx)) {
        unitvar <- "DOMAIN"
      } else {
        unitvar <- "ONEUNIT"
        unitlayerx[[unitvar]] <- 1
      }
    }
 
    varsmiss <- vars2keep[which(!vars2keep %in% names(unitlayerx))]
    if (length(varsmiss) > 0) {
      stop("missing variables: ", paste(varsmiss, collapse=", "))
    }
  } else {
    stop("under construction... please convert unit_layer to POLY")
  }

  ## Check continuous rasters
  ###################################################################
  rastlst.contfn <- tryCatch(
              getrastlst(rastlst.cont, rastfolder, quiet=TRUE, gui=gui),
     	 	            error=function(e) {
			              message(e, "\n")
			              return("stop") })
  if (!is.null(rastlst.contfn)) {
    if (length(rastlst.contfn) == 1) {
      if (rastlst.contfn == "stop") {
        stop("invalid rastlst.contfn: \n",
             toString(sapply(rastlst.cont, normalizePath)))
      }
    }
  }

  if (!is.null(rastlst.contfn)) {
    band.cont <- sapply(rastlst.contfn, function(x) rasterInfo(x)$nbands)
    nlayers.cont <- sum(band.cont)

    ## Check rastlst.cont.stat
    rastlst.cont.statlst <- c("mean", "sum") 
    rastlst.cont.stat <- pcheck.varchar(var2check = rastlst.cont.stat,
                                        varnm = "rastlst.cont.stat", gui=gui, 
                                        checklst = rastlst.cont.statlst,
                                        caption = "Raster zonal stat?")
    if (is.null(rastlst.cont.stat)) rastlst.cont.stat <- "mean"

    ## Check if length of names equals either length of bands or length of rasters
    if (!is.null(rastlst.cont.name) && (!length(rastlst.cont.name) %in% 
			c(length(rastlst.cont), nlayers.cont))) {
      stop(paste0("number of rastlst.cont.name (", length(rastlst.cont.name), ") does not ", 
		             "match number of rastlst.cont layers (", nlayers.cont, ")"))
    }

    ## Check rastlst.cont.NODATA
    if (!is.null(rastlst.cont.NODATA)) {
      if (!is.numeric(rastlst.cont.NODATA)) {
        stop("rastlst.cont.NODATA must be numeric")
      }
      if (length(rastlst.cont.NODATA) == 1 && nlayers.cont > 1) {
        message("using same rastlst.cont.NODATA value for each raster in rastlst.cont")
                     rastlst.cont.NODATA <- rep(rastlst.cont.NODATA, nlayers.cont)
      } else if (length(rastlst.cont.NODATA) > 1 && length(rastlst.cont.NODATA) != nlayers.cont) {
        stop("rastlst.cont.NODATA must be same length as rastlst.cont: ", nlayers.cont)
      }
    }

    ## Check asptransform    
    asptransform <- pcheck.logical(asptransform, varnm = "asptransform", 
		                    title="Transform aspect layer?", first="YES", gui=gui)

    ## Transform aspect 
    if (asptransform) {
      ## Check aspect raster
      rast.aspfn <- getrastlst(rast.asp, rastfolder, gui=gui)  

      if (is.null(rast.aspfn)) {
        stop("must identify aspect raster in rastlst.contfn using rast.asp")
      }
      if (length(rast.aspfn) > 1) {
        stop("only one raster allowed for transforming aspect") 
      }
      if (!rast.aspfn %in% rastlst.contfn) {
        stop("rast.asp must be included in rastlst.contfn")
      }
    }
  }

  ## Check categorical rasters
  ###################################################################
  rastlst.catfn <- tryCatch(
             getrastlst(rastlst.cat, rastfolder, quiet=TRUE, gui=gui),
     	 	           error=function(e) {
			               message(e, "\n")
			             return("stop") })
  if (is.null(rastlst.contfn) && is.null(rastlst.catfn)) {
    message("both rastlst.cont and rastlst.cat are NULL")
  }
  if (!is.null(rastlst.catfn)) {
    if (length(rastlst.catfn) == 1) {
      if (rastlst.catfn == "stop") {
        stop("invalid rastlst.catfn: \n",
             toString(sapply(rastlst.cat, normalizePath)))
      }
    }
    band.cat <- sapply(rastlst.catfn, function(x) rasterInfo(x)$nbands)
    nlayers.cat <- sum(band.cat)

    if (!is.null(rastlst.cat.name) && length(rastlst.cat.name) != length(rastlst.catfn)) {
      stop(paste0("number of rastlst.cat.name (", length(rastlst.cat.name), ") does not ", 
		"match number of rastlst.cat layers (", nlayers.cat, ")"))
    }
    ## Check rastlst.cat.NODATA
    if (!is.null(rastlst.cat.NODATA)) {
      if (!is.numeric(rastlst.cat.NODATA))
        stop("rastlst.cat.NODATA must be numeric")

      if (length(rastlst.cat.NODATA) == 1 && nlayers.cat > 1) {
        message("using same rastlst.cat.NODATA value for each raster in rastlst.cat")
        rastlst.cat.NODATA <- rep(rastlst.cat.NODATA, nlayers.cat)
      } else if (length(rastlst.cat.NODATA) > 1 && length(rastlst.cat.NODATA) != nlayers.cat) {
        stop("rastlst.cat.NODATA must be same length as rastlst.cat: ", nlayers.cat)
      }
    }

    ## Check raster for lookup table
    rast.lutfn <- suppressWarnings(getrastlst(rast.lut, rastfolder, gui=gui))
    
    if (!is.null(rast.lutfn)) {
      if (length(rast.lutfn) > 1) 
        stop("only one categorical raster allowed for grouping classes") 
      if (!rast.lutfn %in% rastlst.catfn)
        stop("rast.lut must be included in rastlst.catfn")

      ## Check rastlut
      rastlutx <- pcheck.table(rastlut, gui=gui, caption="Data table?", returnDT=TRUE)
      if (is.null(rast.lut)) 
        stop("invalid lookup table for", rast.lut)
    } 
  }

  ## npixels    
  npixels <- pcheck.logical(npixels, varnm="npixels", 
		title="Number of pixels?", first="YES", gui=gui)
  
  ## addN    
  addN <- pcheck.logical(addN, varnm="addN", 
                            title="Add N?", first="NO", gui=gui)

  ## Check showext    
  showext <- pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)

  ## Check keepNA    
  keepNA <- pcheck.logical(keepNA, varnm="keepNA", 
		title="Keep NA values?", first="YES", gui=gui)

  ## Check exportNA    
  exportNA <- pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="YES", gui=gui)

  ## Check returnxy 
  returnxy <- pcheck.logical(returnxy, varnm="returnxy", 
		title="Return XY spatial data?", first="NO", gui=gui)  

  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  if (savedata) {
    ## Check exportsp 
    exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
		   title="Export spatial?", first="NO", gui=gui)  
  }
 
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportNA) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
            out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
            overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
            add_layer=add_layer, append_layer=append_layer, gui=gui)
    outlst$add_layer <- TRUE
  }

  ##################################################################
  ## DO WORK
  ##################################################################
 
  #############################################################################
  ## 1) Extract values from unit_layer
  #############################################################################
  unitarea <- NULL
  polyvarlst <- unique(c(unitvar2, unitvar, vars2keep))
  polyvarlstchk <- polyvarlst[!polyvarlst %in% names(sppltx)]

  if (extract) {
    if (length(polyvarlstchk) == length(polyvarlst)) { 
      ## Extract values of polygon layer to points
      extpoly <- tryCatch(
            spExtractPoly(xyplt = sppltx,
                          polyvlst = unitlayerx, 
                          xy.uniqueid = uniqueid, 
                          polyvarlst = polyvarlst, 
                          keepNA = keepNA, 
                          exportNA = exportNA),
     	            error=function(e) {
			              message(e, "\n")
			              return(NULL) })
      if (is.null(extpoly)) {
        stop()
      }
      sppltx <- unique(extpoly$spxyext)
      unitNA <- extpoly$NAlst[[1]]
      outname <- extpoly$outname
      rm(extpoly)
      # gc()
    } else {
      message(unitvar, " already in spplt... not extracting from unit_layer")
    }
  
    ## Check if the name of unitvar and/or unitvar changed (duplicated)
    if (!is.null(unitvar2)) {
      if (outname[1] != unitvar2) {
        message("name changed from ", unitvar2, " to ", outname[1], 
              " because of duplicate names in xyplt")
        names(unitlayerx)[names(unitlayerx) == unitvar2] <- outname[1]
        unitvar2 <- outname[1]
      }
      if (outname[2] != unitvar) {
        message("name changed from ", unitvar, " to ", outname[2], 
              " because of duplicate names in xyplt")
        names(unitlayerx)[names(unitlayerx) == unitvar] <- outname[2]
        unitvar <- outname[2]
      }
    } else {
      if (outname[1] != unitvar) {
        message("name changed from ", unitvar, " to ", outname[1], 
              " because of duplicate names in xyplt")
        names(unitlayerx)[names(unitlayerx) == unitvar] <- outname[1]
        unitvar <- outname[1]
      }
    }
  } 

  ## If unitvar2 is not null, make one variable for zonal and area calculations
  if (!is.null(unitvar2)) {
    unitvar_old <- unitvar
    unitlayerx$UNITVAR <- paste0(unitlayerx[[unitvar2]], "#", unitlayerx[[unitvar]]) 
    unitvar <- "UNITVAR"
    polyvarlst <- c("UNITVAR", vars2keep)
  }

  #############################################################################
  ## 2) Set up outputs - unitzonal, prednames, inputdf, zonalnames
  #############################################################################
  unitzonal <- data.table(unique(sf::st_drop_geometry(unitlayerx[, polyvarlst,
 		                             drop=FALSE])))
  setkeyv(unitzonal, unitvar)
  prednames <- {}
  inputdf <- {}
  zonalnames <- {}
  
  if (extract && addN) {
    ## Get plot counts by domain unit
    ##########################################################################
    pltcnt <- data.table::data.table(sf::st_drop_geometry((sppltx)))
    if (!"AOI" %in% names(pltcnt)) {
      pltcnt$AOI <- 1
    }
    pltcnt <- pltcnt[AOI == 1, .N, by=unitvar]
    message("checking number of plots in domain...")
    message(paste0(utils::capture.output(pltcnt), collapse = "\n"))
    setkeyv(pltcnt, unitvar)

    ## Append plot counts to unitzonal
    unitzonal <- merge(unitzonal, pltcnt, by=unitvar, all.x=TRUE)
    #unitzonal <- unitzonal[pltcnt]
    unitzonal <- DT_NAto0(unitzonal, c("N", vars2keep))
  }

  ###############################################################################
  ## 3) Continuous raster layers - Extract values and get zonal statistics
  ###############################################################################
  preds <- {}
  if (!is.null(rastlst.cont)) {
 
    if (extract) {
      ## Extract values from continuous raster layers
      ###########################################################################
      extdat.rast.cont <- spExtractRast(sppltx, 
                              xy.uniqueid = uniqueid, 
                              rastlst = rastlst.contfn, 
                              interpolate = FALSE, 
                              showext = showext, 
                              var.name = rastlst.cont.name, 
                              rast.NODATA = rastlst.cont.NODATA, 
                              keepNA = keepNA, 
                              exportNA = exportNA, 
                              ncores = ncores,
                              savedata_opts = list(outfolder=outfolder, 
                              overwrite_layer=overwrite_layer)
                              )
      sppltx <- unique(extdat.rast.cont$spplt)
      prednames.cont <- extdat.rast.cont$outnames
      inputdf.cont <- extdat.rast.cont$inputdf
      rm(extdat.rast.cont)
      # gc() 
 
      if (NAto0) {
        for (col in prednames.cont) set(sppltx, which(is.na(sppltx[[col]])), col, 0)
      }

      ## Transform aspect 
      if (asptransform) {
        aspnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rast.aspfn]     
        sppltx$asp_cos <- northness(sppltx[[aspnm]])
        sppltx$asp_sin <- eastness(sppltx[[aspnm]])
        prednames.cont <- c(prednames.cont[prednames.cont != aspnm], "asp_cos", "asp_sin")
      }

    } else {
      if (is.null(rastlst.cont.name)) {
        prednames.cont <- basename.NoExt(rastlst.contfn)
      } else {
        prednames.cont <- rastlst.cont.name
      }

      ## Transform aspect 
      if (asptransform) {
        aspnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rast.aspfn]     
        prednames.cont <- c(prednames.cont[prednames.cont != aspnm], "asp_cos", "asp_sin")
      }

      if (is.null(rastlst.cont.NODATA)) {
        rastlst.cont.NODATA <- as.numeric(NA)
      }
      inputdf.cont <- data.frame(rasterfile = rastlst.contfn,
                                 band = band.cont,
                                 var.name = prednames.cont,
                                 interpolate = FALSE,
                                 windowsize = 1,
                                 statistic = "none",
                                 rast.NODATA = rastlst.cont.NODATA)                               
    }
    prednames <- c(prednames, prednames.cont)
    inputdf <- rbind(inputdf, inputdf.cont)
    zonalnames <- c(zonalnames, prednames)

    ## Extract zonal means from continuous raster layers
    #############################################################################
    #zonalDT.cont <- data.table(DOMAIN = unique(unit_layerx[[unitvar]]))
    zonalDT.cont <- data.table(unique(sf::st_drop_geometry(unitlayerx[,unitvar])))
    setkeyv(zonalDT.cont, unitvar)
    #zonalDT.cont.names <- {}
    message(paste("extracting zonal statistics...")) 

    for (i in 1:length(rastlst.contfn)) {
      rastfn <- rastlst.contfn[i]
      if (inherits(rastfn, "list")) {
        rastfn <- unlist(rastfn)
      }
      rastnm <- inputdf.cont$var.name[inputdf.cont$rasterfile == rastfn]
      rast.cont.NODATA <- rastlst.cont.NODATA[i]
      zonalstat <- rastlst.cont.stat 
      #message(rastfn, "...")

      if (asptransform && identical(rast.aspfn, rastfn)) {
        rastnm2 <- ifelse(is.null(rastnm), "asp_cos", paste0(rastnm, "_cos"))
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          rastnm2 <- c("npixels", rastnm2)
        }  
        zonaldat.rast.cont <- tryCatch(
                 spZonalRast(unitlayerx, 
                        rastfn = rastfn, 
                        polyv.att = unitvar, 
                        zonalstat = zonalstat, 
                        pixelfun = northness, 
                        rast.NODATA = rast.cont.NODATA),
                             error=function(e) {
                               message(e, "\n")
                               return(NULL)})
        if (is.null(zonaldat.rast.cont)) {
          badrast <- c(badrast, i)
          break
        }
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[unitvar]]) <- class(unitzonal[[unitvar]])        

        if (!is.null(rastnm)) {
          setnames(zonalext, outname, rastnm2)
        }
        setkeyv(zonalext, unitvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
  
        rastnm2 <- ifelse(is.null(rastnm), "asp_sin", paste0(rastnm, "_sin"))
        zonalstat <- c(rastlst.cont.stat) 
        zonaldat.rast.cont <- tryCatch(
                spZonalRast(unitlayerx, 
                       rastfn = rastfn, 
                       rast.NODATA = rast.cont.NODATA, 
                       polyv.att = unitvar, 
                       zonalstat = rastlst.cont.stat,
                       pixelfun = eastness),
     	 	                      error=function(e) {
     	 	                        message(e, "\n")
			                          return(NULL) })
        if (is.null(zonaldat.rast.cont)) {
          badrast <- c(badrast, i)
          message("\nerror when calculating zonal statistics for: ", toString(rastnm), "\n")
          break
        }
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[unitvar]]) <- class(unitzonal[[unitvar]])        
        if (!is.null(rastnm2)) {
          setnames(zonalext, outname, rastnm2)
        }
        setkeyv(zonalext, unitvar)
        zonalDT.cont <- zonalDT.cont[zonalext]
 
      } else {
        if (i == 1 && npixels) {
          zonalstat <- c("npixels", rastlst.cont.stat) 
          if (!is.null(rastnm)) {
            rastnm <- c("npixels", rastnm)
          }
        } 
        zonaldat.rast.cont <- tryCatch(
                spZonalRast(unitlayerx, 
                       rastfn = rastfn, 
                       rast.NODATA = rast.cont.NODATA, 
                       polyv.att = unitvar, 
                       zonalstat = zonalstat, 
                       showext = showext),
     	 	                      error=function(e) {
     	 	                        message(e, "\n")
     	 	                        return(NULL) })
        if (is.null(zonaldat.rast.cont)) {
          message("\nerror when calculating zonal statistics for: ", toString(rastnm), "\n")
          badrast <- c(badrast, i)
          break
        }
        zonalext <- setDT(zonaldat.rast.cont$zonalext)
        outname <- zonaldat.rast.cont$outname
        class(zonalext[[unitvar]]) <- class(unitzonal[[unitvar]])        
        if (!is.null(rastnm)) {
          setnames(zonalext, outname, rastnm)
        }
        setkeyv(zonalext, unitvar)
        zonalDT.cont <- zonalDT.cont[zonalext] 
      }
      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cont)
      rm(zonalext)
      # gc() 
    }
    unitzonal <- unitzonal[zonalDT.cont] 
  }
  if (length(badrast) > 0) {
    preds <- c(preds, inputdf.cont[badrast, "var.name"][[1]])
    inputdf.cont <- inputdf.cont[-badrast,]
  }

  ###############################################################################
  ## 4) Categorical raster layers - Extract values and get zonal probabilities
  ###############################################################################
  if (!is.null(rastlst.cat)) {
    predfac.levels <- list()

    if (extract) {

      ## Extract values from categorical raster layers
      ######################################################
      extdat.rast.cat <- spExtractRast(sppltx, 
                              xy.uniqueid = uniqueid, 
                              rastlst = rastlst.catfn, 
                              interpolate = FALSE, 
                              var.name = rastlst.cat.name, 
                              rast.NODATA = rastlst.cat.NODATA, 
                              keepNA = keepNA, 
                              ncores = ncores,
                              exportNA = exportNA, 
                              savedata_opts = list(outfolder=outfolder,
		                          overwrite_layer=overwrite_layer)
		               )
      sppltx <- extdat.rast.cat$sppltext
      prednames.cat <- extdat.rast.cat$outnames
      inputdf.cat <- extdat.rast.cat$inputdf
      prednames <- c(prednames, prednames.cat)
      predfac <- c(predfac, prednames.cat)
      #inputdf <- rbind(inputdf, inputdf.cat)
      rm(extdat.rast.cat)
      # gc() 
 
      if (NAto0) {
        for (col in prednames.cat) set(sppltx, which(is.na(sppltx[[col]])), col, 0)
      }

      if (!is.null(rast.lut)) {
        rast.lutnm <- inputdf.cat$var.name[inputdf.cat$rasterfile == rast.lutfn]

        if (!rast.lutnm %in% names(rastlut)) {
          stop("must have variable named ", rast.lutnm, " in rastlut")
        }
        ## Check that all values of sppltx are in rastlut
        check.matchval(sppltx, rastlut, rast.lutnm, tab1txt="sppltx", tab2txt="rastlut")

        ## Check if class of rast.lutnm in rastlut matches class of rast.lutnm in sppltx
        tabs <- check.matchclass(sppltx, rastlut, uniqueid, rast.lutnm)
        sppltx <- tabs$tab1
        rastlut <- tabs$tab2

        sppltx <- merge(sppltx, rastlut, by=rast.lutnm, all.x=TRUE)
        sppltx <- sppltx[, c(names(sppltx)[!names(sppltx) %in% names(rastlut)],
				names(rastlut))]
      }
    } else {
      if (is.null(rastlst.cat.name)) {
        prednames.cat <- basename.NoExt(rastlst.catfn)
      } else {
        prednames.cat <- rastlst.cat.name
      }

      if (is.null(rastlst.cat.NODATA)) {
        rastlst.cat.NODATA <- as.numeric(NA)
      }
      inputdf.cat <- data.frame(rasterfile = rastlst.catfn,
                                band = band.cat,
                                var.name = prednames.cat,
                                interpolate = FALSE,
                                windowsize = 1,
                                statistic = "none",
                                rast.NODATA = rastlst.cat.NODATA)                               
    }
    prednames <- c(prednames, prednames.cat)
    predfac <- c(predfac, prednames.cat)
    inputdf <- rbind(inputdf, inputdf.cat)
    zonalnames <- c(zonalnames, prednames)

    ## Extract zonal proportions from categorical raster layers
    #############################################################################
    zonalDT.cat <- data.table(unique(sf::st_drop_geometry(unitlayerx[,unitvar])))
    setkeyv(zonalDT.cat, unitvar)
    for (i in 1:length(rastlst.catfn)) {
      rastfn <- rastlst.catfn[i]
      rastnm <- inputdf.cat[inputdf.cat$rasterfile == rastfn, "var.name"][[1]]
      #message(rastfn, "...")
      rast.cat.NODATA <- rastlst.cat.NODATA[i]

      zonalstat <- "proportion"
      if (i == 1 && npixels) {
        zonalstat <- c("npixels", zonalstat) 
      }       
      if (identical(rast.lutfn, rastfn)) {
        zonaldat.rast.cat <- tryCatch(
              spZonalRast(unitlayerx, 
                          rastfn = rastfn, 
                          rast.NODATA = rast.cat.NODATA, 
                          polyv.att = unitvar, 
                          zonalstat = zonalstat, 
                          rastlut = rastlut, 
                          outname = names(rastlut)[2]),
     	 	                     error=function(e) {
     	 	                       message(e, "\n")
     	 	                       return(NULL) })
        if (is.null(zonaldat.rast.cat)) {
          message("\nerror when calculating zonal statistics for ", rastnm)
          badrast <- c(badrast, i)
          break
        }
      } else {
        zonaldat.rast.cat <- tryCatch(
              spZonalRast(unitlayerx, 
                          rastfn = rastfn, 
                          rast.NODATA = rast.cat.NODATA, 
                          polyv.att = unitvar, 
                          outname = rastnm, 
                          zonalstat = zonalstat),
     	 	                     error=function(e) {
     	 	                       message(e, "\n")
     	 	                       return(NULL) })
        if (is.null(zonaldat.rast.cat)) {
          message("\nerror when calculating zonal statistics for ", rastnm)
          badrast <- c(badrast, i)
          break
        }
      }
      zonalext <- setDT(zonaldat.rast.cat$zonalext)
      outname <- zonaldat.rast.cat$outname
      outname[grep("npixels", outname)] <- "npixels"
      setnames(zonalext, c(unitvar, outname))
      class(zonalext[[unitvar]]) <- class(unitzonal[[unitvar]])        
      setkeyv(zonalext, unitvar)
      zonalDT.cat <- zonalDT.cat[zonalext] 
      zonalnames <- c(zonalnames, outname[outname != "npixels"])
      predfac.levels[[rastnm]] <- as.numeric(sapply(strsplit(outname[outname != "npixels"], 
							paste0(rastnm,".")), '[', 2))
      if (npixels) npixels <- FALSE
      rm(zonaldat.rast.cat)
      rm(zonalext)
      # gc() 
    }
    tabs <- check.matchclass(unitzonal, zonalDT.cat, unitvar)
    unitzonal <- tabs$tab1
    zonalDT.cat <- tabs$tab2

    unitzonal <- unitzonal[zonalDT.cat]  
  
    if (length(badrast) > 0) {
      preds <- c(preds, inputdf.cat[badrast, "var.name"][[1]])
      inputdf.cat <- inputdf.cat[-badrast,]
    }
  }

  ## Check if any auxiliary data included. If no return estimation unit info only
  noaux <- ifelse (is.null(rastlst.contfn) && is.null(rastlst.catfn), TRUE, FALSE) 

  
  ###################################################################################
  ## Get totacres from domain polygons (if areacalc = TRUE)
  ###################################################################################
  if (areacalc) {
    unitlayerx <- areacalc.poly(unitlayerx, unit=areaunits)
    areavar <- paste0(areaunits, "_GIS")
    unitarea <- sf::st_drop_geometry(unitlayerx[, c(unitvar, vars2keep, areavar)])
    unitarea <- aggregate(unitarea[[areavar]], unitarea[, c(unitvar, vars2keep), drop=FALSE], sum)
    names(unitarea) <- c(unitvar, vars2keep, areavar)
  }

  if (extract) {
    pltassgn <- sf::st_drop_geometry(sppltx)
    spxy <- sppltx
  }

  ## If unitvar2 is not null, split back into 2 columns
  if (!is.null(unitvar2)) {
    unitvar <- unitvar_old
    
    unitarea <- data.frame(unname( t(data.frame( strsplit(sub("\\|","/",unitarea$UNITVAR), "#") )) ), unitarea)
    setnames(unitarea, c("X1", "X2"), c(unitvar2, unitvar))

    #unitarea$UNITVAR <- NULL
    
    unitzonal <- data.frame(unname( t(data.frame( strsplit(sub("\\|","/",unitzonal$UNITVAR), "#") )) ), unitzonal)
    setnames(unitzonal, c("X1", "X2"), c(unitvar2, unitvar))
    unitzonal$UNITVAR <- NULL
  }	   

 
  ## Write data frames to CSV files
  #######################################
  if (savedata) {

    if (extract) {
      ## Export to shapefile
      if (exportsp && returnxy) {
        spExportSpatial(spxy, 
                        savedata_opts = outlst)
      }    
      message("saving pltassgn...")
      outlst$out_layer <- "pltassgn"
      datExportData(pltassgn, 
                    savedata_opts = outlst)
    }
 
    if (!noaux) {
      message("saving unitzonal...")
      outlst$out_layer <- "unitzonal"
      datExportData(unitzonal,                   
                    savedata_opts = outlst)
    }
    if (areacalc) {
      message("saving unitarea...")
      outlst$out_layer <- "unitarea"
      datExportData(unitarea,           
                    savedata_opts = outlst)
    }
  }

  returnlst <- list(unitvar=unitvar)

  if (length(preds) > 0) {
    prednames <- prednames[!prednames %in% preds] 
    zonalnames <- zonalnames[!zonalnames %in% preds] 
  }

  if (extract) {
    returnlst$pltassgn <- pltassgn
    returnlst$pltassgnid <- uniqueid
  }

  if (areacalc) {
    returnlst$unitarea <- unitarea
    returnlst$areavar <- areavar
  }
  if (!noaux) {
    returnlst$unitzonal <- setDF(unitzonal)
    returnlst$inputdf <- inputdf
    returnlst$prednames <- unique(prednames)
    returnlst$zonalnames <- unique(zonalnames)
    returnlst$predfac <- unique(predfac)
    returnlst$npixelvar <- "npixels"    
  }
  if (length(predfac) > 0) {
    returnlst$predfac.levels <- predfac.levels
  }

  ## Returnxy
  if (extract && returnxy) {
    ## Add coordinate variables
    #xyplt <- data.frame(sf::st_coordinates(sppltx))
    #names(xy.coords) <- c(x,y)
    #sppltx <- sf::st_sf(data.frame(sppltx, xy.coords)) 
    returnlst$spxy <- spxy
    returnlst[["xy.uniqueid"]] <- uniqueid
  }

 
  return(returnlst)
}

