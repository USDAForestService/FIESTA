#' Spatial wrapper - Extracts point attribute values and pixel counts for
#' strata and estimation unit spatial layers.
#' 
#' Wrapper to extract attribute and area from a polygon or raster estimation
#' unit layer and a polygon or raster layer with strata pixel categories.
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' If spatial layers have different projections, the polygon spatial layer is
#' transformed to the projection of raster (See note about on-the-fly
#' projection conversion). If both layers are long/lat coordinate system, they
#' are transformed to default coordinate system (Conus Albers, NAD83).
#' 
#' @param xyplt Data frame, sf object, full pathname to *.csv or *shp, or layer
#' name in a geodatabase. Includes XY coordinates and unique identifier.  If
#' non-spatial, include options in spMakeSpatial_opts parameter.
#' @param xyplt_dsn String. Name of database where xyplt is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param unit_layer sf R object or String. Name of estimation unit spatial
#' layer. Can be a spatial polygon object, full pathname to a shapefile, name
#' of a polygon layer within a database, or a full pathname to raster file.
#' @param unit_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of unit_layer. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if unit_layer is
#' sf object.
#' @param uniqueid String.* Unique identifier of xyplt records.
#' Note: raster unit layers are converted to polygon.
#' @param unitvar String. If unittype="POLY", name of attribute in unit_layer
#' defining estimation units. If NULL, the unit_layer represents one estimation
#' unit.
#' @param unitvar2 String. If unittype="POLY", name of attribute in unit_layer
#' defining a second, hierarchical larger, estimation unit (e.g., Statecd).
#' @param unit.filter String. Filter to subset unit_layer spatial layer.
#' @param strattype String. Spatial layer type of strat_layer ("POLY",
#' "RASTER").  Note: polygon strata layers are converted to raster.
#' @param strat_layer sf R object or full pathname of spatial stratification
#' layer.  Can be a spatial polygon object, full pathname to a shapefile, name
#' of a polygon layer within a database, or a full pathname to raster file.
#' @param strat_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of strat_layer. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if unit_layer is
#' sf object.
#' @param strvar String. If strattype="POLY", name of strata attribute in
#' strat_layer.
#' @param strat_lut Data frame. A look-up table of codes to aggregate. The
#' format of table includes 2 columns, one column same name as strvar.  If
#' strattype="RASTER", strvar="value".
#' @param areaunits String. Output area units ("acres", "hectares",
#' "sqmeters").
#' @param rast.NODATA Numeric. NODATA value if stratlayer is raster (See
#' notes). This values will be converted to NA and removed from output.  if
#' keepNA=TRUE, NA values will not be in included in stratalut but will remain
#' in pltassgn table.
#' @param keepNA Logical. If TRUE, returns data frame of NA values.
#' @param ncores Integer. Number of cores to use for extracting values.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param returnxy Logical. If TRUE, returns xy data as sf object (spxyplt).
#' @param savedata Logical. If TRUE, the input data with extracted values are
#' saved to outfolder.
#' @param exportsp Logical. If savedata=TRUE and returnxy=TRUE, If TRUE, the  
#' extracted strata point data are exported to outfolder.
#' @param exportNA Logical. If TRUE and keepNA=TRUE, NA values are exported to
#' outfolder as a point shapefile.
#' @param spMakeSpatial_opts List. See help(spMakeSpatial_options()) for a list
#' of options. Use to convert X/Y values to simple feature (sf) coordinates.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param vars2keep String vector. Attributes in SAdoms, other than domvar to
#' include in dunitlut output and extract to pltassgn points.
#' @param gui Logical. If gui, user is prompted for parameters.
#'
#' @return \item{pltassgn}{ Data frame. Input xyplt data with extracted
#' estimation unit and strata values appended. } \item{unitarea}{ Data frame.
#' Area by estimation unit. } \item{unitvar}{ Data frame. Variable name for
#' estimation unit in unitarea. } \item{acrevar}{ Data frame. Variable name for
#' area in unitarea. } \item{stratalut}{ Data frame. Strata proportions
#' (weights) by estimation unit and strata. } \item{strvar}{ Data frame.
#' Variable name for strata values in stratalut. } \item{NAlst}{ sf List. If
#' keepNA=TRUE, and NA values exist after data extraction, the spatial NA
#' points are returned. } \item{pltassgnid}{ String. Unique identifier of plot.
#' } \item{spxy}{ Simple feature. If returnxy=TRUE, Spatial coordinates. }
#' \item{xy.uniqueid}{ String. If returnxy=TRUE, unique identifier of spxy. }
#' 
#' If savedata=TRUE, pltassgn and unitarea are saved to outfolder.\cr If
#' exportsp=TRUE, the spatial sf points object is exported to outfolder.\cr.
#' If exportNA=TRUE and NA values exist after data extraction, the spatial NA
#' points are exported to outfolder.
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
#' 
#' unitarea\cr Area by estimation unit is calculated and returned as object
#' named unitarea.  Area is based on the projection of unit_layer. If no
#' unit_layer input, than area is calculated from pixel counts.
#' 
#' polygon to raster\cr If strattype="POLY", a raster template is created based
#' on the masked extent of strat_layer, with strat_layer projected coordinate
#' system and 30 meter pixel size.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' # Create a `SpatialPoints` object from `WYplt`
#' WYspplt <- spMakeSpatialPoints(xyplt = WYplt, 
#'                                xy.uniqueid = "CN", 
#'                                xvar = "LON_PUBLIC", 
#'                                yvar = "LAT_PUBLIC", 
#'                                xy.crs = 4269)
#'                                
#' # Set up stratification from object in `FIESTA`
#' fornffn <- system.file("extdata",
#'                        "sp_data/WYbighorn_forest_nonforest_250m.tif",
#'                        package = "FIESTA")
#'                        
#' # Set up data from FIESTA
#' WYbhfn <- system.file("extdata",
#'                       "sp_data/WYbighorn_adminbnd.shp",
#'                       package = "FIESTA")
#'                                
#' # Run `spGetStrata`
#' spGetStrata(WYspplt, 
#'             uniqueid = "CN", 
#'             unit_layer = WYbhfn, 
#'             strattype = "RASTER", 
#'             strat_layer = fornffn) 
#' @export spGetStrata
spGetStrata <- function(xyplt, 
                        xyplt_dsn = NULL, 
                        unit_layer, 
                        unit_dsn = NULL, 
                        uniqueid = "PLT_CN", 
                        unitvar = NULL,
                        unitvar2 = NULL,						
                        unit.filter = NULL, 
                        strattype = "RASTER", 
                        strat_layer = NULL, 
                        strat_dsn = NULL, 
                        strvar = NULL, 
                        strat_lut = NULL, 
                        areaunits = "acres", 
                        rast.NODATA = NULL, 
                        keepNA = FALSE, 
                        ncores = 1,
                        showext = FALSE, 
                        returnxy = FALSE, 
                        savedata = FALSE, 
                        exportsp = FALSE, 
                        exportNA = FALSE, 
                        spMakeSpatial_opts = NULL,
                        savedata_opts = NULL, 
                        vars2keep = NULL, 
                        gui = FALSE){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) {uniqueid=stratclip=unitarea <- NULL}

  ## Set global variables
  value=count=strwt=polyv.lut=NAlst <- NULL
  unittype <- "POLY"

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows") {
    Filters=rbind(Filters,shp=c("Shapefiles (*.shp)", "*.shp"))
    Filters=rbind(Filters,img=c("Erdas Imagine Images (*.img)", "*.img"))
    Filters=rbind(Filters,tif=c("Raster tif files (*.tif)", "*.tif"))
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv")) }

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spGetStrata))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, spMakeSpatial_opts=spMakeSpatial_opts, savedata_opts=savedata_opts)

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

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Spatial points for data extraction.. 
  ##################################################################################
  sppltx <- pcheck.table(tab=xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)
 
  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, 
                                  xy.uniqueid=uniqueid, 
                                  xvar=xvar, 
                                  yvar=yvar,
                                  xy.crs=xy.crs)
    sppltnames <- names(sppltx)
  } else {
    ## GET uniqueid
    uniqueid <- pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=names(sppltx), caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)
  }
  sppltx.names <- names(sppltx)

  ## Spatial Layers: strattype and unittype
  ##################################################################################
  typelst <- c("POLY", "RASTER") 

  ## Check strattype
  ###################################################################
  strattype <- pcheck.varchar(var2check=strattype, varnm="strattype", 
	                gui=gui, checklst=typelst, caption="Strata type?", 
	                stopifnull=TRUE)


  ## Check strat_lut
  ###################################################################
  if (!is.null(strat_lut)) {
    if (ncol(strat_lut) > 2) {
      stop("strat_lut must be a lookup table with 2 columns")
    }
  }

  ## Check unittype
  ###################################################################
  unittype <- pcheck.varchar(var2check=unittype, varnm="unittype", 
	    gui=gui, checklst="POLY", caption="Estimation unit type?")

  ## check areaunits
  areaunitslst <- c("acres", "hectares", "sqmeters") 
  areaunits <- pcheck.varchar(var2check=areaunits, varnm="areaunits", 
	    gui=gui, checklst=areaunitslst, caption="Area units?", stopifnull=TRUE)
      areaunits <- toupper(areaunits)

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
  unitarea=stratalut <- NULL

  ## Check unit_layer
  unitlayerx <- pcheck.spatial(layer=unit_layer, dsn=unit_dsn, gui=gui, 
	caption="Estimation unit layer?")
  nounit <- ifelse (is.null(unitlayerx), TRUE, FALSE)

  ## unit.filter
  if (!nounit && unittype == "POLY") {
    unitlayerx <- datFilter(unitlayerx, xfilter=unit.filter)$xf
  }
 
  ##################################################################
  ## if unittype == "RASTER"
  ## Note: still working on combo... until then, covert to polygon
  ##################################################################
  if (unittype == "RASTER" && !nounit) {
    #message("converting unit_layer to polygon...")
    stop("currently not available...")

    #unitlayerx <- polygonizeRaster(unitlayerx)
    #unitvar <- "value"
    #unittype == "POLY"
  }

  ##################################################################
  ## if unittype == "POLY"
  ##################################################################
  if (unittype == "POLY" || nounit) {
    if (strattype == "POLY") {
      message("converting strat_layer to raster...")

      polyrast <- spPoly2Rast(polyv=strat_layer, polyv_dsn=strat_dsn, 
		                  polyv.att=strvar, outfolder=outfolder)
      strat_layer <- polyrast$rastfn
      polyv.lut <- polyrast$polyv.lut
      strat_dsn <- NULL
      strvar <- polyrast$polyv.att

      rast.NODATA <- 0

    } else {
      strvar <- "value"
    }
    ##################################################################
    ## if strattype == "RASTER"
    ##################################################################
    ## Check strat_layer
    stratlayerfn <- tryCatch(
            getrastlst(strat_layer, 
                       rastfolder = strat_dsn, 
                       stopifLonLat = TRUE),
                    error=function(e) {
                      message(e, "\n")
                      return("stop") })
    if (is.null(stratlayerfn)) {
      stop("strat_layer is NULL")
    }

    ## Get raster info
    rast_info <- rasterInfo(stratlayerfn)
    stratlayer.res <- rast_info$cellsize
    nbands <- rast_info$nbands
    rast.prj <- rast_info$crs
    rast.bbox <- rast_info$bbox

      ## Check band
#      if (!is.null(band) && nbands > 1) {
#        if (!is.integer(band)) stop("band must be integer")
#        if (band > nbands) stop("invalid band, outside of range")
#      } 
    
    if (!nounit) {
      ## Check unitvar
      unitvar <- pcheck.varchar(var2check=unitvar, varnm="unitvar", gui=gui, 
		          checklst=names(unitlayerx), caption="Estimation unit variable", 
		          warn=paste(unitvar, "not in unit_layer"), multiple=FALSE)
      unitvar2 <- pcheck.varchar(var2check=unitvar2, varnm="unitvar2", gui=gui, 
		          checklst=names(unitlayerx), caption="Estimation unit variable", 
		          warn=paste(unitvar2, "not in unit_layer"), multiple=FALSE)
      if (is.null(unitvar)) {
        unitlayerx$ONEUNIT <- 1
        unitvar <- "ONEUNIT"
      } 
      unitvars <- c(unitvar2, unitvar)
	  
	  
      ## Check projection and reproject spobj if different than rast
      unitlayerprj <- crsCompare(unitlayerx, rast.prj)$x

      ## Check extents
      names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
      bbox1 <- sf::st_bbox(rast.bbox, crs=rast.prj)
      bbox2 <- sf::st_bbox(unitlayerprj)
      if (showext) {
        check.extents(bbox1, bbox2, showext=showext, 
			  layer1nm="rast", layer2nm="unit_layer", stopifnotin=TRUE)
      }

      ## Check vars2keep
      varsmiss <- vars2keep[which(!vars2keep %in% names(unitlayerprj))]
      if (length(varsmiss) > 0) {
        stop("missing variables: ", paste(varsmiss, collapse=", "))
      }

      ## Extract values of polygon unitlayer to points
      ## Note: removing all NA values
      polyvarlst <- unique(c(unitvar2, unitvar, vars2keep))
      polyvarlstchk <- polyvarlst[!polyvarlst %in% names(sppltx)]
      
      if (length(polyvarlstchk) == length(polyvarlst)) { 
        extpoly <- tryCatch(
            spExtractPoly(sppltx, 
                          polyvlst = unitlayerprj, 
		                      xy.uniqueid = uniqueid, 
		                      polyvarlst = polyvarlst,
                          keepNA = FALSE, 
                          exportNA = exportNA),
            error=function(e) {
              message(e, "\n")
              return(NULL) })
        sppltx <- extpoly$spxyext
        unitNA <- extpoly$NAlst[[1]]
        outname <- extpoly$outname
      
        ## Check if the name of unitvar and/or unitvar changed (duplicated)
        if (!is.null(unitvar2)) {
          if (outname[1] != unitvar2) {
            message("name changed from ", unitvar2, " to ", outname[1], 
                  " because of duplicate names in xyplt")
            names(unitlayerprj)[names(unitlayerprj) == unitvar2] <- outname[1]
            unitvar2 <- outname[1]
          }
          if (outname[2] != unitvar) {
            message("name changed from ", unitvar, " to ", outname[2], 
                  " because of duplicate names in xyplt")
            names(unitlayerprj)[names(unitlayerprj) == unitvar] <- outname[2]
            unitvar <- outname[2]
          }
        } else {
          if (outname[1] != unitvar) {
            message("name changed from ", unitvar, " to ", outname[1], 
                  " because of duplicate names in xyplt")
            names(unitlayerprj)[names(unitlayerprj) == unitvar] <- outname[1]
            unitvar <- outname[1]
          }
        }
      } 
      
      ## If unitvar2 is not null, make one variable for zonal and area calculations
      if (!is.null(unitvar2)) {
        unitvar_old <- unitvar
        unitlayerprj$UNITVAR <- paste0(unitlayerprj[[unitvar2]], "#", unitlayerprj[[unitvar]]) 
        unitvar <- "UNITVAR"		
      }

      ## Get pixel counts by estimation unit
      stratalut <- setDT(zonalFreq(src=unitlayerprj, attribute=unitvar, 
			      rasterfile=stratlayerfn, band=1, na.rm=TRUE, ignoreValue=rast.NODATA))
      setnames(stratalut, c("zoneid", "value", "zoneprop"), c(unitvar, strvar, "strwt"))
      strataNA <- stratalut[is.na(get(strvar)), ]
      stratalut <- stratalut[!is.na(get(strvar)), ]
      class(stratalut[[unitvar]]) <- class(unitlayerx[[unitvar]])        

      ## Get unitarea 
      unitlayerprj <- areacalc.poly(unitlayerprj, unit=areaunits)
      areavar <- paste0(areaunits, "_GIS")  
      unitarea <- aggregate(unitlayerprj[[areavar]], list(unitlayerprj[[unitvar]]), sum)
      names(unitarea) <- c(unitvar, areavar)
        
    } else {  ## if nounit == TRUE
      stratalut <- areacalc.pixel(stratlayerfn, rast.NODATA=rast.NODATA)
      stratalut$strwt <- stratalut$count / sum(stratalut$count)
      strvar <- "value"

      unitarea <- sum(stratalut$area)
      unitvar <- NULL
      areavar <- NULL 
    }

    ## Extract values of raster layer to points
    extrast <- spExtractRast(sppltx, 
	                           rastlst = stratlayerfn,
							               var.name = strvar, 
							               xy.uniqueid = uniqueid,
							               keepNA = keepNA, 
							               exportNA = exportNA, 
							               rast.NODATA = rast.NODATA,
							               ncores = ncores, 
							               savedata_opts=savedata_opts)
    sppltx <- extrast$spplt
    pltdat <- extrast$sppltext
    rastfnlst <- extrast$rastfnlst
    outname <- extrast$outnames
    NAlst <- extrast$NAlst[[1]]

    if (!is.null(NAlst)) {
      message("NA values shown in red... ")
      plot(sf::st_geometry(sppltx), pch=16, cex=.5)
      plot(sf::st_geometry(NAlst), add=TRUE, col="red", cex=1, pch=16)
    }
  }

  ## If lookup table, merge and aggregate
  #######################################
  if (!is.null(strat_lut)) {
    tabs <- check.matchclass(stratalut, strat_lut, strvar)
    stratalut <- tabs$tab1
    strat_lut <- tabs$tab2
    
    stratalut <- merge(stratalut, strat_lut, by=strvar)
    strclvar <- names(strat_lut)[names(strat_lut) != strvar]

    stratalut <- stratalut[, lapply(.SD, sum, na.rm=TRUE), 
		by=c(unitvar, strclvar), .SDcols=c("count", "strwt")]
    setorderv(stratalut, c(unitvar, strclvar))
    strvar2 <- checknm("STRATUMCD", names(sppltx))
    setnames(stratalut, strclvar, strvar2)

    tabs <- check.matchclass(sppltx, strat_lut, strvar)
    sppltx <- tabs$tab1
    strat_lut <- tabs$tab2

    sppltx <- merge(sppltx, strat_lut, by=strvar)
    setnames(sppltx, strclvar, strvar2)
    strvar <- strvar2
  } else {
    strvar2 <- checknm("STRATUMCD", names(sppltx))
    setnames(stratalut, strvar, strvar2)
    setnames(sppltx, strvar, strvar2)
    strvar <- strvar2
  } 

  if (!is.null(vars2keep)) {
    stratalut <- merge(stratalut, 
		sf::st_drop_geometry(unitlayerx[, unique(c(unitvar, vars2keep))]),
		by=unitvar)
  }

  ## Change name - count ti P2POINTCNT
  if ("count" %in% names(stratalut)) {
    setnames(stratalut, "count", "P2POINTCNT")
  }

  ##################################################################
  ## Saving data
  ##################################################################
  pltassgn <- sf::st_drop_geometry(sppltx)
  
  ## If unitvar2 is not null, split back into 2 columns
  if (!is.null(unitvar2)) {
	  unitvar <- unitvar_old
	
	  unitarea <- data.frame(unname( t(data.frame( strsplit(sub("\\|","/",unitarea$UNITVAR), "#") )) ), unitarea)
	  setnames(unitarea, c("X1", "X2"), c(unitvar2, unitvar))
	  unitarea$UNITVAR <- NULL

	  stratalut <- data.frame(unname( t(data.frame( strsplit(sub("\\|","/",stratalut$UNITVAR), "#") )) ), stratalut)
	  setnames(stratalut, c("X1", "X2"), c(unitvar2, unitvar))
	  stratalut$UNITVAR <- NULL
  }	   
  spxy <- sppltx[, sppltx.names]
  
  if (savedata) {
    
    ## Export to shapefile
    if (exportsp && returnxy) {
      spExportSpatial(spxy, 
                      savedata_opts = outlst)
    }    
    
    message("saving pltassgn...")
    outlst$out_layer <- "pltassgn"
    datExportData(pltassgn, 
                  savedata_opts = outlst)

    message("saving unitarea...")
    outlst$out_layer <- "unitarea"
    datExportData(unitarea,           
                  savedata_opts = outlst)

    message("saving stratalut...")
    outlst$out_layer <- "stratalut"
    datExportData(stratalut,                   
                  savedata_opts = outlst)
  }
  
  returnlst <- list(bnd=unitlayerx, pltassgn=setDF(pltassgn), 
		  pltassgnid=uniqueid, unitarea=setDF(unitarea), 
		  unitvar=unitvar, unitvar2=unitvar2, areavar=areavar, 
		  areaunits=areaunits,
		  stratalut=setDF(stratalut), strvar=strvar, 
		  getwt=FALSE, strwtvar="strwt")

  ## Returnxy
  if (returnxy) {
    ## Add coordinate variables
    #xyplt <- data.frame(sf::st_coordinates(sppltx))
    #names(xy.coords) <- c(x,y)
    #sppltx <- sf::st_sf(data.frame(sppltx, xy.coords)) 
    returnlst$spxy <- spxy
    returnlst[["xy.uniqueid"]] <- uniqueid

    if (!is.null(NAlst)) {
      returnlst$NAlst <- NAlst
    }
  } else {
    if (!is.null(NAlst)) {
      returnlst$NAlst <- sf::st_drop_geometry(NAlst)
    }
  }
 
  return(returnlst)
}

