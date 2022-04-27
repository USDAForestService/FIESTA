#' Spatial wrapper - Extracts point attribute values and area from a simple
#' feature or raster estimation unit layer.
#' 
#' Wrapper to get point attribute values and area from a simple feature or
#' raster layer of estimation units and calculates area. Points are reprojected
#' on-the-fly to projection of unit_layer using PROJ.4 transformation parameters
#' and rgdal spTransform function.  - Point attribute extraction from
#' simple feature (spExtractPoly) or from raster (spExtractRast) - Calculate
#' area by estimation unit(s) (areacalc.poly/areacalc.pixel)
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' If there is a raster and simple feature layer, and the projection of the
#' simple feature is different than the projection of the raster, the
#' simple feature object is transformed to the projection of raster (See note
#' about on-the-fly projection conversion).
#' 
#' @param xyplt Data frame, sf object, full pathname to *.csv or *shp, or layer
#' name in a geodatabase. Includes XY coordinates and unique identifier.  If
#' non-spatial, include options in spMakeSpatial_opts parameter.
#' @param xyplt_dsn String. Name of database where xyplt is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param uniqueid String.* Unique identifier of xyplt rows.
#' @param unittype String. Spatial layer type of unit_layer ("POLY", "RASTER").
#' @param unit_layer String or sf object. The name of the estimation unit
#' layer. The layer name may be a full pathname to a file, the
#' basename to a file, a spatial layer name from a database, or a
#' SpatialPolygons object with a defined projection.
#' @param unit_dsn String. The data source name (dsn; i.e., folder or database
#' name) of unit_layer. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional.
#' @param unitvar String. Name of estimation unit variable in unit_layer.
#' @param unit.filter String. Filter to subset unit_layer spatial layer.
#' @param areavar String. Name of area variable unit variable in unit_layer. If
#' NULL, calculates area by unitvar.
#' @param areaunits String. Output area units ("acres", "hectares",
#' "sqmeters").
#' @param keepNA Logical. If TRUE, returns data frame of NA values.
#' @param returnxy Logical. If TRUE, returns xy data as sf object (spxyplt).
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param savedata Logical. If TRUE, the input data with extracted values are
#' saved to outfolder.
#' @param exportsp Logical. If TRUE, the extracted strata point data are
#' exported to outfolder.
#' @param exportNA Logical. If TRUE, NA values are exported to outfolder.
#' @param spMakeSpatial_opts List. See help(spMakeSpatial_options()) for a list
#' of options. Use to convert X/Y values to simple feature (sf) coordinates.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param vars2keep String vector. Attributes in SAdoms, other than domvar to
#' include in dunitlut output and extract to pltassgn points.
#' @param gui Logical. If gui, user is prompted for parameters.
#' 
#' @return \item{pltunit}{ Data frame. Input point data with extracted
#' estimation unit and strata values appended. } \item{sppltunit}{
#' SpatialPointsDataframe. Spatial point data with extracted estimation unit
#' values appended. } \item{unitarea}{ Data frame. Area by estimation unit. }
#' \item{unitvar}{ Data frame. Variable name for estimation unit in unitarea. }
#' \item{acrevar}{ Data frame. Variable name for area in unitarea. }
#' \item{pltassgnid}{ String. Unique identifier of plot. }
#' 
#' If savedata=TRUE, pltstrat and unitarea are saved to outfolder (Default
#' name: *_'date'.csv).  If exportshp=TRUE, the SpatialPointsDataFrame object
#' is exported to outfolder (Default name: datext_'date'.shp). Variable names
#' are truncated to 10 characters or less (See note below). Name changes are
#' output to 'outfn'_newnames_'data'.csv in outfolder.
#' @note
#' 
#' If exportsp=TRUE:\cr If out_fmt="shp", the writeOGR (rgdal) function is
#' called. The ArcGIS driver truncates variable names to 10 characters or less.
#' Variable names are changed before export using an internal function
#' (trunc10shp). If Spatial object has more than 1 record, it will be returned
#' but not exported.
#' 
#' On-the-fly projection conversion\cr The spTransform (rgdal) method is used
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
#' @author Tracey S. Frescino, Chris Toney
#' @keywords data
#' @examples 
#' # Set up data from FIESTA
#' WYbhfn <- system.file("extdata",
#'                       "sp_data/WYbighorn_adminbnd.shp",
#'                       package = "FIESTA")
#'                       
#' # Create a `SpatialPoints` object from `WYplt`
#' WYspplt <- spMakeSpatialPoints(xyplt = WYplt, 
#'                                xy.uniqueid = "CN", 
#'                                xvar = "LON_PUBLIC", 
#'                                yvar = "LAT_PUBLIC", 
#'                                xy.crs = 4269)
#'                                
#' # Get estimation unit acres for Bighorn National Forest
#' spGetEstUnit(xyplt = WYplt, 
#'              uniqueid = "CN", 
#'              unit_layer = WYbhfn, 
#'              spMakeSpatial_opts = list(xvar = "LON_PUBLIC", 
#'                                        yvar = "LAT_PUBLIC", 
#'                                        prj = "longlat", 
#'                                        datum = "NAD83"))
#' @export spGetEstUnit
spGetEstUnit <- function(xyplt, 
                         xyplt_dsn = NULL, 
                         uniqueid = "PLT_CN", 
                         unittype = "POLY", 
                         unit_layer, 
                         unit_dsn = NULL, 
                         unitvar = NULL, 
                         unit.filter = NULL, 
                         areavar = NULL, 
                         areaunits = "acres", 
                         keepNA = FALSE, 
                         returnxy = FALSE, 
                         showext = FALSE, 
                         savedata = FALSE, 
                         exportsp = FALSE, 
                         exportNA = FALSE, 
                         spMakeSpatial_opts = NULL,
                         savedata_opts = NULL, 
                         vars2keep = NULL, 
                         gui = FALSE){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  if (gui) {dat=xytable=uniqueid=unionshpnm=savedata=parameters <- NULL}

  ## Set global variables
  value=count=ACRES_GIS <- NULL
  areacalc <- FALSE


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
  formallst <- names(formals(spGetEstUnit))
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
  
  
  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################
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
                                  xy.crs=xy.crs,
                                  prj=prj,
                                  datum=datum,
                                  zone=zone,
                                  zoneS=zoneS,
                                  aea.param=aea.param)
  } else {
    ## GET uniqueid
    sppltnames <- names(sppltx)
    uniqueid <- pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)
  }
  sppltx.names <- names(sppltx)


  ## Check unittype
  ###################################################################
  typelst <- c("POLY", "RASTER") 
  unittype <- pcheck.varchar(var2check=unittype, varnm="unittype", 
	gui=gui, checklst=typelst, caption="Estimation unit type?", stopifnull=TRUE)

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
		title="Keep NULL values?", first="YES", gui=gui)

  ## Check exportNA 
  exportNA <- pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="NO", gui=gui)

  ## Check returnxy 
  returnxy <- pcheck.logical(returnxy, varnm="returnxy", 
		title="Return XY spatial data?", first="NO", gui=gui)  

  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check exportsp 
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial?", first="NO", gui=gui)  
  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata || exportsp || exportNA) {
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

  ##################################################################
  ## DO WORK
  ##################################################################
  unitarea <- NULL
 
  ## Check unitlayer
  unitlayerx <- pcheck.spatial(layer=unit_layer, dsn=unit_dsn, gui=gui, 
	caption="Estimation unit layer?")
  
  if (unittype == "POLY") {

    ## Check unitvar
    unitvar <- pcheck.varchar(var2check=unitvar, varnm="unitvar", gui=gui, 
		checklst=names(unitlayerx), caption="Estimation unit variable", 
		warn=paste(unitvar, "not in unitlayer"))
    if (is.null(unitvar)) {
      unitlayerx$ONEUNIT <- 1
      unitvar <- "ONEUNIT"
    }

    ## Check if unitvar in sppltx
#    if (unitvar %in% names(sppltx)) {
#      sppltx[[unitvar]] <- NULL
#    }

    ## unit.filter
    unitlayerx <- datFilter(unitlayerx, xfilter=unit.filter)$xf


    ## Check areavar
    areavar <- pcheck.varchar(var2check=areavar, varnm="areavar", gui=gui, 
		checklst=names(unitlayerx), caption="Area variable", 
		warn=paste(areavar, "not in arealayer"))
    if (is.null(areavar)) {
      message("calculating area")
      areacalc <- TRUE
    }

    ## Check vars2keep
    varsmiss <- vars2keep[which(!vars2keep %in% names(unitlayerx))]
    if (length(varsmiss) > 0) {
      stop("missing variables: ", paste(varsmiss, collapse=", "))
    }
  
    ## Extract values of polygon unitlayer to points
    extpoly <- spExtractPoly(sppltx, polyvlst=unitlayerx, 
                        xy.uniqueid=uniqueid, polyvarlst=unique(c(unitvar, vars2keep)), 
                        keepNA=keepNA, exportNA=exportNA)
    sppltx <- extpoly$spxyext
    NAlst <- extpoly$NAlst[[1]]
    outname <- extpoly$outname
    if (outname != unitvar) {
      message("unitvar changed from ", unitvar, " to ", outname, 
				" because of duplicate names in xyplt")
      names(unitlayerx)[names(unitlayerx) == unitvar] <- outname
      unitvar <- outname
    }

    ## Calculate area
    if (areacalc) {
      areavar <- "ACRES_GIS"
      unitlayerx <- areacalc.poly(unitlayerx, unit=areaunits,
		areavar=areavar)
    } 

    ## Create unitarea with subset of spatial data frame
    unitarea <- unitlayerx[, c(unitvar, areavar)]
    unitarea <- aggregate(unitarea[[areavar]], list(unitarea[[unitvar]]), sum)
    names(unitarea) <- c(unitvar, areavar)

  } else { # unittype = "RASTER"
 
    ## Extract values of raster layer to points
    extrast <- spExtractRast(sppltx, rastlst=unitlayerx, 
                      var.name=unitvar, xy.uniqueid=uniqueid, 
                      keepNA=keepNA, exportNA=exportNA, 
                      savedata_opts = list(
			                    outfolder=outfolder, 
			                    outfn.pre=outfn.pre, 
			                    outfn.date=outfn.date, 
			                    overwrite_layer=overwrite_layer)
                      )
    sppltx <- extrast$spplt
    NAlst <- extrast$NAlst[[1]]

    if (!is.null(NAlst)) {
      message("NA values shown in red... ")
      plot(sf::st_geometry(sppltx), pch=16, cex=.5)
      plot(sf::st_geometry(NAlst), add=TRUE, col="red", cex=1, pch=16)
    }

    ## Calculate area
    unitarea <- areacalc.pixel(unitlayerx, unit=areaunits) 
  }

  if (!is.null(vars2keep)) {
    unitarea <- merge(unitarea, 
		sf::st_drop_geometry(unitlayerx[, unique(c(unitvar, vars2keep))]),
		by=unitvar)
  }


  ##################################################################
  ## Saving data
  ##################################################################
  pltassgn <- sf::st_drop_geometry(sppltx)

  if (savedata) {
    datExportData(pltassgn, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="pltassgn",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            add_layer=TRUE)) 
    
    datExportData(unitarea, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="unitarea",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            add_layer=TRUE)) 
  }

  ## Export to shapefile
  if (exportsp) {
    spExportSpatial(sppltx, 
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

  if (showext) {
    plot(sf::st_geometry(unitlayerx)) 
    plot(sf::st_geometry(sppltx), add=TRUE) 
  }
  
  returnlst <- list(bndx=unitlayerx, pltassgn=setDF(pltassgn), 
		pltassgnid=uniqueid, unitarea=setDF(unitarea), 
		unitvar=unitvar, areavar=areavar, areaunits=areaunits)
  if (!is.null(NAlst)) {
    returnlst$NAlst <- NAlst
  }
  ## Returnxy
  if (returnxy) {
    ## Add coordinate variables
    #xyplt <- data.frame(sf::st_coordinates(sppltx))
    #names(xy.coords) <- c(x,y)
    #sppltx <- sf::st_sf(data.frame(sppltx, xy.coords)) 
    returnlst$spxy <- sppltx[, sppltx.names]
    returnlst[["xy.uniqueid"]] <- uniqueid
  }

  return(returnlst)
}

