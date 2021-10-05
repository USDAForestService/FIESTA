#' Spatial - Extracts point attribute values from SpatialPolygons layer(s).
#' 
#' Extracts values from one or more polygon layers and appends to input
#' SpatialPoints layer or data frame. Points are reprojected on-the-fly to
#' projection of SpatialPolygons using PROJ.4 transformation parameters and
#' rgdal spTransform function.
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' keepnull\cr If keepnull=FALSE, points are excluded when all extracted
#' variables from any one SpatialPolygons are NULL, returning the points that
#' fall within the ' intersecting polygons.
#' 
#' @param xyplt Data frame object or String. Name of layer with xy coordinates
#' and unique identifier. Can be layer with xy_dsn, full pathname, including
#' extension, or file name (with extension) in xy_dsn folder.
#' @param xyplt_dsn String. Name of database where xyplt is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param uniqueid String.* Unique identifier of xyplt rows.
#' @param polyvlst sf R object or String. Name(s) of polygon layers to extract
#' values. A spatial polygon object, full path to shapefile, or name of a layer
#' within a database.
#' @param polyv_dsn String. Data source name (dsn) where polyvlst layers are
#' found (e.g., *.sqlite, *.gdb, folder name). The dsn varies by driver.  See
#' gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param polyvarlst String vector or list. The name(s) of variable(s) to
#' extract from polygon(s). If extracting multiple variables from more than one
#' polygon, specify names in a list format, corresponding to polyvlst.
#' @param polyvarnmlst String vector or list. Output name(s) of variable(s)
#' extracted from polygon(s). If extracting multiple variables from more than
#' one polygon, specify names in a list format, corresponding to polyvlst. The
#' number of names must match the number of variables in polyvarlst.
#' @param keepNA Logical. If TRUE, keep NA values.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param savedata Logical. If TRUE, the input data with extracted values are
#' saved to outfolder.
#' @param exportsp Logical. If TRUE, the extracted point data are exported to
#' outfolder.
#' @param exportNA Logical. If TRUE, NULL values are exported to outfolder.
#' @param out_fmt String. File format for output ('sqlite','gpkg','shp','gdb').
#' If out_fmt %in% c('sqlite','gpkg'), RSQLite package must be installed.  If
#' out_fmt='gdb', argisbinding package must be installed and functional.
#' @param out_dsn String. Name of dsn for output data (e.g., sqlite database or
#' full pathname to shapefile).
#' @param out_layer String. Name of layer in out_dsn if database.
#' @param outfolder String. If savedata=TRUE or exportsp=TRUE, name of output
#' folder.  If NULL, the working directory is used.
#' @param outfn.pre String. A prefix for out_dsn.
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite_dsn Logical. If TRUE, overwrite dsn.
#' @param overwrite_layer Logical. If TRUE, overwrite layer(s) in dsn.
#' @param ...  Other parameters for spMakeSpatialPoints.
#' @return \item{pltdat}{ SpatialPointsDataFrame object or data frame. Input
#' point data with extracted raster values appended. For multi-part polygons,
#' more than 1 row per point may be output. } \item{var.name}{ String vector.
#' Variable names of extracted variables. }
#' 
#' If savedata=TRUE, outdat data frame is saved to outfolder (Default name:
#' datext_'date'.csv).  If exportsp=TRUE, the SpatialPointsDataFrame object is
#' exported to outfolder (Default name: datext_'date'.shp). Variable names are
#' truncated to 10 characters or less (See note below). Name changes are output
#' to 'outfn'_newnames_'data'.csv in outfolder.
#' @note
#' 
#' If exportshp=TRUE:\cr The writeOGR (rgdal) function is called. The ArcGIS
#' driver truncates variable names to 10 characters or less. Variable names are
#' changed before export using an internal function (trunc10shp). If Spatial
#' object has more than 1 record, it will be returned but not exported.
#' 
#' The spTransform (rgdal) method is used for on-the-fly map projection
#' conversion and datum transformation using PROJ.4 arguments. Datum
#' transformation only occurs if the +datum tag is present in the both the from
#' and to PROJ.4 strings. The +towgs84 tag is used when no datum transformation
#' is needed. PROJ.4 transformations assume NAD83 and WGS84 are identical
#' unless other transformation parameters are specified.  Be aware, providing
#' inaccurate or incomplete CRS information may lead to erroneous data shifts
#' when reprojecting. See spTransform help documentation for more details.
#' 
#' Any names in polygon layers that are the same as in xyplt are renamed to
#' name'_1'.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' 
#' 
#'   ## Get point data from WYplt data in FIESTA
#'   WYplt <- FIESTA::WYplt
#' 
#'   ## Get polygon vector layer from FIESTA external data
#'   WYbhdistfn <- system.file("extdata", "sp_data/WYbighorn_districtbnd.shp", package="FIESTA")
#' 
#'   ## Extract points from polygon vector layer
#'   xyext <- spExtractPoly(xyplt=WYplt, polyvlst=WYbhdistfn,
#' 		uniqueid="CN", xvar="LON_PUBLIC", yvar="LAT_PUBLIC", xy.crs=4269)
#'   names(xyext)
#'   xyext$outnames
#'   spxyext <- xyext$spxyext
#'   head(spxyext)
#'   NAlst <- xyext$NAlst
#' 
#'   ## Plot extracted values of national forest district
#'   plot(spxyext["DISTRICTNU"])
#' 
#' @export spExtractPoly
spExtractPoly <- function(xyplt, xyplt_dsn=NULL, uniqueid="PLT_CN", polyvlst, 
	polyv_dsn=NULL, polyvarlst=NULL, polyvarnmlst=NULL, keepNA=FALSE, 
	showext=FALSE, savedata=FALSE, exportsp=FALSE, exportNA=FALSE, out_fmt="shp", 
	out_dsn=NULL, out_layer="polyext", outfolder=NULL, outfn.pre=NULL, 
 	outfn.date=FALSE, overwrite_dsn=FALSE, overwrite_layer=TRUE, ...){
  ######################################################################################
  ## DESCRIPTION: 
  ## Extracts values from one or more polygon layers and appends to input spatial layer 
  ## or data frame. Points are reprojected on-the-fly to projection of rasters using
  ## PROJ.4 transformation parameters and rgdal spTransform function. Includes options
  ## to use bilinear interpolation or summarize over a window of n pixels using
  ## a specified statistic.
  #########################################################################################
  
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) showext=savedata=exportsp <- NULL

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(FIESTA::spExtractPoly)), 
		names(formals(FIESTA::spMakeSpatialPoints)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Spatial points for data extraction.. 
  ##################################################################################
#  sppltx <- pcheck.table(xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
#			caption="XY coordinates?", stopifnull=TRUE)
  sppltx <- pcheck.spatial(xyplt, dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)

  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, xy.uniqueid=uniqueid, 
		exportsp=FALSE, ...)
  } else {
    ## GET uniqueid
    sppltnames <- names(sppltx)
    uniqueid <- FIESTA::pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(uniqueid, "not in spplt"), stopifnull=TRUE)
  }

  ## Verify polygons
  ########################################################
  if (!is.null(polyvlst) && class(polyvlst) != "list") {
    if ("sf" %in% class(polyvlst) || (methods::canCoerce(polyvlst, "sf"))) {
      polyvlst <- list(polyvlst)
    } else if (is.character(polyvlst)) {
      polyvlst <- as.list(polyvlst) 
    } else {
      stop("polyvlst must be a list object")
    }
  } else if (!"sf" %in% unlist(lapply(polyvlst, class))) {
    stop("invalid list object")
  }
  polyvlst <- lapply(polyvlst, 
		function(layer, polyv_dsn, gui) pcheck.spatial(layer, dsn=polyv_dsn, gui=gui),
 		polyv_dsn, gui)

  ## Check polyvarlst
  if (!is.null(polyvarlst)) {
    if (is.list(polyvarlst)) {
      if (length(polyvlst) != length(polyvarlst))
        stop("the length of polyvarlst must correspond with the length of polyvlst") 
    } else {
      if (length(polyvlst) > 1) {
        stop("polyvarlst must be a list corresponding to the length of polyvlst") 
      } else {  
        polyvarlst <- list(polyvarlst)
      }
    }
  } else {
    polyvarlst <- lapply(polyvlst, function(x) names(x)[!names(x) %in% attr(x, "sf_column")])
    names(polyvarlst) <- names(polyvlst)
  } 
 
  if (!is.null(polyvarnmlst)) {
    if (length(polyvarlst) != length(polyvarnmlst))
      stop("the length of polyvarnmlst must correspond with the length of polyvarlst") 
    if (!is.list(polyvarnmlst)) 
      polyvarnmlst <- as.list(polyvarnmlst)
    
  } else {
    polyvarnmlst <- polyvarlst
  }

   ## Check showext    
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
 
  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## Check exportsp 
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial layer?", first="NO", gui=gui)  

  ## Check keepNA
  keepNA <- FIESTA::pcheck.logical(keepNA, varnm="keepNA", 
		title="Keep NA values?", first="YES", gui=gui)

  ## Check exportNA
  exportNA <- FIESTA::pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="YES", gui=gui)


  ## Check outfolder
  if (savedata || exportsp || exportNA) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_dsn=overwrite_dsn, createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
  }

  ########################################################################
  ### DO THE WORK
  ########################################################################
  NAlst <- list()
  for (i in 1:length(polyvlst)) {
    polyv <- polyvlst[[i]]
    polyvnm <- names(polyvlst)[i]
    if (is.null(polyvnm)) {
      polyvnm <- paste0("poly", i)
    }

    ## Check projections of inlayer point layer vs. polygon layer. 
    ## If different, reproject sppltx to polygon projection.
    prjdat <- crsCompare(sppltx, polyv, nolonglat=TRUE) 
    sppltx <- prjdat$x
    polyv <- prjdat$ycrs

    ## Check extents
    if (showext) {
      bbox1 <- sf::st_bbox(polyv)
      bbox2 <- sf::st_bbox(sppltx)
      check.extents(bbox1, bbox2, showext=showext, layer1nm="polyv", layer2nm="xyplt",
			stopifnotin=TRUE)
    }

    ## Check polyvarlst
    ########################################################  
    polyvars <- polyvarlst[[i]]
    if (!all(polyvars %in% names(polyv))) {
      miss <- polyvars[!polyvars %in% names(polyv)]
      if (length(miss) == length(polyvars)) {
        stop("polyvars not in polyv: ", toString(miss))
      } else {
        message("polyvars not in polyv: ", toString(miss), "... removing from list")
        polyvars <- polyvars[polyvars != miss]
      }
    }

    vars2remove <- names(polyv)[!names(polyv) %in% polyvars]
    if (length(vars2remove) == length(names(polyv))) {
       message("polyvarlst is invalid... extracting all variables")
       polyvars <- names(polyv)
    }

    ## Check polyvarnm
    ########################################################  
    polyvarnm <- polyvarnmlst[[i]]
    if (length(polyvarnm) != length(polyvars)) {
      message("number of names does not match number of attributes... using attribute names")
      polyvarnm <- polyvars
      polyvarnmlst[[i]] <- polyvarnm
    }

    ## Change names in polyvarnms that are the same as sppltx
    ########################################################  
    polyvarnm <- suppressWarnings(sapply(polyvarnm, checknm, names(sppltx)))
    polyvarnmlst[[i]] <- polyvarnm


    ## Subset polyv to polyvars
    ########################################################  
    polyv <- polyv[, polyvars]

    ## Change names in polyv that are the same as sppltx
    ########################################################  
    names(polyv)[names(polyv) %in% polyvars] <- polyvarnm

    ## Extract data from polygon
    ######################################################## 
    #spxyext <- sf::st_intersection(sppltx, polyv[, polyvars])
    spxyext <- unique(sf::st_join(sppltx, polyv))

    ## Set polyvarnm
    ########################################################  
    #names(spxyext)[names(spxyext) %in% polyvars] <- polyvarnm 

    ## Check points outside poly
    ########################################################  
    #sppltout <- sppltx[!sppltx[[uniqueid]] %in% spxyext[[uniqueid]],]

    ## Check null values
    ######################################################## 
    geocol <- attr(polyv, "sf_column")
    polyvcols <- names(polyv)[names(polyv) != geocol]
    sppltout <- spxyext[apply(st_drop_geometry(spxyext[, polyvcols]), 1, 
				function(x) all(is.na(x))),]
    nulln <- nrow(sppltout)

    if (nulln > 0) {
      warning(paste("there are", nulln, "null values for", polyvnm))
      NAlst[[polyvnm]] <- sppltout
    }

    if (exportNA) {
      spExportSpatial(sppltout, out_dsn=out_dsn, 
		out_layer=paste0(out_layer, "_", polyvnm, "_NAvals"), 
		outfolder=outfolder, outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer)
    }
 
    if (!keepNA) {
      ## Subset points inside boundary
      spxyext <- spxyext[!apply(st_drop_geometry(spxyext[, polyvcols]), 1, 
		function(x) all(is.na(x))),]
    }
  }

  if (savedata) {
    datExportData(spxyext, outfolder=outfolder, out_fmt=out_fmt, 
		out_dsn=out_dsn, out_layer="unitarea", 
		outfn.date=outfn.date, overwrite_layer=overwrite_layer,
		add_layer=TRUE)
  }

  ## Export to shapefile
  if (exportsp) {
    spExportSpatial(spxyext, outfolder=outfolder, out_layer=out_layer, 
		outfn.pre=outfn.pre, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer)
  }
  
  returnlst <- list(spxyext=spxyext, outnames=unlist(polyvarnmlst))

  if (length(NAlst) > 0) {
    returnlst$NAlst <- NAlst
  }

  return(returnlst)
}
