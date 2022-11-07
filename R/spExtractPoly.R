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
#' @param xy.uniqueid String.* Unique identifier of xyplt rows.
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
#' @param spMakeSpatial_opts List. See help(spMakeSpatial_options()) for a list
#' of options. Use to convert X/Y values to simple feature (sf) coordinates.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. If out_layer = NULL,
#' default = 'polyext'.
#' @param gui Logical. If gui, user is prompted for parameters.
#'
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
#' # Get point data from WYplt data in FIESTA
#' WYplt <- FIESTA::WYplt
#' 
#' # Get polygon vector layer from FIESTA external data
#' WYbhdistfn <- system.file("extdata",
#'                           "sp_data/WYbighorn_districtbnd.shp",
#'                           package = "FIESTA")
#' 
#' # Extract points from polygon vector layer
#' xyext <- spExtractPoly(xyplt = WYplt,
#'                        polyvlst = WYbhdistfn,
#'                        xy.uniqueid = "CN",
#'                        spMakeSpatial_opts = list(xvar = "LON_PUBLIC",
#'                                                  yvar = "LAT_PUBLIC",
#'                                                  xy.crs = 4269))
#' names(xyext)
#' xyext$outnames
#' spxyext <- xyext$spxyext
#' head(spxyext)
#' NAlst <- xyext$NAlst
#' 
#' # Plot extracted values of national forest district
#' plot(spxyext["DISTRICTNU"])
#' @export spExtractPoly
spExtractPoly <- function(xyplt, 
                          xyplt_dsn = NULL, 
                          xy.uniqueid = "PLT_CN", 
                          polyvlst, 
                          polyv_dsn = NULL, 
                          polyvarlst = NULL, 
                          polyvarnmlst = NULL, 
                          keepNA = FALSE, 
                          showext = FALSE, 
                          savedata = FALSE, 
                          exportsp = FALSE, 
                          exportNA = FALSE, 
                          spMakeSpatial_opts = NULL,
                          savedata_opts = NULL, 
                          gui = FALSE){
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

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spExtractPoly))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  
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
 
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################  
  ## Spatial points for data extraction.. 
  ##################################################################################
#  sppltx <- pcheck.table(xyplt, tab_dsn=xyplt_dsn, tabnm="xyplt", 
#			caption="XY coordinates?", stopifnull=TRUE)
  sppltx <- pcheck.spatial(xyplt, dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)
 

  if (!"sf" %in% class(sppltx)) { 
    ## Create spatial object from xyplt coordinates
    sppltx <- spMakeSpatialPoints(xyplt=sppltx, 
                                  xy.uniqueid=xy.uniqueid, 
                                  xvar=xvar, 
                                  yvar=yvar,
                                  xy.crs=xy.crs,
                                  prj=prj,
                                  datum=datum,
                                  zone=zone,
                                  zoneS=zoneS,
                                  aea.param=aea.param)
    
  } else {
    ## GET xy.uniqueid
    sppltnames <- names(sppltx)
    xy.uniqueid <- pcheck.varchar(var2check=xy.uniqueid, varnm="xy.uniqueid", gui=gui, 
		checklst=sppltnames, caption="UniqueID of spplt", 
		warn=paste(xy.uniqueid, "not in spplt"), stopifnull=TRUE)
  }

  ## Verify polygons
  ########################################################
  if (!is.null(polyvlst) && any(class(polyvlst) != "list")) {
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
  showext <- pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
 
  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui) 

  ## Check exportsp 
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial layer?", first="NO", gui=gui)  

  ## Check keepNA
  keepNA <- pcheck.logical(keepNA, varnm="keepNA", 
		title="Keep NA values?", first="YES", gui=gui)

  ## Check exportNA
  exportNA <- pcheck.logical(exportNA, varnm="exportNA", 
		title="Export NA values?", first="YES", gui=gui)


  ## Check outfolder
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
    if (is.null(out_layer)) {
      out_layer <- "polyext"
    }
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
    #sppltout <- sppltx[!sppltx[[xy.uniqueid]] %in% spxyext[[xy.uniqueid]],]

    ## Check null values
    ######################################################## 
    geocol <- attr(polyv, "sf_column")
    polyvcols <- names(polyv)[names(polyv) != geocol]
    sppltout <- spxyext[apply(sf::st_drop_geometry(spxyext[, polyvcols]), 1, 
				function(x) all(is.na(x))),]
    nulln <- nrow(sppltout)

    if (nulln > 0) {
      warning(paste("there are", nulln, "null values for", polyvnm))
      NAlst[[polyvnm]] <- sppltout
    }

    if (exportNA) {
      outfn.na <- paste0(out_layer, "_", polyvnm, "_NAvals")
      spExportSpatial(sppltout, 
          savedata_opts=list(outfolder=outfolder, 
                              out_fmt=out_fmt, 
                              out_dsn=out_dsn, 
                              out_layer=outfn.na,
                              outfn.pre=outfn.pre, 
                              outfn.date=outfn.date, 
                              overwrite_layer=overwrite_layer,
                              append_layer=append_layer, 
                              add_layer=TRUE))
    }
 
    if (!keepNA) {
      ## Subset points inside boundary
      spxyext <- spxyext[!apply(sf::st_drop_geometry(spxyext[, polyvcols]), 1, 
		function(x) all(is.na(x))),]
    }
  }

  if (savedata) {
    datExportData(spxyext, 
      savedata_opts=list(outfolder=outfolder, 
                          out_fmt=out_fmt, 
                          out_dsn=out_dsn, 
                          out_layer=out_layer,
                          outfn.pre=outfn.pre, 
                          outfn.date=outfn.date, 
                          overwrite_layer=overwrite_layer,
                          append_layer=append_layer,
                          add_layer=TRUE)) 
  }

  ## Export to shapefile
  if (exportsp) {
    spExportSpatial(spxyext, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer=out_layer,
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer, 
                            add_layer=TRUE))
    
  }
  
  returnlst <- list(spxyext=spxyext, outnames=unlist(polyvarnmlst))

  if (length(NAlst) > 0) {
    returnlst$NAlst <- NAlst
  }

  return(returnlst)
}
