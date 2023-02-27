#' Spatial - Clip (intersect) point vector layer with polygon vector layer.
#' 
#' Wrapper for sf::st_intersection, to clip (intersect) point vector layer with
#' a polygon vector layer.
#' 
#' The sf::st_intersection function is used to clip points. \cr
#' 
#' If the projection of clippolyv is not the same as the xyplt, the xyplt layer
#' layer will be reprojected to the same projection as the clippoly before
#' intersection.
#' 
#' @param xyplt sf R object or String. Point data to clip. Can be a spatial
#' points object, full pathname to a shapefile, or name of a layer within a
#' database.
#' @param xyplt_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of layer to clip. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html).
#' @param uniqueid String.* Unique identifier of xyplt rows.
#' @param clippolyv sf R object or String. Name of clipping polygon spatial
#' polygon object, full path to shapefile, or name of a layer within a
#' database.
#' @param clippolyv_dsn String. Data source name (dsn; e.g., sqlite or
#' shapefile pathname) of clipping polygon. The dsn varies by driver. See gdal
#' OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param clippolyv.filter String. Filter to subset clippolyv spatial layer.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param keepNA Logical. If TRUE, keep NA values after data intersection.
#' @param returnsp Logical. If TRUE, returns sf object of points. If FALSE,
#' returns data frame of points (i.e., drops sf geometry).
#' @param othertabnms String vector. Name(s) of R objects, comma-delimited
#' files, or database layers to subset. Must include quotes (e.g.,
#' othertabnms=c("tree", "cond")).
#' @param stopifnotin Logical. If TRUE, stops if boundaries do not overlap.  If
#' FALSE, returns NULL.
#' @param savedata Logical. If TRUE, save data to outfolder.
#' @param exportsp Logical. If TRUE, the clipped spatial point data are
#' exported.
#' @param spMakeSpatial_opts List. See help(spMakeSpatial_options()) for a list
#' of options. Use to convert X/Y values to simple feature (sf) coordinates.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options for saving data. If out_layer = NULL, default = 'pntclip'.
#'
#' @return A list of the following objects:
#' 
#' \item{clip_xyplt}{ sf object. The input xyplt, clipped to polygon boundary
#' layer.  The projection will be same as clippolyv projection. }
#' \item{xy.uniqueid}{ String. Unique identifier of clip_xy. }
#' \item{clip_polyv}{ SpatialPolygonsDataFrame. The polygon boundary layer used
#' for clipping. } \item{clip_tabs}{ Data frame(s). Other tables in intabs
#' clipped to boundary. }
#' 
#' If exportsp=TRUE, the sf object will be written to out_dsn (See note).
#' @note On-the-fly projection conversion\cr The spTransform (sf) method is
#' used for on-the-fly map projection conversion and datum transformation using
#' PROJ.4 arguments. Datum transformation only occurs if the +datum tag is
#' present in the both the from and to PROJ.4 strings. The +towgs84 tag is used
#' when no datum transformation is needed. PROJ.4 transformations assume NAD83
#' and WGS84 are identical unless other transformation parameters are
#' specified.  Be aware, providing inaccurate or incomplete CRS information may
#' lead to erroneous data shifts when reprojecting. See spTransform help
#' documentation for more details.
#' 
#' ESRI Shapefile Driver\cr If exportsp=TRUE:\cr The st_write (sf) function
#' is called. If out_fmt="shp", the ESRI Shapefile driver truncates variable
#' names to 10 characters or less. Variable names are changed before export
#' using an internal function (trunc10shp). If sf object has more than 1
#' record, it will be returned but not exported.
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
#' xyext <- spClipPoint(xyplt = WYplt,
#'                      clippolyv = WYbhdistfn,
#'                      clippolyv.filter = "DISTRICTNU == '03'",
#'                      uniqueid = "CN",
#'                      spMakeSpatial_opts = list(xvar = "LON_PUBLIC",
#'                                                yvar = "LAT_PUBLIC",
#'                                                xy.crs = 4269))
#' names(xyext)
#' xyplt <- xyext$clip_xyplt
#' polyv <- xyext$clip_polyv
#' 
#' # Plot extracted values of national forest district
#' plot(sf::st_geometry(polyv))
#' plot(sf::st_geometry(xyplt), add = TRUE)
#' @export spClipPoint
spClipPoint <- function(xyplt, 
                        xyplt_dsn = NULL, 
                        uniqueid = "PLT_CN", 
                        clippolyv, 
                        clippolyv_dsn = NULL, 
                        clippolyv.filter = NULL, 
                        showext = FALSE, 
                        keepNA = FALSE, 
                        returnsp = TRUE, 
                        othertabnms = NULL,
                        stopifnotin = TRUE, 
                        savedata = FALSE, 
                        exportsp = FALSE, 
                        spMakeSpatial_opts = NULL,
                        savedata_opts = NULL){
  ###################################################################################
  ## DESCRIPTION: 
  ## Clip (intersect) point vector layer with polygon vector layer. 
  ###################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  if (gui) xyplt=uniqueid=exportsp <- NULL

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spClipPoint))
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
  

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################  
  intabs <- NULL

  ## Spatial points for clipping.. 
  ##################################################################################
  sppntx <- pcheck.spatial(xyplt, dsn=xyplt_dsn, tabnm="xyplt", 
			caption="XY coordinates?", stopifnull=TRUE)
 
  if (!"sf" %in% class(sppntx)) { 
    ## Create spatial object from xyplt coordinates
    sppntx <- spMakeSpatialPoints(sppntx, 
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
    sppntnames <- names(sppntx)
    uniqueid <- pcheck.varchar(var2check=uniqueid, 
		varnm="uniqueid", gui=gui, 
		checklst=sppntnames, caption="UniqueID of xyplt", 
		warn=paste(uniqueid, "not in xyplt"), stopifnull=TRUE)
  }

  ###################################################################################
  ##  STEP #2: GET INLAYER BY CLIPPING TO BOUNDARY IF NECESSARY   
  ###################################################################################

  ## Check polyvx
  clippolyvx <- pcheck.spatial(clippolyv, dsn=clippolyv_dsn, gui=gui, 
				tabnm="clippoly", caption="Clipping polygon?", stopifnull=TRUE)
    
  ## clippolyv.filter
  clippolyvx <- datFilter(clippolyvx, xfilter=clippolyv.filter, stopifnull=TRUE)$xf

   ## Check showext    
  showext <- pcheck.logical(showext, varnm="showext", 
		title="Plot extents?", first="YES", gui=gui)
  ## Check returnsp
  returnsp <- pcheck.logical(returnsp, "Return spatial object?", "YES", gui=gui)

  ## Check keepNA
  keepNA <- pcheck.logical(keepNA, "Keep null values?", "NO", gui=gui)

  ## Check exportsp 
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", title="Export spatial layer?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (exportsp || savedata) {
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
      out_layer <- "pntclip"
    }
  }
  
  if (!is.null(othertabnms) && !is.character(othertabnms)) {
    stop("othertabnms must be a string vector of object or file names")
  }

  
  ##################################################################
  ## DO WORK
  ##################################################################
  ## Check projections. Reproject points to clippolyv projection.
  prjdat <- crsCompare(sppntx, clippolyvx, nolonglat=TRUE)
  sppntx <- prjdat$x
  clippolyvx <- prjdat$ycrs

  ## Check extents
  bbox1 <- sf::st_bbox(clippolyvx)
  bbox2 <- sf::st_bbox(sppntx)
 
  ## Check if extents overlap... if not and stopifnotin=TRUE, return NULL
  chk <- check.extents(bbox1, bbox2, showext, layer1nm="polyv", layer2nm="sppntx",
			stopifnotin=stopifnotin, quiet=TRUE)
  if (is.null(chk)) return(NULL)
 

  ## Clip points that intersect polygon
  injoin <- sf::st_join(sppntx, clippolyvx, join=sf::st_intersects, left=FALSE)
  inpnts <- sppntx[sppntx[[uniqueid]] %in% injoin[[uniqueid]],]

  if (showext) {
    plot(sf::st_geometry(clippolyvx))
    plot(sf::st_geometry(inpnts), add=TRUE, col="blue", cex=.25)
  }

  ## Get outside points
  if (keepNA) {
    outpnt <- sppntx[!sppntx[[uniqueid]] %in% injoin[[uniqueid]],]
  }

  ## Clip othertables
  if (!is.null(othertabnms)) {
    if (!all(sapply(othertabnms, exists))) {
      miss <- othertabnms[which(!sapply(othertabnms, exists))]
      stop("invalid othertabnms: ", paste(miss, collapse=", "))
    }
    othertabs <- lapply(othertabnms, function(x) get(x, envir=environment()))
    intabs <- clip.othertables(inpnts[[uniqueid]], othertabnms=othertabnms,
		othertabs=othertabs, savedata=savedata, outfn.pre=outfn.pre, 
		outfolder=outfolder, out_dsn=out_dsn, outfn.date=outfn.date, 
		overwrite_layer=overwrite_layer)
  } 

  ## Write data to outfolder
  if (exportsp) {
    if (out_fmt == "shp" && nrow(inpnts) > length(unique(inpnts[[uniqueid]]))) {
      message("cannot export shapefile... more than 1 record per uniqueid")
    }
    
    spExportSpatial(inpnts, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer=out_layer,
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer, 
                            add_layer=TRUE))
    
    spExportSpatial(clippolyvx, 
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
    datExportData(inpnts, 
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
    

  if (!returnsp) inpnts <- sf::st_drop_geometry(inpnts)
  returnlst <- list(clip_xyplt=inpnts, uniqueid=uniqueid, clip_polyv=clippolyvx)
  if (!is.null(intabs)) returnlst$clip_tabs <- intabs
  
  return(returnlst)

}

