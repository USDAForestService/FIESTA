#' Spatial - Clip (intersect) polygon vector layer with polygon vector layer.
#' 
#' Wrapper for sf::st_intersection, to clip (intersect) polygon vector layer
#' with another polygon vector layer.
#' 
#' The sf::st_intersection function is used to clip polygons. \cr
#' 
#' areacalc\cr If areacalc = TRUE and the clipped spatial object is not in a
#' projected coordinate system (i.e., longlat), the object will be reprojected
#' to the Albers Equal Area projection before area is calculated.
#' 
#' @param polyv sf R object or String. Polygon data to clip. Can be a spatial
#' polygon object, full pathname to a shapefile, or name of a layer within a
#' database.
#' @param polyv_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of layer to clip. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if polyv is sf
#' object.
#' @param clippolyv SpatialPolygons class R object or String. Name of the
#' polygon spatial layer to use for clipping.
#' @param clippolyv_dsn String. Data source name (dsn; i.e., pathname or
#' database name) of clippolyv_layer. The dsn varies by driver. See gdal OGR
#' vector formats (https://www.gdal.org/ogr_formats.html). Optional if
#' clippolyv_layer is an R object.
#' @param clippolyv.filter String. Filter to subset clippolyv spatial layer.
#' @param buffdist Number. The distance to buffer the polygon before clipping.
#' Uses sf::st_buffer. The distance is based on units of polygon, st_crs(x)$units.
#' @param validate Logical. If TRUE, validates polyv and clippolyv before 
#' clipping. Uses sf::st_make_valid with default parameters 
#' (geos_method='valid_structure', geos_keep_collapsed=FALSE).
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param areacalc Logical. If TRUE, calculate area of clipped polygons and
#' append to attribute table (See details).
#' @param areaunits String. If TRUE, calculate area of clipped polygons and
#' append to attribute table ("ACRES", "HECTARES", "SQKM"). If NULL, units of
#' polyv.
#' @param nolonglat Logical. If TRUE, and both layer's coordinate system is
#' long/lat, the layers are converted to a projected CRS before clipping.
#' @param exportsp Logical. If TRUE, the spatial clipped object is exported to
#' outfolder (see spExportSpatial for details).
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options for saving data. If out_layer = NULL, default = 'polyclip'.
#' 
#' @return sf object of clipped polygon. If polyv and clippolyv have different
#' projections, the projection of returned object will have the same projection
#' as polyv (See note about on-the-fly projection conversion).
#' 
#' If exportsp=TRUE, the sf object will be written to outfolder (See note).
#' @note
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
#' ESRI Shapefile Driver\cr 
#' If exportsp=TRUE:\cr 
#' The st_write (sf) function is called. If out_fmt="shp", the ESRI Shapefile 
#' driver truncates variable names to 10 characters or less. Variable names are 
#' changed before export using an internal function (trunc10shp). If sf object 
#' has more than 1 record, it will be returned but not exported.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \dontrun{
#' # Load in USAco data from geodata package
#' USAco <- geodata::gadm(country="United States", level=2, path=tempdir())
#' 
#' # Set up data from FIESTA
#' WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp",
#'                                     package = "FIESTA"))
#' 
#' # Clip polygon with WY Bighorn object from FIESTA
#' WYbhco <- spClipPoly(polyv = USAco,
#'                      clippolyv = WYbhfn)     
#' }
#' @export spClipPoly
spClipPoly <- function(polyv, 
                       polyv_dsn = NULL, 
                       clippolyv, 
                       clippolyv_dsn = NULL, 
                       clippolyv.filter = NULL, 
                       buffdist = NULL,
                       validate = FALSE,
                       showext = FALSE, 
                       areacalc = FALSE, 
                       areaunits = "ACRES", 
                       nolonglat = TRUE,
                       exportsp = FALSE, 
                       savedata_opts = NULL) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Clip (intersect) a polygon vector layer with another polygon vector layer. 
  ## Arguments:
  ## areaunits - area calculation units ("ACRES", "HECTARES", "SQKM")
  #####################################################################################
 
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){polyv=clippolyv=exportsp=areacalc <- NULL}


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spClipPoly))
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

  ## Get poly and clippoly layers
  polyvx <- pcheck.spatial(layer = polyv, dsn = polyv_dsn, gui=gui, 
	                caption = "Poly to clip?")
  if (validate) {
    polyvx <- sf::st_make_valid(polyvx, 
                                geos_method = 'valid_structure', 
                                geos_keep_collapsed = FALSE)
  }
  clippolyvx <- pcheck.spatial(layer = clippolyv, dsn = clippolyv_dsn, 
	                 gui=gui, caption="Clipping poly?")
 
  ## clippolyv.filter
  clippolyvx <- datFilter(clippolyvx, xfilter = clippolyv.filter)$xf

  ## Check buffdist
  if (!is.null(buffdist)) {
    if (!is.numeric(buffdist)) stop("invalid buffdist... must be numeric")
  }
  
  ## Check validate
  validate <- pcheck.logical(validate, "Validate polys?", "NO")
  
  ## Check areacalc
  areacalc <- pcheck.logical(areacalc, "Calculate area?", "YES")

  ## Check exportsp 
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", title="Export spatial layer?", 
                             first="NO", gui=gui)
  
  ## Check output parameters
  if (exportsp) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
                            out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
                            overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
                            add_layer=add_layer, append_layer=append_layer, gui=gui)
    if (is.null(out_layer)) {
      out_layer <- "polyclip"
    }
    outlst$out_layer <- out_layer
  }

  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projections of polygons 
  prjdat <- crsCompare(clippolyvx, polyvx, nolonglat=nolonglat)
  clippolyvx <- prjdat$x
  polyvx <- prjdat$ycrs

  if (!is.null(buffdist)) {
    ## This will buffer the polygon 1 pixel to include all pixels inside boundary
    clippolyvx <- sf::st_buffer(clippolyvx, dist=buffdist)
  }

  if (validate) {
    clippolyvx <- sf::st_make_valid(clippolyvx, 
                                    geos_method = 'valid_structure', 
                                    geos_keep_collapsed = FALSE)
    clippolyvx <- sf::st_cast(clippolyvx)
  }
  
  ## Check extents
  bbox1 <- sf::st_bbox(clippolyvx)
  bbox2 <- sf::st_bbox(polyvx)
  check.extents(bbox1, bbox2, showext, layer1nm="polyv", layer2nm="clippoly",
			stopifnotin=TRUE)

  ## Clip poly
  ipoly <- sf::st_intersection(polyvx, sf::st_union(clippolyvx))


  ## Calculate area
  if (areacalc) {
    ipoly <- areacalc.poly(ipoly, unit=areaunits)
  }
 
  ## Export clipped poly
  if (exportsp) {
    outlst$add_layer <- TRUE
    spExportSpatial(ipoly, savedata_opts = outlst)
  }

  return(ipoly)    
}
