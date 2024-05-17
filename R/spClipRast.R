#' Spatial - Subsets a raster to a polygon extent or boundary.
#' 
#' Subsets a raster to the extent or masked boundary of a spatial polygon
#' object or shapefile (*.shp), with option to write the new file to the
#' outfolder with specified format (fmt).
#' 
#' Use spClipRast() to prompt for input.
#' 
#' If the projection of polyv is different than the projection of rast, the
#' polyv SpatialPolygons object is converted to the projection of rast (See
#' note about on-the-fly projection conversion).
#' 
#' @param clippolyv SpatialPolygons class R object or String. Name of the
#' polygon spatial layer to use for clipping.
#' @param clippolyv_dsn String. The data source name (dsn; i.e., pathname or
#' database name) of clippolyv. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if polyv_layer is
#' an R object.
#' @param clippolyv.filter String. Filter to subset clippolyv spatial layer.
#' @param rast String or Raster. Raster name, including extension. Option to
#' include full path.
#' @param rastfolder String. Name of the raster folder. Optional.
#' @param rast.crs EPSG code or PROJ.4 string. Defined coordinate reference
#' system if rast has no crs defined.
#' @param bands Numeric vector. If rast is a multi-layer raster and only 1 or
#' some layers are desired, specify layer number(s) in a vector format. If
#' NULL, all layers are summed.
#' @param NODATA Number. The NODATA value for background values. If NODATA is
#' NULL, and a NODATA value is defined on the input raster, the default is the
#' defined NODATA value, else it is defined based on its datatype (see 
#' DEFAULT_NODATA for default data values).
#' @param buffdist Number. The distance to buffer the polygon before clipping
#' raster. Uses sf::st_buffer. The distance is based on units of the raster.
#' @param validate Logical. If TRUE, validates polyv and clippolyv before 
#' clipping. Uses sf::st_make_valid with default parameters 
#' (geos_method='valid_structure', geos_keep_collapsed=FALSE).
#' @param maskByPolygons Logical. If TRUE, rast is clipped to boundary of
#' polygon.  If FALSE, rast is clipped to extent of polygon.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param fmt String. Format for exported raster. Default is format of unput
#' raster.  ("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI",
#' "EHdr", "HFA", "VRT").  VRT is a virtual raster (See note below).
#' @param compress Logical. If TRUE, compress the final output.
#' @param compressType String. An optional compression type ('LZW', "DEFLATE',
#' "PACKBITS'). Note: If format = 'HFA', a default compression type is used.
#' @param outfolder String. The output folder.
#' @param outfn String. Name of output data file. If NULL, default is
#' 'rastclip'.  If no extension, a default is provided to match output format.
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param overwrite Logical. If TRUE, overwrite files in outfolder.
#' @return \item{value}{ Spatial S4 object. A clipped raster file. }
#' 
#' The clipped raster is written to outfolder with specified format or same
#' format as input raster.
#' @note On-the-fly projection conversion\cr 
#' The spTransform (sf) method is
#' used for on-the-fly map projection conversion and datum transformation using
#' PROJ.4 arguments. Datum transformation only occurs if the +datum tag is
#' present in the both the from and to PROJ.4 strings. The +towgs84 tag is used
#' when no datum transformation is needed. PROJ.4 transformations assume NAD83
#' and WGS84 are identical unless other transformation parameters are
#' specified.  Be aware, providing inaccurate or incomplete CRS information may
#' lead to erroneous data shifts when reprojecting. See spTransform help
#' documentation for more details.
#' 
#' VRT format Virtual raster format is a pointer to a temporary file, commonly
#' used as an intermediate step between processes. The VRT format ignores
#' option to maskByPolygons.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#' \donttest{
#' # Get polygon vector layer from FIESTA external data
#' WYbhdistfn <- system.file("extdata",
#'                           "sp_data/WYbighorn_districtbnd.shp", 
#'                           package = "FIESTA")
#' WYbhdist <- FIESTA::spImportSpatial(WYbhdistfn)
#' WYbhdist
#' 
#' # Get raster layers from FIESTA external data
#' demfn <- system.file("extdata",
#'                      "sp_data/WYbighorn_dem_250m.img",
#'                      package = "FIESTA")
#' 
#' # Clip raster to district = '03'
#' dem03 <- spClipRast(rast = demfn,
#'                     clippolyv = WYbhdistfn,
#'                     clippolyv.filter = "DISTRICTNU == '03'",
#'                     overwrite = TRUE,
#'                     outfolder = tempdir())
#' terra::plot(terra::rast(dem03))
#' 
#' # Clip raster to district = '06'
#' dem06 <- spClipRast(rast = demfn,
#'                     clippolyv = WYbhdistfn,
#'                     clippolyv.filter = "DISTRICTNU == '06'",
#'                     overwrite = TRUE,
#'                     outfolder = tempdir())
#' 
#' # Plot extracted values of national forest district
#' terra::plot(terra::rast(dem06))
#' }
#' @export spClipRast

spClipRast <- function(rast, 
                       rastfolder = NULL, 
                       clippolyv, 
                       clippolyv_dsn = NULL, 
                       clippolyv.filter = NULL, 
                       rast.crs = NULL, 
                       bands = NULL, 
                       NODATA = NULL, 
                       buffdist = NULL,
                       validate = FALSE,
                       maskByPolygons = TRUE, 
                       showext = FALSE, 
                       fmt = "GTiff", 
                       compress = FALSE, 
                       compressType = "DEFLATE",
                       outfolder = NULL,
                       outfn = "rastclip", 
                       outfn.pre = NULL, 
                       outfn.date = FALSE, 
                       overwrite = FALSE) {
  
  #####################################################################################
  ## DESCRIPTION: 
  ## Clips a raster with a polygon.
  #####################################################################################
  
  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spClipRast)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## If gui.. set variables to NULL
  if(gui){poly=clippoly=unionpoly=savedata <- NULL}
  
  drivers <- data.frame(
    fmt = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", "EHdr", "HFA", "VRT"),
    DefaultExt = c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", "bil", "img", "vrt"),
    stringsAsFactors=FALSE
  )	
  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################
  
  ## Get poly and clippoly layers
  clippolyvx <- pcheck.spatial(layer=clippolyv, dsn=clippolyv_dsn, gui=gui, 
                               caption="Poly to clip?")
  
  ## clippolyv.filter
  clippolyvx <- datFilter(clippolyvx, xfilter=clippolyv.filter)$xf

  ## Verify raster
  rastfn <- getrastlst(rast, rastfolder, gui=gui)
  
  ## Get names of raster
  rastnm <- basename.NoExt(rastfn)
  
  ## Get raster info
  rast_info <- rasterInfo(rastfn)
  rast.fmt <- rast_info$format
  rast.prj <- rast_info$crs
  nbands <- rast_info$nbands
  nodata <- rast_info$nodata_value
  rast.dtyp <- rast_info$datatype
  rast.bbox <- rast_info$bbox
  
  ## Check if rast has a defined projection 
  if (is.null(rast.prj) || is.na(rast.prj)) {
    message("raster has no defined projection")
    if (is.null(rast.crs)) {
      stop("include rast.crs to define projection")
    } else {
      rast.prj <- rast.crs
    }
  }
  
  ## Get default extension
  defaultext <- drivers[match(rast.fmt, drivers$fmt), "DefaultExt"]
  
  ## Check bands
  if (!is.null(bands)) {
    bandlst <- 1:nbands
    if (any(bands < min(bandlst) | bands > max(bandlst))) {
      noband <- bands[which(bands < min(bandlst) | bands > max(bandlst))]
      message("bands are out of range:", paste(noband, collapse=","), " ...clipping all bands")
      bands <- NULL
    }
  }
  
  ## Check NODATA
  if (is.null(NODATA)) {
	  if (is.na(nodata)) {
	    nodata <- getDefaultNodata(rast.dtyp)
	  } 
  } else {
    if (!is.numeric(NODATA) || length(NODATA) > 1) {
      stop("NODATA must be numeric")
	  } else {
	    nodata <- NODATA
	  }
  }
  
  ## Check buffdist
  if (!is.null(buffdist)) {
    if (!is.numeric(buffdist)) stop("invalid buffdist... must be numeric")
  }
  
  ## Check validate
  validate <- pcheck.logical(validate, "Validate polys?", "NO")
  
  ## Check maskByPolygons
  maskByPolygons <- pcheck.logical(maskByPolygons, varnm="maskByPolygons", 
                           title="Mask by polygon?", first="NO", gui=gui)
  
  ## Check showext
  showext <- pcheck.logical(showext, varnm="showext", title="Show Extents?", 
                            first="NO", gui=gui)
    
  ## Check fmt
  fmt <- pcheck.varchar(var2check=fmt, varnm="fmt", gui=gui, 
                        checklst=drivers$fmt, caption="Export format")
  if (is.null(fmt)) {
    message("no format specified... using format of input rast")
    fmt <- rast.fmt
  }
  fmt.ext <- drivers[match(fmt, drivers$fmt), "DefaultExt"]
  
  ## Check compression
  compress <- pcheck.logical(compress, varnm="compress", 
                             title="Compress?", first="NO", gui=gui)

  ## Check compression type
  co <- NULL
  if (compress) {
    if (fmt == "HFA") {
      co <- paste0("COMPRESSION=", TRUE)
    } else {
      compresslst <- c("LZW", "PACKBITS", "DEFLATE")
      compress <- pcheck.varchar(var2check=compress, varnm="compress", 
                        checklst=compresslst, caption="Compress output?", gui=gui)
      if (!is.null(compress)) {
        co <- paste0("COMPRESS=", compress)
      }
    }
  }  
  
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
                              title="Overwrite files?", first="NO", gui=gui)  
  outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
                               title="Add date to outfiles?", first="YES", gui=gui)  
  outfolder <- pcheck.outfolder(outfolder, gui)
  
  
  outfilenm <- getoutfn(outfn=outfn, outfolder=outfolder, outfn.pre=outfn.pre, 
                        outfn.date=outfn.date, overwrite=overwrite, ext=fmt.ext)
  
  
  ##################################################################
  ## DO WORK
  ##################################################################
  
  ## Check projections of polygons 
  clippolyvx <- crsCompare(clippolyvx, rast.prj, crs.default=rast.crs, nolonglat=TRUE)$x
  
  if (!is.null(buffdist)) {
    ## This will buffer the polygon 1 pixel to include all pixels inside boundary
    clippolyvx <- sf::st_buffer(clippolyvx, dist=buffdist)
  }
  
  ## Validate polygon
  if (validate) {
    clippolyvx <- sf::st_make_valid(clippolyvx, 
                                    geos_method = 'valid_structure', 
                                    geos_keep_collapsed = FALSE)
    clippolyvx <- sf::st_cast(clippolyvx)
  }
  
  ## Check extents
  names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
  bbox1 <- sf::st_bbox(clippolyvx)
  bbox2 <- sf::st_bbox(rast.bbox, crs=rast.prj)
  check.extents(bbox1, bbox2, showext, layer1nm="clippolyv", layer2nm="rast",
                stopifnotin=TRUE)
  
  
  ## Clip raster
  clipRaster(src = clippolyvx, 
             srcfile = rastfn, 
			       src_band = bands, 
			       dstfile = outfilenm, 
             fmt = fmt, 
			       maskByPolygons = maskByPolygons, 
			       init = nodata, 
			       dstnodata = nodata, 
             options = co)
   
  return(outfilenm)    
}
