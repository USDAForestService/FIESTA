#' Spatial - Reprojects an Esri shapefile (*shp) or S4 Spatial object.
#' 
#' Reprojects an Esri shapefile (*.shp) or S4 Spatial object to a new
#' geographic or projected coordinate system, with option to save new object.
#' 
#' 
#' @param rastfn String or Raster. File name(s) with extensions, or raster
#' object(s).  Note: raster objects must be written to file.
#' @param bands Numeric vector. If rast is a multi-layer raster and only 1 or
#' some layers are desired, specify layer number(s) in a vector format. If
#' NULL, all layers are projected.
#' @param crs Coordinate Reference System (CRS). The CRS of rastfn if not
#' defined.  EPSG:code, PROJ.4 declaration, or .prj file containing WKT. For
#' example, PROJ.4: "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84
#' +datum=WGS84 +units=m +no_defs".  If NULL, and the CRS of rastfn is not
#' defined, uses crs.default.
#' @param rast.ref String or Raster. File name(s) with extensions, or raster
#' object to use as reference raster.
#' @param crs.new Coordinate Reference System. New CRS for rastfn.  EPSG:code,
#' PROJ.4 declaration, or .prj file containing WKT. For example, PROJ.4:
#' "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m
#' +no_defs".
#' @param res.new Integer vector. One or two values defining new resolution of
#' raster (in target georeferenced units) (e.g., 30 or c(30,30)).
#' @param bbox.new <xmin ymin xmax ymax> Georeferenced extent or bounding box
#' of new raster.
#' @param dtype.new String. Force a data type of new raster. If NULL, the data
#' type will be same as rastfn (e.g., Byte, Int16, UInt16).
#' @param NODATA.new Integer. Set nodata values for new raster. New files will
#' be initialized to this value and if possible the nodata value will be
#' recorded in the output file. Use a value of "None" to ensure that nodata is
#' not defined. If NULL, NODATA and rastfn has a set NODATA value, this value
#' will be used for new raster.
#' @param resamp.method Method for resampling ('near', 'bilinear', 'cubic',
#' 'cubicspline', 'landzos', 'average', 'mode', 'min', 'max', 'med', 'q1',
#' 'q3').
#' @param crs.default Coordinate Reference System. A default CRS if
#' crs.new=NULL.  The default is: EPSG:5070, Conus Albers, PRJ4='+proj=aea
#' +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96, +x_0=0 +y_0=0", "+ellps=GRS80
#' +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'.
#' @param compress String. An optional compression type ('LZW', "DEFLATE',
#' "PACKBITS').
#' @param BigTIFF Logical. If TRUE, compress option for big files (> 4GB).
#' @param outfolder String. If exportsp=TRUE, name of output folder. If NULL,
#' the working directory is used.
#' @param outfn String. Name of output raster. If NULL, default is 'polyrast'.
#' @param outext String. Name of raster extension (fmt). If NULL, uses
#' extension from outfn or rastfn.
#' @param overwrite Logical. If TRUE, overwrites raster file.
#' @return \item{rastfn.new}{ String. Full path name to reprojected raster. }
#' @note
#' 
#' Coordinate Reference Systems (CRS)\cr An ellipse is an estimated model
#' describing the basic shape of the Earth and is the basis for all coordinate
#' systems. There are many ellipsoids designed for local (e.g., NAD27) or
#' global (e.g., WGS84, GRS80) use. The datum defines the reference position of
#' the coordinate axes associated with a specific ellipsoid. Specifying the
#' datum also defines the ellipsoid, whereas specifying the ellipsoid does not
#' provide information of the datum.
#' 
#' WGS84 vs NAD83 WGS84 and NAD83 datums are often used interchangeably, and
#' use very similar ellipsoids (WGS84 and GRS80, respectively), but have
#' different reference points. Slight tectonic shifts through time have caused
#' increased divergence between the two, with NAD83 datum intended to track
#' movements more consistently.
#' 
#' Common Datums and associated spheroid (ellipsoid):\cr NAD27 - North American
#' Datum of 1927 (Clarke 1866 spheroid)\cr NAD83 - North American Datum of 1983
#' (GRS 1980 spheroid)\cr WGS84 - World Geodetic System of 1984 (WGS 1984
#' spheroid)\cr
#' 
#' From R, use projInfo for list of different projections and datums.\cr >
#' projInfo(type="proj")\cr > projInfo(type="datum")\cr
#' 
#' Common EPSG Geodetic codes in U.S.\cr EPSG:4326 - Longitude/Latitude (WGS84)
#' - Common for global displays (used by Google Earth)\cr EPSG:4269 -
#' Longitude/Latitude (NAD83) - Common by U.S. Federal Agencies\cr
#' 
#' The sf::st_transform (GDAL) method is used for map projection conversion and
#' datum transformation using PROJ.4 arguments. Datum transformation only
#' occurs if the +datum tag is present in the both the from and to PROJ.4
#' strings. The +towgs84 tag is used when no datum transformation is needed.
#' PROJ.4 transformations assume NAD83 and WGS84 are identical unless other
#' transformation parameters are specified. Be aware, providing inaccurate or
#' incomplete CRS information may lead to erroneous data shifts when
#' reprojecting. See spTransform help documentation for more details.
#' @author Tracey S. Frescino, Chris Toney
#' @keywords spatial
#' @examples 
#' \donttest{
#' # Get raster layers from FIESTA external data
#' demfn <- system.file("extdata",
#'                      "sp_data/WYbighorn_dem_250m.img",
#'                      package = "FIESTA")
#' 
#' # Plot original projection
#' raster::plot(raster::raster(demfn))
#' 
#' # Check original projection
#' sf::st_crs(raster::raster(demfn))$proj4string
#' 
#' # Reproject raster
#' reprojected <- spReprojectRaster(rastfn = demfn,
#'                                  crs.new = "EPSG:32613",
#'                                  outfolder = tempdir())
#'                                  
#' # Plot new projection
#' raster::plot(raster::raster(reprojected))
#' 
#' # Check new projection
#' sf::st_crs(raster::raster(reprojected))$proj4string
#' }
#' @export spReprojectRaster
spReprojectRaster <- function(rastfn, 
                              bands = NULL, 
                              crs = NULL, 
                              rast.ref = NULL, 
                              crs.new = NULL, 
                              res.new = NULL, 
                              bbox.new = NULL, 
                              dtype.new = NULL, 
                              NODATA.new = NULL, 
                              resamp.method = "near", 
                              crs.default = "EPSG:5070", 
                              compress = NULL, 
                              BigTIFF = FALSE,
                              outfolder = NULL, 
                              outfn = NULL, 
                              outext = NULL, 
                              overwrite = FALSE){

  ##################################################################################
  ## DESCRIPTION: reproject raster. If crs.new is not defined, uses crs.default. 	
  ## Default projection: NAD83 - Conus Albers (EPSG:5070)
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ## compress ('LZW', 'DEFLATE', 'PACKBITS')
  ##################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables
  s_srs=of=ot=te=tr=r=dstnodata=co=addOptions <- NULL
  gui <- FALSE

  drivers <- data.frame(
	fmt = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", 
		"EHdr", "HFA", "VRT"),
	DefaultExt = c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", 
		"bil", "img", "vrt"),
	stringsAsFactors=FALSE
  )	

  ## If gui.. set variables to NULL
  if (gui) savedata <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Verify rasters 
  srcfile <- getrastlst.rgdal(rastfn, gui=gui)
  srcinfo <- rasterInfo(srcfile)

  ## Get names of raster 
  rastnm <- basename.NoExt(srcfile) 
 
  ## Get number of bands in each raster and set names 
  rast.nbands <- srcinfo$nbands

  ## Check bands
  if (is.null(bands))
    bands <- 1
  if (!is.numeric(bands) || any(bands > 1:rast.nbands))
    stop("invalid bands... must be integer(s) less than ", rast.nbands)

  ## Check rast.ref
  rast.ref <- getrastlst.rgdal(rast.ref, gui=gui)
  if (!is.null(rast.ref)) {
    info.ref <- rasterInfo(rast.ref)
    bbox.new <- info.ref$bbox
    crs.new <- info.ref$crs
    res.new <- info.ref$cellsize
  }

  ## Check resolution
  rast.res <- srcinfo$cellsize
  if (is.null(res.new)) {
    tr <- rast.res
  } else {
    if (!is.numeric(rast.res) || length(rast.res) > 2)
      stop("invalid rast.res... must be numeric integer of 1 or 2 values")
    if (res.new[1] != rast.res[1])
      message("changing resolution from ", rast.res[1], " to ", res.new[1])
    if (length(res.new) == 1) res.new <- c(res.new, res.new)
    tr <- res.new
  }     

  ## Get projection of raster
  rast.prj <- srcinfo$crs
  if (is.na(rast.prj) || is.null(rast.prj) || rast.prj == "") {
    if (is.null(crs)) {
      stop(rastfn, " does not have defined projection.. specify with crs parameter")
    } else {
      s_srs <- sf::st_crs(crs)
      s_srs <- crs
    }
  }

  ## Check new projection
  if (is.null(crs.new)) {
    if (is.null(crs.default)) stop("need to define new crs")
    crs.new <- crs.default 
  }
  t_srs <- sf::st_crs(crs.new)
  t_srs <- crs.new

  ## Check dtype.new
  dtype <- srcinfo$datatype
  if (!is.null(dtype.new)) {
    dtypelst <- c("Byte", "Int16", "UInt16", "Uint32", "Int32", "Float32",
		"Float64", "CInt16", "CInt32", "CFloat32", "CFloat64")
    dtype <- pcheck.varchar(var2check=dtypelst, 
	varnm="dtypelst", gui=gui, checklst=dtypelst, caption="Data type?")
    if (dtype != dtype.new)
      message("changing data type from ", dtype, " to ", dtype.new)
    ot <- dtype.new
  } else {
    ot <- dtype
  }

  ## Check NODATA value
  NODATA <- srcinfo$nodata_value
  if (!is.null(NODATA.new)) {
    if (!is.numeric(NODATA.new) || length(NODATA.new) > 1) 
      stop("invalid NODATA value... must be numeric")
    if (NODATA.new != NODATA)
      message("changing NODATA value from ", NODATA, " to ", NODATA.new)
    dstnodata <- NODATA.new
  } else {
    dstnodata <- NODATA
  }
 
  ## Check resamp.method
  resamp.methodlst <- c("near", "bilinear", "cubic", "cubicspline", 
	      "lanczos", "average", "mode", "min", "max", "med", "q1", "q3")
  r <- pcheck.varchar(var2check=resamp.method, 
	varnm="resamp.methodlst", gui=gui,
	checklst=resamp.methodlst, caption="Resample method?")


  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)
  
  ## Check outfn
  if (is.null(outfn)) {
    outfn <- paste0(rastnm, "_prj")
  }

  ## Check outext and outfilenm
  if (is.null(outext)) {
    outext <- getext(outfn)
    if (is.na(outext) || outext == "") {
      outext <- "tif"
    }
  }

  outext.tmp <- unlist(strsplit(outext, "\\."))
  if (length(outext.tmp) > 1) {
    outext <- outext.tmp[length(outext.tmp)]   
    if (!outext %in% drivers[["DefaultExt"]]) stop("outext is invalid") 
  }
  outfilenm <- getoutfn(outfn, outfolder=outfolder, outfn.pre=NULL, ext=outext,
	overwrite=overwrite)

  ## Get output raster format
  of <- drivers[drivers$DefaultExt == outext, "fmt"]
  
  ## Check compression
  compresslst <- c("LZW", "PACKBITS", "DEFLATE")
  compress <- pcheck.varchar(var2check=compress, 
                             varnm="compress", gui=gui,
                             checklst=compresslst, caption="Compress output?")
  if (!is.null(compress)) {
    co <- paste0("COMPRESS=", compress)
  }
  
  if (outext == "tif" && !is.null(co)) {
    ## Check BigTIFF
    BigTIFF <- pcheck.logical(BigTIFF, 
                            varnm="BigTIFF", 
                            title="BigTIFF compression?", 
                            first="NO", gui=gui)
    co <- c(co, "BIGTIFF=YES")
  }
  

  ##################################################################
  ## DO WORK
  ##################################################################

  reprojectRaster(srcfile = srcfile, 
                  dstfile = outfilenm, 
                  t_srs = t_srs, 
                  s_srs = s_srs, 
                  tr = tr, 
                  of = of, 
                  ot = ot, 
                  r = r, 
                  co = co, 
                  dstnodata = dstnodata[1], 
                  addOptions = c("-ovr", "NONE"))

  return(outfilenm)
 
}
