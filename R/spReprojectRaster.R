spReprojectRaster <- function(rastfn, bands=NULL, crs=NULL, crs.new=NULL,
    	res.new=NULL, bbox.new=NULL, dtype.new=NULL, NODATA.new=NULL,
 	resamp.method="near", crs.default="EPSG:5070", outfolder=NULL, 
	outfn=NULL, outext="img") {

  ##################################################################################
  ## DESCRIPTION: reproject raster. If crs.new is not defined, uses crs.default. 	
  ## Default projection: NAD83 - Conus Albers (EPSG:5070)
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
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
    dtype <- FIESTA::pcheck.varchar(var2check=dtypelst, 
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
  r <- FIESTA::pcheck.varchar(var2check=resamp.method, 
	varnm="resamp.methodlst", gui=gui,
	checklst=resamp.methodlst, caption="Resample method?")


  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)
  
  ## Check outfn
  if (is.null(outfn)) 
    outfn <- paste0(rastnm, "_prj")

  ## Check outext and outfilenm
  outext.tmp <- unlist(strsplit(outext, "\\."))
  if (length(outext.tmp) > 1) {
    outext <- outext.tmp[length(outext.tmp)]   
    if (!outext %in% drivers[["DefaultExt"]]) stop("outext is invalid") 
  }
  outfilenm <- getoutfn(outfn, outfolder=outfolder, outfn.pre=NULL, ext=outext)

  ## Get output raster format
  of <- drivers[drivers$DefaultExt == outext, "fmt"]


  ##################################################################
  ## DO WORK
  ##################################################################

  rastfn.new <- FIESTA::reprojectRaster(srcfile=srcfile, dstfile=outfilenm, 
	t_srs=t_srs, s_srs=s_srs, of=of, ot=ot, r=r, dstnodata=dstnodata)

  
  return(rastfn.new)

}
