spClipRast <- function(rast, rastfolder=NULL, clippolyv, clippolyv_dsn=NULL, 
	clippolyv.filter=NULL, rast.crs=NULL, bands=NULL, NODATA=NULL, buffdist=NULL,
 	maskByPolygons=TRUE, showext=FALSE, fmt="HFA", outfolder=NULL,
 	outfn="rastclip", outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Clips a raster with a polygon.
  #####################################################################################

  if (!"rgdal" %in% rownames(installed.packages()))
    stop("spClipRast function requires package rgdal")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(FIESTA::spClipRast)) 
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
  rastfn <- getrastlst.rgdal(rast, rastfolder, gui=gui)

  ## Get names of raster
  rastnm <- FIESTA::basename.NoExt(rastfn)

  ## Import raster
  rast_info2 <- suppressWarnings(rgdal::GDALinfo(rastfn))

  rast_info <- rasterInfo(rastfn)
  rast.fmt <- attr(rast_info2, "driver")
  rast.df <- attr(rast_info2, "df")
  rast.prj <- rast_info$crs
  nbands <- rast_info$nbands
  nodata <- rast_info$nodata_value
  rast.dtyp <- rast_info$datatype
  rast.bbox <- rast_info$bbox

  ## Check if rast has a defined projection 
  if (is.null(rast.prj)) {
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
    if (!is.null(nodata) && length(unique(nodata)) == 1) {
      NODATA <- unique(rast.df$NoDataValue)
    } else {
      NODATA <- 0
    } 
  } else {
    if (!is.na(NODATA) && NODATA != 0) {
      if (!is.numeric(NODATA)) {
        message("invalid NODATA value... using 0")
        NODATA <- 0
      }
      if (rast.dtyp %in% c("INT1U", "INT2U", "INT4U") && NODATA < 0) {
        message("Cannot have a negative NODATA value for raster of datatype: ", 
			rast.dtyp, ", using 0")
        NODATA <- 0
      }
    }
  }

  ## Check buffdist
  if (!is.null(buffdist)) 
    if (!is.numeric(buffdist)) stop("invalid buffdist... must be numeric")
  
  ## Check maskByPolygons
  maskByPolygons <- FIESTA::pcheck.logical(maskByPolygons, varnm="maskByPolygons", 
		title="Mask by polygon?", first="NO", gui=gui)

  ## Check showext
  showext <- FIESTA::pcheck.logical(showext, varnm="showext", title="Show Extents?", 
		first="NO", gui=gui)

  ## Check setNODATA
#  setNODATA <- FIESTA::pcheck.logical(setNODATA, varnm="setNODATA", title="Set NODATA?", 
#		first="NO", gui=gui)

  ## Check fmt
  fmt <- FIESTA::pcheck.varchar(var2check=fmt, varnm="fmt", gui=gui, 
		checklst=drivers$fmt, caption="Export format")
  if (is.null(fmt)) {
    message("no format specified... using format of input rast")
    fmt <- rast.fmt
  }
  fmt.ext <- drivers[match(fmt, drivers$fmt), "DefaultExt"]


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
  outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
  outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)


  outfilenm <- getoutfn(outfn=outfn, outfolder=outfolder, outfn.pre=outfn.pre, 
	outfn.date=outfn.date, overwrite=overwrite, ext=fmt.ext)


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projections of polygons 
  clippolyvprj <- crsCompare(clippolyvx, rast.prj, crs.default=rast.crs, nolonglat=TRUE)$x

  if (!is.null(buffdist))
    ## This will buffer the polygon 1 pixel to include all pixels inside boundary
    clippolyvprj <- sf::st_buffer(clippolyvprj, width=buffdist)

  ## Check extents
  names(rast.bbox) <- c("xmin", "ymin", "xmax", "ymax")
  bbox1 <- sf::st_bbox(clippolyvprj)
  bbox2 <- sf::st_bbox(rast.bbox, crs=rast.prj)
  check.extents(bbox1, bbox2, showext, layer1nm="clippolyv", layer2nm="rast",
			stopifnotin=TRUE)


  ## Clip raster
  clipRaster(src=clippolyvprj, srcfile=rastfn, src_band=bands, dstfile=outfilenm, 
		fmt=fmt, init=NODATA, maskByPolygons=maskByPolygons)


  return(outfilenm)    
}
