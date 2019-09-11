spClipRast <- function(polyv_layer, polyv_dsn=NULL, rast, rastfolder=NULL, 
	rast.prjstr=NULL, bands=NULL, NODATA=NULL, buffdist=NULL, maskByPolygons=TRUE, 
	showext=FALSE, setNODATA=FALSE, fmt="HFA", outfolder=NULL, outfn=NULL, 
	overwrite=FALSE) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Clips, or intersects a polygon vector with another polygon vector with option 
  ## to export to an ArcGIS shapefile.
  #####################################################################################

  if (!"rgeos" %in% rownames(installed.packages()))
    stop("spClipRast function requires package rgeos")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){poly=clippoly=unionpoly=savedata <- NULL}

  drivers <- data.frame(
	fmt = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", "EHdr", "HFA"),
	DefaultExt = c(".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"),
	stringsAsFactors=FALSE
  )	


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Get poly and clippoly layers
  polyvx <- FIESTA::pcheck.spatial(layer=polyv_layer, dsn=polyv_dsn, gui=gui, 
	caption="Poly to clip?")

  ## Verify raster
  rastfn <- FIESTA::getrastlst(rast, rastfolder, filenames=TRUE)

  ## Get names of raster
  rastnm <- FIESTA::basename.NoExt(rastfn)


  ## Import raster
  rast_info <- suppressWarnings(rgdal::GDALinfo(rastfn))
  rast.fmt <- attr(rast_info, "driver")
  rast.df <- attr(rast_info, "df")
  nbands <- rast_info[["bands"]]

  rastx <- raster(rastfn)
  rastdatatype <- raster::dataType(rastx)
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
    if (all(rast.df$hasNoDataValue) && length(unique(rast.df$NoDataValue)) == 1) {
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
      if (rastdatatype %in% c("INT1U", "INT2U", "INT4U") && NODATA < 0) {
        message("Cannot have a negative NODATA value for raster of datatype: ", 
			rastdatatype, ", using 0")
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
  setNODATA <- FIESTA::pcheck.logical(setNODATA, varnm="setNODATA", title="Set NODATA?", 
		first="NO", gui=gui)

  ## Check fmt
  fmt <- FIESTA::pcheck.varchar(var2check=fmt, varnm="fmt", gui=gui, 
		checklst=drivers$fmt, caption="Export format")
  if (is.null(fmt)) {
    message("no format specified... using format of input rast")
    fmt <- rast.fmt
  }
  fmt.ext <- drivers[match(fmt, drivers$fmt), "DefaultExt"]


  ## Check outfolder
  outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

  ## Check outfn
  if (is.null(outfn)) outfn <- "rastclip"
  outfn <- paste0(outfn, fmt.ext)

  if (!overwrite) {
    ext <- raster::extension(outfn)
    outfn <- FIESTA::fileexistsnm(outfolder, basename.NoExt(outfn), strsplit(ext, "\\.")[[1]][2])
  }
  if (!is.null(outfolder)) {
    outfn <- paste(outfolder, outfn, sep="/")
    if (!is.null(ext)) outfn <- paste0(outfn, ext)
  } 



  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projections of polygons 
  prjdat <- FIESTA::CRScompare(rastx, polyvx, nolonglat=TRUE)
  rastx <- prjdat$layer1
  polyvprj <- prjdat$layer2


  if (!is.null(buffdist))
    ## This will buffer the polygon 1 pixel to include all pixels inside boundary
    polyvprj <- rgeos::gBuffer(polyvprj, width=buffdist)

  ## Check extents
  msg <- FIESTA::check.extents(polyvprj, rastx, showext, layer1nm="polyv", 
		layer2nm="rastx")
  if (msg == "non-overlapping extents") stop("msg")


  FIESTA::clipRaster(src=polyvprj, srcfile=rastfn, src_band=bands, dstfile=outfn, 
		fmt=fmt, init=NODATA, maskByPolygons=maskByPolygons)
  rastclip <- raster(outfn)

  GDALinfo(rastfn)
  dataType(raster(rastfn))
  dataType(rastclip)
  rastclip@file@nodatavalue

  cat(
  " #################################################################################", 
  "\n", paste("Clip file written to: "), "\n", outfn, "\n", 
  "#################################################################################",
  "\n" )

  return(rastclip)    
}
