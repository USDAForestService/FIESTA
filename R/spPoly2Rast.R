spPoly2Rast <- function(polyv, polyv_dsn=NULL, polyv.att, polyv.lut=NULL, 
	rastfn.template=NULL, outfolder=NULL, outfn="polyrast", outext="img", 
	outfn.pre=NULL, outfn.date=TRUE, overwrite=FALSE) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Clips, or intersects a polygon vector with another polygon vector with option 
  ## to export to an ArcGIS shapefile.
  #####################################################################################

  if (!"rgdal" %in% rownames(installed.packages()))
    stop("spPoly2Rast function requires package rgdal")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){poly=clippoly=unionpoly=savedata <- NULL}

  drivers <- data.frame(
	fmt = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", 
		"EHdr", "HFA", "VRT"),
	DefaultExt = c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", 
		"bil", "img", "vrt"),
	stringsAsFactors=FALSE
  )	

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Get poly and clippoly layers
  polyvx <- pcheck.spatial(layer=polyv, dsn=polyv_dsn, gui=gui, caption="Poly to clip?")

  ## Check polyv.att
  polyv.att <- FIESTA::pcheck.varchar(var2check=polyv.att, varnm="polyv.att", gui=gui, 
		checklst=names(polyvx), caption="Polygon attribute to rasterize", 
		warn=paste(polyv.att, "not in polyvx"), stopifnull=TRUE)


  if (is.character(polyvx[[polyv.att]])) {
    if (is.null(polyv.lut)) {
      message("creating lookup table of codes")

      NAME <- sort(unique(polyvx[[polyv.att]]))
      CODE <- seq(1:length(NAME))
      polyv.lut <- data.frame(NAME, CODE, stringsAsFactors=FALSE)
      names(polyv.lut) <- c(polyv.att, "CODE")
    }
  }
  if (!is.null(polyv.lut)) {
    if (!polyv.att %in% names(polyv.lut)) 
      stop(polyv.att, " must be in polyv.lut") 
    polyvx <- merge(polyvx, polyv.lut, by.x=polyv.att)
    polyv.att <- names(polyv.lut)[names(polyv.lut) != polyv.att]
  }
    
  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
  outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
  outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

  ## Check outext and outfilenm
  outext.tmp <- unlist(strsplit(outext, "\\."))
  if (length(outext.tmp) > 1) {
    outext <- outext.tmp[length(outext.tmp)]   
    if (!outext %in% drivers[["DefaultExt"]]) stop("outext is invalid") 
  }
  outfilenm <- getoutfn(outfn, outfolder=outfolder, outfn.pre=outfn.pre, 
		outfn.date=outfn.date, overwrite=overwrite, ext=outext)


  if (!is.null(rastfn.template)) {

    ## verify raster
    rastfn <- getrastlst.rgdal(rastfn.template)

    ## get format from raster
    rast_info2 <- suppressWarnings(rgdal::GDALinfo(rastfn))
    rast.fmt <- attr(rast_info2, "driver")

    rast_info <- rasterInfo(rastfn)
    rast.prj <- rast_info$crs
    nbands <- rast_info$nbands

    ## Check if projections match
    polyvx <- crsCompare(polyvx, rast.prj, nolonglat=TRUE)$x


    ## Create virtual raster by clipping raster template to extent of polyvx
    rastclip <- spClipRast(rast=rastfn, clippolyv=polyvx, maskByPolygons=FALSE, 
			outfolder=outfolder, outfn="tmp", fmt="VRT")

    ## Create blank raster from clipped virtual raster
    rast.fmt <- drivers[drivers$DefaultExt == outext, "fmt"]
    rast <- rasterFromRaster(rastclip, fmt=rast.fmt, dstfile=outfilenm)

    ## Rasterize polygons
    polyrast <- rasterizePolygons(src=polyvx, burn_value=polyv.att, rasterfile=rast)

  } else {

    ## Project if longlat coordinate system
    polyvx <- checksf.longlat(polyvx)

    ## Create blank raster from polyvx
    rast <- rasterFromVectorExtent(polyvx, res=30, dstfile=outfilenm, fmt="HFA")

    ## Rasterize polygons
    polyrast <- rasterizePolygons(src=polyvx, burn_value=polyv.att, rasterfile=rast)

  }

  returnlst <- list(rastfn=outfilenm)
  if (!is.null(polyv.lut))
    returnlst$polyv.lut <- polyv.lut
  returnlst$polyv.att <- polyv.att

  return(returnlst)    
}

