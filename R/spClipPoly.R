spClipPoly <- function(polyv_layer=NULL, polyv_dsn=NULL, clippolyv_layer, clippolyv_dsn=NULL, 
	showext=FALSE, areacalc=FALSE, exportshp=FALSE, outfolder=NULL, outshpnm=NULL, 
	outfn.date=TRUE, overwrite=FALSE) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Clips, or intersects a polygon vector with another polygon vector with option 
  ## to export to an ArcGIS shapefile.
  #####################################################################################

  if (!"rgeos" %in% rownames(installed.packages()))
    stop("spClipPoly function requires package rgeos")


  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){poly=clippoly=unionpoly=savedata <- NULL}


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Get poly and clippoly layers
  polyvx <- FIESTA::pcheck.spatial(layer=polyv_layer, dsn=polyv_dsn, gui=gui, 
	caption="Poly to clip?")

  clippolyvx <- FIESTA::pcheck.spatial(layer=clippolyv_layer, dsn=clippolyv_dsn, 
	gui=gui, caption="Clipping poly?")

  ## Check areacalc
  areacalc <- FIESTA::pcheck.logical(areacalc, "Calculate area?", "YES")

  ## Check exportshp
  exportshp <- FIESTA::pcheck.logical(exportshp, "Export to shapefile?", "YES")


  ## Check outfolder
  if (exportshp) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    ## Check outfn
    if (is.null(outshpnm)) {
      outshpnm <- "polyclip"
    } else {
      if (raster::extension(outshpnm) == ".shp")
        outshpnm <- substr(outshpnm, nchar(outshpnm)-3, nchar(outshpnm))
    }
  }


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projections of polygons 
  prjdat <- FIESTA::CRScompare(polyvx, clippolyvx, nolonglat=TRUE)
  polyvx <- prjdat$layer1
  clippolyprj <- prjdat$layer2


  ## Check extents
  msg <- FIESTA::check.extents(polyvx, clippolyprj, showext, layer1nm="polyv", 
		layer2nm="clippolyv")
  if (msg == "non-overlapping extents") stop("msg")

  ## Clip poly
  ipoly <- raster::crop(polyvx, clippolyprj)


  ## Calculate area
  if (areacalc) 
    ipoly <- FIESTA::areacalc.poly(ipoly)


  if (exportshp) 
    ## Output shapefile to outfolder
    FIESTA::spExportShape(ipoly, outshpnm=outshpnm, outfolder=outfolder, 
		outfn.date=outfn.date, overwrite=overwrite)
  

  return(ipoly)    
}
