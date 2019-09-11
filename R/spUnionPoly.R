spUnionPoly <- function(poly1_dsn, poly1_layer, poly2_dsn, poly2_layer, 
	clip=FALSE, showext=FALSE, areacalc=FALSE, exportshp=FALSE, outfolder=NULL, 
	outshpnm=NULL, overwrite=FALSE){

  #####################################################################################
  ## DESCRIPTION: 
  ## Generates one polygon from two SpatialPolygonsDataFrame objects, including
  ## features and attributes from both polygons.
  #####################################################################################

  if (!"rgeos" %in% rownames(installed.packages()))
    stop("spUnionPoly function requires package rgeos")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  polyv1x <- FIESTA::pcheck.spatial(dsn=poly1_dsn, layer=poly1_layer, gui=gui, 
		caption="Polygon1?")
  polyv2x <- FIESTA::pcheck.spatial(dsn=poly2_dsn, layer=poly2_layer, gui=gui, 
		caption="Polygon2?")

  ## Check clip
  clip <- FIESTA::pcheck.logical(clip, "Clip polygons?", "YES")

  ## Check areacalc
  areacalc <- FIESTA::pcheck.logical(areacalc, "Calculate area?", "YES")


  ## Check exportshp
  exportshp <- FIESTA::pcheck.logical(exportshp, "Export to shapefile?", "YES")

  ## Check outfolder
  if (exportshp) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)

    ## Check outfn
    if (is.null(outshpnm)) {
      outshpnm <- "polyunion"
    } else {
      if (raster::extension(outshpnm) == ".shp")
        outshpnm <- substr(outshpnm, nchar(outshpnm)-3, nchar(outshpnm))
    }
  }

  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projection
  prjdat <- CRScompare(polyv1x, polyv2x, nolonglat=TRUE)
  polyv1x <- prjdat$layer1
  polyv2prj <- prjdat$layer2

  ## Check extents
  msg <- FIESTA::check.extents(polyv1x, polyv2prj, showext, layer1nm="polyv1x", 
		layer2nm="polyv2x")
  if (msg == "non-overlapping extents") stop("msg")


  ## Union polygons
  upoly <- raster::union(polyv1x, polyv2prj)
  if (showext) sp::plot(upoly, add=TRUE)

  ## Clip polygons
  if (clip) 
    upoly <- raster::crop(upoly, polyv2prj)
   
  ## Calculate area
  if (areacalc) 
    upoly <- FIESTA::areacalc.poly(upoly)


  if (exportshp) 
    ## Output shapefile to outfolder
    FIESTA::spExportShape(upoly, outshpnm=outshpnm, outfolder=outfolder, overwrite=overwrite)

  return(upoly)

}

