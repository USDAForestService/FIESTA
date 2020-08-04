spUnionPoly <- function(polyv1, polyv1_dsn=NULL, polyv2, polyv2_dsn=NULL, 
	showext=FALSE, areacalc=FALSE, areavar="ACRES_GIS", exportsp=FALSE, ...) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Generates one polygon from two SpatialPolygonsDataFrame objects, including
  ## features and attributes from both polygons.
  #####################################################################################

  if (!"sf" %in% rownames(installed.packages()))
    stop("spUnionPoly function requires package sf")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  polyv1x <- pcheck.spatial(layer=polyv1, dsn=polyv1_dsn, gui=gui, 
		caption="Polygon1?")
  polyv2x <- pcheck.spatial(layer=polyv2, dsn=polyv2_dsn, gui=gui, 
		caption="Polygon2?")

  ## Check areacalc
  areacalc <- FIESTA::pcheck.logical(areacalc, "Calculate area?", "YES")

  ## Check exportsp
  exportsp <- FIESTA::pcheck.logical(exportsp, "Export to shapefile?", "YES")


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (exportsp) {
    overwrite <- FIESTA::pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- FIESTA::pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui)
  }


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projection
  prjdat <- crsCompare(polyv2x, polyv1x, nolonglat=TRUE)
  polyv2x <- prjdat$x
  polyv1x <- prjdat$ycrs

  ## Check extents
#  msg <- FIESTA::check.extents(polyv1x, polyv2prj, showext, layer1nm="polyv1x", 
#		layer2nm="polyv2x")
#  if (msg == "non-overlapping extents") stop("msg")

  ## Union polygons
  upoly <- layerUnion(polyv1x, polyv2x)
  if (showext) plot(sf::st_geometry(upoly, add=TRUE))

   
  ## Calculate area
  if (areacalc) 
    upoly <- areacalc.poly(upoly, areavar=areavar)


  ## Output shapefile to outfolder
  if (exportsp) 
    spExportSpatial(upoly, ...)

  return(upoly)

}

