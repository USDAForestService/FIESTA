spClipPoly <- function(polyv, polyv_dsn=NULL, clippolyv, clippolyv_dsn=NULL, 
	clippolyv.filter=NULL, showext=FALSE, areacalc=FALSE, areaunits="ACRES",
 	exportsp=FALSE, ...) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Clip (intersect) a polygon vector layer with another polygon vector layer. 
  ## Arguments:
  ## areaunits - area calculation units ("ACRES", "HECTARES", "SQKM")
  #####################################################################################
  if (!"sf" %in% rownames(installed.packages()))
    stop("spClipPoint function requires package sf")

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if(gui){polyv=clippolyv=exportsp=areacalc <- NULL}


  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Get poly and clippoly layers
  polyvx <- pcheck.spatial(layer=polyv, dsn=polyv_dsn, gui=gui, 
	caption="Poly to clip?")

  clippolyvx <- pcheck.spatial(layer=clippolyv, dsn=clippolyv_dsn, 
	gui=gui, caption="Clipping poly?")

  ## clippolyv.filter
  clippolyvx <- datFilter(clippolyvx, xfilter=clippolyv.filter)$xf


  ## Check areacalc
  areacalc <- FIESTA::pcheck.logical(areacalc, "Calculate area?", "YES")

  ## Check exportsp
  exportsp <- FIESTA::pcheck.logical(exportsp, "Export sf?", "YES")


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projections of polygons 
  prjdat <- crsCompare(clippolyvx, polyvx, nolonglat=TRUE)
  clippolyvx <- prjdat$x
  polyvx <- prjdat$ycrs


  ## Check extents
  bbox1 <- sf::st_bbox(clippolyvx)
  bbox2 <- sf::st_bbox(polyvx)
  check.extents(bbox1, bbox2, showext, layer1nm="polyv", layer2nm="clippoly",
			stopifnotin=TRUE)

  ## Clip poly
  ipoly <- suppressWarnings(sf::st_intersection(polyvx, sf::st_union(clippolyvx)))


  ## Calculate area
  if (areacalc) 
    ipoly <- areacalc.poly(ipoly, unit=areaunits)

  ## Export clipped poly
  if (exportsp) 
    spExportSpatial(ipoly, ...)
  

  return(ipoly)    
}
