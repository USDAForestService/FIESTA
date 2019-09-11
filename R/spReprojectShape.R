spReprojectShape <- function(splayer, splayer_dsn=NULL, EPSGCD.new=NULL, 
	prj4str.new=NULL, prj.new=NULL, datum.new=NULL, zone.new=NULL, 
	zoneS.new=FALSE, aea.param.new="USGS", exportshp=FALSE, ...){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if (gui) savedata <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ### Shapefile  
  splayerx <- pcheck.spatial(layer=splayer, dsn=splayer_dsn, 
	gui=gui, caption="Spatial Object?")


  ## check EPSGCD.new
  if (!is.null(EPSGCD.new)) {
    EPSG <- rgdal::make_EPSG()
    if (!EPSGCD.new %in% EPSG[["code"]]) stop("EPSGCD.new is invalid")

    prj4str <- na.omit(EPSG[EPSG[["code"]] == EPSGCD.new, "prj4"])[[1]]
  }

  ## Check prj4str.new
  if (is.null(prj4str.new)) {

    ## Check datum
    if (is.null(datum.new)) 
      datum.new <- FIESTA::getprjatt(sp::proj4string(splayerx), "datum", stopifnull=TRUE)

    prj4str.new <- FIESTA::build.prj4str(prj=prj.new, datum=datum.new, 
		zone=zone.new, zoneS=zoneS.new, aea.param=aea.param.new, gui=gui)
  } else {
    #if (!grepl("+proj", prj4str.new)) stop("prj4str.new missing +proj")

    if (!rgdal::checkCRSArgs(prj4str.new)[[1]]) stop("invalid prj4str.new")
    if (!grepl("+datum", prj4str.new)) warning("prj4str.new missing +datum")
  }

  ## Check savedata 
  exportshp <- FIESTA::pcheck.logical(exportshp, varnm="exportshp", 
	title="Export shape?", first="YES", gui=gui)


  ##################################################################
  ## DO WORK
  ##################################################################

  splayerxprj <- sp::spTransform(splayerx, CRS(prj4str.new))

  
  ## Output shapefile to outfolder
  if (exportshp)
    FIESTA::spExportShape(splayerxprj, ...)

  return(splayerxprj)

}
