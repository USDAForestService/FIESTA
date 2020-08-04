spReprojectVector <- function(layer, dsn=NULL, crs.new, exportsp=FALSE, ...){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if (gui) savedata <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Check spatial file  
  layerx <- pcheck.spatial(layer=layer, dsn=dsn, gui=gui, caption="Spatial Object?")


  ## Check savedata 
  exportsp <- FIESTA::pcheck.logical(exportsp, varnm="exportsp", 
	title="Export spatial layer?", first="YES", gui=gui)


  ##################################################################
  ## DO WORK
  ##################################################################

  layerxprj <- sf::st_transform(layerx, crs=crs.new)

  
  ## Output shapefile to outfolder
  if (exportsp)
    spExportSpatial(layerxprj, ...)

  return(layerxprj)

}
