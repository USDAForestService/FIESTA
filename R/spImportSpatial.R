spImportSpatial <- function(layer, dsn=NULL){
  ###################################################################################
  ## PURPOSE: Import a shapefile.  
  ##
  ## OUTPUTS:
  ## spobj  Spatial object
  ####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  spobj <- pcheck.spatial(layer=layer, dsn=dsn)

  return(spobj)
}



