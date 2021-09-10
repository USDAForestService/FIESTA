spImportSpatial <- function(layer=NULL, dsn=NULL, sql=NULL, polyfix=FALSE, gui=FALSE){
  ###################################################################################
  ## PURPOSE: Import a spatial layer (e.g., ESRI shapefile, feature layer).  
  ##
  ## OUTPUTS:
  ## spobj  simple feature (sf) object
  ####################################################################################

  if (is.null(sql)) {
    sql <- NA
  }

  ## Check sql
  spobj <- tryCatch(pcheck.spatial(dsn=dsn, layer=layer, 
			polyfix=polyfix, sql=sql, gui=gui),
     	 error=function(e) {
			message("invalid spatial layer\n")
			return(NULL) })

  return(spobj)
}


