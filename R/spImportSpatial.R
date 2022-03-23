#' Spatial - Imports a spatial vector layer to an S4 Spatial object.
#' 
#' Imports a spatial vector layer to an S4 Spatial object.
#' 
#' 
#' @param layer Data frame object or String. Name of spatial layer. Can be
#' layer with dsn, full pathname, including extension, or file name (with
#' extension) in xy_dsn folder.
#' @param dsn String. Name of database where layer is. The dsn varies by
#' driver. See gdal OGR vector formats (https://www.gdal.org/ogr_formats.html).
#' @param sql String. A sql syntax query to subset spatial layer.
#' @param polyfix Logical. If polyfix=TRUE, uses buffer with 0 width to clean
#' up polygons.
#' @param gui Logical. If TRUE, search for layer within dsn.
#' @return A spatial object
#' @note Wrapper for sf package... st_read function.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' # Import data from `FIESTA`, save as object in environment
#' WYbh <- spImportSpatial(system.file("extdata",
#'                                     "sp_data/WYbighorn_adminbnd.shp",
#'                                     package = "FIESTA"))
#' @export spImportSpatial
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


