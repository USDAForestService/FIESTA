#' Spatial - Generate a unioned sf object with polygons and attribtutes from
#' two sf polygon objects.
#' 
#' Generate a unioned sf object with polygons and attribtutes from two sf
#' polygon objects.
#' 
#' *If variable = NULL, then it will prompt user for input.
#' 
#' Uses raster function union to merge two polygons and crop, if clip=TRUE.
#' Generates a new ID for each polygon and appends attributes from both
#' polygons.
#' 
#' areacalc\cr If areacalc = TRUE and the unioned spatial object is not in a
#' projected coordinate system (i.e., longlat), the object will be reprojected
#' to the Albers Equal Area projection before area is calculated.
#' 
#' @param polyv1 sf R object or String. Polygon data to union. Can be a spatial
#' polygon object, full pathname to a shapefile, or name of a layer within a
#' database.
#' @param polyv1_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of layer to union. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if polyv1 is sf
#' object.
#' @param polyv2 sf R object or String. Polygon data to union. Can be a spatial
#' polygon object, full pathname to a shapefile, or name of a layer within a
#' database.
#' @param polyv2_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of layer to union. The dsn varies by driver. See gdal OGR vector
#' formats (https://www.gdal.org/ogr_formats.html). Optional if polyv2 is sf
#' object.
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param areacalc Logical. If TRUE, calculate area of unioned polygons and
#' append to attribute table (See details).
#' @param areavar String. Name of area variable.
#' @param exportsp Logical. If TRUE, the spatial unioned object is exported to
#' outfolder (see spExportSpatial for details).
#' @param ...  Parameters for spExportSpatial.
#' @return sf object of unioned polygon. If polyv1 and polyv2 have different
#' projections, the projection of returned object will have the same projection
#' as poly1 (See note about on-the-fly projection conversion).
#' 
#' If exportsp=TRUE, the sf object will be written to outfolder (See note).
#' @note
#' 
#' On-the-fly projection conversion\cr The spTransform (rgdal) method is used
#' for on-the-fly map projection conversion and datum transformation using
#' PROJ.4 arguments. Datum transformation only occurs if the +datum tag is
#' present in the both the from and to PROJ.4 strings. The +towgs84 tag is used
#' when no datum transformation is needed. PROJ.4 transformations assume NAD83
#' and WGS84 are identical unless other transformation parameters are
#' specified.  Be aware, providing inaccurate or incomplete CRS information may
#' lead to erroneous data shifts when reprojecting. See spTransform help
#' documentation for more details.
#' 
#' ESRI Shapefile Driver\cr If exportsp=TRUE:\cr The writeOGR (rgdal) function
#' is called. If out_fmt="shp", the ESRI Shapefile driver truncates variable
#' names to 10 characters or less. Variable names are changed before export
#' using an internal function (trunc10shp). If sf object has more than 1
#' record, it will be returned but not exported.
#' @author Tracey S. Frescino
#' @keywords data
#' @export spUnionPoly
spUnionPoly <- function(polyv1, polyv1_dsn=NULL, polyv2, polyv2_dsn=NULL, 
	showext=FALSE, areacalc=FALSE, areavar="ACRES_GIS", exportsp=FALSE, ...) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Generates one polygon from two SpatialPolygonsDataFrame objects, including
  ## features and attributes from both polygons.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  polyv1x <- pcheck.spatial(layer=polyv1, dsn=polyv1_dsn, gui=gui, 
		caption="Polygon1?")
  polyv2x <- pcheck.spatial(layer=polyv2, dsn=polyv2_dsn, gui=gui, 
		caption="Polygon2?")

  ## Check areacalc
  areacalc <- pcheck.logical(areacalc, "Calculate area?", "YES")

  ## Check exportsp
  exportsp <- pcheck.logical(exportsp, "Export to shapefile?", "YES")


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (exportsp) {
    overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
		title="Overwrite files?", first="NO", gui=gui)  
    outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
		title="Add date to outfiles?", first="YES", gui=gui)  
    outfolder <- pcheck.outfolder(outfolder, gui)
  }


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projection
  prjdat <- crsCompare(polyv2x, polyv1x, nolonglat=TRUE)
  polyv2x <- prjdat$x
  polyv1x <- prjdat$ycrs

  ## Check extents
#  msg <- check.extents(polyv1x, polyv2prj, showext, layer1nm="polyv1x", 
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

