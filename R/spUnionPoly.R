#' Spatial - Generate a unioned sf object with polygons and attributes from
#' two sf polygon objects.
#' 
#' Generate a unioned sf object with polygons and attributes from two sf
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
#' @param validate Logical. If TRUE, validates polyv and clippolyv before 
#' clipping. Uses sf::st_make_valid with default parameters 
#' (geos_method='valid_structure', geos_keep_collapsed=FALSE).
#' @param showext Logical. If TRUE, layer extents are displayed in plot window.
#' @param areacalc Logical. If TRUE, calculate area of unioned polygons and
#' append to attribute table (See details).
#' @param areavar String. Name of area variable.
#' @param exportsp Logical. If TRUE, the spatial unioned object is exported to
#' outfolder (see spExportSpatial for details).
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when export = TRUE. If out_layer = NULL,
#' default = 'polyunion'.
#' @param ... For extendibility.
#' 
#' @return sf object of unioned polygon. If polyv1 and polyv2 have different
#' projections, the projection of returned object will have the same projection
#' as poly1 (See note about on-the-fly projection conversion).
#' 
#' If exportsp=TRUE, the sf object will be written to outfolder (See note).
#' @note
#' 
#' On-the-fly projection conversion\cr The spTransform (sf) method is used
#' for on-the-fly map projection conversion and datum transformation using
#' PROJ.4 arguments. Datum transformation only occurs if the +datum tag is
#' present in the both the from and to PROJ.4 strings. The +towgs84 tag is used
#' when no datum transformation is needed. PROJ.4 transformations assume NAD83
#' and WGS84 are identical unless other transformation parameters are
#' specified.  Be aware, providing inaccurate or incomplete CRS information may
#' lead to erroneous data shifts when reprojecting. See spTransform help
#' documentation for more details.
#' 
#' ESRI Shapefile Driver\cr If exportsp=TRUE:\cr The st_write (sf) function
#' is called. If out_fmt="shp", the ESRI Shapefile driver truncates variable
#' names to 10 characters or less. Variable names are changed before export
#' using an internal function (trunc10shp). If sf object has more than 1
#' record, it will be returned but not exported.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \dontrun{
#' # Set up data from `FIESTA` and `raster`
#' WYbhfn <- system.file("extdata",
#'                       "sp_data/WYbighorn_adminbnd.shp",
#'                       package = "FIESTA")
#' WYbh <- spImportSpatial(WYbhfn)
#'
#' # Load in USAco data from geodata package
#' USAco <- geodata::gadm(country="United States", level=2, path=tempdir())    
#' 
#' # Generate unioned `sf` object
#' polyUnion <- spUnionPoly(polyv1 = USAco[USAco$NAME_1 == "Wyoming",], 
#'                          polyv2 = WYbh, 
#'                          areacalc = TRUE)
#'                          
#' # Plot the result
#' plot(sf::st_geometry(polyUnion))
#' }
#' @export spUnionPoly
spUnionPoly <- function(polyv1, 
                        polyv1_dsn = NULL, 
                        polyv2, 
                        polyv2_dsn = NULL, 
                        validate = FALSE,
                        showext = FALSE, 
                        areacalc = FALSE, 
                        areavar = "ACRES_GIS", 
                        exportsp = FALSE, 
                        savedata_opts = NULL,
                        ...) {

  #####################################################################################
  ## DESCRIPTION: 
  ## Generates one polygon from two SpatialPolygonsDataFrame objects, including
  ## features and attributes from both polygons.
  #####################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spUnionPoly))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  
  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ##################################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################################
  polyv1x <- pcheck.spatial(layer=polyv1, dsn=polyv1_dsn, gui=gui, 
		caption="Polygon1?")
  polyv2x <- pcheck.spatial(layer=polyv2, dsn=polyv2_dsn, gui=gui, 
		caption="Polygon2?")
  
  ## Check validate
  validate <- pcheck.logical(validate, "Validate polys?", "NO")
  
  ## Validate polygon
  if (validate) {
    polyv1x <- sf::st_make_valid(polyv1x, 
                                 geos_method = 'valid_structure', 
                                 geos_keep_collapsed = FALSE)
    polyv1x <- sf::st_cast(polyv1x)
    
    polyv2x <- sf::st_make_valid(polyv2x, 
                                 geos_method = 'valid_structure', 
                                 geos_keep_collapsed = FALSE)
    polyv2x <- sf::st_cast(polyv2x)
  }
  

  ## Check areacalc
  areacalc <- pcheck.logical(areacalc, "Calculate area?", "YES")

  ## Check exportsp
  exportsp <- pcheck.logical(exportsp, "Export to shapefile?", "YES")


  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (exportsp) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
          out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
          overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
          add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
    if (is.null(out_layer)) {
      out_layer <- "polyunion"
    }
  }


  ##################################################################
  ## DO WORK
  ##################################################################

  ## Check projection
  prjdat <- crsCompare(polyv2x, polyv1x, nolonglat=TRUE)
  polyv2x <- prjdat$x
  polyv1x <- prjdat$ycrs

  #st_agr(polyv1x) <- "constant"
  #st_agr(polyv2x) <- "constant"


  ## Check extents
#  msg <- check.extents(polyv1x, polyv2prj, showext, layer1nm="polyv1x", 
#		layer2nm="polyv2x")
#  if (msg == "non-overlapping extents") stop("msg")

  ## 
  ## Union polygons
  upoly <- layerUnion(polyv1x, polyv2x)
  if (showext) plot(sf::st_geometry(upoly, add=TRUE))

   
  ## Calculate area
  if (areacalc) {
    upoly <- areacalc.poly(upoly, areavar=areavar)
  }

  ## Output shapefile to outfolder
  if (exportsp) {
    spExportSpatial(upoly, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="spxyplt",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer, 
                            add_layer=TRUE))
  }
    

  return(upoly)

}

