#' Spatial - Reprojects an sf spatial object.
#' 
#' Reprojects an sf spatial object to a new coordinate reference system.
#' 
#' 
#' @param layer sf class R object or String. The spatial layer must have a
#' defined projection (test using sf::st_crs(layer)).
#' @param dsn String. Data source name (dsn; i.e., folder or database name) of
#' splayer. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if layer is an R object.
#' @param crs.new EPSG Integer or PROJ.4 String. New EPSG Geodetic Parameter
#' Dataset definition or gdal PROJ.4 string identifying the new coordinate
#' system (e.g., "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80
#' +towgs84=0,0,0").
#' @param exportsp Logical. If TRUE, the spatial reprojected object is exported
#' to outfolder (see spExportSpatial for details).
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options for saving data. If out_layer = NULL, default = 'layerprj'.
#'
#' @return \item{layerprj}{ sf object. Reprojected spatial layer. }
#' 
#' If exportsp = TRUE, a spatial layer is written to outfolder (See note).
#' @note
#' 
#' Coordinate Reference Systems (CRS)\cr An ellipse is an estimated model
#' describing the basic shape of the Earth and is the basis for all coordinate
#' systems. There are many ellipsoids designed for local (e.g., NAD27) or
#' global (e.g., WGS84, GRS80) use. The datum defines the reference position of
#' the coordinate axes associated with a specific ellipsoid. Specifying the
#' datum also defines the ellipsoid, whereas specifying the ellipsoid does not
#' provide information of the datum.
#' 
#' WGS84 vs NAD83 WGS84 and NAD83 datums are often used interchangeably, and
#' use very similar ellipsoids (WGS84 and GRS80, respectively), but have
#' different reference points. Slight tectonic shifts through time have caused
#' increased divergence between the two, with NAD83 datum intended to track
#' movements more consistently.
#' 
#' Common Datums and associated spheroid (ellipsoid):\cr NAD27 - North American
#' Datum of 1927 (Clarke 1866 spheroid)\cr NAD83 - North American Datum of 1983
#' (GRS 1980 spheroid)\cr WGS84 - World Geodetic System of 1984 (WGS 1984
#' spheroid)\cr
#' 
#' From R, use projInfo for list of different projections and datums.\cr >
#' projInfo(type="proj")\cr > projInfo(type="datum")\cr
#' 
#' Common EPSG Geodetic codes in U.S.\cr EPSG:4326 - Longitude/Latitude (WGS84)
#' - Common for global displays (used by Google Earth)\cr EPSG:4269 -
#' Longitude/Latitude (NAD83) - Common by U.S. Federal Agencies\cr
#' 
#' The sf::st_transform (GDAL) method is used for map projection conversion and
#' datum transformation using PROJ.4 arguments. Datum transformation only
#' occurs if the +datum tag is present in the both the from and to PROJ.4
#' strings. The +towgs84 tag is used when no datum transformation is needed.
#' PROJ.4 transformations assume NAD83 and WGS84 are identical unless other
#' transformation parameters are specified. Be aware, providing inaccurate or
#' incomplete CRS information may lead to erroneous data shifts when
#' reprojecting. See spTransform help documentation for more details.
#' 
#' ESRI Shapefile Driver\cr If exportsp=TRUE:\cr The writeOGR (rgdal) function
#' is called. If out_fmt="shp", the ESRI Shapefile driver truncates variable
#' names to 10 characters or less. Variable names are changed before export
#' using an internal function (trunc10shp). If sf object has more than 1
#' record, it will be returned but not exported.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' \donttest{
#' # Set up `SpatialPoints` object
#' WYspplt <- spMakeSpatialPoints(xyplt = WYplt, 
#'                                xy.uniqueid = "CN", 
#'                                xvar = "LON_PUBLIC", 
#'                                yvar = "LAT_PUBLIC", 
#'                                prj = "longlat", 
#'                                datum = "NAD83")
#' # Check CRS
#' sf::st_crs(WYspplt)
#' 
#' # Set up projection
#' prj <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#' 
#' # Use `spReprojectVector` to reproject the vector
#' WYspplt.utm12 <- spReprojectVector(layer = WYspplt, 
#'                                    crs.new = prj)
#' # Check results
#' sf::st_crs(WYspplt.utm12)
#' }
#' @export spReprojectVector
spReprojectVector <- function(layer, 
                              dsn = NULL, 
                              crs.new, 
                              exportsp = FALSE, 
                              savedata_opts = NULL){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## If gui.. set variables to NULL
  if (gui) savedata <- NULL

  ## Default projection: NAD83 - Conus Albers
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ## EPSG:5070 NAD83/Conus Albers

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(spReprojectVector)) 
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
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  ## Check spatial file  
  layerx <- pcheck.spatial(layer=layer, dsn=dsn, gui=gui, caption="Spatial Object?")


  ## Check exportsp 
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", title="Export spatial layer?", 
                    first="NO", gui=gui)
  
  ## Check output parameters
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
      out_layer <- "layerprj"
    }
  }


  ##################################################################
  ## DO WORK
  ##################################################################

  layerxprj <- sf::st_transform(layerx, crs=crs.new)

  
  ## Output shapefile to outfolder
  if (exportsp) {
    spExportSpatial(layerxprj, 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer=out_layer,
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer, 
                            add_layer=TRUE))
  }

  return(layerxprj)

}
